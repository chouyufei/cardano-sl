{-# LANGUAGE TemplateHaskell #-}

-- | Module for safe (zero-memory) signing

module Pos.Crypto.SafeSigning
       ( EncryptedSecretKey (..)
       , PassPhrase
       , SafeSigner
       , emptyPassphrase
       , noPassEncrypt
       , checkPassMatches
       , changeEncPassphrase
       , encToPublic
       , mkEncSecret
       , mkEncSecretWithSalt
       , safeSign
       , safeToPublic
       , safeKeyGen
       , safeDeterministicKeyGen
       , withSafeSigner
       , fakeSigner
       , createProxyCert
       , createProxySecretKey
       , safeCreateProxyCert
       , safeCreateProxySecretKey
       ) where

import qualified Cardano.Crypto.Wallet as CC
import           Data.ByteArray        (ByteArray, ByteArrayAccess, ScrubbedBytes)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as BSL
import           Data.Coerce           (coerce)
import           Data.Default          (def)
import           Data.Text.Buildable   (build)
import qualified Data.Text.Buildable   as B
import qualified Prelude
import           Universum

import           Pos.Binary.Class      (Bi, Raw)
import qualified Pos.Binary.Class      as Bi
import           Pos.Crypto.Random     (secureRandomBS)
import qualified Pos.Crypto.Scrypt     as S
import           Pos.Crypto.Signing    (ProxyCert (..), ProxySecretKey (..),
                                        PublicKey (..), SecretKey (..), Signature (..),
                                        sign, toPublic)
import           Pos.Crypto.SignTag    (SignTag (SignProxySK), signTag)

data EncryptedSecretKey = EncryptedSecretKey
    { eskPayload :: !CC.XPrv
    , eskHash    :: !S.EncryptedPass
    }

instance Show EncryptedSecretKey where
    show _ = "<encrypted key>"

instance B.Buildable EncryptedSecretKey where
    build _ = "<encrypted key>"

newtype PassPhrase = PassPhrase ScrubbedBytes
    deriving (Eq, Ord, Monoid, NFData, ByteArray, ByteArrayAccess)

instance Show PassPhrase where
    show _ = "<passphrase>"

instance Buildable PassPhrase where
    build _ = "<passphrase>"

-- | Empty passphrase used in development.
emptyPassphrase :: PassPhrase
emptyPassphrase = PassPhrase mempty

-- | Parameters used to evaluate hash of passphrase.
passScryptParam :: S.ScryptParams
passScryptParam =
    fromMaybe (error "Bad passphrase scrypt parameters") $
    S.mkScryptParams def
        { S.spHashLen = 32  -- maximal passphrase length
        }

-- | Wrap raw secret key, attaching hash to it.
-- Hash is evaluated using given salt.
mkEncSecretWithSalt
    :: Bi PassPhrase
    => S.Salt -> PassPhrase -> CC.XPrv -> EncryptedSecretKey
mkEncSecretWithSalt salt pp payload =
    EncryptedSecretKey payload $ S.encryptPassWithSalt passScryptParam salt pp

-- | Wrap raw secret key, attachind hash to it.
-- Hash is evaluated using generated salt.
mkEncSecret
    :: (Bi PassPhrase, MonadIO m)
    => PassPhrase -> CC.XPrv -> m EncryptedSecretKey
mkEncSecret pp payload =
    EncryptedSecretKey payload <$> S.encryptPassIO passScryptParam pp

-- | Generate a public key using an encrypted secret key and passphrase
encToPublic :: EncryptedSecretKey -> PublicKey
encToPublic (EncryptedSecretKey sk _) = PublicKey (CC.toXPub sk)

-- | Re-wrap unencrypted secret key as an encrypted one
noPassEncrypt
    :: Bi PassPhrase
    => SecretKey -> EncryptedSecretKey
noPassEncrypt (SecretKey k) =
    mkEncSecretWithSalt (S.mkSalt ("Potak" :: Text)) emptyPassphrase k

checkPassMatches
    :: (Bi PassPhrase, Alternative f)
    => PassPhrase -> EncryptedSecretKey -> f ()
checkPassMatches pp (EncryptedSecretKey _ pph) =
    guard (S.verifyPass passScryptParam pp pph)

-- | Regerates secret key with new passphrase.
-- This operation remains corresponding public key unchanged.
-- However, derived (child) keys change.
changeEncPassphrase
    :: (Bi PassPhrase, MonadIO m)
    => PassPhrase
    -> PassPhrase
    -> EncryptedSecretKey
    -> m (Maybe EncryptedSecretKey)
changeEncPassphrase oldPass newPass esk@(EncryptedSecretKey sk _)
    | isJust $ checkPassMatches oldPass esk =
        Just <$> mkEncSecret newPass (CC.xPrvChangePass oldPass newPass sk)
    | otherwise = return Nothing

signRaw' :: Maybe SignTag
         -> PassPhrase
         -> EncryptedSecretKey
         -> ByteString
         -> Signature Raw
signRaw' mbTag (PassPhrase pp) (EncryptedSecretKey sk _) x =
    Signature (CC.sign pp sk (tag <> x))
  where
    tag = maybe mempty signTag mbTag

sign'
    :: Bi a
    => SignTag -> PassPhrase -> EncryptedSecretKey -> a -> Signature a
sign' t pp sk = coerce . signRaw' (Just t) pp sk . BSL.toStrict . Bi.encode

safeCreateKeypairFromSeed
    :: BS.ByteString
    -> PassPhrase
    -> Maybe (CC.XPub, CC.XPrv)
safeCreateKeypairFromSeed seed (PassPhrase pp) = do
    prv <- CC.generate seed pp
    return (CC.toXPub prv, prv)

safeKeyGen
    :: (MonadIO m, Bi PassPhrase)
    => PassPhrase -> m (PublicKey, EncryptedSecretKey)
safeKeyGen pp = liftIO $ do
    seed <- secureRandomBS 32
    case safeCreateKeypairFromSeed seed pp of
        Nothing -> error "Pos.Crypto.SafeSigning.safeKeyGen:\
                         \ creating keypair from seed failed"
        Just (pk, sk) -> do
            encSk <- mkEncSecret pp sk
            return (PublicKey pk, encSk)

safeDeterministicKeyGen
    :: Bi PassPhrase
    => BS.ByteString
    -> PassPhrase
    -> Maybe (PublicKey, EncryptedSecretKey)
safeDeterministicKeyGen seed pp =
    bimap PublicKey (mkEncSecretWithSalt (S.mkSalt seed) pp) <$>
    safeCreateKeypairFromSeed seed pp

-- | SafeSigner datatype to encapsulate sensible data
data SafeSigner = SafeSigner EncryptedSecretKey PassPhrase
                | FakeSigner SecretKey

safeSign :: Bi a => SignTag -> SafeSigner -> a -> Signature a
safeSign t (SafeSigner sk pp) = sign' t pp sk
safeSign t (FakeSigner sk)    = sign t sk

safeToPublic :: SafeSigner -> PublicKey
safeToPublic (SafeSigner sk _) = encToPublic sk
safeToPublic (FakeSigner sk)   = toPublic sk

-- | We can make SafeSigner only inside IO bracket, so
-- we can manually cleanup all IO buffers we use to store passphrase
-- (when we'll actually use them)
withSafeSigner
    :: (MonadIO m, Bi PassPhrase)
    => EncryptedSecretKey
    -> m PassPhrase
    -> (Maybe SafeSigner -> m a)
    -> m a
withSafeSigner sk ppGetter action = do
    pp <- ppGetter
    let mss = checkPassMatches pp sk $> SafeSigner sk pp
    action mss

-- | We need this to be able to perform signing with unencrypted `SecretKey`s,
-- where `SafeSigner` is required
fakeSigner :: SecretKey -> SafeSigner
fakeSigner = FakeSigner

-- [CSL-1157] `createProxyCert` and `createProxySecretKey` are not safe and
--   left here because of their implementation details
--   in future should be removed completely, now left for compatibility with tests

-- | Proxy certificate creation from secret key of issuer, public key
-- of delegate and the message space ω.
createProxyCert :: (Bi w) => SecretKey -> PublicKey -> w -> ProxyCert w
createProxyCert = safeCreateProxyCert . fakeSigner

-- | Creates proxy secret key
createProxySecretKey :: (Bi w) => SecretKey -> PublicKey -> w -> ProxySecretKey w
createProxySecretKey = safeCreateProxySecretKey . fakeSigner

-- | Proxy certificate creation from secret key of issuer, public key
-- of delegate and the message space ω.
safeCreateProxyCert :: (Bi w) => SafeSigner -> PublicKey -> w -> ProxyCert w
safeCreateProxyCert ss (PublicKey delegatePk) o = coerce $ ProxyCert sig
  where
    Signature sig = safeSign SignProxySK ss $
                      mconcat
                          ["00", CC.unXPub delegatePk, Bi.encodeStrict o]

-- | Creates proxy secret key
safeCreateProxySecretKey :: (Bi w) => SafeSigner -> PublicKey -> w -> ProxySecretKey w
safeCreateProxySecretKey ss delegatePk w =
    ProxySecretKey w (safeToPublic ss) delegatePk $ safeCreateProxyCert ss delegatePk w
