{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- This module is to be moved later anywhere else, just to have a
-- starting point

-- | Types representing client (wallet) requests on wallet API.
module Pos.Wallet.Web.ClientTypes
      ( SyncProgress (..)
      , CId (..)
      , CHash (..)
      , CPassPhrase (..)
      , MCPassPhrase
      , CProfile (..)
      , CPwHash
      , CTx (..)
      , CTxId
      , CTxMeta (..)
      , CTExMeta (..)
      , CInitialized (..)
      , AccountId (..)
      , CAccountId (..)
      , CWAddressMeta (..)
      , CAddress (..)
      , CAccount (..)
      , CWalletAssurance (..)
      , CAccountMeta (..)
      , CAccountInit (..)
      , CWallet (..)
      , CWalletMeta (..)
      , CWalletInit (..)
      , CUpdateInfo (..)
      , CWalletRedeem (..)
      , CPaperVendWalletRedeem (..)
      , CCoin
      , mkCCoin
      , coinFromCCoin
      , PassPhraseLU
      , CElectronCrashReport (..)
      , NotifyEvent (..)
      , WithDerivationPath (..)
      , Wal (..)
      , Addr (..)
      , addressToCId
      , cIdToAddress
      , encToCId
      , passPhraseToCPassPhrase
      , cPassPhraseToPassPhrase
      , mkCTx
      , mkCTxId
      , txIdToCTxId
      , txContainsTitle
      , toCUpdateInfo
      , walletAddrMetaToAccount
      , fromCAccountId
      , toCAccountId
      ) where

import           Universum

import           Control.Arrow          ((&&&))
import qualified Data.ByteString.Lazy   as LBS
import           Data.Default           (Default, def)
import           Data.Hashable          (Hashable (..))
import           Data.Text              (Text, isInfixOf, splitOn, toLower)
import           Data.Text.Buildable    (build)
import           Data.Time.Clock.POSIX  (POSIXTime)
import           Data.Typeable          (Typeable)
import           Formatting             (bprint, sformat, (%))
import qualified Formatting             as F
import qualified Prelude
import qualified Serokell.Util.Base16   as Base16
import           Servant.Multipart      (FileData, FromMultipart (..), lookupFile,
                                         lookupInput)

import           Pos.Aeson.Types        ()
import           Pos.Binary.Class       (decodeFull, encodeStrict)
import           Pos.Client.Txp.History (TxHistoryEntry (..))
import           Pos.Core.Coin          (mkCoin)
import           Pos.Core.Types         (ScriptVersion)
import           Pos.Crypto             (EncryptedSecretKey, PassPhrase, encToPublic,
                                         hashHexF)
import           Pos.Txp.Core.Types     (Tx (..), TxId, TxOut, txOutAddress, txOutValue)
import           Pos.Types              (Address (..), BlockVersion, ChainDifficulty,
                                         Coin, SoftwareVersion, decodeTextAddress,
                                         makePubKeyAddress, sumCoins, unsafeGetCoin,
                                         unsafeIntegerToCoin)
import           Pos.Update.Core        (BlockVersionData (..), StakeholderVotes,
                                         UpdateProposal (..), isPositiveVote)
import           Pos.Update.Poll        (ConfirmedProposalState (..))
import           Pos.Util.BackupPhrase  (BackupPhrase)


data SyncProgress = SyncProgress
    { _spLocalCD   :: ChainDifficulty
    , _spNetworkCD :: Maybe ChainDifficulty
    , _spPeers     :: Word
    } deriving (Show, Generic, Typeable)

instance Default SyncProgress where
    def = SyncProgress 0 mzero 0

-- Notifications
data NotifyEvent
    = ConnectionOpened
    -- _ | NewWalletTransaction CId
    -- _ | NewTransaction
    | NetworkDifficultyChanged ChainDifficulty -- ie new block or fork (rollback)
    | LocalDifficultyChanged ChainDifficulty -- ie new block or fork (rollback)
    | ConnectedPeersChanged Word
    | UpdateAvailable
    | ConnectionClosed
    deriving (Show, Generic)

-- | Client hash
newtype CHash = CHash Text
    deriving (Show, Eq, Ord, Generic, Buildable)

instance Hashable CHash where
    hashWithSalt s (CHash h) = hashWithSalt s h

-- | Client address
-- @w@ is phantom type and stands for type of item this address belongs to.
newtype CId w = CId CHash
    deriving (Show, Eq, Ord, Generic, Hashable, Buildable)

-- | Marks address as belonging to wallet set.
data Wal = Wal
    deriving (Show, Generic)

-- | Marks address as belonging to account.
data Addr = Addr
    deriving (Show, Generic)

-- TODO: this is not completely safe. If someone changes
-- implementation of Buildable Address. It should be probably more
-- safe to introduce `class PSSimplified` that would have the same
-- implementation has it is with Buildable Address but then person
-- will know it will probably change something for purescript.
-- | Transform Address into CId
addressToCId :: Address -> CId w
addressToCId = CId . CHash . sformat F.build

cIdToAddress :: CId w -> Either Text Address
cIdToAddress (CId (CHash h)) = decodeTextAddress h

encToCId :: EncryptedSecretKey -> CId w
encToCId = addressToCId . makePubKeyAddress . encToPublic

-- | Client transaction id
newtype CTxId = CTxId CHash
    deriving (Show, Eq, Generic, Hashable)

mkCTxId :: Text -> CTxId
mkCTxId = CTxId . CHash

-- | transform TxId into CTxId
txIdToCTxId :: TxId -> CTxId
txIdToCTxId = mkCTxId . sformat hashHexF

convertTxOutputs :: [TxOut] -> [(CId w, CCoin)]
convertTxOutputs = map (addressToCId . txOutAddress &&& mkCCoin . txOutValue)

mkCTx
    :: ChainDifficulty    -- ^ Current chain difficulty (to get confirmations)
    -> TxHistoryEntry     -- ^ Tx history entry
    -> CTxMeta            -- ^ Transaction metadata
    -> CTx
mkCTx diff THEntry {..} meta = CTx {..}
  where
    ctId = txIdToCTxId _thTxId
    outputs = toList $ _txOutputs _thTx
    ctAmount = mkCCoin . unsafeIntegerToCoin . sumCoins $ map txOutValue outputs
    ctConfirmations = maybe 0 fromIntegral $ (diff -) <$> _thDifficulty
    ctMeta = meta
    ctInputAddrs = map addressToCId _thInputAddrs
    ctOutputAddrs = map addressToCId _thOutputAddrs

newtype CPassPhrase = CPassPhrase Text
    deriving (Eq, Generic)

-- | This is most common use case for 'CPassPhrase', as there is a default
-- value for it
type MCPassPhrase = Maybe CPassPhrase

instance Show CPassPhrase where
    show _ = "<pass phrase>"

passPhraseToCPassPhrase :: PassPhrase -> CPassPhrase
passPhraseToCPassPhrase passphrase =
    CPassPhrase . Base16.encode $ encodeStrict passphrase

cPassPhraseToPassPhrase
    :: CPassPhrase -> Either Text PassPhrase
cPassPhraseToPassPhrase (CPassPhrase text) =
    first toText . decodeFull . LBS.fromStrict =<< Base16.decode text

----------------------------------------------------------------------------
-- Wallet
----------------------------------------------------------------------------

-- | Wallet identifier
data AccountId = AccountId
    { -- | Address of wallet set this wallet belongs to
      aiWSId  :: CId Wal
    , -- | Derivation index of this wallet key
      aiIndex :: Word32
    } deriving (Eq, Show, Generic, Typeable)

instance Hashable AccountId

instance Buildable AccountId where
    build AccountId{..} =
        bprint (F.build%"@"%F.build) aiWSId aiIndex

newtype CAccountId = CAccountId Text
    deriving (Eq, Show, Generic, Buildable)

toCAccountId :: AccountId -> CAccountId
toCAccountId = CAccountId . sformat F.build

fromCAccountId :: CAccountId -> Either Text AccountId
fromCAccountId (CAccountId url) =
    case splitOn "@" url of
        [part1, part2] -> do
            aiWSId  <- addressToCId <$> decodeTextAddress part1
            aiIndex <- maybe (Left "Invalid wallet index") Right $
                            readMaybe $ toString part2
            return AccountId{..}
        _ -> Left "Expected 2 parts separated by '@'"

-- | Account identifier
data CWAddressMeta = CWAddressMeta
    { -- | Address of wallet set this account belongs to
      cwamWSId         :: CId Wal
    , -- | First index in derivation path of this account key
      caaWalletIndex   :: Word32
    , -- | Second index in derivation path of this account key
      cwamAccountIndex :: Word32
    , -- | Actual adress of this account
      cwamId           :: CId Addr
    } deriving (Eq, Ord, Show, Generic, Typeable)

instance Buildable CWAddressMeta where
    build CWAddressMeta{..} =
        bprint (F.build%"@"%F.build%"@"%F.build%" ("%F.build%")")
        cwamWSId caaWalletIndex cwamAccountIndex cwamId

walletAddrMetaToAccount :: CWAddressMeta -> AccountId
walletAddrMetaToAccount CWAddressMeta{..} = AccountId
    { aiWSId  = cwamWSId
    , aiIndex = caaWalletIndex
    }

instance Hashable CWAddressMeta

newtype CCoin = CCoin
    { getCCoin :: Text
    } deriving (Show, Eq, Generic)

mkCCoin :: Coin -> CCoin
mkCCoin = CCoin . show . unsafeGetCoin

coinFromCCoin :: CCoin -> Maybe Coin
coinFromCCoin = fmap mkCoin . readMaybe . toString . getCCoin

-- | Passphrase last update time
type PassPhraseLU = POSIXTime

-- | A level of assurance for the wallet "meta type"
data CWalletAssurance
    = CWAStrict
    | CWANormal
    deriving (Show, Eq, Generic)

-- | Single account in a wallet
data CAddress = CAddress
    { cadId     :: !(CId Addr)
    , cadAmount :: !CCoin
    } deriving (Show, Generic)

-- Includes data which are not provided by Cardano
data CAccountMeta = CAccountMeta
    { caName      :: !Text
    } deriving (Show, Generic)

instance Default CAccountMeta where
    def = CAccountMeta "Personal Wallet"

-- | Client Wallet (CW)
-- (Flow type: walletType)
data CAccount = CAccount
    { caId        :: !CAccountId
    , caMeta      :: !CAccountMeta
    , caAddresses :: ![CAddress]
    , caAmount    :: !CCoin
    } deriving (Show, Generic, Typeable)

-- | Query data for wallet creation
data CAccountInit = CAccountInit
    { caInitMeta :: !CAccountMeta
    , cwInitWId  :: !(CId Wal)
    } deriving (Show, Generic)

-- | Query data for redeem
data CWalletRedeem = CWalletRedeem
    { crWalletId :: !CAccountId
    , crSeed     :: !Text -- TODO: newtype!
    } deriving (Show, Generic)

-- | Meta data of 'CWallet'
data CWalletMeta = CWalletMeta
    { cwName      :: !Text
    , cwAssurance :: !CWalletAssurance
    , csUnit      :: !Int -- ^ https://issues.serokell.io/issue/CSM-163#comment=96-2480
    } deriving (Show, Eq, Generic)

instance Default CWalletMeta where
    def = CWalletMeta "Personal Wallet Set" CWANormal 0

-- | Client Wallet Set (CW)
data CWallet = CWallet
    { cwId             :: !(CId Wal)
    , cwMeta           :: !CWalletMeta
    , cwAccountsNumber :: !Int
    , cwAmount         :: !CCoin
    , cwHasPassphrase  :: !Bool
    , cwPassphraseLU   :: !PassPhraseLU  -- last update time
    } deriving (Eq, Show, Generic)

-- TODO: Newtype?
-- | Query data for wallet set creation
data CWalletInit = CWalletInit
    { cwInitMeta     :: !CWalletMeta
    , cwBackupPhrase :: !BackupPhrase
    } deriving (Eq, Show, Generic)

class WithDerivationPath a where
    getDerivationPath :: a -> [Word32]

instance WithDerivationPath (CId Wal) where
    getDerivationPath _ = []

instance WithDerivationPath AccountId where
    getDerivationPath AccountId{..} = [aiIndex]

instance WithDerivationPath CWAddressMeta where
    getDerivationPath CWAddressMeta{..} = [caaWalletIndex, cwamAccountIndex]

-- | Query data for redeem
data CPaperVendWalletRedeem = CPaperVendWalletRedeem
    { pvWalletId     :: !CAccountId
    , pvSeed         :: !Text -- TODO: newtype!
    , pvBackupPhrase :: !BackupPhrase
    } deriving (Show, Generic)

----------------------------------------------------------------------------
-- Profile
----------------------------------------------------------------------------

-- | Password hash of client profile
type CPwHash = Text -- or Base64 or something else

-- | Client profile (CP)
-- all data of client are "meta data" - that is not provided by Cardano
-- (Flow type: accountType)
-- TODO: Newtype?
data CProfile = CProfile
    { cpLocale      :: Text
    } deriving (Show, Generic, Typeable)

-- | Added default instance for `testReset`, we need an inital state for
-- @CProfile@
instance Default CProfile where
    def = CProfile mempty

----------------------------------------------------------------------------
-- Transactions
----------------------------------------------------------------------------

-- | meta data of transactions
data CTxMeta = CTxMeta
    { ctmTitle       :: Text
    , ctmDescription :: Text
    , ctmDate        :: POSIXTime
    } deriving (Show, Generic)

-- | Client transaction (CTx)
-- Provides all Data about a transaction needed by client.
-- It includes meta data which are not part of Cardano, too
-- (Flow type: transactionType)
data CTx = CTx
    { ctId            :: CTxId
    , ctAmount        :: CCoin
    , ctConfirmations :: Word
    , ctMeta          :: CTxMeta
    , ctInputAddrs    :: [CId Addr]
    , ctOutputAddrs   :: [CId Addr]
    } deriving (Show, Generic, Typeable)

txContainsTitle :: Text -> CTx -> Bool
txContainsTitle search = isInfixOf (toLower search) . toLower . ctmTitle . ctMeta

-- | meta data of exchanges
data CTExMeta = CTExMeta
    { cexTitle       :: Text
    , cexDescription :: Text
    , cexDate        :: POSIXTime
    , cexRate        :: Text
    , cexLabel       :: Text -- counter part of client's 'exchange' value
    , cexId          :: CId Addr
    } deriving (Show, Generic)

-- | Update system data
data CUpdateInfo = CUpdateInfo
    { cuiSoftwareVersion :: !SoftwareVersion
    , cuiBlockVesion     :: !BlockVersion
    , cuiScriptVersion   :: !ScriptVersion
    , cuiImplicit        :: !Bool
--    , cuiProposed        :: !HeaderHash
--    , cuiDecided         :: !HeaderHash
--    , cuiConfirmed       :: !HeaderHash
--    , cuiAdopted         :: !(Maybe HeaderHash)
    , cuiVotesFor        :: !Int
    , cuiVotesAgainst    :: !Int
    , cuiPositiveStake   :: !CCoin
    , cuiNegativeStake   :: !CCoin
    } deriving (Show, Generic, Typeable)

-- | Return counts of negative and positive votes
countVotes :: StakeholderVotes -> (Int, Int)
countVotes = foldl' counter (0, 0)
  where counter (n, m) vote = if isPositiveVote vote
                              then (n + 1, m)
                              else (n, m + 1)

-- | Creates 'CTUpdateInfo' from 'ConfirmedProposalState'
toCUpdateInfo :: ConfirmedProposalState -> CUpdateInfo
toCUpdateInfo ConfirmedProposalState {..} =
    let UnsafeUpdateProposal {..} = cpsUpdateProposal
        cuiSoftwareVersion  = upSoftwareVersion
        cuiBlockVesion      = upBlockVersion
        cuiScriptVersion    = bvdScriptVersion upBlockVersionData
        cuiImplicit         = cpsImplicit
--        cuiProposed         = cpsProposed
--        cuiDecided          = cpsDecided
--        cuiConfirmed        = cpsConfirmed
--        cuiAdopted          = cpsAdopted
        (cuiVotesFor, cuiVotesAgainst) = countVotes cpsVotes
        cuiPositiveStake    = mkCCoin cpsPositiveStake
        cuiNegativeStake    = mkCCoin cpsNegativeStake
    in CUpdateInfo {..}

----------------------------------------------------------------------------
-- Reportin
----------------------------------------------------------------------------

-- | Represents a knowledge about how much time did it take for client
-- (wallet) to initialize. All numbers are milliseconds.
data CInitialized = CInitialized
    { cTotalTime :: Word -- ^ Total time from very start to main
                            -- post-sync screen.
    , cPreInit   :: Word -- ^ Time passed from beginning to network
                            -- connection with peers established.
    } deriving (Show, Generic)


data CElectronCrashReport = CElectronCrashReport
    { cecVersion     :: Text
    , cecPlatform    :: Text
    , cecProcessType :: Text
    , cecGuid        :: Text
    , cecVersionJson :: Text
    , cecProductName :: Text
    , cecProd        :: Text
    , cecCompanyName :: Text
    , cecUploadDump  :: FileData
    } deriving (Show, Generic)

instance FromMultipart CElectronCrashReport where
    fromMultipart form = do
        let look t = lookupInput t form
        CElectronCrashReport
          <$> look "ver"
          <*> look "platform"
          <*> look "process_type"
          <*> look "guid"
          <*> look "_version"
          <*> look "_productName"
          <*> look "prod"
          <*> look "_companyName"
          <*> lookupFile "upload_file_minidump" form
