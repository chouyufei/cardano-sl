-- | Genesis values related to GodTossing SSC.

module Pos.Ssc.GodTossing.Genesis
       ( GenesisGtData(..)
       , compileGenGtData

       , genesisCertificates
       , genesisDevVssKeyPairs
       ) where

import qualified Data.HashMap.Strict               as HM
import           Data.List                         (zipWith3)
import qualified Data.Text                         as T
import           Formatting                        (int, sformat, (%))
import           Universum

import           Pos.Binary.Class                  (asBinary)
import           Pos.Core                          (EpochIndex (..), genesisDevKeyPairs)
import           Pos.Core.Address                  (addressHash)
import           Pos.Core.Constants                (genesisN, isDevelopment)
import           Pos.Crypto                        (VssKeyPair, VssPublicKey,
                                                    deterministicVssKeyGen,
                                                    toVssPublicKey)
import           Pos.Ssc.GodTossing.Constants      (vssMaxTTL, vssMinTTL)
import           Pos.Ssc.GodTossing.Core.Types     (VssCertificatesMap, mkVssCertificate)
import           Pos.Ssc.GodTossing.Genesis.Parser (compileGenGtData)
import           Pos.Ssc.GodTossing.Genesis.Types  (GenesisGtData (..))

-- | List of 'VssKeyPair's in genesis.
genesisDevVssKeyPairs :: [VssKeyPair]
genesisDevVssKeyPairs = map gen [0 .. genesisN - 1]
  where
    gen :: Int -> VssKeyPair
    gen =
        deterministicVssKeyGen .
        encodeUtf8 .
        T.take 32 .
        sformat ("My awesome 32-byte seed :) #" %int % "             ")

-- | List of 'VssPublicKey' in genesis.
genesisDevVssPublicKeys :: [VssPublicKey]
genesisDevVssPublicKeys = map toVssPublicKey genesisDevVssKeyPairs

-- | Certificates in genesis represented as 'VssCertificatesMap'.
genesisCertificates :: VssCertificatesMap
genesisCertificates
    | isDevelopment = case certEntries of
          c0:c1:_:cs -> HM.fromList $ c0 : c1 : cs
          _          -> error "genesisCertificates: can't happen"
    | otherwise     = ggdVssCertificates compileGenGtData
  where
    ttlExp :: Int -> EpochIndex
    ttlExp 1 = EpochIndex vssMinTTL - 1
    ttlExp _ = vssMaxTTL - 1

    mkCertEntry i (pk, sk) vssPk =
        (addressHash pk, mkVssCertificate sk (asBinary vssPk) (ttlExp i))

    certEntries = zipWith3 mkCertEntry
                    [0..]
                    genesisDevKeyPairs
                    genesisDevVssPublicKeys
