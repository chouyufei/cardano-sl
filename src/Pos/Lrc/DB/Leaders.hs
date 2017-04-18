{-# LANGUAGE ScopedTypeVariables #-}

-- | Leaders part of LRC DB.

module Pos.Lrc.DB.Leaders
       (
         -- * Getters
         getLeaders

       -- * Operations
       , putLeaders

       -- * Initialization
       , prepareLrcLeaders
       ) where

import           Universum

import           Pos.Binary.Class  (encodeStrict)
import           Pos.Binary.Core   ()
import           Pos.Context.Class (WithNodeContext)
import           Pos.DB.Class      (MonadDB)
import           Pos.Lrc.DB.Common (getBi, putBi)
import           Pos.Types         (EpochIndex, SlotLeaders)

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

getLeaders :: MonadDB m => EpochIndex -> m (Maybe SlotLeaders)
getLeaders = getBi . leadersKey

----------------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------------

putLeaders :: MonadDB m => EpochIndex -> SlotLeaders -> m ()
putLeaders epoch = putBi (leadersKey epoch)

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareLrcLeaders
    :: forall ssc m.
       (WithNodeContext ssc m, MonadDB m)
    => m ()
prepareLrcLeaders = undefined

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

leadersKey :: EpochIndex -> ByteString
leadersKey = mappend "l/" . encodeStrict
