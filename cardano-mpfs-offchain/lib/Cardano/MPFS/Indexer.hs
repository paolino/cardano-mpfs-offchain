{- |
Module      : Cardano.MPFS.Indexer
Description : Chain sync follower interface
License     : Apache-2.0
-}
module Cardano.MPFS.Indexer
    ( -- * Indexer interface
      Indexer (..)
    ) where

import Cardano.MPFS.Types (SlotNo)

-- | Interface for the chain sync indexer that
-- follows the blockchain and updates local state.
data Indexer m = Indexer
    { start :: m ()
    -- ^ Start the indexer
    , stop :: m ()
    -- ^ Stop the indexer
    , pause :: m ()
    -- ^ Pause indexing
    , resume :: m ()
    -- ^ Resume indexing after a pause
    , getTip :: m SlotNo
    -- ^ Get the current chain tip slot
    }
