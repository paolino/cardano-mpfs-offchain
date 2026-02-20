{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Cardano.MPFS.Indexer.Columns
-- Description : Column family GADT for cage persistent state
-- License     : Apache-2.0
--
-- Type-safe column family definitions for the cage
-- indexer's RocksDB-backed persistent state. Each
-- constructor selects a column with its key-value
-- types enforced at the type level.
module Cardano.MPFS.Indexer.Columns
    ( -- * Column selector
      CageColumns (..)

      -- * Checkpoint type
    , CageCheckpoint (..)
    ) where

import Control.Lens (type (:~:) (..))
import Database.KV.Transaction
    ( GCompare (..)
    , GEq (..)
    , GOrdering (..)
    , KV
    )

import Cardano.MPFS.Types
    ( BlockId
    , Request
    , SlotNo
    , TokenId
    , TokenState
    , TxIn
    )

-- | Chain sync checkpoint stored in the cage-cfg
-- column family.
data CageCheckpoint = CageCheckpoint
    { checkpointSlot :: !SlotNo
    -- ^ Slot of the last processed block
    , checkpointBlockId :: !BlockId
    -- ^ Header hash of the last processed block
    }
    deriving stock (Eq, Show)

-- | Column family selector for cage persistent
-- state. Each constructor identifies a column and
-- encodes its key-value types.
data CageColumns x where
    -- | Token state: maps token identifiers to
    -- their on-chain state.
    CageTokens
        :: CageColumns (KV TokenId TokenState)
    -- | Pending requests: maps UTxO references to
    -- request details.
    CageRequests
        :: CageColumns (KV TxIn Request)
    -- | Singleton checkpoint: stores the last
    -- processed block position.
    CageCfg
        :: CageColumns (KV () CageCheckpoint)

instance GEq CageColumns where
    geq CageTokens CageTokens = Just Refl
    geq CageRequests CageRequests = Just Refl
    geq CageCfg CageCfg = Just Refl
    geq _ _ = Nothing

instance GCompare CageColumns where
    gcompare CageTokens CageTokens = GEQ
    gcompare CageTokens _ = GLT
    gcompare _ CageTokens = GGT
    gcompare CageRequests CageRequests = GEQ
    gcompare CageRequests CageCfg = GLT
    gcompare CageCfg CageRequests = GGT
    gcompare CageCfg CageCfg = GEQ
