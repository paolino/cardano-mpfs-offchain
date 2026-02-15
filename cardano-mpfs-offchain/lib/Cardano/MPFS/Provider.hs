{- |
Module      : Cardano.MPFS.Provider
Description : Blockchain query interface
License     : Apache-2.0
-}
module Cardano.MPFS.Provider
    ( -- * Provider interface
      Provider (..)

      -- * Placeholder types
    , Address
    , UTxO
    , Tx
    , ProtocolParams
    , ExUnits
    ) where

-- | Blockchain address (placeholder).
type Address = ()

-- | Unspent transaction output (placeholder).
type UTxO = ()

-- | Serialised transaction (placeholder).
type Tx = ()

-- | Protocol parameters (placeholder).
type ProtocolParams = ()

-- | Execution units (placeholder).
type ExUnits = ()

-- | Interface for querying the blockchain.
data Provider m = Provider
    { queryUTxOs :: Address -> m [UTxO]
    -- ^ Look up UTxOs at an address
    , queryProtocolParams :: m ProtocolParams
    -- ^ Fetch current protocol parameters
    , evaluateTx :: Tx -> m ExUnits
    -- ^ Evaluate execution units for a transaction
    }
