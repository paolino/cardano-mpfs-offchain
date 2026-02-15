{- |
Module      : Cardano.MPFS.Provider
Description : Blockchain query interface
License     : Apache-2.0
-}
module Cardano.MPFS.Provider
    ( -- * Provider interface
      Provider (..)

      -- * Chain data types
    , UTxO (..)
    , Value (..)
    , ProtocolParams (..)
    , ExUnits (..)
    ) where

import Data.ByteString (ByteString)
import Numeric.Natural (Natural)

import Cardano.MPFS.Types
    ( Address
    , AssetName
    , Lovelace
    , OutputRef
    , PolicyId
    , TxCBOR
    )

-- | Multi-asset value on chain.
data Value = Value
    { lovelace :: !Lovelace
    -- ^ Ada amount in lovelace
    , assets :: ![(PolicyId, AssetName, Natural)]
    -- ^ Native assets as flat triples
    }

-- | Unspent transaction output.
data UTxO = UTxO
    { utxoRef :: !OutputRef
    -- ^ The output reference
    , utxoAddress :: !Address
    -- ^ The address holding the UTxO
    , utxoValue :: !Value
    -- ^ The value locked in the UTxO
    , utxoDatumHash :: !(Maybe ByteString)
    -- ^ Optional datum hash (32 bytes)
    , utxoInlineDatum :: !(Maybe ByteString)
    -- ^ Optional inline datum (CBOR bytes)
    , utxoReferenceScript
        :: !(Maybe ByteString)
    -- ^ Optional reference script (CBOR bytes)
    }

-- | Protocol parameters (opaque CBOR blob).
newtype ProtocolParams = ProtocolParams
    { unProtocolParams :: ByteString
    }

-- | Execution units for script evaluation.
data ExUnits = ExUnits
    { memory :: !Natural
    -- ^ Memory units
    , steps :: !Natural
    -- ^ CPU steps
    }

-- | Interface for querying the blockchain.
data Provider m = Provider
    { queryUTxOs :: Address -> m [UTxO]
    -- ^ Look up UTxOs at an address
    , queryProtocolParams :: m ProtocolParams
    -- ^ Fetch current protocol parameters
    , evaluateTx :: TxCBOR -> m ExUnits
    -- ^ Evaluate execution units for a transaction
    }
