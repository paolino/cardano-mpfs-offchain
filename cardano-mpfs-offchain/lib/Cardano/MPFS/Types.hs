{- |
Module      : Cardano.MPFS.Types
Description : Core domain types for the MPFS offchain service
License     : Apache-2.0
-}
module Cardano.MPFS.Types
    ( -- * Cardano primitives
      Address (..)
    , TxHash (..)
    , TxCBOR (..)
    , VerificationKeyHash (..)
    , PolicyId (..)
    , AssetName (..)
    , Lovelace (..)

      -- * Token identification
    , TokenId (..)

      -- * Transaction references
    , OutputRef (..)

      -- * Merkle Patricia Forestry
    , Root (..)
    , Operation (..)

      -- * Requests
    , Request (..)

      -- * Token state
    , TokenState (..)

      -- * Facts
    , Fact (..)

      -- * Chain position
    , SlotNo (..)
    , BlockId (..)
    ) where

import Data.ByteString (ByteString)
import Data.Word (Word64)

-- | Serialised Cardano address.
newtype Address = Address
    { unAddress :: ByteString
    }

-- | Transaction hash (32 bytes Blake2b-256).
newtype TxHash = TxHash
    { unTxHash :: ByteString
    }

-- | Serialised CBOR transaction.
newtype TxCBOR = TxCBOR
    { unTxCBOR :: ByteString
    }

-- | Verification key hash (28 bytes Blake2b-224).
newtype VerificationKeyHash = VerificationKeyHash
    { unVerificationKeyHash :: ByteString
    }

-- | Policy ID (28 bytes, hash of minting script).
newtype PolicyId = PolicyId
    { unPolicyId :: ByteString
    }

-- | Asset name (0–32 bytes).
newtype AssetName = AssetName
    { unAssetName :: ByteString
    }

-- | Lovelace amount.
newtype Lovelace = Lovelace
    { unLovelace :: Word64
    }

-- | Unique identifier for a token managed by the
-- MPFS service. Corresponds to the on-chain
-- asset name derived from SHA2-256(txId ++ index).
newtype TokenId = TokenId
    { unTokenId :: ByteString
    }

-- | Reference to a specific UTxO on chain.
data OutputRef = OutputRef
    { txId :: !TxHash
    -- ^ Transaction hash
    , index :: !Word64
    -- ^ Output index within the transaction
    }

-- | MPF root hash representing the current state
-- of a trie.
newtype Root = Root
    { unRoot :: ByteString
    }

-- | An operation to perform on a key in the trie.
data Operation
    = -- | Insert a new key-value pair
      Insert !ByteString
    | -- | Delete a key
      Delete !ByteString
    | -- | Update an existing key with a new value
      Update !ByteString !ByteString

-- | A request to modify a token's trie.
data Request = Request
    { requestToken :: !TokenId
    -- ^ The token whose trie is being modified
    , requestOwner :: !VerificationKeyHash
    -- ^ The owner's verification key hash
    , requestKey :: !ByteString
    -- ^ The key to operate on
    , requestValue :: !Operation
    -- ^ The operation to perform
    }

-- | Current on-chain state of a token.
data TokenState = TokenState
    { owner :: !VerificationKeyHash
    -- ^ Owner's verification key hash
    , root :: !Root
    -- ^ Current root hash of the token's trie
    }

-- | A key-value fact stored in a trie.
data Fact = Fact
    { key :: !ByteString
    -- ^ The fact's key
    , value :: !ByteString
    -- ^ The fact's value
    }

-- | Slot number on the Cardano blockchain.
newtype SlotNo = SlotNo
    { unSlotNo :: Word64
    }

-- | Block identifier.
newtype BlockId = BlockId
    { unBlockId :: ByteString
    }
