{-# LANGUAGE RankNTypes #-}

{- |
Module      : Cardano.MPFS.Trie
Description : Per-token MPF trie management interface
License     : Apache-2.0
-}
module Cardano.MPFS.Trie
    ( -- * Trie manager
      TrieManager (..)

      -- * Single trie operations
    , Trie (..)

      -- * Placeholder types
    , Proof
    ) where

import Data.ByteString (ByteString)

import Cardano.MPFS.Types (Root, TokenId)

-- | Merkle proof (placeholder).
type Proof = ()

-- | Manager for per-token tries.
data TrieManager m = TrieManager
    { withTrie
        :: forall a. TokenId -> (Trie m -> m a) -> m a
    -- ^ Run an action with access to a token's trie
    , createTrie :: TokenId -> m ()
    -- ^ Create a new empty trie for a token
    , deleteTrie :: TokenId -> m ()
    -- ^ Delete a token's trie
    }

-- | Operations on a single trie.
data Trie m = Trie
    { insert
        :: ByteString -> ByteString -> m Root
    -- ^ Insert a key-value pair, returning new root
    , delete :: ByteString -> m Root
    -- ^ Delete a key, returning new root
    , lookup :: ByteString -> m (Maybe ByteString)
    -- ^ Look up a value by key
    , getRoot :: m Root
    -- ^ Get current root hash
    , getProof :: ByteString -> m Proof
    -- ^ Generate a Merkle proof for a key
    }
