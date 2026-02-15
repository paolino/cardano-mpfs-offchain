{- |
Module      : Cardano.MPFS.TxBuilder
Description : Transaction construction interface
License     : Apache-2.0
-}
module Cardano.MPFS.TxBuilder
    ( -- * Transaction builder interface
      TxBuilder (..)
    ) where

import Data.ByteString (ByteString)

import Cardano.MPFS.Provider (Address, Tx)
import Cardano.MPFS.Types (OutputRef, TokenId)

-- | Interface for constructing transactions for
-- all MPFS protocol operations.
data TxBuilder m = TxBuilder
    { bootToken
        :: Address -> m Tx
    -- ^ Create a new MPFS token
    , requestInsert
        :: TokenId
        -> ByteString
        -> ByteString
        -> Address
        -> m Tx
    -- ^ Request inserting a key-value pair
    , requestDelete
        :: TokenId
        -> ByteString
        -> Address
        -> m Tx
    -- ^ Request deleting a key
    , updateToken
        :: TokenId -> Address -> m Tx
    -- ^ Process pending requests for a token
    , retractRequest
        :: OutputRef -> Address -> m Tx
    -- ^ Cancel a pending request
    , endToken
        :: TokenId -> Address -> m Tx
    -- ^ Retire an MPFS token
    }
