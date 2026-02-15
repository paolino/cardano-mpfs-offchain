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

import Cardano.MPFS.Types
    ( Address
    , OutputRef
    , TxCBOR
    , TokenId
    )

-- | Interface for constructing transactions for
-- all MPFS protocol operations.
data TxBuilder m = TxBuilder
    { bootToken
        :: Address -> m TxCBOR
    -- ^ Create a new MPFS token
    , requestInsert
        :: TokenId
        -> ByteString
        -> ByteString
        -> Address
        -> m TxCBOR
    -- ^ Request inserting a key-value pair
    , requestDelete
        :: TokenId
        -> ByteString
        -> Address
        -> m TxCBOR
    -- ^ Request deleting a key
    , updateToken
        :: TokenId -> Address -> m TxCBOR
    -- ^ Process pending requests for a token
    , retractRequest
        :: OutputRef -> Address -> m TxCBOR
    -- ^ Cancel a pending request
    , endToken
        :: TokenId -> Address -> m TxCBOR
    -- ^ Retire an MPFS token
    }
