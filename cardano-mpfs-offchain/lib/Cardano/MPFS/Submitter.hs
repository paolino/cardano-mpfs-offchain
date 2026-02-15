{- |
Module      : Cardano.MPFS.Submitter
Description : Transaction submission interface
License     : Apache-2.0
-}
module Cardano.MPFS.Submitter
    ( -- * Submitter interface
      Submitter (..)

      -- * Result type
    , SubmitResult (..)
    ) where

import Data.ByteString (ByteString)

import Cardano.MPFS.Types (TxCBOR, TxHash)

-- | Result of submitting a transaction.
data SubmitResult
    = -- | Transaction accepted into the mempool
      Submitted !TxHash
    | -- | Transaction was rejected
      Rejected !ByteString
    -- ^ Rejection reason (UTF-8 encoded)

-- | Interface for submitting transactions to the
-- blockchain.
data Submitter m = Submitter
    { submitTx :: TxCBOR -> m SubmitResult
    -- ^ Submit a signed transaction
    }
