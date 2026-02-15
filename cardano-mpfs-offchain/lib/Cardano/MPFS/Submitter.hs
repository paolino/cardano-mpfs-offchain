{- |
Module      : Cardano.MPFS.Submitter
Description : Transaction submission interface
License     : Apache-2.0
-}
module Cardano.MPFS.Submitter
    ( -- * Submitter interface
      Submitter (..)

      -- * Placeholder types
    , SubmitResult
    ) where

import Cardano.MPFS.Provider (Tx)

-- | Result of submitting a transaction (placeholder).
type SubmitResult = ()

-- | Interface for submitting transactions to the
-- blockchain.
data Submitter m = Submitter
    { submitTx :: Tx -> m SubmitResult
    -- ^ Submit a signed transaction
    }
