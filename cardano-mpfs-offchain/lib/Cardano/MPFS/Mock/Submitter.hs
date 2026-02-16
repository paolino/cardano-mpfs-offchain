{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Cardano.MPFS.Mock.Submitter
-- Description : No-op mock Submitter implementation
-- License     : Apache-2.0
--
-- Provides a mock 'Submitter IO' that always reports
-- rejection. Useful for testing components that
-- build but don't submit transactions.
module Cardano.MPFS.Mock.Submitter
    ( -- * Construction
      mkMockSubmitter
    ) where

import Cardano.MPFS.Submitter
    ( SubmitResult (..)
    , Submitter (..)
    )

-- | Create a mock 'Submitter IO'. Rejects all
-- transactions with a descriptive message.
mkMockSubmitter :: Submitter IO
mkMockSubmitter =
    Submitter
        { submitTx = \_ ->
            pure
                $ Rejected
                    "mock submitter: not connected"
        }
