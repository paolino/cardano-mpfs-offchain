-- |
-- Module      : Cardano.MPFS.Mock.TxBuilder
-- Description : No-op mock TxBuilder implementation
-- License     : Apache-2.0
--
-- Provides a mock 'TxBuilder IO' that throws on all
-- operations. Transaction building requires real
-- protocol params and UTxOs; the mock is a
-- placeholder for type-checking and wiring tests.
module Cardano.MPFS.Mock.TxBuilder
    ( -- * Construction
      mkMockTxBuilder
    ) where

import Cardano.MPFS.TxBuilder (TxBuilder (..))

-- | Create a mock 'TxBuilder IO'. All operations
-- throw with a descriptive error message.
mkMockTxBuilder :: TxBuilder IO
mkMockTxBuilder =
    TxBuilder
        { bootToken = \_ ->
            error
                "mkMockTxBuilder: bootToken \
                \not implemented"
        , requestInsert = \_ _ _ _ ->
            error
                "mkMockTxBuilder: requestInsert \
                \not implemented"
        , requestDelete = \_ _ _ ->
            error
                "mkMockTxBuilder: requestDelete \
                \not implemented"
        , updateToken = \_ _ ->
            error
                "mkMockTxBuilder: updateToken \
                \not implemented"
        , retractRequest = \_ _ ->
            error
                "mkMockTxBuilder: retractRequest \
                \not implemented"
        , endToken = \_ _ ->
            error
                "mkMockTxBuilder: endToken \
                \not implemented"
        }
