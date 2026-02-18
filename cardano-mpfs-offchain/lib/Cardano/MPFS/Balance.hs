{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Cardano.MPFS.Balance
-- Description : Simple transaction balancing
-- License     : Apache-2.0
--
-- Balance a transaction by adding a fee-paying UTxO
-- and a change output. The fee is estimated via
-- 'setMinFeeTx' from @cardano-ledger-api@.
module Cardano.MPFS.Balance
    ( -- * Balancing
      balanceTx

      -- * Errors
    , BalanceError (..)
    ) where

import Data.Sequence.Strict ((|>))
import Data.Set qualified as Set
import Lens.Micro ((&), (.~), (^.))

import Cardano.Ledger.Api.Tx
    ( Tx
    , bodyTxL
    , estimateMinFeeTx
    )
import Cardano.Ledger.Api.Tx.Body
    ( feeTxBodyL
    , inputsTxBodyL
    , outputsTxBodyL
    )
import Cardano.Ledger.Api.Tx.Out
    ( TxOut
    , coinTxOutL
    , mkBasicTxOut
    )
import Cardano.Ledger.BaseTypes (Inject (..))

import Cardano.MPFS.Types
    ( Addr
    , Coin (..)
    , ConwayEra
    , PParams
    , TxIn
    )

-- | Fee-paying UTxO has insufficient ada.
data BalanceError
    = -- | @InsufficientFee required available@
      InsufficientFee !Coin !Coin
    deriving (Eq, Show)

-- | Balance a transaction by adding a fee-paying
-- UTxO and a change output.
--
-- One additional key witness is assumed for the fee
-- input. The fee is found by iterating
-- 'setMinFeeTx' to a fixpoint: each round builds
-- the full transaction (with change output and fee
-- field set) and re-estimates until the fee
-- stabilises.
balanceTx
    :: PParams ConwayEra
    -> (TxIn, TxOut ConwayEra)
    -- ^ Fee-paying UTxO
    -> Addr
    -- ^ Change address
    -> Tx ConwayEra
    -- ^ Unbalanced transaction
    -> Either BalanceError (Tx ConwayEra)
balanceTx pp (feeInput, feeUtxo) changeAddr tx =
    let body = tx ^. bodyTxL
        inputCoin = feeUtxo ^. coinTxOutL
        newInputs =
            Set.insert
                feeInput
                (body ^. inputsTxBodyL)
        origOutputs = body ^. outputsTxBodyL
        -- Build a candidate tx for a given fee.
        -- Change is clamped to 0 so fee estimation
        -- works even when funds are insufficient.
        buildTx f =
            let Coin avail = inputCoin
                Coin req = f
                change = max 0 (avail - req)
                changeOut =
                    mkBasicTxOut
                        changeAddr
                        (inject (Coin change))
                finalBody =
                    body
                        & inputsTxBodyL
                            .~ newInputs
                        & outputsTxBodyL
                            .~ ( origOutputs
                                    |> changeOut
                               )
                        & feeTxBodyL .~ f
            in  tx & bodyTxL .~ finalBody
        -- Iterate until the fee stabilises.
        go !n currentFee
            | n > (10 :: Int) =
                error
                    "balanceTx: fee did not \
                    \converge in 10 iterations"
            | otherwise =
                let candidate =
                        buildTx currentFee
                    newFee =
                        estimateMinFeeTx
                            pp
                            candidate
                            1 -- key witnesses
                            0 -- Byron witnesses
                            0 -- ref scripts bytes
                in  if newFee <= currentFee
                        then currentFee
                        else go (n + 1) newFee
        initFee = Coin 0
        fee = go 0 initFee
        Coin available = inputCoin
        Coin required = fee
        changeAmount = available - required
    in  if changeAmount < 0
            then
                Left (InsufficientFee fee inputCoin)
            else Right (buildTx fee)
