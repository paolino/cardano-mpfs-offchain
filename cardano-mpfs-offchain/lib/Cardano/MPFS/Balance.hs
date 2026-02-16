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
    , setMinFeeTx
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
-- input. The fee is estimated with a placeholder
-- change output carrying the full input value, so
-- the actual fee may be marginally lower than
-- charged (the change output shrinks).
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
        placeholder =
            mkBasicTxOut changeAddr (inject inputCoin)
        draftBody =
            body
                & inputsTxBodyL .~ newInputs
                & outputsTxBodyL
                    .~ (origOutputs |> placeholder)
        draftTx = tx & bodyTxL .~ draftBody
        -- One extra key witness for the fee input
        feeEstTx = setMinFeeTx pp draftTx 1
        fee = feeEstTx ^. bodyTxL . feeTxBodyL
        Coin available = inputCoin
        Coin required = fee
        changeAmount = available - required
    in  if changeAmount < 0
            then
                Left (InsufficientFee fee inputCoin)
            else
                let changeOut =
                        mkBasicTxOut
                            changeAddr
                            ( inject
                                (Coin changeAmount)
                            )
                    finalBody =
                        body
                            & inputsTxBodyL
                                .~ newInputs
                            & outputsTxBodyL
                                .~ ( origOutputs
                                        |> changeOut
                                   )
                            & feeTxBodyL .~ fee
                in  Right
                        (tx & bodyTxL .~ finalBody)
