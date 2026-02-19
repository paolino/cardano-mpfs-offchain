-- |
-- Module      : Cardano.MPFS.TxBuilder.Real.Retract
-- Description : Retract request transaction
-- License     : Apache-2.0
module Cardano.MPFS.TxBuilder.Real.Retract
    ( retractRequestImpl
    ) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Lens.Micro ((&), (.~))

import Cardano.Ledger.Address (Addr)
import Cardano.Ledger.Alonzo.Scripts (AsIx (..))
import Cardano.Ledger.Api.Tx
    ( Tx
    , mkBasicTx
    , witsTxL
    )
import Cardano.Ledger.Api.Tx.Body
    ( collateralInputsTxBodyL
    , inputsTxBodyL
    , mkBasicTxBody
    , referenceInputsTxBodyL
    )
import Cardano.Ledger.Api.Tx.Wits
    ( Redeemers (..)
    , rdmrsTxWitsL
    , scriptTxWitsL
    )
import Cardano.Ledger.Conway.Scripts
    ( ConwayPlutusPurpose (..)
    )
import Cardano.Ledger.Core (hashScript)
import Cardano.Ledger.TxIn (TxIn)

import Cardano.MPFS.Balance (balanceTx)
import Cardano.MPFS.OnChain
    ( UpdateRedeemer (..)
    , cageAddr
    )
import Cardano.MPFS.Provider (Provider (..))
import Cardano.MPFS.State
    ( Requests (..)
    , State (..)
    )
import Cardano.MPFS.TxBuilder.Config
    ( CageConfig (..)
    )
import Cardano.MPFS.TxBuilder.Real.Internal
import Cardano.MPFS.Types
    ( ConwayEra
    , Request (..)
    )

-- | Build a retract-request transaction.
--
-- The requester spends their request UTxO
-- (returning locked ADA) while referencing the
-- state UTxO.
retractRequestImpl
    :: CageConfig
    -> Provider IO
    -> State IO
    -> TxIn
    -> Addr
    -> IO (Tx ConwayEra)
retractRequestImpl cfg prov st reqTxIn addr = do
    -- 1. Look up the request to find its token
    mReq <- getRequest (requests st) reqTxIn
    req <- case mReq of
        Nothing ->
            error "retractRequest: unknown request"
        Just x -> pure x
    -- 2. Query cage UTxOs to find request + state
    let scriptAddr = cageAddr (network cfg)
    cageUtxos <- queryUTxOs prov scriptAddr
    let reqUtxo = findUtxoByTxIn reqTxIn cageUtxos
    (reqIn, _reqOut) <- case reqUtxo of
        Nothing ->
            error
                "retractRequest: request UTxO \
                \not found on chain"
        Just x -> pure x
    -- 3. Find state UTxO for this token
    let Request{requestToken = tid} = req
    stateUtxo <-
        case findStateUtxo tid cageUtxos of
            Nothing ->
                error
                    "retractRequest: state UTxO \
                    \not found"
            Just x -> pure x
    let (stateIn, _stateOut) = stateUtxo
    -- 4. Get wallet UTxO for fees
    pp <- queryProtocolParams prov
    walletUtxos <- queryUTxOs prov addr
    feeUtxo <- case walletUtxos of
        [] -> error "retractRequest: no UTxOs"
        (u : _) -> pure u
    -- 5. Build tx
    let script = mkCageScript cfg
        scriptHash = hashScript script
        allInputs =
            Set.fromList [reqIn, fst feeUtxo]
        reqIx = spendingIndex reqIn allInputs
        reqRef = txInToRef reqIn
        redeemer = Retract reqRef
        spendPurpose =
            ConwaySpending (AsIx reqIx)
        redeemers =
            Redeemers
                $ Map.singleton
                    spendPurpose
                    ( toLedgerData redeemer
                    , defaultSpendExUnits
                    )
        body =
            mkBasicTxBody
                & inputsTxBodyL
                    .~ Set.singleton reqIn
                & referenceInputsTxBodyL
                    .~ Set.singleton stateIn
                & collateralInputsTxBodyL
                    .~ Set.singleton
                        (fst feeUtxo)
        tx =
            mkBasicTx body
                & witsTxL . scriptTxWitsL
                    .~ Map.singleton
                        scriptHash
                        script
                & witsTxL . rdmrsTxWitsL
                    .~ redeemers
    case balanceTx pp feeUtxo addr tx of
        Left err ->
            error
                $ "retractRequest: "
                    <> show err
        Right balanced -> pure balanced
