{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Cardano.MPFS.E2E.CageSpec
-- Description : E2E tests for the full cage protocol
-- License     : Apache-2.0
module Cardano.MPFS.E2E.CageSpec (spec) where

import Control.Concurrent (threadDelay)
import Data.ByteString qualified as BS
import Data.ByteString.Short (ShortByteString)
import Data.Map.Strict qualified as Map
import Lens.Micro ((^.))
import System.Environment (lookupEnv)
import Test.Hspec
    ( Spec
    , describe
    , expectationFailure
    , it
    , runIO
    , shouldSatisfy
    )

import Cardano.Ledger.Api.Tx
    ( Tx
    , bodyTxL
    , txIdTx
    )
import Cardano.Ledger.Api.Tx.Body (mintTxBodyL)
import Cardano.Ledger.BaseTypes
    ( Network (..)
    , TxIx (..)
    )
import Cardano.Ledger.Mary.Value
    ( MultiAsset (..)
    )
import Cardano.Ledger.TxIn (TxIn (..))

import Cardano.MPFS.Application
    ( AppConfig (..)
    , withApplication
    )
import Cardano.MPFS.Blueprint
    ( extractCompiledCode
    , loadBlueprint
    )
import Cardano.MPFS.Context (Context (..))
import Cardano.MPFS.E2E.Devnet (withCardanoNode)
import Cardano.MPFS.E2E.Setup
    ( addKeyWitness
    , devnetMagic
    , genesisAddr
    , genesisDir
    , genesisSignKey
    , keyHashFromSignKey
    )
import Cardano.MPFS.OnChain
    ( cageAddr
    , cagePolicyId
    )
import Cardano.MPFS.Provider (Provider (..))
import Cardano.MPFS.State
    ( Requests (..)
    , State (..)
    , Tokens (..)
    )
import Cardano.MPFS.Submitter
    ( SubmitResult (..)
    , Submitter (..)
    )
import Cardano.MPFS.Trie (TrieManager (..))
import Cardano.MPFS.TxBuilder (TxBuilder (..))
import Cardano.MPFS.TxBuilder.Config
    ( CageConfig (..)
    )
import Cardano.MPFS.Types
    ( AssetName (..)
    , Coin (..)
    , ConwayEra
    , Operation (..)
    , Request (..)
    , Root (..)
    , TokenId (..)
    , TokenState (..)
    )

-- | Full cage protocol E2E test spec.
-- Skips when @MPFS_BLUEPRINT@ is not set.
spec :: Spec
spec = describe "Cage E2E" $ do
    mPath <-
        runIO $ lookupEnv "MPFS_BLUEPRINT"
    case mPath of
        Nothing ->
            it
                "skipped (MPFS_BLUEPRINT \
                \not set)"
                (pure () :: IO ())
        Just path -> do
            ebp <-
                runIO $ loadBlueprint path
            case ebp of
                Left err ->
                    it
                        ( "blueprint error: "
                            <> err
                        )
                        (expectationFailure err)
                Right bp ->
                    case extractCompiledCode
                        "cage."
                        bp of
                        Nothing ->
                            it "no compiled code"
                                $ expectationFailure
                                    "cage script not \
                                    \found in blueprint"
                        Just scriptBytes ->
                            cageFlowSpec
                                scriptBytes

-- ---------------------------------------------------------
-- Test implementation
-- ---------------------------------------------------------

-- | Full cage flow: boot, request, update,
-- and retract.
cageFlowSpec :: ShortByteString -> Spec
cageFlowSpec scriptBytes =
    it "boot, request, update, retract"
        $ withE2E scriptBytes
        $ \ctx -> do
            let scriptAddr = cageAddr Testnet

            -- Step 1: Boot token
            unsignedBoot <-
                bootToken
                    (txBuilder ctx)
                    genesisAddr
            let signedBoot =
                    addKeyWitness
                        genesisSignKey
                        unsignedBoot
            bootResult <-
                submitTx
                    (submitter ctx)
                    signedBoot
            assertSubmitted bootResult
            awaitTx

            -- Extract TokenId from mint field
            let tokenId =
                    extractTokenId signedBoot

            -- Register in mock state + trie
            createTrie
                (trieManager ctx)
                tokenId
            let ts =
                    TokenState
                        { owner =
                            keyHashFromSignKey
                                genesisSignKey
                        , root =
                            Root
                                ( BS.replicate
                                    32
                                    0
                                )
                        , maxFee =
                            Coin 1_000_000
                        , processTime =
                            300_000
                        , retractTime =
                            600_000
                        }
            putToken
                (tokens (state ctx))
                tokenId
                ts

            -- Assert: cage address has UTxO
            cageUtxos <-
                queryUTxOs
                    (provider ctx)
                    scriptAddr
            cageUtxos
                `shouldSatisfy` (not . null)

            -- Step 2: Request insert
            unsignedReq <-
                requestInsert
                    (txBuilder ctx)
                    tokenId
                    "hello"
                    "world"
                    genesisAddr
            let signedReq =
                    addKeyWitness
                        genesisSignKey
                        unsignedReq
            reqResult <-
                submitTx
                    (submitter ctx)
                    signedReq
            assertSubmitted reqResult
            awaitTx

            -- Assert: cage has more UTxOs now
            cageUtxos2 <-
                queryUTxOs
                    (provider ctx)
                    scriptAddr
            length cageUtxos2
                `shouldSatisfy` (> length cageUtxos)

            -- Step 3: Update token
            unsignedUpdate <-
                updateToken
                    (txBuilder ctx)
                    tokenId
                    genesisAddr
            let signedUpdate =
                    addKeyWitness
                        genesisSignKey
                        unsignedUpdate
            updateResult <-
                submitTx
                    (submitter ctx)
                    signedUpdate
            assertSubmitted updateResult
            awaitTx

            -- Assert: still has cage UTxOs but
            -- request was consumed
            cageUtxos3 <-
                queryUTxOs
                    (provider ctx)
                    scriptAddr
            cageUtxos3
                `shouldSatisfy` (not . null)
            -- Fewer UTxOs: state remains,
            -- request consumed
            length cageUtxos3
                `shouldSatisfy` (< length cageUtxos2)

            -- Step 4: Request + retract
            -- Submit a second request
            unsignedReq2 <-
                requestInsert
                    (txBuilder ctx)
                    tokenId
                    "bye"
                    "moon"
                    genesisAddr
            let signedReq2 =
                    addKeyWitness
                        genesisSignKey
                        unsignedReq2
            req2Result <-
                submitTx
                    (submitter ctx)
                    signedReq2
            assertSubmitted req2Result
            awaitTx

            -- Extract request TxIn (cage output
            -- is at index 0 in balanced tx)
            let req2TxIn =
                    TxIn
                        (txIdTx signedReq2)
                        (TxIx 0)
            -- Register in mock state
            let req2 =
                    Request
                        { requestToken = tokenId
                        , requestOwner =
                            keyHashFromSignKey
                                genesisSignKey
                        , requestKey = "bye"
                        , requestValue =
                            Insert "moon"
                        , requestFee =
                            Coin 1_000_000
                        , requestSubmittedAt = 0
                        }
            putRequest
                (requests (state ctx))
                req2TxIn
                req2

            cageUtxos4 <-
                queryUTxOs
                    (provider ctx)
                    scriptAddr
            -- Has request + state UTxOs
            length cageUtxos4
                `shouldSatisfy` (> length cageUtxos3)

            -- Retract the second request
            unsignedRetract <-
                retractRequest
                    (txBuilder ctx)
                    req2TxIn
                    genesisAddr
            let signedRetract =
                    addKeyWitness
                        genesisSignKey
                        unsignedRetract
            retractResult <-
                submitTx
                    (submitter ctx)
                    signedRetract
            assertSubmitted retractResult
            awaitTx

            -- Assert: request UTxO gone
            cageUtxos5 <-
                queryUTxOs
                    (provider ctx)
                    scriptAddr
            length cageUtxos5
                `shouldSatisfy` (< length cageUtxos4)

-- ---------------------------------------------------------
-- Bracket
-- ---------------------------------------------------------

-- | Start a devnet node, wire a full 'Context IO',
-- wait for N2C to connect, then run the action.
withE2E
    :: ShortByteString
    -> (Context IO -> IO a)
    -> IO a
withE2E scriptBytes action = do
    gDir <- genesisDir
    withCardanoNode gDir $ \sock -> do
        let appCfg =
                AppConfig
                    { networkMagic =
                        devnetMagic
                    , socketPath = sock
                    , channelCapacity = 16
                    , cageConfig = cageCfg
                    }
        withApplication appCfg $ \ctx -> do
            _ <-
                queryProtocolParams
                    (provider ctx)
            action ctx
  where
    cageCfg =
        CageConfig
            { cageScriptBytes =
                scriptBytes
            , defaultProcessTime =
                300_000
            , defaultRetractTime =
                600_000
            , defaultMaxFee =
                Coin 1_000_000
            , network = Testnet
            }

-- ---------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------

-- | Assert that a submit result is 'Submitted'.
assertSubmitted :: SubmitResult -> IO ()
assertSubmitted (Submitted _) = pure ()
assertSubmitted (Rejected reason) =
    expectationFailure
        $ "Tx rejected: " <> show reason

-- | Extract the 'TokenId' from a boot
-- transaction's mint field.
extractTokenId :: Tx ConwayEra -> TokenId
extractTokenId tx =
    let MultiAsset ma =
            tx ^. bodyTxL . mintTxBodyL
        [(an, _)] =
            Map.toList
                (ma Map.! cagePolicyId)
    in  TokenId an

-- | Wait for a transaction to be confirmed
-- (~20 devnet blocks at 0.1s slots).
awaitTx :: IO ()
awaitTx = threadDelay 2_000_000
