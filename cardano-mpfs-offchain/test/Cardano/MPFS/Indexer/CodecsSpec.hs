-- |
-- Module      : Cardano.MPFS.Indexer.CodecsSpec
-- Description : Round-trip property tests for cage codecs
-- License     : Apache-2.0
module Cardano.MPFS.Indexer.CodecsSpec (spec) where

import Control.Lens (preview, review)

import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (forAll, (===))

import Cardano.MPFS.Generators
    ( genBlockId
    , genRequest
    , genSlotNo
    , genTokenId
    , genTokenState
    , genTxIn
    )
import Cardano.MPFS.Indexer.Codecs
    ( checkpointPrism
    , requestPrism
    , tokenIdPrism
    , tokenStatePrism
    , txInPrism
    , unitPrism
    )
import Cardano.MPFS.Indexer.Columns
    ( CageCheckpoint (..)
    )

spec :: Spec
spec = describe "Codecs" $ do
    describe "tokenIdPrism"
        $ prop "round-trips"
        $ forAll genTokenId
        $ \tid ->
            preview tokenIdPrism (review tokenIdPrism tid)
                === Just tid

    describe "tokenStatePrism"
        $ prop "round-trips"
        $ forAll genTokenState
        $ \ts ->
            preview
                tokenStatePrism
                (review tokenStatePrism ts)
                === Just ts

    describe "txInPrism"
        $ prop "round-trips"
        $ forAll genTxIn
        $ \txin ->
            preview txInPrism (review txInPrism txin)
                === Just txin

    describe "requestPrism"
        $ prop "round-trips"
        $ forAll genTokenId
        $ \tid ->
            forAll (genRequest tid) $ \req ->
                preview
                    requestPrism
                    (review requestPrism req)
                    === Just req

    describe "unitPrism"
        $ prop "round-trips"
        $ preview unitPrism (review unitPrism ())
            === Just ()

    describe "checkpointPrism"
        $ prop "round-trips"
        $ forAll genSlotNo
        $ \s ->
            forAll genBlockId $ \b ->
                let cp = CageCheckpoint s b
                in  preview
                        checkpointPrism
                        (review checkpointPrism cp)
                        === Just cp
