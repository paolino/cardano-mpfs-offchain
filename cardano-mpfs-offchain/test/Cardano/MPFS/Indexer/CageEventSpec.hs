{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Cardano.MPFS.Indexer.CageEventSpec
-- Description : Tests for CageEvent inverse operations
-- License     : Apache-2.0
module Cardano.MPFS.Indexer.CageEventSpec
    ( spec
    ) where

import Data.Map.Strict qualified as Map
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )
import Test.QuickCheck
    ( Gen
    , forAll
    , listOf1
    , property
    )

import Cardano.MPFS.Generators
    ( genRequest
    , genRoot
    , genTokenId
    , genTokenState
    , genTxIn
    )
import Cardano.MPFS.Indexer.CageEvent
    ( CageEvent (..)
    , CageInverseOp (..)
    , inversesOf
    )
import Cardano.MPFS.Types
    ( Request (..)
    , Root (..)
    , TokenId (..)
    , TokenState (..)
    , TxIn
    )

spec :: Spec
spec = describe "CageEvent" $ do
    describe "inversesOf" $ do
        it "boot produces InvRemoveToken" $
            forAll genBoot $ \(tid, ts) ->
                inversesOf noTokens noReqs (CageBoot tid ts)
                    `shouldBe` [InvRemoveToken tid]

        it "request produces InvRemoveRequest" $
            forAll genReqEvent $ \(txIn, req) ->
                inversesOf noTokens noReqs (CageRequest txIn req)
                    `shouldBe` [InvRemoveRequest txIn]

        it "retract produces InvRemoveRequest" $
            forAll genTxIn $ \txIn ->
                inversesOf noTokens noReqs (CageRetract txIn)
                    `shouldBe` [InvRemoveRequest txIn]

        it "burn with known token produces InvRestoreToken"
            $ forAll genBurnKnown
            $ \(tid, ts) ->
                let tokens = Map.singleton tid ts
                in  inversesOf
                        (`Map.lookup` tokens)
                        noReqs
                        (CageBurn tid)
                        `shouldBe` [InvRestoreToken tid ts]

        it "burn with unknown token produces empty" $
            forAll genTokenId $ \tid ->
                inversesOf noTokens noReqs (CageBurn tid)
                    `shouldBe` []

        it
            "update with known token produces InvRestoreRoot + InvRemoveRequest per consumed"
            $ forAll genUpdateKnown
            $ \(tid, ts, newRoot, consumed) ->
                let tokens = Map.singleton tid ts
                    result =
                        inversesOf
                            (`Map.lookup` tokens)
                            noReqs
                            (CageUpdate tid newRoot consumed)
                in  result
                        `shouldBe` ( InvRestoreRoot tid (root ts)
                                        : map InvRemoveRequest consumed
                                   )

        it "update with unknown token produces only InvRemoveRequest per consumed"
            $ forAll genUpdateUnknown
            $ \(tid, newRoot, consumed) ->
                inversesOf
                    noTokens
                    noReqs
                    (CageUpdate tid newRoot consumed)
                    `shouldBe` map InvRemoveRequest consumed

-- Helpers

noTokens :: TokenId -> Maybe TokenState
noTokens = const Nothing

noReqs :: TxIn -> Maybe Request
noReqs = const Nothing

-- Generators

genBoot :: Gen (TokenId, TokenState)
genBoot = (,) <$> genTokenId <*> genTokenState

genReqEvent :: Gen (TxIn, Request)
genReqEvent = do
    tid <- genTokenId
    txIn <- genTxIn
    req <- genRequest tid
    pure (txIn, req)

genBurnKnown :: Gen (TokenId, TokenState)
genBurnKnown = (,) <$> genTokenId <*> genTokenState

genUpdateKnown
    :: Gen (TokenId, TokenState, Root, [TxIn])
genUpdateKnown =
    (,,,)
        <$> genTokenId
        <*> genTokenState
        <*> genRoot
        <*> listOf1 genTxIn

genUpdateUnknown :: Gen (TokenId, Root, [TxIn])
genUpdateUnknown =
    (,,)
        <$> genTokenId
        <*> genRoot
        <*> listOf1 genTxIn
