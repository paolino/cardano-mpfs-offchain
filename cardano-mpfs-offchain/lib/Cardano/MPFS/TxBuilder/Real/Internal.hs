{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Cardano.MPFS.TxBuilder.Real.Internal
-- Description : Shared helpers for real transaction builders
-- License     : Apache-2.0
module Cardano.MPFS.TxBuilder.Real.Internal
    ( -- * Script construction
      mkCageScript

      -- * Datum helpers
    , mkRequestDatum
    , toPlcData
    , toLedgerData
    , mkInlineDatum
    , extractCageDatum

      -- * Reference conversion
    , txInToRef
    , addrKeyHashBytes

      -- * UTxO lookup
    , findUtxoByTxIn
    , findStateUtxo
    , findRequestUtxos

      -- * Indexing
    , spendingIndex

      -- * Execution units
    , defaultMintExUnits
    , defaultSpendExUnits

      -- * Constants
    , emptyRoot
    ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as SBS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word32)
import Lens.Micro ((^.))

import Cardano.Crypto.Hash (hashToBytes)
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Scripts
    ( fromPlutusScript
    , mkPlutusScript
    )
import Cardano.Ledger.Api.Scripts.Data
    ( Data (..)
    , Datum (..)
    , binaryDataToData
    , dataToBinaryData
    )
import Cardano.Ledger.Api.Tx.Out
    ( TxOut
    , datumTxOutL
    , valueTxOutL
    )
import Cardano.Ledger.BaseTypes (TxIx (..))
import Cardano.Ledger.Core
    ( Script
    , extractHash
    )
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Keys (KeyHash (..))
import Cardano.Ledger.Mary.Value
    ( MaryValue (..)
    , MultiAsset (..)
    )
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..))
import Cardano.Ledger.Plutus.Language
    ( Language (PlutusV3)
    , Plutus (..)
    , PlutusBinary (..)
    )
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import PlutusCore.Data qualified as PLC
import PlutusTx.Builtins.Internal
    ( BuiltinByteString (..)
    , BuiltinData (..)
    )
import PlutusTx.IsData.Class
    ( FromData (..)
    , ToData (..)
    )

import Cardano.MPFS.OnChain
    ( CageDatum (..)
    , OnChainOperation (..)
    , OnChainRequest (..)
    , OnChainTokenId (..)
    , OnChainTxOutRef (..)
    , cagePolicyId
    )
import Cardano.MPFS.TxBuilder.Config
    ( CageConfig (..)
    )
import Cardano.MPFS.Types
    ( AssetName (..)
    , ConwayEra
    , TokenId (..)
    )

-- | Empty MPF root (32 zero bytes).
emptyRoot :: ByteString
emptyRoot = BS.replicate 32 0

-- | Hardcoded execution units for minting.
-- A later phase will use evaluateTx for precise
-- values.
defaultMintExUnits :: ExUnits
defaultMintExUnits =
    ExUnits 500_000_000 500_000

-- | Hardcoded execution units for spending.
defaultSpendExUnits :: ExUnits
defaultSpendExUnits =
    ExUnits 500_000_000 500_000

-- | Build the cage 'Script' from config bytes.
mkCageScript :: CageConfig -> Script ConwayEra
mkCageScript cfg =
    let plutus =
            Plutus @PlutusV3
                $ PlutusBinary
                $ cageScriptBytes cfg
    in  case mkPlutusScript plutus of
            Just ps -> fromPlutusScript ps
            Nothing ->
                error
                    "mkCageScript: invalid \
                    \PlutusV3 script"

-- | Build a 'CageDatum' for a request.
mkRequestDatum
    :: TokenId
    -> Addr
    -> ByteString
    -> OnChainOperation
    -> Integer
    -> PLC.Data
mkRequestDatum tid addr key op fee =
    let onChainTid =
            OnChainTokenId
                $ BuiltinByteString
                $ SBS.fromShort
                $ let AssetName sbs = unTokenId tid
                  in  sbs
        datum =
            OnChainRequest
                { requestToken = onChainTid
                , requestOwner =
                    BuiltinByteString
                        (addrKeyHashBytes addr)
                , requestKey = key
                , requestValue = op
                , requestFee = fee
                , requestSubmittedAt = 0
                }
    in  toPlcData (RequestDatum datum)

-- | Convert a 'ToData' value to
-- 'PlutusCore.Data.Data'.
toPlcData :: (ToData a) => a -> PLC.Data
toPlcData x =
    let BuiltinData d = toBuiltinData x in d

-- | Convert a 'ToData' value to a ledger 'Data'.
toLedgerData
    :: (ToData a) => a -> Data ConwayEra
toLedgerData = Data . toPlcData

-- | Wrap 'PlutusCore.Data.Data' as an inline
-- 'Datum'.
mkInlineDatum :: PLC.Data -> Datum ConwayEra
mkInlineDatum d =
    Datum
        $ dataToBinaryData
            (Data d :: Data ConwayEra)

-- | Convert a ledger 'TxIn' to an on-chain
-- 'OnChainTxOutRef'.
txInToRef :: TxIn -> OnChainTxOutRef
txInToRef (TxIn (TxId h) (TxIx ix)) =
    OnChainTxOutRef
        { txOutRefId =
            BuiltinByteString
                (hashToBytes (extractHash h))
        , txOutRefIdx = fromIntegral ix
        }

-- | Extract the payment key hash raw bytes from
-- an 'Addr'. Returns empty bytes for script
-- addresses (requests from scripts are unusual
-- but not forbidden).
addrKeyHashBytes :: Addr -> ByteString
addrKeyHashBytes
    (Addr _ (KeyHashObj (KeyHash h)) _) =
        hashToBytes h
addrKeyHashBytes _ = BS.empty

-- | Find a UTxO by its 'TxIn'.
findUtxoByTxIn
    :: TxIn
    -> [(TxIn, TxOut ConwayEra)]
    -> Maybe (TxIn, TxOut ConwayEra)
findUtxoByTxIn needle =
    find' (\(tin, _) -> tin == needle)
  where
    find' _ [] = Nothing
    find' p (x : xs)
        | p x = Just x
        | otherwise = find' p xs

-- | Find the state UTxO for a token by checking
-- the 'MultiAsset' for the cage policy + token's
-- asset name.
findStateUtxo
    :: TokenId
    -> [(TxIn, TxOut ConwayEra)]
    -> Maybe (TxIn, TxOut ConwayEra)
findStateUtxo tid = find' isState
  where
    assetName = unTokenId tid
    isState (_, txOut) =
        case txOut ^. valueTxOutL of
            MaryValue _ (MultiAsset ma) ->
                case Map.lookup cagePolicyId ma of
                    Just assets ->
                        Map.member assetName assets
                    Nothing -> False
    find' _ [] = Nothing
    find' p (x : xs)
        | p x = Just x
        | otherwise = find' p xs

-- | Find all request UTxOs for a token by decoding
-- their inline datums.
findRequestUtxos
    :: TokenId
    -> [(TxIn, TxOut ConwayEra)]
    -> [(TxIn, TxOut ConwayEra)]
findRequestUtxos tid = filter isRequest
  where
    targetName = unTokenId tid
    isRequest (_, txOut) =
        case extractCageDatum txOut of
            Just (RequestDatum req) ->
                let OnChainRequest
                        { requestToken =
                            OnChainTokenId
                                (BuiltinByteString bs)
                        } = req
                in  AssetName (SBS.toShort bs)
                        == targetName
            _ -> False

-- | Extract a 'CageDatum' from an inline datum
-- in a 'TxOut'.
extractCageDatum
    :: TxOut ConwayEra -> Maybe CageDatum
extractCageDatum txOut =
    case txOut ^. datumTxOutL of
        Datum bd ->
            let Data plcData =
                    binaryDataToData bd
            in  fromBuiltinData (BuiltinData plcData)
        _ -> Nothing

-- | Compute the spending index of a 'TxIn' in
-- the sorted set of all inputs.
spendingIndex :: TxIn -> Set.Set TxIn -> Word32
spendingIndex needle inputs =
    let sorted = Set.toAscList inputs
    in  go 0 sorted
  where
    go _ [] =
        error "spendingIndex: TxIn not in set"
    go n (x : xs)
        | x == needle = n
        | otherwise = go (n + 1) xs
