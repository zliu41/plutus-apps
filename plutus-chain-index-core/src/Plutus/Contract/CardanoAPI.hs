{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-|

Interface to the transaction types from 'cardano-api'

-}
module Plutus.Contract.CardanoAPI(
    fromCardanoBlock
  , fromCardanoTx
  , module Export
) where

import Cardano.Api qualified as C
import Data.Set qualified as Set
import Ledger qualified as P
import Ledger.Tx.CardanoAPI as Export
import Plutus.ChainIndex.Tx (ChainIndexTx (..), TxOutInAnyEra (..))
import Plutus.ChainIndex.Tx qualified as ChainIndex.Tx

fromCardanoBlock :: C.BlockInMode C.CardanoMode -> Either FromCardanoError [ChainIndexTx]
fromCardanoBlock (C.BlockInMode (C.Block C.BlockHeader {} txs) eraInMode) =
  case eraInMode of
    -- Unfortunately, we need to pattern match again all eras because
    -- 'fromCardanoTx' has the constraints 'C.IsCardanoEra era', but not
    -- 'C.BlockInMode'.
    C.ByronEraInCardanoMode   -> traverse (fromCardanoTx eraInMode) txs
    C.ShelleyEraInCardanoMode -> traverse (fromCardanoTx eraInMode) txs
    C.AllegraEraInCardanoMode -> traverse (fromCardanoTx eraInMode) txs
    C.MaryEraInCardanoMode    -> traverse (fromCardanoTx eraInMode) txs
    C.AlonzoEraInCardanoMode  -> traverse (fromCardanoTx eraInMode) txs
    C.BabbageEraInCardanoMode -> traverse (fromCardanoTx eraInMode) txs

-- | Convert a Cardano API tx of any given era to a Plutus chain index tx.
fromCardanoTx
  :: C.IsCardanoEra era
  => C.EraInMode era C.CardanoMode
  -> C.Tx era
  -> Either FromCardanoError ChainIndexTx
fromCardanoTx eraInMode tx@(C.Tx txBody@(C.TxBody C.TxBodyContent{..}) _) = do
    -- txOutputs <- traverse fromCardanoTxOutBabbage txOuts
    let scriptMap = plutusScriptsFromTxBody txBody
        isTxScriptValid = fromTxScriptValidity txScriptValidity
        (datums, redeemers) = scriptDataFromCardanoTxBody txBody
        inputs =
          if isTxScriptValid
            then fst <$> txIns
            else case txInsCollateral of
                   C.TxInsCollateralNone     -> []
                   C.TxInsCollateral _ txins -> txins

    pure ChainIndexTx
            { _citxTxId = fromCardanoTxId (C.getTxId txBody)
            , _citxValidRange = fromCardanoValidityRange txValidityRange
            -- If the transaction is invalid, we use collateral inputs
            , _citxInputs = Set.fromList $ fmap ((`P.TxIn` Nothing) . fromCardanoTxIn) inputs
            -- No outputs if the one of scripts failed
            , _citxOutputs = if isTxScriptValid then ChainIndex.Tx.ValidTx (TxOutInAnyEra (eraInModeToEra eraInMode) <$> txOuts)
                                                else ChainIndex.Tx.InvalidTx
            , _citxData = datums
            , _citxRedeemers = redeemers
            , _citxScripts = scriptMap
            , _citxCardanoTx = Just $ SomeTx tx eraInMode
            }

-- TODO: not exported by Cardano.Api
eraInModeToEra :: C.EraInMode era mode -> C.CardanoEra era
eraInModeToEra C.ByronEraInByronMode     = C.ByronEra
eraInModeToEra C.ShelleyEraInShelleyMode = C.ShelleyEra
eraInModeToEra C.ByronEraInCardanoMode   = C.ByronEra
eraInModeToEra C.ShelleyEraInCardanoMode = C.ShelleyEra
eraInModeToEra C.AllegraEraInCardanoMode = C.AllegraEra
eraInModeToEra C.MaryEraInCardanoMode    = C.MaryEra
eraInModeToEra C.AlonzoEraInCardanoMode  = C.AlonzoEra
eraInModeToEra C.BabbageEraInCardanoMode = C.BabbageEra
