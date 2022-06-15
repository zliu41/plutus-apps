{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Plutus.Script.Evaluation.Dump
  ( dumpScriptEvents,
  )
where

import Cardano.Api qualified as Cardano
import Cardano.Ledger.Alonzo.Scripts (getCostModelParams)
import Cardano.Ledger.Alonzo.TxInfo qualified as Alonzo
import Cardano.Ledger.BaseTypes qualified as Ledger
import Codec.Serialise qualified as CBOR
import Control.Applicative (Alternative ((<|>)))
import Control.Exception (handle, throwIO)
import Control.Monad.Extra (when, whenJust)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State (evalStateT, get, put)
import Data.ByteString.Base16 qualified as B16
import Data.Foldable (toList, traverse_)
import Data.List (sortBy)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Word (Word64)
import Plutus.ApiCommon (ProtocolVersion (ProtocolVersion))
import Plutus.Script.Evaluation.Options qualified as O
import Plutus.Script.Evaluation.Types (Block, Checkpoint (Checkpoint), ScriptM,
                                       StreamerState (StreamerState, ssCount, ssEvents, ssV1CostParams, ssV2CostParams))
import Plutus.Streaming (ApplyBlockException, ChainSyncEvent (RollBackward, RollForward), ledgerStateEvents,
                         withChainSyncEventStream)
import PlutusLedgerApi.Test.EvaluationEvent (ScriptEvaluationData (ScriptEvaluationData),
                                             ScriptEvaluationEvent (PlutusV1Event, PlutusV2Event),
                                             ScriptEvaluationEvents (ScriptEvaluationEvents, eventsCostParamsV1, eventsCostParamsV2, eventsEvents),
                                             ScriptEvaluationResult (ScriptEvaluationFailure, ScriptEvaluationSuccess))
import PyF (fmt)
import Streaming (MFunctor (hoist), MonadIO (liftIO), Of, Stream)
import Streaming.Prelude qualified as S
import System.Directory.Extra (listFiles, removeFile)
import System.FilePath (isExtensionOf, takeBaseName, (<.>), (</>))
import System.IO (hPrint, stderr)

-- | Stream blocks from a local node, and periodically dump ledger events
-- and checkpoint ledger state.
dumpScriptEvents :: O.Options -> IO ()
dumpScriptEvents opts = do
  (env, ledgerStateAtGenesis) <-
    either (fail . Text.unpack . Cardano.renderInitialLedgerStateError) pure
      =<< runExceptT (Cardano.initialLedgerState (O.optsConfigPath opts))
  let dir = O.optsDir opts
      go :: [FilePath] -> IO ()
      go fps = do
        (chainPoint, ledgerState, onApplyBlockException) <- case fps of
          -- No checkpoint to use, so start from Genesis.
          [] -> pure (Cardano.ChainPointAtGenesis, ledgerStateAtGenesis, throwIO)
          -- Try the latest checkpoint, and if we get an `ApplyBlockException` (which likely
          -- means the checkpointed block was rolled back), try the next one.
          latestStateFile : rest -> do
            putStrLn $ "Reading ledger state from " <> latestStateFile
            Checkpoint chainPoint ledgerState <- CBOR.readFileDeserialise latestStateFile
            cleanupStateAndEventFiles dir latestStateFile
            putStrLn
              [fmt|
Starting from checkpoint in {latestStateFile}
  slot: {maybe "Genesis" show (Cardano.chainPointToSlotNo chainPoint)}
  hash: {maybe "Genesis" renderBlockHash (Cardano.chainPointToHeaderHash chainPoint)}
|]
            pure (chainPoint, ledgerState, \(e :: ApplyBlockException) -> hPrint stderr e >> go rest)

        handle onApplyBlockException
          . withChainSyncEventStream
            (O.optsSocketPath opts)
            (O.optsNetworkId opts)
            chainPoint
          $ \blockStream -> do
            let eventStream ::
                  Stream
                    (Of (ChainSyncEvent Block, (Cardano.LedgerState, [Cardano.LedgerEvent])))
                    ScriptM
                    ()
                eventStream =
                  hoist liftIO $
                    ledgerStateEvents env ledgerState Cardano.QuickValidation blockStream
            flip evalStateT (StreamerState 0 Nothing Nothing []) $
              runStream dir (O.optsBlocksPerFile opts) (O.optsEventsPerFile opts) eventStream

  go =<< listStateFiles dir

runStream ::
  forall r.
  FilePath ->
  -- | Blocks per file
  Word64 ->
  -- | Events per file
  Word64 ->
  Stream
    (Of (ChainSyncEvent Block, (Cardano.LedgerState, [Cardano.LedgerEvent])))
    ScriptM
    r ->
  ScriptM r
runStream dir blocksPerFile eventsPerFile stream = do
  S.mapM_ (uncurry (uncurry . checkpoint)) stream
  where
    checkpoint :: ChainSyncEvent Block -> Cardano.LedgerState -> [Cardano.LedgerEvent] -> ScriptM ()
    checkpoint ev ledgerState ledgerEvents = case ev of
      RollForward block _tip -> do
        streamerState <- get
        let currentV1CostParams = ssV1CostParams streamerState
            currentV2CostParams = ssV2CostParams streamerState
            (newScriptEvents, newV1CostParams, newV2CostParams) = toScriptEvents ledgerEvents
        when (ssCount streamerState `mod` 200 == 0) . liftIO $
          putStrLn $ "blocks processed: " <> show (ssCount streamerState)
        if ssCount streamerState >= blocksPerFile
          || fromIntegral (length (ssEvents streamerState)) >= eventsPerFile
          || changed currentV1CostParams newV1CostParams
          || changed currentV2CostParams newV2CostParams
          then do
            liftIO $ putStrLn "Creating new checkpoint"
            time <- liftIO getCurrentTime
            let eventFile = dir </> iso8601Show time <.> eventsFileExt
            whenJust (NonEmpty.nonEmpty (ssEvents streamerState)) $ \evs -> do
              let scriptEvents =
                    ScriptEvaluationEvents
                      { eventsCostParamsV1 = currentV1CostParams,
                        eventsCostParamsV2 = currentV2CostParams,
                        eventsEvents = evs
                      }
              liftIO $ CBOR.writeFileSerialise eventFile scriptEvents
            -- Writing state (checkpoint) file after events file ensures the events of a
            -- checkpoint are persisted.
            let stateFile = dir </> iso8601Show time <.> stateFileExt
                chainPoint = blockChainPoint block
            liftIO $ CBOR.writeFileSerialise stateFile (Checkpoint chainPoint ledgerState)
            put $ StreamerState 0 newV1CostParams newV2CostParams []
            liftIO $
              putStrLn
                [fmt|
Created new checkpoint in {stateFile}
  number of blocks: {ssCount streamerState}
  number of script evaluation events: {length (ssEvents streamerState)}
  slot: {maybe "Genesis" show (Cardano.chainPointToSlotNo chainPoint)}
  hash: {maybe "Genesis" renderBlockHash (Cardano.chainPointToHeaderHash chainPoint)}
|]
          else do
            put $
              streamerState
                { ssEvents = newScriptEvents ++ ssEvents streamerState,
                  ssV1CostParams = currentV1CostParams <|> newV1CostParams,
                  ssV2CostParams = currentV2CostParams <|> newV2CostParams,
                  ssCount = ssCount streamerState + 1
                }
      RollBackward {} ->
        -- Nothing special needs to be done on `RollBackward`, since there's no harm
        -- dumping events in blocks that are rolled back. In fact it's a good thing - it
        -- gives us more data to test with.
        pure ()

changed :: Maybe [Integer] -> Maybe [Integer] -> Bool
changed params1 params2 = fromMaybe False $ (/=) <$> params1 <*> params2

stateFileExt, eventsFileExt :: String
stateFileExt = "state"
eventsFileExt = "event"

blockChainPoint :: Block -> Cardano.ChainPoint
blockChainPoint (Cardano.BlockInMode (Cardano.Block (Cardano.BlockHeader slot hash _) _) _) =
  Cardano.ChainPoint slot hash

toScriptEvents :: [Cardano.LedgerEvent] -> ([ScriptEvaluationEvent], Maybe [Integer], Maybe [Integer])
toScriptEvents = foldr alg ([], Nothing, Nothing)
  where
    alg ::
      Cardano.LedgerEvent ->
      ([ScriptEvaluationEvent], Maybe [Integer], Maybe [Integer]) ->
      ([ScriptEvaluationEvent], Maybe [Integer], Maybe [Integer])
    alg ledgerEvent acc = case ledgerEvent of
      Cardano.SuccessfulPlutusScript ds -> foldr (alg' ScriptEvaluationSuccess) acc ds
      Cardano.FailedPlutusScript ds     -> foldr (alg' ScriptEvaluationFailure) acc (toList ds)
      _                                 -> acc

    alg' ::
      ScriptEvaluationResult ->
      Alonzo.PlutusDebug ->
      ([ScriptEvaluationEvent], Maybe [Integer], Maybe [Integer]) ->
      ([ScriptEvaluationEvent], Maybe [Integer], Maybe [Integer])
    alg' res d (scriptEvents, v1, v2) = case d of
      Alonzo.PlutusDebugV1 costModel units script inputs (Ledger.ProtVer major minor) ->
        let params = M.elems (getCostModelParams costModel)
            budget = Alonzo.transExUnits units
            ver = ProtocolVersion (fromIntegral major) (fromIntegral minor)
            evaluationData = ScriptEvaluationData ver budget script inputs
         in (PlutusV1Event evaluationData res : scriptEvents, v1 <|> Just params, v2)
      Alonzo.PlutusDebugV2 costModel units script inputs (Ledger.ProtVer major minor) ->
        let params = M.elems (getCostModelParams costModel)
            budget = Alonzo.transExUnits units
            ver = ProtocolVersion (fromIntegral major) (fromIntegral minor)
            evaluationData = ScriptEvaluationData ver budget script inputs
         in (PlutusV2Event evaluationData res : scriptEvents, v1, v2 <|> Just params)

listStateFiles :: FilePath -> IO [FilePath]
listStateFiles =
  fmap (sortBy (flip compare) . filter (stateFileExt `isExtensionOf`)) . listFiles

-- | Remove the state and event files whose timestamps are greater than the given state file
cleanupStateAndEventFiles :: FilePath -> FilePath -> IO ()
cleanupStateAndEventFiles dir stateFile = do
  newerStateAndEventFiles <-
    takeWhile (\f -> takeBaseName f > takeBaseName stateFile)
      . filter (\f -> stateFileExt `isExtensionOf` f || eventsFileExt `isExtensionOf` f)
      <$> listFiles dir
  traverse_ removeFile newerStateAndEventFiles

renderBlockHash :: Cardano.Hash Cardano.BlockHeader -> Text.Text
renderBlockHash = Text.decodeLatin1 . B16.encode . Cardano.serialiseToRawBytes
