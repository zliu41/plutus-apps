module Main (main) where

import Codec.Serialise qualified as CBOR
import Control.Monad
import Plutus.Script.Evaluation.Types
import Plutus.V1.Ledger.Api qualified as V1
-- import Plutus.V2.Ledger.Scripts qualified as V2
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Alonzo.TxInfo qualified as Alonzo
import Cardano.Ledger.BaseTypes qualified as Ledger
import Data.Foldable
import Data.Map qualified as M
import Data.Traversable
import Plutus.ApiCommon
import PlutusLedgerApi.Test.EvaluationEvent

main :: IO ()
main = do
  let eventFile = "/home/zliu41/cardano-dump/2022-06-08T03:54:14.395408748Z.event"
  scriptEvents :: [ScriptEvent] <- CBOR.readFileDeserialise eventFile
  putStrLn $ "len === " ++ show (length scriptEvents)
  let plutusEvents = scriptEvents >>= ev2ev
  let newEventFile = "/home/zliu41/cardano-dump-clean/test.event"
  CBOR.writeFileSerialise newEventFile plutusEvents
  -- for_ (zip ([1..] :: [Integer]) scriptEvents) $ \(idx, ev) -> do
  --   -- putStrLn $ "event: " ++ show idx
  --   case ev of
  --     ScriptEventSuccess xs -> do
  --       when (length xs > 0) $ do
  --         putStrLn $ "Got events: " ++ show (length xs)
  --         for_ xs $ \x -> do
  --           case x of
  --             Alonzo.PlutusDebugV1 costModel _units script inputs (Ledger.ProtVer major minor) -> do
  --               putStrLn $ "V1!!!"
  --               let ctx = getEvaluationContext costModel
  --               let ver = ProtocolVersion (fromIntegral major) (fromIntegral minor)
  --               let (_, res) = V1.evaluateScriptCounting ver Quiet ctx script inputs
  --               case res of
  --                 Right _ -> putStrLn $ "Good"
  --                 Left _ -> undefined
  --             _ -> do
  --               putStrLn $ "V2!!!"
  --     ScriptEventFailure xs -> do
  --       putStrLn $ "Failed! " ++ show (length xs)

ev2ev :: ScriptEvent -> [ScriptEvaluationEvent]
ev2ev = \case
  ScriptEventSuccess dbgs
    | null dbgs -> []
    | otherwise -> map (toEvent True) dbgs
  ScriptEventFailure dbgs -> map (toEvent False) (toList dbgs)

toEvent :: Bool -> Alonzo.PlutusDebug -> ScriptEvaluationEvent
toEvent success = \case
  Alonzo.PlutusDebugV1 costModel units script inputs (Ledger.ProtVer major minor) ->
    let params = M.elems (getCostModelParams costModel)
        budget = Alonzo.transExUnits units
        ver = ProtocolVersion (fromIntegral major) (fromIntegral minor)
        d = ScriptEvaluationData ver params budget script inputs
    in PlutusV1Event d (if success then ScriptEvaluationSuccess else ScriptEvaluationFailure)
  Alonzo.PlutusDebugV2 costModel units script inputs (Ledger.ProtVer major minor) ->
    let params = M.elems (getCostModelParams costModel)
        budget = Alonzo.transExUnits units
        ver = ProtocolVersion (fromIntegral major) (fromIntegral minor)
        d = ScriptEvaluationData ver params budget script inputs
    in PlutusV2Event d (if success then ScriptEvaluationSuccess else ScriptEvaluationFailure)
