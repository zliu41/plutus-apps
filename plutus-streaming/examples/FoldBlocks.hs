{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE ApplicativeDo    #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE StrictData       #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Api hiding (LedgerStateError (ApplyBlockError))
import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Alonzo.TxInfo
import Codec.Serialise qualified as CBOR
import Codec.Serialise.Decoding qualified as CBOR
import Codec.Serialise.Encoding qualified as CBOR
-- import Common (Options (Options, optionsChainPoint, optionsNetworkId, optionsSocketPath), parseOptions, printJson,
--                workaround)
import Cardano.Api.Shelley hiding (ApplyBlockError)
import Cardano.Tracing.Render
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Short qualified as SBS
import Data.IORef
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe
import Data.Proxy
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.String
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format.ISO8601
import Data.Word
import Ledger qualified
import Options.Applicative qualified as O
import Orphans ()
import Plutus.Script.Utils.V1.Scripts qualified as V1
import Plutus.Streaming
import Streaming
import Streaming.Prelude qualified as S
import System.FilePath
import System.IO.Unsafe
import Unsafe.Coerce

{-
Example:

cabal v2-run plutus-streaming:fold -- \
  --socket-path $HOME/cardano/db/node.socket \
  --config $HOME/cardano/mainnet-config.json \
  --mainnet \
  --chunk-size 10 \
  --dir $HOME/cardano-dump \
  --checkpoint-path /home/zliu41/cardano-dump/2022-06-06T12:18:50.334048649Z.state

-}

newtype ApplyBlockError = ApplyBlockError LedgerStateError

instance Show ApplyBlockError where
  show (ApplyBlockError e) = Text.unpack (renderLedgerStateError e)

instance Exception ApplyBlockError

data Options = Options
  { optsSocketPath     :: FilePath,
    optsConfigPath     :: FilePath,
    optsNetworkId      :: NetworkId,
    optsChunkSize      :: Int,
    optsCheckpointFile :: FilePath,
    optsDir            :: FilePath
  } deriving (Show)

options :: O.Parser Options
options = do
  optsSocketPath <- O.strOption $
      mconcat
        [ O.long "socket-path",
          O.metavar "SOCKET_PATH",
          O.help "Node socket path"
        ]
  optsConfigPath <- O.strOption $
      mconcat
        [ O.long "config",
          O.metavar "CONFIG_PATH",
          O.help "Node config path"
        ]
  optsNetworkId <- networkIdParser
  optsChunkSize <- O.option O.auto $
      mconcat
        [ O.long "chunk-size",
          O.metavar "CHUNK_SIZE",
          O.help "chunk size"
        ]
  optsCheckpointFile <- O.strOption $
      mconcat
        [ O.long "checkpoint-path",
          O.value "",
          O.help "CHECKPPINT PATH"
        ]
  optsDir <- O.strOption $
      mconcat
        [ O.short 'd',
          O.long "dir",
          O.metavar "DIR",
          O.help "Data dump directory"
        ]
  pure Options {..}

networkIdParser :: O.Parser NetworkId
networkIdParser =
  pMainnet' <|> fmap Testnet testnetMagicParser
  where
    pMainnet' :: O.Parser NetworkId
    pMainnet' =
      O.flag'
        Mainnet $ mconcat
        [ O.long "mainnet",
          O.help "Use the mainnet magic id."
        ]


testnetMagicParser :: O.Parser Cardano.Api.NetworkMagic
testnetMagicParser =
  Cardano.Api.NetworkMagic
    <$> (O.option
      O.auto $ mconcat
      [ O.long "testnet-magic",
        O.metavar "NATURAL",
        O.help "Specify a testnet magic id."
      ])

parserInfo :: O.ParserInfo Options
parserInfo =
      O.info
        (options O.<**> O.helper)
        mempty

main :: IO ()
main = do
  opts <- O.execParser parserInfo
  (env, ledgerStateAtGenesis) <-
    either (fail . Text.unpack . renderInitialLedgerStateError) pure =<<
     runExceptT (initialLedgerState (optsConfigPath opts))
  let ledgerState = ledgerStateAtGenesis
      chainPoint = ChainPointAtGenesis
      -- chainPoint = ChainPoint (SlotNo 4999)
      --   (fromString "2c96784c55986b9ea26116c6cc9ee70582ed6cad00fd30a849c98b874d5a6b89")
  _ <- runExceptT $ foldBlocks (optsConfigPath opts) (optsSocketPath opts) QuickValidation () act
  pure ()
  (chainPoint, ledgerState) <- if null (optsCheckpointFile opts)
    then pure (ChainPointAtGenesis, ledgerStateAtGenesis)
    else do
      streamState <- CBOR.readFileDeserialise (optsCheckpointFile opts)
      -- let actualChainPoint = case ssChainPoint streamState of
      --       ChainPointAtGenesis -> ChainPointAtGenesis
      --       ChainPoint _ hash -> ChainPoint (SlotNo 55033) hash
      case ssChainPoint streamState of
        ChainPointAtGenesis -> putStrLn "Got Genesis chainpoint!"
        ChainPoint _ hash   -> putStrLn $ "Got chain point at::: " <> show (ssChainPoint streamState)
      pure (ssChainPoint streamState, ssLedgerState streamState)

  let onError :: ApplyBlockError -> IO ()
      onError _ = putStrLn $ "GOT ERROR!!!!!!!!!!! "


  let consumeStream = handle onError $ withChainSyncEventStream (optsSocketPath opts) (optsNetworkId opts) chainPoint
        $ \stream -> do
          let liftedStream :: Stream (Of (SimpleChainSyncEvent, (LedgerState, [LedgerEvent]))) M ()
              liftedStream = hoist liftIO $ ledgerState' env ledgerState QuickValidation stream
              act = consume (optsDir opts) (optsChunkSize opts) liftedStream
          evalStateT act (CheckpointState 0 [])

  consumeStream


act :: Env -> LedgerState -> [LedgerEvent] -> BlockInMode CardanoMode -> () -> IO ()
act env _ _ block _ = do
  modifyIORef' ref (+1)
  cnt <- readIORef ref
  when (cnt `mod` 100 == 0) $ do
    putStrLn $ "chain point is::: " ++ show (getChainPoint block)

ref :: IORef Int
ref = unsafePerformIO $ newIORef 0
{-# NOINLINE ref #-}


type History a = Seq (SlotNo, a)

type SimpleChainSyncEvent = ChainSyncEvent (BlockInMode CardanoMode)

data ScriptEvent
  = ScriptEventSuccess [PlutusDebug]
  | ScriptEventFailure (NonEmpty PlutusDebug)

instance CBOR.Serialise ScriptEvent where
  encode = \case
    ScriptEventSuccess ds -> CBOR.encodeListLen 2 <> CBOR.encodeWord 0 <> toCBOR ds
    ScriptEventFailure ds -> CBOR.encodeListLen 2 <> CBOR.encodeWord 1 <> toCBOR ds

  decode = do
    CBOR.decodeListLenOf 2
    CBOR.decodeWord >>= \case
      0 -> ScriptEventSuccess <$> fromCBOR
      1 -> ScriptEventFailure <$> fromCBOR
      _ -> fail "unknown tag"

getChainPoint :: BlockInMode CardanoMode -> ChainPoint
getChainPoint (BlockInMode (Block (BlockHeader slot hash _) _) _) = ChainPoint slot hash

encodeChainPoint :: ChainPoint -> CBOR.Encoding
encodeChainPoint p = CBOR.encode $ case p of
  ChainPointAtGenesis  -> Nothing
  ChainPoint slot hash -> Just (slot, serialiseToRawBytes hash)

decodeChainPoint :: CBOR.Decoder s ChainPoint
decodeChainPoint = CBOR.decode >>= \case
  Nothing -> pure ChainPointAtGenesis
  Just (slot, hashRawBytes) ->
    maybe (fail "Unable to decode block hash")
      (pure . ChainPoint slot)
      (deserialiseFromRawBytes (proxyToAsType (Proxy @(Hash BlockHeader))) hashRawBytes)

data StreamerState = StreamerState
  { ssChainPoint  :: ChainPoint,
    ssLedgerState :: LedgerState
  }

instance CBOR.Serialise StreamerState where
  encode (StreamerState chainPoint ledgerState) = mconcat
    [CBOR.encodeListLen 2, encodeChainPoint chainPoint, encodeLedgerState ledgerState]
  decode = do
    CBOR.decodeListLenOf 2
    StreamerState <$> decodeChainPoint <*> decodeLedgerState

data CheckpointState = CheckpointState
  { csCount  :: Int,
    csEvents :: [ScriptEvent]
  }

type M = StateT CheckpointState IO

consume :: forall r. FilePath -> Int ->
  Stream (Of (SimpleChainSyncEvent, (LedgerState, [LedgerEvent]))) M r -> M r
consume dir chunkSize stream = do
  S.mapM_ (uncurry (uncurry . checkpoint)) stream
  where
    checkpoint :: SimpleChainSyncEvent -> LedgerState -> [LedgerEvent]
      -> M ()
    checkpoint ev ledgerState ledgerEvents = case ev of
      RollForward block tip -> do
        checkpointState <- get
        if csCount checkpointState >= chunkSize
          then do
            time <- liftIO getCurrentTime
            let eventFile = dir </> iso8601Show time <.> "events"
            liftIO $ CBOR.writeFileSerialise eventFile (csEvents checkpointState)
            let stateFile = dir </> iso8601Show time <.> "state"
            liftIO $ CBOR.writeFileSerialise stateFile (StreamerState (getChainPoint block) ledgerState)
            liftIO $ case getChainPoint block of
                ChainPointAtGenesis -> putStrLn $ "checkpoint point at genesis a block"
                ChainPoint (SlotNo sss) hash -> do
                  -- let conPoint = toConsensusPointInMode CardanoMode (getChainPoint block)
                  -- let hashStr = Text.decodeLatin1 $ B16.encode $ SBS.fromShort (unsafeCoerce hash)
                  --     rendered = renderPointAsPhrase conPoint
                  -- putStrLn $ "\nCHECKPOINTING: " ++ stateFile
                  putStrLn $ "chainpoint: " ++ show (getChainPoint block)
                  -- putStrLn $ "ACTUAL SLOT::::::::: " ++ show sss
                  -- putStrLn $ "hash: " ++ show hashStr
                  -- putStrLn $ "RENDERED " ++ show rendered
                  -- case tip of
                  --   ChainTipAtGenesis    -> pure ()
                  --   ChainTip tipSlot _ _ -> putStrLn $ "TIPSLOT ========== " ++ show tipSlot
            put $ CheckpointState 0 []
           -- liftIO $ putStrLn $ "ACTUAL SLOT === " ++ show actualSlot
          else do
            when (csCount checkpointState == 0) $ liftIO $ do
              case getChainPoint block of
                ChainPointAtGenesis -> putStrLn $ "got chain point at genesis a block"
                ChainPoint _ hash   -> putStrLn $ " got a block with hash :: " ++ show (getChainPoint block)
            put $ checkpointState {
                csEvents = mapMaybe toScriptEvent ledgerEvents ++ csEvents checkpointState,
                csCount = csCount checkpointState + 1}
      RollBackward{} -> pure ()


toScriptEvent :: LedgerEvent -> Maybe ScriptEvent
toScriptEvent = \case
  SuccessfulPlutusScript ds -> Just (ScriptEventSuccess ds)
  FailedPlutusScript ds     -> Just (ScriptEventFailure ds)
  _                         -> Nothing


-- ledgerState ::
--   forall m r.
--   Monad m =>
--   Env ->
--   LedgerState ->
--   ValidationMode ->
--   Stream (Of SimpleChainSyncEvent) m r ->
--   Stream (Of (LedgerState, [LedgerEvent])) m r
-- ledgerState env ls0 vm = S.map snd . ledgerState' env ls0 vm

-- | This function works under the assumption that the stream of blocks it
-- receives is valid. The function will trigger an exception if
-- 1. a block it receives does not apply on top of the ledger state
-- 2. a rollback goes past the security parameter
-- FIXME, for the moment I kept this function pure but it requires us to do
-- some up-front IO to obtain the initial ledger state from the network
-- config file.
ledgerState' ::
  forall m r.
  Monad m =>
  Env ->
  LedgerState ->
  ValidationMode ->
  Stream (Of SimpleChainSyncEvent) m r ->
  Stream (Of (SimpleChainSyncEvent, (LedgerState, [LedgerEvent]))) m r
ledgerState' env ls0 vm =
  S.scanned step initialHistory projection
  where
    step ::
      (History LedgerState, [LedgerEvent]) ->
      SimpleChainSyncEvent ->
      (History LedgerState, [LedgerEvent])
    step (history, _) (RollForward (BlockInMode blk _) _) =
      unsafePushBlock history blk
    step _ (RollBackward ChainPointAtGenesis _) =
      initialHistory
    step (history, _) (RollBackward (ChainPoint sn _) _) =
      unsafeRollback history sn

    initialHistory :: (History LedgerState, [LedgerEvent])
    initialHistory = (Seq.singleton (0, ls0), [])

    -- This function is unsafe because it might result in an empty history,
    -- breaking the assumption of unsafePushBlock and projection
    unsafeRollback :: History LedgerState -> SlotNo -> (History LedgerState, [LedgerEvent])
    unsafeRollback history sn =
      let history' = Seq.dropWhileL ((> sn) . fst) history
       in (history', [])

    -- This function is unsafe because it will assume the given block will
    -- successfully apply on top of the ledger state.
    unsafePushBlock :: History LedgerState -> Block era -> (History LedgerState, [LedgerEvent])
    unsafePushBlock history@((_, ls) :<| _) blk@(Block (BlockHeader sn _ _) _) = unsafePerformIO $ do
      pure $ case applyBlock env ls vm blk of
        Left e ->
          error $ "applyBlock failed " <> show e
        Right (ls', lse) ->
          let history' = fst $ Seq.splitAt (fromIntegral $ envSecurityParam env + 1) ((sn, ls') :<| history)
           in (history', lse)
    unsafePushBlock Seq.Empty _ = error "Impossible! History should never be empty"

    projection :: (History LedgerState, [LedgerEvent]) -> (LedgerState, [LedgerEvent])
    projection ((_, ls) :<| _, lse) = (ls, lse)
    projection (Seq.Empty, _)       = error "Impossible! History should never be empty"
