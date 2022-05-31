
import Prelude

import Cardano.Api

import System.Directory
import System.FilePath.Posix ((</>))

import PlutusExample.Alonzo.AlwaysFails (alwaysFailsScriptV1, alwaysFailsScriptV2)
import PlutusExample.Alonzo.AlwaysSucceeds (alwaysSucceedsScriptV1, alwaysSucceedsScriptV2)
import PlutusExample.Alonzo.CustomDatumRedeemerGuess (customGuessScriptV1, customGuessScriptV2)
import PlutusExample.Alonzo.DatumRedeemerGuess (guessScriptStakeV1, guessScriptStakeV2, guessScriptV1, guessScriptV2)
import PlutusExample.Alonzo.Loop (loopScriptV1, loopScriptV2)
import PlutusExample.Alonzo.MintingScript (apiExamplePlutusMintingScriptV1, apiExamplePlutusMintingScriptV2)
import PlutusExample.Alonzo.RedeemerContextScripts (scriptContextTestMintingScriptV1, scriptContextTextPayingScriptV1)
import PlutusExample.Alonzo.Sum (sumScriptV1, sumScriptV2)

import PlutusExample.Babbage.RequireRedeemer (requireRedeemerScriptV2)

main :: IO ()
main = do
  let alonzoDir = "generated-plutus-scripts/alonzo"
      babbageDir = "generated-plutus-scripts/babbage"
  createDirectoryIfMissing True alonzoDir
  createDirectoryIfMissing True babbageDir

  -- Alonzo (Script with only Plutus V1 ScriptContext functionality)

  _ <- writeFileTextEnvelope (alonzoDir </> "always-fails-V1.plutus") Nothing alwaysFailsScriptV1
  _ <- writeFileTextEnvelope (alonzoDir </> "always-fails-V2.plutus") Nothing alwaysFailsScriptV2

  _ <- writeFileTextEnvelope (alonzoDir </> "always-succeeds-spending-V1.plutus") Nothing alwaysSucceedsScriptV1
  _ <- writeFileTextEnvelope (alonzoDir </> "always-succeeds-spending-V2.plutus") Nothing alwaysSucceedsScriptV2

  _ <- writeFileTextEnvelope (alonzoDir </> "custom-guess-42-datum-42-V1.plutus") Nothing customGuessScriptV1
  _ <- writeFileTextEnvelope (alonzoDir </> "custom-guess-42-datum-42-V2.plutus") Nothing customGuessScriptV2

  _ <- writeFileTextEnvelope (alonzoDir </> "guess-42-datum-42-txin-V1.plutus") Nothing guessScriptV1
  _ <- writeFileTextEnvelope (alonzoDir </> "guess-42-datum-42-txin-V2.plutus") Nothing guessScriptV1

  _ <- writeFileTextEnvelope (alonzoDir </> "guess-42-stake-V1.plutus") Nothing guessScriptStakeV1
  _ <- writeFileTextEnvelope (alonzoDir </> "guess-42-stake-V2.plutus") Nothing guessScriptStakeV2

  _ <- writeFileTextEnvelope (alonzoDir </> "anyone-can-mint-V1.plutus") Nothing apiExamplePlutusMintingScriptV1
  _ <- writeFileTextEnvelope (alonzoDir </> "anyone-can-mint-V2.plutus") Nothing apiExamplePlutusMintingScriptV2

  _ <- writeFileTextEnvelope (alonzoDir </> "sum-V1.plutus") Nothing sumScriptV1
  _ <- writeFileTextEnvelope (alonzoDir </> "sum-V2.plutus") Nothing sumScriptV2

  _ <- writeFileTextEnvelope (alonzoDir </> "loop-V1.plutus") Nothing loopScriptV1
  _ <- writeFileTextEnvelope (alonzoDir </> "loop-V2.plutus") Nothing loopScriptV2

  _ <- writeFileTextEnvelope (alonzoDir </> "context-equivalance-test-V1.plutus") Nothing scriptContextTextPayingScriptV1

  _ <- writeFileTextEnvelope (alonzoDir </> "minting-context-equivalance-test-V1.plutus") Nothing scriptContextTestMintingScriptV1

  -- Babbage (Scripts with Plutus V2 ScriptContext functionality)

  _ <- writeFileTextEnvelope (babbageDir </> "required-redeemer-V2.plutus") Nothing requireRedeemerScriptV2

  return ()
