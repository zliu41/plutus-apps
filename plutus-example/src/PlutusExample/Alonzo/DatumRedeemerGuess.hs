{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module PlutusExample.Alonzo.DatumRedeemerGuess
  ( guessScriptV1
  , guessScriptV2
  , guessScriptStakeV1
  , guessScriptStakeV2
  ) where

import Prelude hiding (($), (&&), (==))

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1, PlutusScriptV2)

import Codec.Serialise
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS

import Plutus.V1.Ledger.Api qualified as Plutus.V1
import Plutus.V2.Ledger.Api qualified as Plutus.V2
import PlutusTx (toBuiltinData)
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..), unless, (.))

{-
  Spending script
-}

{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData  -> ()
mkValidator datum redeemer _txContext
  |    datum    == toBuiltinData (42 :: Integer)
    && redeemer == toBuiltinData (42 :: Integer) = ()
  | otherwise = traceError "Incorrect datum. Expected 42."

-- V1

validatorV1 :: Plutus.V1.Validator
validatorV1 = Plutus.V1.mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

scriptV1 :: Plutus.V1.Script
scriptV1 = Plutus.V1.unValidatorScript validatorV1

datumRedeemerGuessScriptShortBsV1 :: SBS.ShortByteString
datumRedeemerGuessScriptShortBsV1 = SBS.toShort . LBS.toStrict $ serialise scriptV1

guessScriptV1 :: PlutusScript PlutusScriptV1
guessScriptV1 = PlutusScriptSerialised datumRedeemerGuessScriptShortBsV1

-- V2

validatorV2 :: Plutus.V2.Validator
validatorV2 = Plutus.V2.mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

scriptV2 :: Plutus.V2.Script
scriptV2 = Plutus.V2.unValidatorScript validatorV2

datumRedeemerGuessScriptShortBsV2 :: SBS.ShortByteString
datumRedeemerGuessScriptShortBsV2 = SBS.toShort . LBS.toStrict $ serialise scriptV2

guessScriptV2 :: PlutusScript PlutusScriptV2
guessScriptV2 = PlutusScriptSerialised datumRedeemerGuessScriptShortBsV1

{-
  Staking script
-}

{-# INLINEABLE mkValidatorStake #-}
mkValidatorStake :: BuiltinData -> BuiltinData -> ()
mkValidatorStake redeemer _txContext
  | redeemer == toBuiltinData (42 :: Integer) = ()
  | otherwise = traceError "Incorrect datum. Expected 42."

-- V1

validatorStakeV1 :: Plutus.V1.StakeValidator
validatorStakeV1 = Plutus.V1.mkStakeValidatorScript $$(PlutusTx.compile [||mkValidatorStake||])

scriptStakeV1 :: Plutus.V1.Script
scriptStakeV1 = Plutus.V1.unStakeValidatorScript validatorStakeV1

datumRedeemerGuessScriptStakeShortBsV1 :: SBS.ShortByteString
datumRedeemerGuessScriptStakeShortBsV1 = SBS.toShort . LBS.toStrict $ serialise scriptStakeV1

guessScriptStakeV1 :: PlutusScript PlutusScriptV1
guessScriptStakeV1 = PlutusScriptSerialised datumRedeemerGuessScriptStakeShortBsV1

-- V2

validatorStakeV2 :: Plutus.V2.StakeValidator
validatorStakeV2 = Plutus.V2.mkStakeValidatorScript $$(PlutusTx.compile [||mkValidatorStake||])

scriptStakeV2 :: Plutus.V2.Script
scriptStakeV2 = Plutus.V2.unStakeValidatorScript validatorStakeV2

datumRedeemerGuessScriptStakeShortBsV2 :: SBS.ShortByteString
datumRedeemerGuessScriptStakeShortBsV2 = SBS.toShort . LBS.toStrict $ serialise scriptStakeV2

guessScriptStakeV2 :: PlutusScript PlutusScriptV2
guessScriptStakeV2 = PlutusScriptSerialised datumRedeemerGuessScriptStakeShortBsV2
