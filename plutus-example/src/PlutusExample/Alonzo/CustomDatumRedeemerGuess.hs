{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module PlutusExample.Alonzo.CustomDatumRedeemerGuess
  ( MyCustomDatum(..)
  , MyCustomRedeemer(..)
  , customGuessScriptV1
  , customGuessScriptV2
  , customDatumRedeemerGuessScriptAsShortBsV1
  , customDatumRedeemerGuessScriptAsShortBsV2
  ) where

import Prelude hiding (($), (&&), (==))

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1, PlutusScriptV2)

import Codec.Serialise
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS

import Plutus.Script.Utils.V1.Scripts qualified as PSU.V1
import Plutus.Script.Utils.V2.Scripts qualified as PSU.V2
import Plutus.V1.Ledger.Api qualified as Plutus.V1
import Plutus.V2.Ledger.Api qualified as Plutus.V2
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup ((<>)), unless, (.))

newtype MyCustomDatum = MyCustomDatum Integer
newtype MyCustomRedeemer = MyCustomRedeemer Integer

PlutusTx.unstableMakeIsData ''MyCustomDatum
PlutusTx.unstableMakeIsData ''MyCustomRedeemer

-- V1

{-# INLINABLE mkValidatorV1 #-}
mkValidatorV1 :: MyCustomDatum -> MyCustomRedeemer -> Plutus.V1.ScriptContext -> Bool
mkValidatorV1 (MyCustomDatum d) (MyCustomRedeemer r) _ =
  d == 42 && r == 42

validatorV1 :: Plutus.V1.Validator
validatorV1 = Plutus.V1.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
 where
     wrap = PSU.V1.mkUntypedValidator mkValidatorV1

scriptV1 :: Plutus.V1.Script
scriptV1 = Plutus.V1.unValidatorScript validatorV1

customDatumRedeemerGuessScriptAsShortBsV1 :: SBS.ShortByteString
customDatumRedeemerGuessScriptAsShortBsV1 = SBS.toShort . LBS.toStrict $ serialise scriptV1

customGuessScriptV1 :: PlutusScript PlutusScriptV1
customGuessScriptV1 = PlutusScriptSerialised customDatumRedeemerGuessScriptAsShortBsV1

-- V2

{-# INLINABLE mkValidatorV2 #-}
mkValidatorV2 :: MyCustomDatum -> MyCustomRedeemer -> Plutus.V2.ScriptContext -> Bool
mkValidatorV2 (MyCustomDatum d) (MyCustomRedeemer r) _ =
  d == 42 && r == 42

validatorV2 :: PSU.V2.Validator
validatorV2 = Plutus.V2.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
 where
     wrap = PSU.V2.mkUntypedValidator mkValidatorV2

scriptV2 :: Plutus.V2.Script
scriptV2 = Plutus.V2.unValidatorScript validatorV2

customDatumRedeemerGuessScriptAsShortBsV2 :: SBS.ShortByteString
customDatumRedeemerGuessScriptAsShortBsV2 = SBS.toShort . LBS.toStrict $ serialise scriptV2

customGuessScriptV2 :: PlutusScript PlutusScriptV2
customGuessScriptV2 = PlutusScriptSerialised customDatumRedeemerGuessScriptAsShortBsV2
