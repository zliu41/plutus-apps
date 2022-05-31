{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module PlutusExample.Babbage.RequireRedeemer
  ( requireRedeemerScriptV2
  , requireRedeemerScriptShortBsV2
  ) where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import Prelude hiding (($))

import Codec.Serialise
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS

import Plutus.Script.Utils.V2.Scripts.Validators as PSU.V2
import Plutus.V2.Ledger.Api qualified as Plutus.V2
import PlutusTx qualified
import PlutusTx.Builtins
import PlutusTx.Eq as PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless, (.))

-- serialiseData is a PlutusV2 builtin

{-# INLINABLE mkValidatorV2 #-}
mkValidatorV2 :: BuiltinData -> BuiltinData -> Plutus.V2.ScriptContext -> Bool
mkValidatorV2 _ redeemer _ = serialiseData redeemer PlutusTx./= emptyByteString


validatorV2 :: Plutus.V2.Validator
validatorV2 = Plutus.V2.mkValidatorScript
   $$(PlutusTx.compile [|| wrap ||])
 where
   wrap = PSU.V2.mkUntypedValidator mkValidatorV2

scriptV2 :: Plutus.V2.Script
scriptV2 = Plutus.V2.unValidatorScript validatorV2

requireRedeemerScriptShortBsV2 :: SBS.ShortByteString
requireRedeemerScriptShortBsV2 = SBS.toShort . LBS.toStrict $ serialise scriptV2

requireRedeemerScriptV2 :: PlutusScript PlutusScriptV2
requireRedeemerScriptV2 = PlutusScriptSerialised requireRedeemerScriptShortBsV2
