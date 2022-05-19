{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module PlutusExample.PlutusVersion2.RequireRedeemer
  ( requireRedeemerScript
  , requireRedeemerScriptShortBs
  ) where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import Prelude hiding (($))

import Codec.Serialise
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS

import Plutus.Script.Utils.V1.Scripts.MonetaryPolicies as Scripts
import Plutus.Script.Utils.V1.Scripts.Validators as Scripts
import Plutus.V1.Ledger.Contexts (ScriptContext)
import Plutus.V1.Ledger.Scripts as Scripts
import Plutus.V2.Ledger.Api qualified as Plutus
import PlutusTx qualified
import PlutusTx.Builtins
import PlutusTx.Eq as PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless, (.))

-- serialiseData is a PlutusV2 builtin

{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> ScriptContext -> Bool
mkValidator _ redeemer _ = serialiseData redeemer PlutusTx./= emptyByteString


validator :: Plutus.Validator
validator = Plutus.mkValidatorScript
   $$(PlutusTx.compile [|| wrap ||])
 where
   wrap = Scripts.mkUntypedValidator mkValidator

script :: Plutus.Script
script = Plutus.unValidatorScript validator

requireRedeemerScriptShortBs :: SBS.ShortByteString
requireRedeemerScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

requireRedeemerScript :: PlutusScript PlutusScriptV2
requireRedeemerScript = PlutusScriptSerialised requireRedeemerScriptShortBs
