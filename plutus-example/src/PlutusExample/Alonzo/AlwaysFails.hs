{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module PlutusExample.Alonzo.AlwaysFails
  ( alwaysFailsScriptV1
  , alwaysFailsScriptV2
  , alwaysFailsScriptShortBsV1
  , alwaysFailsScriptShortBsV2
  ) where

import Prelude hiding (($))

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1, PlutusScriptV2)

import Codec.Serialise
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS

import Plutus.V1.Ledger.Api qualified as Plutus.V1
import Plutus.V2.Ledger.Api qualified as Plutus.V2
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..), unless, (.))

{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _ _ _ = PlutusTx.Prelude.error ()

-- V1

validatorV1 :: Plutus.V1.Validator
validatorV1 = Plutus.V1.mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

scriptV1 :: Plutus.V1.Script
scriptV1 = Plutus.V1.unValidatorScript validatorV1

alwaysFailsScriptShortBsV1 :: SBS.ShortByteString
alwaysFailsScriptShortBsV1 = SBS.toShort . LBS.toStrict $ serialise scriptV1

alwaysFailsScriptV1 :: PlutusScript PlutusScriptV1
alwaysFailsScriptV1 = PlutusScriptSerialised alwaysFailsScriptShortBsV1

-- V2

validatorV2 :: Plutus.V2.Validator
validatorV2 = Plutus.V2.mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

scriptV2 :: Plutus.V2.Script
scriptV2 = Plutus.V2.unValidatorScript validatorV1

alwaysFailsScriptShortBsV2 :: SBS.ShortByteString
alwaysFailsScriptShortBsV2 = SBS.toShort . LBS.toStrict $ serialise scriptV2

alwaysFailsScriptV2 :: PlutusScript PlutusScriptV2
alwaysFailsScriptV2 = PlutusScriptSerialised alwaysFailsScriptShortBsV2
