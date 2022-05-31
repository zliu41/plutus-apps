{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module PlutusExample.Alonzo.AlwaysSucceeds
  ( alwaysSucceedsScriptV1
  , alwaysSucceedsScriptV2
  , alwaysSucceedsScriptShortBsV1
  , alwaysSucceedsScriptShortBsV2
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
mkValidator _ _ _ = ()

-- V1

validatorV1 :: Plutus.V1.Validator
validatorV1 = Plutus.V1.mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

scriptV1 :: Plutus.V1.Script
scriptV1 = Plutus.V1.unValidatorScript validatorV1

alwaysSucceedsScriptShortBsV1 :: SBS.ShortByteString
alwaysSucceedsScriptShortBsV1 = SBS.toShort . LBS.toStrict $ serialise scriptV1

alwaysSucceedsScriptV1 :: PlutusScript PlutusScriptV1
alwaysSucceedsScriptV1 = PlutusScriptSerialised alwaysSucceedsScriptShortBsV1

-- V2

validatorV2 :: Plutus.V2.Validator
validatorV2 = Plutus.V2.mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

scriptV2 :: Plutus.V2.Script
scriptV2 = Plutus.V2.unValidatorScript validatorV2

alwaysSucceedsScriptShortBsV2 :: SBS.ShortByteString
alwaysSucceedsScriptShortBsV2 = SBS.toShort . LBS.toStrict $ serialise scriptV2

alwaysSucceedsScriptV2 :: PlutusScript PlutusScriptV2
alwaysSucceedsScriptV2 = PlutusScriptSerialised alwaysSucceedsScriptShortBsV2

