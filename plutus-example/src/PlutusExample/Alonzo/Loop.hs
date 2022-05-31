{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module PlutusExample.Alonzo.Loop
  ( loopScriptV1
  , loopScriptV2
  , loopScriptShortBsV1
  , loopScriptShortBsV2
  ) where

import Prelude hiding (pred, ($), (&&), (<), (==))

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1, PlutusScriptV2)

import Codec.Serialise
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS

import Plutus.V1.Ledger.Api qualified as Plutus.V1
import Plutus.V2.Ledger.Api qualified as Plutus.V2
import PlutusTx
import PlutusTx.Builtins (unsafeDataAsI)
import PlutusTx.Prelude hiding (Semigroup (..), unless, (.))

{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _datum redeemer _txContext
  = if n < 1000000
       then traceError "redeemer is < 1000000"
       else loop n
  where
    n = unsafeDataAsI redeemer
    loop i = if i == 1000000 then () else loop $ pred i

-- V1

validatorV1 :: Plutus.V1.Validator
validatorV1 = Plutus.V1.mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

scriptV1 :: Plutus.V1.Script
scriptV1 = Plutus.V1.unValidatorScript validatorV1

loopScriptShortBsV1 :: SBS.ShortByteString
loopScriptShortBsV1 = SBS.toShort . LBS.toStrict $ serialise scriptV1

loopScriptV1 :: PlutusScript PlutusScriptV1
loopScriptV1 = PlutusScriptSerialised loopScriptShortBsV1

-- V2

validatorV2 :: Plutus.V2.Validator
validatorV2 = Plutus.V2.mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

scriptV2 :: Plutus.V2.Script
scriptV2 = Plutus.V2.unValidatorScript validatorV2

loopScriptShortBsV2 :: SBS.ShortByteString
loopScriptShortBsV2 = SBS.toShort . LBS.toStrict $ serialise scriptV2

loopScriptV2 :: PlutusScript PlutusScriptV2
loopScriptV2 = PlutusScriptSerialised loopScriptShortBsV2
