{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module PlutusExample.Alonzo.Sum
  where

import Prelude hiding (($), (+), (-), (==))

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1, PlutusScriptV2)

import Codec.Serialise
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS

import Plutus.Script.Utils.V1.Scripts qualified as PSU.V1
import Plutus.Script.Utils.V2.Scripts qualified as PSU.V2
import Plutus.V1.Ledger.Api qualified as Plutus.V1
import Plutus.V2.Ledger.Api qualified as Plutus.V2
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..), unless, (.))


smartSum :: Integer -> Integer
smartSum a = loop a 0
 where
  loop !n !acc = if n==0
    then acc
    else loop (n - 1) (n + acc)

-- | The validation function (DataValue -> RedeemerValue -> ScriptContext -> Bool)
{-# INLINABLE validateSum #-}
validateSum :: Integer -> Integer -> x -> Bool
validateSum n s _ = isGoodSum n s

{-# INLINABLE isGoodSum #-}
isGoodSum :: Integer -> Integer -> Bool
isGoodSum n s = smartSum n == s

-- V1

validatorV1 :: Plutus.V1.Validator
validatorV1 = Plutus.V1.mkValidatorScript $$(PlutusTx.compile [|| wrap ||])
 where
     wrap = PSU.V1.mkUntypedValidator validateSum

scriptV1 :: Plutus.V1.Script
scriptV1 = Plutus.V1.unValidatorScript validatorV1

sumScriptShortBsV1 :: SBS.ShortByteString
sumScriptShortBsV1 = SBS.toShort . LBS.toStrict $ serialise scriptV1

sumScriptV1 :: PlutusScript PlutusScriptV1
sumScriptV1 = PlutusScriptSerialised sumScriptShortBsV1

-- V2

validatorV2 :: Plutus.V2.Validator
validatorV2 = Plutus.V2.mkValidatorScript $$(PlutusTx.compile [|| wrap ||])
 where
     wrap = PSU.V2.mkUntypedValidator validateSum

scriptV2 :: Plutus.V2.Script
scriptV2 = Plutus.V2.unValidatorScript validatorV2

sumScriptShortBsV2 :: SBS.ShortByteString
sumScriptShortBsV2 = SBS.toShort . LBS.toStrict $ serialise scriptV2

sumScriptV2 :: PlutusScript PlutusScriptV2
sumScriptV2 = PlutusScriptSerialised sumScriptShortBsV2
