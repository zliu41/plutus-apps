{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}


module PlutusExample.Alonzo.MintingScript
  ( apiExamplePlutusMintingScriptV1
  , apiExamplePlutusMintingScriptV2
  , mintingScriptShortBsV1
  , mintingScriptShortBsV2
  ) where

import Prelude hiding (($))

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1, PlutusScriptV2)

import Codec.Serialise
import Data.ByteString.Lazy qualified as LB
import Data.ByteString.Short qualified as SBS

import Plutus.Script.Utils.V1.Scripts qualified as PSU.V1
import Plutus.Script.Utils.V2.Scripts qualified as PSU.V2
import Plutus.V1.Ledger.Api qualified as Plutus.V1
import Plutus.V2.Ledger.Api qualified as Plutus.V2
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..), unless, (.))

-- V1

{-# INLINABLE mkPolicyV1 #-}
mkPolicyV1 :: BuiltinData -> Plutus.V1.ScriptContext -> Bool
mkPolicyV1 _redeemer _ctx = True

policyV1 :: Plutus.V1.MintingPolicy
policyV1 = Plutus.V1.mkMintingPolicyScript $$(PlutusTx.compile [|| wrap ||])
 where
     wrap = PSU.V1.mkUntypedMintingPolicy mkPolicyV1

plutusScriptV1 :: Plutus.V1.Script
plutusScriptV1 = Plutus.V1.unMintingPolicyScript policyV1

validatorV1 :: Plutus.V1.Validator
validatorV1 = Plutus.V1.Validator $ Plutus.V1.unMintingPolicyScript policyV1

mintingScriptShortBsV1 :: SBS.ShortByteString
mintingScriptShortBsV1 = SBS.toShort . LB.toStrict $ serialise plutusScriptV1

apiExamplePlutusMintingScriptV1 :: PlutusScript PlutusScriptV1
apiExamplePlutusMintingScriptV1 = PlutusScriptSerialised mintingScriptShortBsV1

-- V2

{-# INLINABLE mkPolicyV2 #-}
mkPolicyV2 :: BuiltinData -> Plutus.V2.ScriptContext -> Bool
mkPolicyV2 _redeemer _ctx = True

policyV2 :: Plutus.V2.MintingPolicy
policyV2 = Plutus.V2.mkMintingPolicyScript $$(PlutusTx.compile [|| wrap ||])
 where
     wrap = PSU.V2.mkUntypedMintingPolicy mkPolicyV2

scriptV2 :: Plutus.V2.Script
scriptV2 =
  Plutus.V2.unMintingPolicyScript policyV2

validatorV2 :: Plutus.V2.Validator
validatorV2 = Plutus.V2.Validator $ Plutus.V2.unMintingPolicyScript policyV2

mintingScriptShortBsV2 :: SBS.ShortByteString
mintingScriptShortBsV2 = SBS.toShort . LB.toStrict $ serialise scriptV2

apiExamplePlutusMintingScriptV2 :: PlutusScript PlutusScriptV2
apiExamplePlutusMintingScriptV2 = PlutusScriptSerialised mintingScriptShortBsV2
