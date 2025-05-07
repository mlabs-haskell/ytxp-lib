{-# LANGUAGE OverloadedLists #-}

module Cardano.YTxP (
  apply,
) where

import Cardano.Binary qualified as CBOR
import Cardano.YTxP.Control.Yielding.Scripts (yielding)
import Cardano.YTxP.SDK.SdkParameters (
  AuthorisedScriptsSTCS (AuthorisedScriptsSTCS),
  SdkParameters (
    SdkParameters
  ),
 )
import Data.ByteString.Short qualified as SBS
import Data.Coerce (coerce)
import Data.Data (Proxy (Proxy))
import Data.Text qualified as T
import GHC.Natural (naturalToInteger)
import Plutarch.Internal.Term (Config, compile)
import Plutarch.LedgerApi.V3 (PScriptContext, scriptHash)
import Plutarch.Script (serialiseScript)
import PlutusLedgerApi.V3 (ScriptHash (ScriptHash))
import PlutusTx.Blueprint (
  ArgumentBlueprint (
    MkArgumentBlueprint,
    argumentDescription,
    argumentPurpose,
    argumentSchema,
    argumentTitle
  ),
  CompiledValidator (
    MkCompiledValidator,
    compiledValidatorCode,
    compiledValidatorHash
  ),
  ContractBlueprint (
    MkContractBlueprint,
    contractDefinitions,
    contractId,
    contractPreamble,
    contractValidators
  ),
  ParameterBlueprint (
    MkParameterBlueprint,
    parameterDescription,
    parameterPurpose,
    parameterSchema,
    parameterTitle
  ),
  Preamble (
    MkPreamble,
    preambleDescription,
    preambleLicense,
    preamblePlutusVersion,
    preambleTitle,
    preambleVersion
  ),
  Purpose (Mint, Spend, Withdraw),
  ValidatorBlueprint (
    MkValidatorBlueprint,
    validatorCompiled,
    validatorDatum,
    validatorDescription,
    validatorParameters,
    validatorRedeemer,
    validatorTitle
  ),
  definitionRef,
 )
import PlutusTx.Builtins qualified as PlutusTx
import Ply (reifyVersion)
import Ply.Plutarch (
  ParamsOf,
  PlyArgOf,
  ReferencedTypesOf,
  VersionOf,
  derivePDefinitions,
  mkParamSchemas,
 )

--------------------------------------------------------------------------------

apply :: Config -> SdkParameters -> [ContractBlueprint]
apply config (SdkParameters svNonces mpNonces stcs) =
  mkBlueprint
    config
    Spend
    (yielding # pconstant (coerce stcs) # pzero)
    : fmap
      ( \nonce ->
          mkBlueprint config Withdraw $
            yielding # pconstant (coerce stcs) # pconstant (naturalToInteger nonce)
      )
      svNonces
      <> fmap
        ( \nonce ->
            mkBlueprint config Mint $
              yielding # pconstant (coerce stcs) # pconstant (naturalToInteger nonce)
        )
        mpNonces

type PType = PScriptContext :--> PUnit

mkBlueprint :: Config -> Purpose -> ClosedTerm PType -> ContractBlueprint
mkBlueprint config purpose ct =
  MkContractBlueprint
    { contractId = Nothing
    , contractPreamble =
        MkPreamble
          { preambleTitle = "Example Contract" -- TODO:
          , preambleDescription = Nothing
          , preambleVersion = "1.0.0"
          , preamblePlutusVersion =
              reifyVersion $ Proxy @(VersionOf PType)
          , preambleLicense = Nothing
          }
    , contractValidators = [scriptBP]
    , contractDefinitions =
        -- Note: We have to manually prepend datum/redeemer to the types because it does not exist on the Plutarch type.
        derivePDefinitions @(PData ': ParamsOf PType)
    }
  where
    scriptBP =
      MkValidatorBlueprint
        { validatorTitle = "Example" -- TODO
        , validatorDescription = Nothing
        , validatorParameters =
            map
              ( \sch ->
                  MkParameterBlueprint
                    { parameterTitle = Nothing
                    , parameterDescription = Nothing
                    , parameterPurpose = [purpose]
                    , parameterSchema = sch
                    }
              )
              -- Note: When using 'mkParamSchemas', the second type argument should only contain the params (i.e from 'ParamsOf'), not the datum/redeemer.
              $ mkParamSchemas @(ReferencedTypesOf (PData ': ParamsOf PType)) @(ParamsOf PType)
        , validatorRedeemer =
            MkArgumentBlueprint
              { argumentTitle = Nothing
              , argumentDescription = Nothing
              , argumentPurpose = [purpose]
              , argumentSchema = definitionRef @(PlyArgOf PData) -- TODO PYieldingRedeemer
              }
        , validatorDatum = Nothing
        , validatorCompiled =
            Just
              MkCompiledValidator
                { compiledValidatorHash = PlutusTx.fromBuiltin hash
                , compiledValidatorCode = CBOR.serialize' . SBS.fromShort $ serialiseScript script
                }
        }
    script = either (error . T.unpack) id $ compile config ct
    ScriptHash hash = scriptHash script
