{-# LANGUAGE OverloadedLists #-}

{- |
Module      : Cardano.YTxP
Description : Yielding Transaction Pattern Library (ytxp-lib)

This module provides the blueprint for the Yielding Transaction Pattern Library (ytxp-lib).
-}
module Cardano.YTxP (
  ytxpBlueprint,
) where

import Cardano.Binary qualified as CBOR
import Cardano.YTxP.Control.Yielding (PYieldingRedeemer)
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
import Data.Set qualified as Set
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

{- |
Type alias for the Plutarch type.
-}
type PType = PScriptContext :--> PUnit

{- |
Type alias for the referenced types.
-}
type YieldingReferenceTypes = ReferencedTypesOf (PData ': ParamsOf PType)

{- |
Generates the blueprint for the Yielding Transaction Pattern Library.

@since 0.1.0
-}
ytxpBlueprint ::
  -- | title
  T.Text ->
  -- | description
  T.Text ->
  -- | version
  T.Text ->
  Config ->
  SdkParameters ->
  ContractBlueprint
ytxpBlueprint title description version config params =
  MkContractBlueprint
    { contractId = Nothing
    , contractPreamble =
        -- TODO Use compiler field
        MkPreamble
          { preambleTitle = title
          , preambleDescription = Just description
          , -- TODO Should be optional and set to Nothing
            preambleVersion = version
          , preamblePlutusVersion =
              reifyVersion $ Proxy @(VersionOf PType)
          , preambleLicense = Nothing
          }
    , contractValidators =
        Set.fromList $ yieldingBlueprints config params
    , contractDefinitions =
        -- TODO: Should be optional and set to Nothing
        derivePDefinitions @(PData ': ParamsOf PType)
    }

{- |
Generates the validator blueprints.

@since 0.1.0
-}
yieldingBlueprints ::
  Config -> SdkParameters -> [ValidatorBlueprint YieldingReferenceTypes]
yieldingBlueprints config (SdkParameters nonces stcs) =
  fmap
    ( \nonce ->
        mkYieldingBlueprint config $
          yielding # pconstant (coerce stcs) # pdata (pconstant $ naturalToInteger nonce)
    )
    nonces

{- |
Creates a validator blueprint.

@since 0.1.0
-}
mkYieldingBlueprint ::
  Config ->
  ClosedTerm PType ->
  ValidatorBlueprint YieldingReferenceTypes
mkYieldingBlueprint config ct =
  MkValidatorBlueprint
    { validatorTitle = "Yielding validator"
    , validatorDescription = Nothing
    , validatorParameters =
        map
          ( \sch ->
              MkParameterBlueprint
                { parameterTitle = Nothing
                , parameterDescription = Nothing
                , parameterPurpose = []
                , parameterSchema = sch
                }
          )
          $ mkParamSchemas @YieldingReferenceTypes @(ParamsOf PType)
    , validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "Yielding redeemer"
          , argumentDescription = Nothing
          , argumentPurpose = []
          , argumentSchema = definitionRef @(PlyArgOf PYieldingRedeemer)
          }
    , validatorDatum = Nothing
    , validatorCompiled =
        Just
          MkCompiledValidator
            { compiledValidatorHash = PlutusTx.fromBuiltin hash
            , compiledValidatorCode = CBOR.serialize' . SBS.fromShort $ serialiseScript script
            }
    }
  where
    script = either (error . T.unpack) id $ compile config ct
    ScriptHash hash = scriptHash script
