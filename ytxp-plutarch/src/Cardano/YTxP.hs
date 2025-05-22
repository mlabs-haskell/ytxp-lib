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
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Text qualified as T
import GHC.Natural (naturalToInteger)
import Plutarch.Internal.Term (Config, compile, tracingMode)
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
ytxpBlueprint :: Config -> SdkParameters -> ContractBlueprint
ytxpBlueprint config params =
  MkContractBlueprint
    { contractId = Nothing
    , contractPreamble =
        MkPreamble
          { preambleTitle = "Yielding Transaction Pattern Library (ytxp-lib)"
          , preambleDescription = Nothing
          , preambleVersion = "1.0.0"
          , preamblePlutusVersion =
              reifyVersion $ Proxy @(VersionOf PType)
          , preambleLicense = Nothing
          }
    , contractValidators =
        Set.fromList $ yieldingBlueprints config params
    , contractDefinitions =
        -- Note (see Ply example): We have to manually prepend datum/redeemer to the types because it does not exist on the Plutarch type.
        derivePDefinitions @(PData ': ParamsOf PType)
    }

{- |
Generates the validator blueprints.

@since 0.1.0
-}
yieldingBlueprints ::
  Config -> SdkParameters -> [ValidatorBlueprint YieldingReferenceTypes]
yieldingBlueprints config (SdkParameters svNonces mpNonces cvNonces vvNonces pvNonces stcs) =
  mkYieldingBlueprint
    config
    "Yielding Spending"
    (yielding # pconstant (coerce stcs) # pdata pzero)
    : fmap
      ( \nonce ->
          mkYieldingBlueprint config "Yielding Rewarding" $
            yielding # pconstant (coerce stcs) # pdata (pconstant $ naturalToInteger nonce)
      )
      svNonces
      <> fmap
        ( \nonce ->
            mkYieldingBlueprint config "Yielding Minting" $
              yielding # pconstant (coerce stcs) # pdata (pconstant $ naturalToInteger nonce)
        )
        mpNonces
      <> fmap
        ( \nonce ->
            mkYieldingBlueprint config "Yielding Certifying" $
              yielding # pconstant (coerce stcs) # pdata (pconstant $ naturalToInteger nonce)
        )
        cvNonces
      <> fmap
        ( \nonce ->
            mkYieldingBlueprint config "Yielding Voting" $
              yielding # pconstant (coerce stcs) # pdata (pconstant $ naturalToInteger nonce)
        )
        vvNonces
      <> fmap
        ( \nonce ->
            mkYieldingBlueprint config "Yielding Proposing" $
              yielding # pconstant (coerce stcs) # pdata (pconstant $ naturalToInteger nonce)
        )
        pvNonces

{- |
Creates a validator blueprint.

@since 0.1.0
-}
mkYieldingBlueprint ::
  Config ->
  T.Text ->
  ClosedTerm PType ->
  ValidatorBlueprint YieldingReferenceTypes
mkYieldingBlueprint config title ct =
  MkValidatorBlueprint
    { validatorTitle = title
    , validatorDescription =
        Just $
          if isJust (tracingMode config)
            then "Compiled with traces"
            else "Compiled without traces"
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
          -- Note (see Ply example): When using 'mkParamSchemas', the second type argument should only contain the params (i.e from 'ParamsOf'), not the datum/redeemer.
          $ mkParamSchemas @YieldingReferenceTypes @(ParamsOf PType)
    , validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "Yielding redeemer"
          , argumentDescription = Nothing
          , argumentPurpose = []
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
  where
    script = either (error . T.unpack) id $ compile config ct
    ScriptHash hash = scriptHash script
