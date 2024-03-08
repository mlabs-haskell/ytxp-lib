module Cardano.YTxP.Control.Yielding.Validator (
  -- * Validator
  YieldingValidatorScript (getYieldingValidatorScript),
  compileYieldingValidator,

  -- * Script Credential
  YieldingValidatorCredential,
  mkYieldingValidatorCredential,
) where

import Cardano.YTxP.Control.Stubs (alwaysSucceedsValidator,
                                   noncedValidatorWrapper)
import Cardano.YTxP.Control.YieldList.MintingPolicy (YieldListSTCS)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Plutarch (Config, compile)
import Plutarch.Api.V2 (PScriptContext, scriptHash)
import Plutarch.Script (Script)
import PlutusLedgerApi.V2 (Credential (ScriptCredential))

--------------------------------------------------------------------------------
-- Yielding Validator Script

-- | @since 0.1.0
newtype YieldingValidatorScript = YieldingValidatorScript
  { -- | @since 0.1.0
    getYieldingValidatorScript :: Script
  }
  deriving (
    -- | @since 0.1.0
    ToJSON,
    -- | @since 0.1.0
    FromJSON
    ) via (HexStringScript "YieldingValidatorScript")

compileYieldingValidator ::
  Config ->
  YieldListSTCS ->
  (Either Text)
    YieldingValidatorScript
compileYieldingValidator config ylstcs = do
  let
    yieldingValidator ::
      forall (s :: S).
      ( Term s (PData :--> PData :--> PScriptContext :--> POpaque)
      )
    yieldingValidator = mkYieldingValidator ylstcs

  script <- compile config yieldingValidator
  pure $ YieldingValidatorScript script

-------------------------------------------------------------------------------
-- Yielding Validator Credential

-- | Opaque, semantic newtype for the YieldList state thread currency symbol
newtype YieldingValidatorCredential = YieldingValidatorCredential Credential

mkYieldingValidatorCredential ::
  YieldingValidatorScript -> YieldingValidatorCredential
mkYieldingValidatorCredential (YieldingValidatorScript script) =
  YieldingValidatorCredential $ ScriptCredential (scriptHash script)

--------------------------------------------------------------------------------
-- Helpers (Unexported)

mkYieldingValidator ::
  forall (s :: S).
  YieldListSTCS ->
  Term s (PData :--> PData :--> PScriptContext :--> POpaque)
mkYieldingValidator _ylstcs =
  noncedValidatorWrapper
    @PString
    "YieldingValidator"
    alwaysSucceedsValidator
