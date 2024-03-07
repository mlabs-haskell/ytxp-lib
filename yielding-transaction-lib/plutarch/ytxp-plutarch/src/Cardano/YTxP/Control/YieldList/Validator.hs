{-# LANGUAGE TypeApplications #-}

module Cardano.YTxP.Control.YieldList.Validator (
  -- * Validator
  YieldListValidatorScript,
  compileYieldListValidator,

  -- * Credential
  YieldListValidatorCredential,
  mkYieldListValidatorCredential,
) where

import Cardano.YTxP.Control.Stubs (alwaysSucceedsValidator,
                                   noncedValidatorWrapper)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Plutarch (Config, compile)
import Plutarch.Api.V2 (PScriptContext, scriptHash)
import Plutarch.Script (Script)
import PlutusLedgerApi.V2 (Credential (ScriptCredential))

--------------------------------------------------------------------------------
-- YieldListValidatorScript

-- | @since 0.1.0
newtype YieldListValidatorScript = YieldListValidatorScript Script
  deriving (
    -- | @since 0.1.0
    ToJSON,
    -- | @since 0.1.0
    FromJSON
    ) via (HexStringScript "YieldListValidatorScript")

compileYieldListValidator ::
  Config ->
  (forall (s :: S). Term s (PData :--> PData :--> PScriptContext :--> POpaque)) ->
  Either Text YieldListValidatorScript
compileYieldListValidator config scriptToWrap = do
  script <- compile config (yieldListValWrapper # scriptToWrap)
  pure $ YieldListValidatorScript script

--------------------------------------------------------------------------------
-- YieldListCTCS

-- | Opaque, semantic newtype for the YieldList state thread currency symbol
newtype YieldListValidatorCredential
  = YieldListValidatorCredential PlutusLedgerApi.V2.Credential

mkYieldListValidatorCredential ::
  YieldListValidatorScript ->
  YieldListValidatorCredential
mkYieldListValidatorCredential (YieldListValidatorScript script) =
  YieldListValidatorCredential
    . PlutusLedgerApi.V2.ScriptCredential
    . scriptHash
    $ script

--------------------------------------------------------------------------------
-- Helpers (Unexported)

yieldListValWrapper ::
  forall (s :: S).
  Term
    s
    ( (PData :--> PData :--> PScriptContext :--> POpaque)
        :--> PData
        :--> PData
        :--> PScriptContext
        :--> POpaque
    )
yieldListValWrapper = plam $ \_scriptToWrap ->
  noncedValidatorWrapper
    @PString
    "YieldListValidator"
    alwaysSucceedsValidator

{-
use a yieldListValWrapper to build a validator that
takes the wrapper and add in logic according to the semantics in
the spec; copied below

Semantics

    - When spent, a YieldList UTxO must burn all of its tokens besides minAda. &ldquo;Updates&rdquo; are not
    permissible; you must chain a creation and deletion.
    - A transaction spending a UTxO at this address must have exactly two inputs:
      - A wallet input, not containing a YieldListSTT
      - A UTxO at the YieldListValidator address with exactly one YieldListSTT
    - A transaction spending a UTxO at this address must have exactly one output:
      - A "change" UTxO, not containing a YieldListSTT

    Additional validation semantics <span class="underline">must</span> be user defined via wrapping scripts.
    Examples include alwaysFail (immutable), multisig, admin sig, or governance wrappers.

-}
