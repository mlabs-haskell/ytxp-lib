{-# LANGUAGE TypeApplications #-}

module Cardano.YTxP.Control.YieldList.Validator (
  -- * Validator
  YieldListValidatorScript,
  compileYieldListValidator,

  -- * Credential
  YieldListValidatorCredential,
  mkYieldListValidatorWrapperCredential,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Plutarch (Config, compile)
import Plutarch.Api.V1.Value (PCurrencySymbol, padaToken)
import Plutarch.Api.V2 (PScriptContext, PScriptPurpose (PSpending), scriptHash)
import Plutarch.Script (Script)
import PlutusLedgerApi.V2 (Credential (ScriptCredential))
import Prettyprinter (Pretty)
import Utils (
  phasNoScriptOutput,
  phasOnlyOneInputWithTxOutRefSymbolAndTokenName,
  phasOnlyOnePubKeyInputAndNoTokenWithSymbol,
  phasOnlyOnePubKeyOutputAndNoTokenWithSymbol,
 )

--------------------------------------------------------------------------------
-- YieldListValidatorScript

-- | @since 0.1.0
newtype YieldListValidatorScript = YieldListValidatorScript Script
  deriving
    ( -- | @since 0.1.0
      ToJSON
    , -- | @since 0.1.0
      FromJSON
    )
    via (HexStringScript "YieldListValidatorScript")
  deriving
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Pretty
    )
    via SerialForm

compileYieldListValidator ::
  Config ->
  (forall (s :: S). Term s (PData :--> PData :--> PScriptContext :--> POpaque)) ->
  Either Text YieldListValidatorScript
compileYieldListValidator config scriptToWrap = do
  script <- compile config (mkYieldListValidatorWrapper # scriptToWrap)
  pure $ YieldListValidatorScript script

--------------------------------------------------------------------------------
-- YieldListCTCS

-- | Opaque, semantic newtype for the YieldList state thread currency symbol
newtype YieldListValidatorCredential
  = YieldListValidatorCredential PlutusLedgerApi.V2.Credential

mkYieldListValidatorWrapperCredential ::
  YieldListValidatorScript ->
  YieldListValidatorCredential
mkYieldListValidatorWrapperCredential (YieldListValidatorScript script) =
  YieldListValidatorCredential
    . PlutusLedgerApi.V2.ScriptCredential
    . scriptHash
    $ script

--------------------------------------------------------------------------------
-- Helpers (Unexported)

{-
  The `mkYieldListValidatorWrapper` script is used to build a validator that takes a wrapper validator
  as an argument, while performing additional checks of its own, which are outlined below.

  When spent, a YieldList UTxO must burn all of its tokens except for minAda.
  Updates are not permissible, you must chain a creation and deletion.

  The validator performs the following checks:
    - The transaction is spending a UTXO at this address (checked via `PSpending`)
    - There are exactly two inputs:
      * A wallet input, not containing a YieldListSTT
      * A UTxO at the YieldListValidator address with exactly one YieldListSTT
    - There must be exactly one output:
      * A "change" output that does not contain a YieldListSTT

  Additional validation semantics must be user defined via wrapping scripts.
  Examples include:
    - alwaysFail (immutable)
    - multisig
    - admin sig
    - governance wrappers

-}
mkYieldListValidatorWrapper ::
  forall (s :: S).
  Term
    s
    ( (PData :--> PData :--> PScriptContext :--> POpaque)
        :--> PCurrencySymbol
        :--> PData
        :--> PData
        :--> PScriptContext
        :--> POpaque
    )
mkYieldListValidatorWrapper = plam $ \_scriptToWrap yieldListSymbol _datum _redeemer context -> unTermCont $ do
  let txInfo = pfromData $ pfield @"txInfo" # context
      purpose = pfromData $ pfield @"purpose" # context
      inputs = pfromData $ pfield @"inputs" # txInfo
      outputs = pfromData $ pfield @"outputs" # txInfo

  PSpending ((pfield @"_0" #) -> yieldListInputRef) <- pmatchC purpose

  pguardC "Should have exactly two inputs" $
    plength # inputs #== pconstant 2

  pguardC
    "Only one wallet input, that does not contain yield list symbol, allowed"
    $ phasOnlyOnePubKeyInputAndNoTokenWithSymbol
      # inputs
      # yieldListSymbol

  pguardC
    "Should have only one script input at the YieldListValidator with exactly one yield list symbol"
    $ phasOnlyOneInputWithTxOutRefSymbolAndTokenName
      # inputs
      # yieldListInputRef
      # yieldListSymbol
      # padaToken

  pguardC
    "Only one output allowed - a wallet output that does not contain the yield list symbol"
    $ phasOnlyOnePubKeyOutputAndNoTokenWithSymbol
      # outputs
      # yieldListSymbol
      #&& phasNoScriptOutput
      # outputs

  pure $ popaque $ pconstant ()
