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
import Plutarch.Api.V1.Value (PCurrencySymbol)
import Plutarch.Api.V2 (PScriptContext, PScriptPurpose (PSpending), scriptHash)
import Plutarch.Script (Script)
import PlutusLedgerApi.V2 (Credential (ScriptCredential))
import Prettyprinter (Pretty)
import Utils (pands, phasOneScriptInputAtValidatorWithExactlyOneToken,
              poutputsDoNotContainToken)

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
-- YieldListSTCS

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
    - There is one UTxO input at the YieldListValidator address with exactly one YieldListSTT
    - Due to the wrapping script, there is a potential for other script inputs and outputs beyond the ones
      we account for in this script. There may also be other wallet inputs and outputs, depending on the
      type of wrapping script.
      * We need to ensure that these other inputs or outputs do not carry a YieldListSTT

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

  pure
    $ popaque
    $ ptraceIfFalse
      "mkYieldListValidatorWrapper failed"
      ( pands
          [ -- For efficiency reasons we use this helper to make a couple of checks,
            -- Namely, it ensures that there is one input at the yield list validator
            -- with exactly one YieldListSTT.
            -- It also checks that there are no other script inputs with a YieldListSTT.
            -- (There is no need to check the wallet inputs as a YieldListSTT is never
            --  sent to one in the first place.)
            ptraceIfFalse
              ( mconcat
                  [ "Must have one input at the yield list validator with exactly one YieldListSTT,"
                  , " and no other inputs containing a YieldListSTT"
                  ]
              )
              $ phasOneScriptInputAtValidatorWithExactlyOneToken inputs
                # yieldListSymbol
                # yieldListInputRef
          , ptraceIfFalse
              "Must be no YieldListSTT at any of the outputs"
              $ poutputsDoNotContainToken outputs # yieldListSymbol
          ]
      )
