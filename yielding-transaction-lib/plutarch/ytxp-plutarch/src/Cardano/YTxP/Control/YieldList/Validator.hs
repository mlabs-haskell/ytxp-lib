{-# LANGUAGE TypeApplications #-}

module Cardano.YTxP.Control.YieldList.Validator (
  -- * Validator
  YieldListValidatorScript,
  compileYieldListValidator,

  -- * Credential
  YieldListValidatorCredential,
  mkYieldListValidatorWrapperCredential,
) where

import Cardano.YTxP.Control.Vendored (psymbolValueOf)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Plutarch (Config, compile)
import Plutarch.Api.V1.Value (PCurrencySymbol, padaToken, pvalueOf)
import Plutarch.Api.V2 (
  PScriptContext,
  PScriptPurpose (PSpending),
  PTxInInfo,
  PTxOut,
  PTxOutRef,
  scriptHash,
 )
import Plutarch.Script (Script)
import PlutusLedgerApi.V2 (Credential (ScriptCredential))
import Prettyprinter (Pretty)
import Utils (
  pisPubKeyOutput,
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

  pguardC
    ( "Must be exactly two inputs:"
        <> " one wallet input that does not contain a token with YieldListSTCS"
        <> " and one yield list validator input with exactly one token with YieldListSTCS"
    )
    $ phasOnlyTwoInputsPubKeyWithNoSymbolScriptWithSymbol
      # inputs
      # yieldListSymbol
      # yieldListInputRef

  pguardC
    "Only one output allowed - a wallet output that does not contain the yield list symbol"
    $ phasOnlyOneOutputIsPubKeyAndNoSymbol
      # outputs
      # yieldListSymbol

  pure $ popaque $ pconstant ()

{- | This function checks that there are exactly two inputs in the given input list,
that one of these inputs is a pub key input and the other one is a script input,
that the pub key input does not contain a token with the given symbol (YieldListSTCS here),
and that the script input contains exactly one token with the given symbol,
and the `outRef` of this input matches the given `PTxOutRef` (the YieldList validator itself here)
Written like this to keep iterations through the list etc. to a minimum.
-}
phasOnlyTwoInputsPubKeyWithNoSymbolScriptWithSymbol ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList PTxInInfo
        :--> PCurrencySymbol
        :--> PTxOutRef
        :--> PBool
    )
phasOnlyTwoInputsPubKeyWithNoSymbolScriptWithSymbol = phoistAcyclic $
  plam $ \txInInfos symbol txOutRef ->
    pmatch txInInfos $ \case
      PCons inputOne tailOfListOne ->
        pmatch tailOfListOne $ \case
          -- Pattern match on the tail of the list to check that it contains only one element
          -- And hence exactly two elements overall
          PCons inputTwo tailOfListTwo ->
            pnull
              # tailOfListTwo
              #&& ( pmatch (ptxInInfoIsPubKey # inputOne) $ \case
                      -- If the first input is a pub key input then we check that
                      -- it does not contain a token with the YieldListSTCS,
                      -- and that the second input is a yield list validator one that contains exactly one yield list token
                      PTrue ->
                        pcheckPubKeyInput
                          # inputOne
                          # symbol
                          #&& pcheckScriptInput
                          # inputTwo
                          # symbol
                          # txOutRef
                      -- If the first input is not a pubkey input then we check that
                      -- the second input is pub key input and it does not contain a token with the YieldListSTCS,
                      -- and that the first input is a yield list validator input that contains exactly one yield list token
                      PFalse ->
                        (ptxInInfoIsPubKey # inputTwo)
                          #&& pcheckPubKeyInput
                          # inputTwo
                          # symbol
                          #&& pcheckScriptInput
                          # inputOne
                          # symbol
                          # txOutRef
                  )
          PNil -> pconstant False
      PNil -> pconstant False

-- | Check the credential is a `PubKeyCredential`
ptxInInfoIsPubKey ::
  forall (s :: S).
  Term s (PTxInInfo :--> PBool)
ptxInInfoIsPubKey = phoistAcyclic $
  plam $
    \txInInfo -> pisPubKeyOutput #$ pfromData $ pfield @"resolved" # txInInfo

-- | Check that there are no tokens with given symbol in the input
pcheckPubKeyInput ::
  forall (s :: S).
  Term
    s
    ( PTxInInfo
        :--> PCurrencySymbol
        :--> PBool
    )
pcheckPubKeyInput = phoistAcyclic $
  plam $ \txInInfo symbol ->
    ( psymbolValueOf
        # symbol
        # (pfromData $ pfield @"value" #$ pfromData $ pfield @"resolved" # txInInfo)
    )
      #== pconstant 0

{- | Check that the given input is a script input and that it
contains exactly one token with the given symbol
-}
pcheckScriptInput ::
  forall (s :: S).
  Term
    s
    ( PTxInInfo
        :--> PCurrencySymbol
        :--> PTxOutRef
        :--> PBool
    )
pcheckScriptInput = phoistAcyclic $
  plam $ \txInInfo symbol txOutRef ->
    txOutRef
      #== (pfromData $ pfield @"outRef" # txInInfo)
      #&& ( pvalueOf
              # (pfromData $ pfield @"value" #$ pfromData $ pfield @"resolved" # txInInfo)
              # symbol
              # padaToken
          )
      #== pconstant 1

{- | Check that there is only one output in the given list,
that the output is a `PubKey` output,
and that the input does not contain a token with the given symbol.
-}
phasOnlyOneOutputIsPubKeyAndNoSymbol ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList PTxOut
        :--> PCurrencySymbol
        :--> PBool
    )
phasOnlyOneOutputIsPubKeyAndNoSymbol = phoistAcyclic $
  plam $ \outputs symbol ->
    pmatch outputs $ \case
      PCons output tailOfList ->
        pnull
          # tailOfList
          #&& pisPubKeyOutput
          # output
          #&& (psymbolValueOf # symbol # (pfromData $ pfield @"value" # output))
          #== pconstant 0
      PNil -> pconstant False
