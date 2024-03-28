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
import Plutarch.Api.V1 (
  PCredential (PPubKeyCredential),
 )
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
  pands,
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

  pure
    $ popaque
    $ pmatch
      ( pands
          [ phasOnlyTwoInputsPubKeyWithNoSymbolScriptWithSymbol inputs
              # yieldListSymbol
              # yieldListInputRef
          , phasOnlyOneOutputIsPubKeyAndNoSymbol outputs # yieldListSymbol
          ]
      )
    $ \case
      PTrue -> pconstant ()
      PFalse -> perror

{- | This function checks that there are exactly two inputs in the given input list,
that one of these inputs is a pub key input and the other one is a script input,
that the pub key input does not contain a token with the given symbol (YieldListSTCS here),
and that the script input contains exactly one token with the given symbol,
and the `outRef` of this input matches the given `PTxOutRef` (the YieldList validator itself here)

It is written in this manner (repetition etc.) in order to make the script as efficient as possible.
For example, we avoid abstracting out some repeated patterns into nested Plutarch-level helper functions,
as this would come at the cost of an increase in Ex-units.
-}
phasOnlyTwoInputsPubKeyWithNoSymbolScriptWithSymbol ::
  Term s (PBuiltinList PTxInInfo) ->
  Term
    s
    ( PCurrencySymbol
        :--> PTxOutRef
        :--> PBool
    )
phasOnlyTwoInputsPubKeyWithNoSymbolScriptWithSymbol txInInfos =
  plam $ \symbol txOutRef ->
    pmatch txInInfos $ \case
      PCons inputOne tailOfListOne ->
        pmatch tailOfListOne $ \case
          -- Pattern match on the tail of the list to check that it contains only one element
          -- And hence exactly two elements overall
          PCons inputTwo tailOfListTwo ->
            pnull
              # tailOfListTwo
              #&& ( pmatch
                      ( pfromData
                          $ pfield @"credential"
                            #$ pfromData
                          $ pfield @"address"
                            #$ pfromData
                          $ pfield @"resolved" # inputOne
                      )
                      $ \case
                        -- If the first input is a pub key input then we check that
                        -- it does not contain a token with the YieldListSTCS,
                        -- and that the second input is a yield list validator one that
                        -- contains exactly one yield list token
                        PPubKeyCredential _ ->
                          ( psymbolValueOf
                              # symbol
                              # (pfromData $ pfield @"value" #$ pfromData $ pfield @"resolved" # inputOne)
                              #== pconstant 0
                          )
                            #&& ( txOutRef
                                    #== (pfromData $ pfield @"outRef" # inputTwo)
                                    #&& ( pvalueOf
                                            # ( pfromData
                                                  $ pfield @"value"
                                                    #$ pfromData
                                                  $ pfield @"resolved" # inputTwo
                                              )
                                            # symbol
                                            # padaToken
                                        )
                                    #== pconstant 1
                                )
                        -- If the first input is not a pubkey input then we check that
                        -- the second input is pub key input and it does not contain a token with the YieldListSTCS,
                        -- and that the first input is a yield list validator input
                        -- that contains exactly one yield list token
                        _ -> pmatch
                          ( pfromData
                              $ pfield @"credential"
                                #$ pfromData
                              $ pfield @"address"
                                #$ pfromData
                              $ pfield @"resolved" # inputTwo
                          )
                          $ \case
                            PPubKeyCredential _ ->
                              ( ( psymbolValueOf
                                    # symbol
                                    # ( pfromData
                                          $ pfield @"value"
                                            #$ pfromData
                                          $ pfield @"resolved"
                                            # inputTwo
                                      )
                                )
                                  #== pconstant 0
                              )
                                #&& ( txOutRef
                                        #== (pfromData $ pfield @"outRef" # inputOne)
                                        #&& ( pvalueOf
                                                # ( pfromData
                                                      $ pfield @"value"
                                                        #$ pfromData
                                                      $ pfield @"resolved" # inputOne
                                                  )
                                                # symbol
                                                # padaToken
                                            )
                                        #== pconstant 1
                                    )
                            _ -> pconstant False
                  )
          PNil -> pconstant False
      PNil -> pconstant False

{- | Check that there is only one output in the given list,
that the output is a `PubKey` output,
and that the input does not contain a token with the given symbol.
-}
phasOnlyOneOutputIsPubKeyAndNoSymbol ::
  Term s (PBuiltinList PTxOut) ->
  Term
    s
    ( PCurrencySymbol
        :--> PBool
    )
phasOnlyOneOutputIsPubKeyAndNoSymbol outputs =
  plam $ \symbol ->
    pmatch outputs $ \case
      PCons output tailOfList ->
        pnull
          # tailOfList
          #&& ( pmatch
                  ( pfromData
                      $ pfield @"credential"
                        #$ pfromData
                      $ pfield @"address"
                        # output
                  )
                  $ \case
                    PPubKeyCredential _ -> pconstant True
                    _ -> pconstant False
              )
          #&& ( psymbolValueOf
                  # symbol
                  # (pfromData $ pfield @"value" # output)
              )
          #== pconstant 0
      PNil -> pconstant False
