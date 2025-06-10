{- | This module export a helper function that produces a two argument yielding script that
we use to implement the logic for yielding validator, minting policy and staking validator
-}
module Cardano.YTxP.Control.Yielding.Helper (
  yieldingHelper,
  oneshotHelper,
)
where

import Cardano.YTxP.Control.Yielding (
  PAuthorisedScriptPurpose (PMinting, PRewarding, PSpending),
  authorisedScriptProofIndex,
  getAuthorisedScriptHash,
 )
import Plutarch.LedgerApi.AssocMap (PMap (PMap))
import Plutarch.LedgerApi.V3 (
  PCredential (PPubKeyCredential, PScriptCredential),
  PCurrencySymbol,
  PRedeemer (PRedeemer),
  PScriptContext,
  PTxOutRef,
  paddress'credential,
  pfindOwnInput,
  pscriptContext'redeemer,
  pscriptContext'txInfo,
  ptxInInfo'resolved,
  ptxInfo'inputs,
  ptxInfo'mint,
  ptxInfo'referenceInputs,
  ptxInfo'wdrl,
  ptxOut'address,
 )
import Plutarch.Maybe (pisJust)
import Utils (pcheck, pscriptHashToCurrencySymbol)

-- -   Look at the UTxO at the `n` th entry in the `txInfoReferenceInputs`, where `n` is equal to `authorisedScriptIndex`.
--     -   Call this UTxO `authorisedScriptUTxO`.
--     -   Check that this UTxO is carrying exactly one token with the `authorisedScriptSTCS`. Blow up if not.
--     -   Obtain the hash of the reference script from the authorisedScriptUTxO. Call this hash `AuthorisedScriptHash`.
-- -   Obtain evidence that the a script with `AuthorisedScriptHash` was triggered.
--     If not, blow up. In practice, this will involve either:
--     -   Looking at the `txInfoWithdrawls` field for a staking validator being triggered with the correct StakingCredential
--     -   Looking at the `txInfoInputs` field for a UTxO being spent at the correct address
--     -   Looking at the `txInfoMints` field for a mint with the correct currency symbol

yieldingHelper ::
  forall (s :: S).
  Term s (PCurrencySymbol :--> PScriptContext :--> PUnit)
yieldingHelper = plam $ \pylstcs ctx' -> unTermCont $ do
  ctx <- pmatchC ctx'
  redeemer' <- pletC $ pscriptContext'redeemer ctx
  txInfo <- pmatchC $ pscriptContext'txInfo ctx
  PRedeemer redeemer <- pmatchC redeemer'
  yieldingRedeemer <- pfromData . fst <$> ptryFromC redeemer
  yieldingRedeemer' <- pmatchC yieldingRedeemer
  scriptProofIndex <-
    pletC $ pto $ pfromData $ authorisedScriptProofIndex yieldingRedeemer'

  let txInfoRefInputs = pfromData $ ptxInfo'referenceInputs txInfo
      authorisedScriptHash = getAuthorisedScriptHash # pylstcs # txInfoRefInputs # yieldingRedeemer
      authorisedScriptPurpose = pfromData $ pfstBuiltin # scriptProofIndex
      authorisedScriptIndex = pfromData $ psndBuiltin # scriptProofIndex

  pure $
    pcheck $
      pmatch authorisedScriptPurpose $ \case
        PMinting ->
          let txInfoMints = pfromData $ ptxInfo'mint txInfo
              authorisedScriptMint = pto (pto txInfoMints) #!! authorisedScriptIndex
              currencySymbol = pscriptHashToCurrencySymbol authorisedScriptHash
           in ptraceInfoIfFalse
                "Minting policy does not match expected authorised minting policy"
                $ pfromData (pfstBuiltin # authorisedScriptMint) #== currencySymbol
        PSpending -> unTermCont $ do
          let txInfoInputs = pfromData $ ptxInfo'inputs txInfo
          authorisedScriptInput <-
            pmatchC $ pfromData $ txInfoInputs #!! authorisedScriptIndex
          out <- pmatchC $ ptxInInfo'resolved authorisedScriptInput
          address <- pmatchC $ ptxOut'address out
          let credential = paddress'credential address
          return $
            pmatch credential $ \case
              PScriptCredential hash ->
                ptraceInfoIfFalse "Input does not match expected authorised validator" $
                  pfromData hash #== authorisedScriptHash
              PPubKeyCredential _ ->
                ptraceInfoError "Input at specified index is not a script input"
        PRewarding -> unTermCont $ do
          PMap txInfoWithdrawals <- pmatchC $ pfromData $ ptxInfo'wdrl txInfo
          let authorisedScriptWithdrawal = txInfoWithdrawals #!! authorisedScriptIndex
          return $
            pmatch (pfromData $ pfstBuiltin # authorisedScriptWithdrawal) $ \case
              PScriptCredential hash ->
                ptraceInfoIfFalse
                  "Withdrawal does not match expected authorised staking validator"
                  $ pfromData hash #== authorisedScriptHash
              PPubKeyCredential _ ->
                ptraceInfoError
                  "Staking credential at specified index is not a script credential"

-- | One-Shot check
oneshotHelper ::
  forall (s :: S).
  Term
    s
    ( PTxOutRef :--> PScriptContext :--> PBool
    )
oneshotHelper = plam $ \oref ctx -> pmatch ctx $
  \ctx' -> pmatch (pscriptContext'txInfo ctx') $
    \txInfo ->
      -- FIXME: There is no list fusion atm.
      let inputs = pmap # plam pfromData # pfromData (ptxInfo'inputs txInfo)
       in pisJust #$ pfindOwnInput # inputs # oref
