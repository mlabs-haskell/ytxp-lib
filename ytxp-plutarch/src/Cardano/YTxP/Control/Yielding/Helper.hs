{- | This module export a helper function that produces a two argument yielding script that
we use to implement the logic for yielding validator, minting policy and staking validator
-}
module Cardano.YTxP.Control.Yielding.Helper (yieldingHelper) where

import Cardano.YTxP.Control.Yielding (PAuthorisedScriptPurpose (PMinting, PRewarding, PSpending), getAuthorisedScriptHash)
import Cardano.YTxP.SDK.SdkParameters (AuthorisedScriptsSTCS)
import Plutarch.Api.V1.Address (
  PCredential (PPubKeyCredential, PScriptCredential),
 )
import Plutarch.Api.V2 (
  PScriptContext,
  PStakingCredential (PStakingHash, PStakingPtr),
 )
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
  AuthorisedScriptsSTCS ->
  Term s (PData :--> PScriptContext :--> POpaque)
yieldingHelper ylstcs = plam $ \redeemer ctx -> unTermCont $ do
  txInfo <- pletC $ pfromData $ pfield @"txInfo" # ctx
  let txInfoRefInputs = pfromData $ pfield @"referenceInputs" # txInfo
  yieldingRedeemer <- pfromData . fst <$> ptryFromC redeemer
  let authorisedScriptHash = getAuthorisedScriptHash ylstcs # txInfoRefInputs # yieldingRedeemer
      authorisedScriptProofIndex = pto $ pfromData $ pfield @"authorisedScriptProofIndex" # yieldingRedeemer
      authorisedScriptPurpose = pfromData $ pfstBuiltin # authorisedScriptProofIndex
      authorisedScriptIndex = pfromData $ psndBuiltin # authorisedScriptProofIndex

  pure $
    pcheck $
      pmatch authorisedScriptPurpose $ \case
        PMinting ->
          let txInfoMints = pfromData $ pfield @"mint" # txInfo
              authorisedScriptMint = pto (pto txInfoMints) #!! authorisedScriptIndex
              currencySymbol = pscriptHashToCurrencySymbol authorisedScriptHash
           in ptraceIfFalse "Minting policy does not match expected yielded to minting policy" $
                pfromData (pfstBuiltin # authorisedScriptMint) #== currencySymbol
        PSpending ->
          let txInfoInputs = pfromData $ pfield @"inputs" # txInfo
              authorisedScriptInput = txInfoInputs #!! authorisedScriptIndex
              out = pfield @"resolved" # authorisedScriptInput
              address = pfield @"address" # pfromData out
              credential = pfield @"credential" # pfromData address
           in pmatch (pfromData credential) $ \case
                PScriptCredential ((pfield @"_0" #) -> hash) ->
                  ptraceIfFalse "Input does not match expected yielded to validator" $
                    hash #== authorisedScriptHash
                PPubKeyCredential _ ->
                  ptraceError "Input at specified index is not a script input"
        PRewarding ->
          let txInfoWithdrawals = pfromData $ pfield @"wdrl" # txInfo
              authorisedScriptWithdrawal = pto txInfoWithdrawals #!! authorisedScriptIndex
           in pmatch (pfromData $ pfstBuiltin # authorisedScriptWithdrawal) $ \case
                PStakingHash ((pfield @"_0" #) -> credential) ->
                  pmatch credential $ \case
                    PScriptCredential ((pfield @"_0" #) -> hash) ->
                      ptraceIfFalse "Withdrawal does not match expected yielded to staking validator" $
                        hash #== authorisedScriptHash
                    PPubKeyCredential _ ->
                      ptraceError "Staking credential at specified index is not a script credential"
                PStakingPtr _ ->
                  ptraceError "No staking validator found"
