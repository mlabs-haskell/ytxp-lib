{- | This module export a helper function that produces a two argument yielding script that
we use to implement the logic for yielding validator, minting policy and staking validator
-}
module Cardano.YTxP.Control.Yielding.Helper (yieldingHelper) where

import Cardano.YTxP.Control.YieldList (
  PYieldedToHash (PYieldedToMP, PYieldedToSV, PYieldedToValidator),
 )
import Cardano.YTxP.Control.Yielding (getYieldedToHash)
import Cardano.YTxP.SDK.SdkParameters (YieldListSTCS)
import Plutarch.Api.V1.Address (
  PCredential (PPubKeyCredential, PScriptCredential),
 )
import Plutarch.Api.V2 (
  PScriptContext,
  PStakingCredential (PStakingHash, PStakingPtr),
 )
import Utils (pscriptHashToCurrencySymbol)

-- -   Look at the UTxO at the `n` th entry in the `txInfoReferenceInputs`, where `n` is equal to `yieldListInputIndex`.
--     -   Call this UTxO `yieldListUTxO`.
--     -   Check that this UTxO is carrying exactly one token with the `yieldListSTCS`. Blow up if not.
-- -   "Unsafely" deserialize the datum of the `yieldListUTxO` to a value `yieldList :: YieldList`
-- -   Grab the correct `YieldToHash` by looking at the `n` th entry of `yieldList`, where `n` is equal to
--     `yieldListIndex`. Call this hash `yieldToHash`.
-- -   Obtain evidence that the a script with `yieldToHash` was triggered via the `checkYieldList` function.
--     If not, blow up. In practice, this will involve either:
--     -   Looking at the `txInfoWithdrawls` field for a staking validator being triggered with the correct StakingCredential
--     -   Looking at the `txInfoInputs` field for a UTxO being spent at the correct address
--     -   Looking at the `txInfoMints` field for a mint with the correct currency symbol

yieldingHelper ::
  forall (s :: S).
  YieldListSTCS ->
  Term s (PData :--> PScriptContext :--> POpaque)
yieldingHelper ylstcs = plam $ \redeemer ctx -> unTermCont $ do
  txInfo <- pletC $ pfromData $ pfield @"txInfo" # ctx
  let txInfoRefInputs = pfromData $ pfield @"referenceInputs" # txInfo
  yieldingRedeemer <- pfromData . fst <$> ptryFromC redeemer
  let yieldToHash = getYieldedToHash ylstcs # txInfoRefInputs # yieldingRedeemer
      yieldToIndex = pto $ pfromData $ pfield @"yieldListScriptToYieldIndex" # yieldingRedeemer

  pure $
    popaque $
      pmatch yieldToHash $ \case
        PYieldedToValidator ((pfield @"scriptHash" #) -> yieldToHash') ->
          let txInfoInputs = pfromData $ pfield @"inputs" # txInfo
              yieldToInput = txInfoInputs #!! yieldToIndex
              out = pfield @"resolved" # yieldToInput
              address = pfield @"address" # pfromData out
              credential = pfield @"credential" # pfromData address
           in pmatch (pfromData credential) $ \case
                PScriptCredential ((pfield @"_0" #) -> hash) ->
                  ptraceIfFalse "Input does not match expected yielded to validator" $
                    hash #== yieldToHash'
                PPubKeyCredential _ ->
                  ptraceError "Input at specified index is not a script input"
        PYieldedToMP ((pfield @"scriptHash" #) -> yieldToHash') ->
          let txInfoMints = pfromData $ pfield @"mint" # txInfo
              yieldToMint = pto (pto txInfoMints) #!! yieldToIndex
              currencySymbol = pscriptHashToCurrencySymbol yieldToHash'
           in ptraceIfFalse "Minting policy does not match expected yielded to minting policy" $
                pfromData (pfstBuiltin # yieldToMint) #== currencySymbol
        PYieldedToSV ((pfield @"scriptHash" #) -> yieldToHash') ->
          let txInfoWithdrawals = pfromData $ pfield @"wdrl" # txInfo
              yieldToWithdrawal = pto txInfoWithdrawals #!! yieldToIndex
           in pmatch (pfromData $ pfstBuiltin # yieldToWithdrawal) $ \case
                PStakingHash ((pfield @"_0" #) -> credential) ->
                  pmatch credential $ \case
                    PScriptCredential ((pfield @"_0" #) -> hash) ->
                      ptraceIfFalse "Withdrawal does not match expected yielded to staking validator" $
                        hash #== yieldToHash'
                    PPubKeyCredential _ ->
                      ptraceError "Staking credential at specified index is not a script credential"
                PStakingPtr _ ->
                  ptraceError "No staking validator found"
