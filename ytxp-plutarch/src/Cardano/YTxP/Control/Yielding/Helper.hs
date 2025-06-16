{- | This module export a helper function that produces a two argument yielding script that
we use to implement the logic for yielding validator, minting policy and staking validator
-}
module Cardano.YTxP.Control.Yielding.Helper (
  yieldingHelper,
  AuthorisedScriptPurpose (
    Minting,
    Spending,
    Rewarding
  ),
) where

import Cardano.YTxP.Control.Yielding (
  PAuthorisedScriptPurpose (PMinting, PRewarding, PSpending),
  authorisedScriptProofIndex,
  getAuthorisedScriptHash,
 )
import Data.Set (Set)
import Data.Set qualified as Set
import Plutarch.Builtin.Bool (pfalse)
import Plutarch.LedgerApi.AssocMap (PMap (PMap))
import Plutarch.LedgerApi.V3 (
  PCredential (PPubKeyCredential, PScriptCredential),
  PCurrencySymbol,
  PRedeemer (PRedeemer),
  PScriptContext,
  PScriptHash,
  PTxInfo,
  paddress'credential,
  pscriptContext'redeemer,
  pscriptContext'txInfo,
  ptxInInfo'resolved,
  ptxInfo'inputs,
  ptxInfo'mint,
  ptxInfo'referenceInputs,
  ptxInfo'wdrl,
  ptxOut'address,
 )
import Utils (pcheck, pscriptHashToCurrencySymbol)

{- |
This is used to identify the authorized script validation that we want to use.
-}
data AuthorisedScriptPurpose = Minting | Spending | Rewarding
  deriving stock (Show, Eq, Ord)

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
  Set AuthorisedScriptPurpose ->
  Term s (PCurrencySymbol :--> PScriptContext :--> PUnit)
yieldingHelper purposes = plam $ \pylstcs ctx' -> unTermCont $ do
  ctx <- pmatchC ctx'
  redeemer' <- pletC $ pscriptContext'redeemer ctx
  txInfo <- pletC $ pscriptContext'txInfo ctx
  txInfo' <- pmatchC txInfo
  PRedeemer redeemer <- pmatchC redeemer'
  yieldingRedeemer <- pfromData . fst <$> ptryFromC redeemer
  yieldingRedeemer' <- pmatchC yieldingRedeemer
  scriptProofIndex <-
    pletC $ pto $ pfromData $ authorisedScriptProofIndex yieldingRedeemer'

  let txInfoRefInputs = pfromData $ ptxInfo'referenceInputs txInfo'
      authorisedScriptHash = getAuthorisedScriptHash # pylstcs # txInfoRefInputs # yieldingRedeemer
      authorisedScriptPurpose = pfromData $ pfstBuiltin # scriptProofIndex
      authorisedScriptIndex = pfromData $ psndBuiltin # scriptProofIndex

  pure $
    pcheck $
      authorisedScriptValidation
        purposes
        authorisedScriptIndex
        authorisedScriptHash
        txInfo
        authorisedScriptPurpose

authorisedScriptValidation ::
  forall (s :: S).
  Set AuthorisedScriptPurpose ->
  Term s PInteger ->
  Term s PScriptHash ->
  Term s PTxInfo ->
  Term s PAuthorisedScriptPurpose ->
  Term s PBool
authorisedScriptValidation purposes authorisedScriptPurpose authorisedScriptIndex authorisedScriptHash txInfo =
  flip (foldr (.) id) pfalse $
    g
      <$> Set.toList purposes
      <*> pure authorisedScriptPurpose
      <*> pure authorisedScriptIndex
      <*> pure authorisedScriptHash
      <*> pure txInfo
  where
    g Minting = mintingAuthorisationPurpose @s
    g Spending = spendingAuthorisationPurpose
    g Rewarding = rewardingAuthorisationPurpose

mintingAuthorisationPurpose ::
  forall (s :: S).
  Term s PInteger ->
  Term s PScriptHash ->
  Term s PTxInfo ->
  Term s PAuthorisedScriptPurpose ->
  Term s PBool ->
  Term s PBool
mintingAuthorisationPurpose authorisedScriptIndex authorisedScriptHash txInfo purpose =
  pif
    (purpose #== pcon PMinting)
    ( pmatch txInfo $ \txInfo' ->
        let txInfoMints = pfromData $ ptxInfo'mint txInfo'
            authorisedScriptMint = pto (pto txInfoMints) #!! authorisedScriptIndex
            currencySymbol = pscriptHashToCurrencySymbol authorisedScriptHash
         in ptraceInfoIfFalse
              "Minting policy does not match expected authorised minting policy"
              $ pfromData (pfstBuiltin # authorisedScriptMint) #== currencySymbol
    )

rewardingAuthorisationPurpose ::
  forall (s :: S).
  Term s PInteger ->
  Term s PScriptHash ->
  Term s PTxInfo ->
  Term s PAuthorisedScriptPurpose ->
  Term s PBool ->
  Term s PBool
rewardingAuthorisationPurpose authorisedScriptIndex authorisedScriptHash txInfo purpose =
  pif
    (purpose #== pcon PRewarding)
    ( unTermCont $ do
        txInfo' <- pmatchC txInfo
        PMap txInfoWithdrawals <- pmatchC $ pfromData $ ptxInfo'wdrl txInfo'
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
    )

spendingAuthorisationPurpose ::
  forall (s :: S).
  Term s PInteger ->
  Term s PScriptHash ->
  Term s PTxInfo ->
  Term s PAuthorisedScriptPurpose ->
  Term s PBool ->
  Term s PBool
spendingAuthorisationPurpose authorisedScriptIndex authorisedScriptHash txInfo purpose =
  pif
    (purpose #== pcon PSpending)
    ( unTermCont $ do
        txInfo' <- pmatchC txInfo
        let txInfoInputs = pfromData $ ptxInfo'inputs txInfo'
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
    )
