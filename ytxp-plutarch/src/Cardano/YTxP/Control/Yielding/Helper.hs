{- | This module provides a helper function to produce a two-argument yielding script.
     This script is used to implement the logic for yielding validators, minting policies,
     and staking validators. The helper function ensures that the correct script is
     authorized to perform the specified action based on the provided purposes.

     The yielding script performs the following steps:
     - Validates the script context and redeemer.
     - Retrieves the reference inputs and authorized script hash.
     - Checks the authorized script purpose and index.
     - Validates the transaction based on the authorized script purpose.

     The module exports the 'yieldingHelper' function and the 'AuthorisedScriptPurpose' data type.
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

{- |
This function is used to validate the authorized script purpose and index.
-}
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

{- |
This function is used to validate the minting authorisation purpose.
-}
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

{- |
This function is used to validate the rewarding authorisation purpose.
-}
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

{- |
This function is used to validate the spending authorisation purpose.
-}
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

{- |
This function is used to produce a two-argument yielding script.
-}
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
