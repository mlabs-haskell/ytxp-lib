module Utils (
  phasTokenOfCurrencySymbolTokenNameAndAmount,
  phasOnlyOnePubKeyInputAndNoTokenWithSymbol,
  phasOnlyOnePubKeyOutputAndNoTokenWithSymbol,
  phasValidOutputDatum,
  pisScriptCredential,
)
where

import Plutarch.Api.V1 (
  PCredential (PPubKeyCredential),
 )
import Plutarch.Api.V1.Value (
  PTokenName,
  pvalueOf,
 )
import Plutarch.Api.V2 (
  AmountGuarantees,
  KeyGuarantees,
  PCurrencySymbol,
  POutputDatum (POutputDatum),
  PTxInInfo,
  PTxOut,
  PValue,
 )

{- | Check that there is only token of given `PCurrencySymbol`
 and `PTokenName` with given amount contained in the given PValue.
 We check that the length is equal to two to account for the additional
 entry automatically included in the 'mint' field:
   (PCurrencySymbol 0x,PMap [(PTokenName 0x,0)])])
-}
phasTokenOfCurrencySymbolTokenNameAndAmount ::
  forall (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (s :: S).
  Term
    s
    ( PValue keys amounts
        :--> PCurrencySymbol
        :--> PTokenName
        :--> PInteger
        :--> PBool
    )
phasTokenOfCurrencySymbolTokenNameAndAmount = phoistAcyclic $
  plam $ \value symbol tokenName amount ->
    pvalueOf
      # value
      # symbol
      # tokenName
      #== amount
      #&& (plength #$ pto $ pto $ pto value)
      #== 2

{- | Check there is only one `PubKey` input and ensure that
input does not contain token with the given `CurrencySymbol`
-}
phasOnlyOnePubKeyInputAndNoTokenWithSymbol ::
  forall (s :: S).
  Term s (PBuiltinList PTxInInfo :--> PCurrencySymbol :--> PTokenName :--> PBool)
phasOnlyOnePubKeyInputAndNoTokenWithSymbol = phoistAcyclic $
  plam $ \inputs symbol tokenName ->
    ptxOutListCheck # (pgetPubKeyInputs # inputs) # symbol # tokenName

{- | Check there is only one `PubKey` output and ensure that
output does not contain token with the given `CurrencySymbol`
-}
phasOnlyOnePubKeyOutputAndNoTokenWithSymbol ::
  forall (s :: S).
  Term s (PBuiltinList PTxOut :--> PCurrencySymbol :--> PTokenName :--> PBool)
phasOnlyOnePubKeyOutputAndNoTokenWithSymbol = phoistAcyclic $
  plam $
    \txOuts symbol tokenName ->
      ptxOutListCheck # (pgetPubKeyOutputs # txOuts) # symbol # tokenName

{- | Helper for checking that there is exactly one element in the `PTxOut` list
and that element does not contain a token with the given `CurrenySymbol`
-}
ptxOutListCheck ::
  forall (s :: S).
  Term s (PBuiltinList PTxOut :--> PCurrencySymbol :--> PTokenName :--> PBool)
ptxOutListCheck = phoistAcyclic $
  plam $ \txOuts symbol tokenName ->
    pmatch txOuts $ \case
      PCons txOut xs ->
        ( ( pvalueOf
              # (pfromData $ pfield @"value" # txOut)
              # symbol
              # tokenName
          )
            #== pconstant 0
        )
          #&& (pnull # xs)
      _ -> pconstant False

-- | Get all the `PubKey` outputs from the list of `PTxOut`
pgetPubKeyOutputs ::
  forall (s :: S). Term s (PBuiltinList PTxOut :--> PBuiltinList PTxOut)
pgetPubKeyOutputs = pfilter # pisPubKeyOutput

-- | Get all the `PubKey` inputs from the list of `PTxInInfo`
pgetPubKeyInputs ::
  forall (s :: S). Term s (PBuiltinList PTxInInfo :--> PBuiltinList PTxOut)
pgetPubKeyInputs = phoistAcyclic $
  plam $ \inputs ->
    pfoldr
      # plam
        ( \txInInfo acc ->
            pif
              (pisPubKeyOutput #$ pfromData $ pfield @"resolved" # txInInfo)
              (pcons # (pfromData $ pfield @"resolved" # txInInfo) # acc)
              acc
        )
      # pnil
      # inputs

-- | Check that the `PTxOut` is a `PubKey` output
pisPubKeyOutput :: forall (s :: S). Term s (PTxOut :--> PBool)
pisPubKeyOutput = phoistAcyclic $
  plam $ \output ->
    pisPubKey
      #$ pfromData
      $ pfield @"credential"
        #$ pfromData
      $ pfield @"address" # output

-- | Check that the given credential is a `PPubKeyCredential`
pisPubKey :: forall (s :: S). Term s (PCredential :--> PBool)
pisPubKey = phoistAcyclic $
  plam $ \credential ->
    pmatch credential $ \case
      PPubKeyCredential _ -> pconstant True
      _ -> pconstant False

-- | Check the output contains a valid datum (not finished)
phasValidOutputDatum ::
  forall (s :: S).
  Term s (PTxOut :--> PBool)
phasValidOutputDatum = phoistAcyclic $
  plam $ \txOut ->
    pmatch (pfromData $ pfield @"datum" # txOut) $ \case
      POutputDatum _ -> pconstant True
      _ -> pconstant False

-- | Check that the given credential is a `PScriptCredential`
pisScriptCredential :: forall (s :: S). Term s (PCredential :--> PBool)
pisScriptCredential = phoistAcyclic $
  plam $
    \credential -> pnot #$ pisPubKey # credential
