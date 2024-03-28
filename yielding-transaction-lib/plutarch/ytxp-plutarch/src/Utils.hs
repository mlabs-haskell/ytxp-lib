module Utils (
  phasTokenOfCurrencySymbolTokenNameAndAmount,
  phasOnlyOneValidScriptOutputWithToken,
  phasOnlyOnePubKeyOutputAndNoTokenWithSymbol,
  poutputsDoNotContainTokenWithSymbol,
  pands,
)
where

import Cardano.YTxP.Control.Vendored (psymbolValueOf)
import Plutarch.Api.V1 (
  PCredential (PPubKeyCredential),
 )
import Plutarch.Api.V1.Value (
  PCurrencySymbol,
  PTokenName,
  PValue,
  padaSymbol,
  pvalueOf,
 )
import Plutarch.Api.V2 (
  AmountGuarantees,
  KeyGuarantees,
  POutputDatum (POutputDatum),
  PTxOut,
 )
import Plutarch.Extra.Map (pkeys)
import Plutarch.Extra.Maybe (pjust, pnothing)
import Data.List.NonEmpty (nonEmpty)

-- | Like Haskell's `and` but for Plutarch terms
-- `Plutarch.Bool` has the same function but does not export it.
pands :: [Term s PBool] -> Term s PBool
pands ts' =
  case nonEmpty ts' of
    Nothing -> pcon PTrue
    Just ts -> foldl1 (#&&) ts

-- | Check that the outputs do not contain a token with the given symbol
poutputsDoNotContainTokenWithSymbol ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList PTxOut
        :--> PCurrencySymbol
        :--> PBool
    )
poutputsDoNotContainTokenWithSymbol = phoistAcyclic $
  plam $ \txOuts symbol ->
    pmatch (pfilter # (poutputContainsTokenWithSymbol # symbol) # txOuts) $ \case
      PNil -> pconstant True
      _ -> pconstant False

-- | Check that the given output contains at least one token with the given symbol
poutputContainsTokenWithSymbol ::
  forall (s :: S).
  Term s (PCurrencySymbol :--> PTxOut :--> PBool)
poutputContainsTokenWithSymbol = phoistAcyclic $
  plam $
    \symbol txOut ->
      (pconstant 0)
        #< (psymbolValueOf # symbol #$ pfromData $ pfield @"value" # txOut)

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

{- | Check there is only one `PubKey` output and ensure that
output does not contain token with the given `CurrencySymbol`
-}
phasOnlyOnePubKeyOutputAndNoTokenWithSymbol ::
  forall (s :: S).
  Term s (PBuiltinList PTxOut :--> PCurrencySymbol :--> PBool)
phasOnlyOnePubKeyOutputAndNoTokenWithSymbol = phoistAcyclic $
  plam $
    \txOuts symbol ->
      ptxOutListCheck # (pgetPubKeyOutputs # txOuts) # symbol

{- | Helper for checking that there is exactly one element in the `PTxOut` list
and that element does not contain a token with the given `CurrenySymbol`
-}
ptxOutListCheck ::
  forall (s :: S).
  Term s (PBuiltinList PTxOut :--> PCurrencySymbol :--> PBool)
ptxOutListCheck = phoistAcyclic $
  plam $ \txOuts symbol ->
    pmatch txOuts $ \case
      PCons txOut tailOfList ->
        ( ( psymbolValueOf
              # symbol
              # (pfromData $ pfield @"value" # txOut)
          )
            #== pconstant 0
        )
          #&& (pnull # tailOfList)
      _ -> pconstant False

-- | Get all the `PubKey` outputs from the list of `PTxOut`
pgetPubKeyOutputs ::
  forall (s :: S). Term s (PBuiltinList PTxOut :--> PBuiltinList PTxOut)
pgetPubKeyOutputs = pfilter # pisPubKeyOutput

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

{- | Check that there is one script output with a token with the
given `PCurrencySymbol` and `PTokenName` and that the `PValue`
at this output contains no other tokens aside from Ada and that
the output also contains a valid (according to the spec) `POutputDatum`.
-}
phasOnlyOneValidScriptOutputWithToken ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList PTxOut
        :--> PCurrencySymbol
        :--> PTokenName
        :--> PBool
    )
phasOnlyOneValidScriptOutputWithToken = phoistAcyclic $
  plam $ \txOuts symbol tokenName ->
    pmatch (phasOneScriptOutputWithToken # txOuts # symbol # tokenName) $ \case
      PNothing -> pconstant False
      PJust txOut ->
        (pcontainsOnlyAdaAndGivenSymbol # txOut # symbol)
          #&& phasValidOutputDatum
          # txOut

{- | Check that the given `PTxOut` does not contain any tokens
other than Ada and tokens with the given `PCurrencySymbol`
-}
pcontainsOnlyAdaAndGivenSymbol ::
  forall (s :: S).
  Term s (PTxOut :--> PCurrencySymbol :--> PBool)
pcontainsOnlyAdaAndGivenSymbol = phoistAcyclic $
  plam $
    \txOut symbol -> pnull #$ pgetOtherNonAdaSymbols # txOut # symbol

{- | This helpers returns a list of all `PCurrencySymbol` contained
 in the `value` of the given `PTxOut` except symbols that match
 the given `PCurrencySymbol` argument or the `Ada` symbol.
-}
pgetOtherNonAdaSymbols ::
  forall (s :: S).
  Term s (PTxOut :--> PCurrencySymbol :--> PBuiltinList (PAsData PCurrencySymbol))
pgetOtherNonAdaSymbols = phoistAcyclic $
  plam $
    \txOut symbol ->
      pfilter
        # ( plam $ \symbolInValue ->
              ( pnot
                  #$ (pfromData symbolInValue)
                  #== symbol
                  #|| (pfromData symbolInValue)
                  #== padaSymbol
              )
          )
        # (pkeys #$ pto $ pfromData $ pfield @"value" # txOut)

{- | Check that there is one script output in the list of `PXOut`
that contains one token with the given `PCurrencySymbol` and `PTokenName`
-}
phasOneScriptOutputWithToken ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList PTxOut
        :--> PCurrencySymbol
        :--> PTokenName
        :--> PMaybe PTxOut
    )
phasOneScriptOutputWithToken = phoistAcyclic $
  plam $ \txOuts symbol tokenName ->
    pmatch (pcheckForScriptOutputWithToken # txOuts # symbol # tokenName) $ \case
      PCons txOut tailOfList -> pif (pnull # tailOfList) (pjust # txOut) pnothing
      _ -> pnothing

{- | Filters the given list of `PTxOut` for a script output containing
one token with given `PCurrencySymbol` and `PTokenName`
-}
pcheckForScriptOutputWithToken ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList PTxOut
        :--> PCurrencySymbol
        :--> PTokenName
        :--> PBuiltinList PTxOut
    )
pcheckForScriptOutputWithToken = phoistAcyclic $
  plam $ \txOuts symbol tokenName ->
    pfilter
      # ( plam $ \txOut ->
            ( pisScriptOutput # txOut
            )
              #&& (pvalueOf # (pfromData $ pfield @"value" # txOut) # symbol # tokenName)
              #== (pconstant 1)
        )
      # txOuts

-- | Check the output contains a valid datum (not finished)
phasValidOutputDatum ::
  forall (s :: S).
  Term s (PTxOut :--> PBool)
phasValidOutputDatum = phoistAcyclic $
  plam $ \txOut ->
    pmatch (pfromData $ pfield @"datum" # txOut) $ \case
      POutputDatum _ -> pconstant True
      _ -> pconstant False

-- | Check that the given `PTxOut` is a `PScriptCredential`
pisScriptOutput :: forall (s :: S). Term s (PTxOut :--> PBool)
pisScriptOutput = phoistAcyclic $
  plam $
    \txOut ->
      pnot #$ pisPubKey #$ pfromData $
        pfield @"credential" #$ pfromData $
          pfield @"address" # txOut
