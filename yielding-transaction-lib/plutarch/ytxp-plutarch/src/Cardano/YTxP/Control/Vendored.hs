{- | Vendored utilities from open source libraries.
See the appropriate License for details on usage.
-}
module Cardano.YTxP.Control.Vendored (
  applyScript,
  psymbolValueOf,
) where

import Plutarch.Api.V1.Value (
  PCurrencySymbol,
  PValue (PValue),
 )
import Plutarch.Api.V2 (
  AmountGuarantees,
  KeyGuarantees,
  PMap (PMap),
 )
import Plutarch.Extra.Maybe (pexpectJustC)
import Plutarch.Script (Script (Script))
import UntypedPlutusCore (Program (Program, _progAnn, _progTerm, _progVer))
import UntypedPlutusCore.Core.Type qualified as UplcType

{- | Apply a function to an argument on the compiled 'Script' level.

Vendored from LPE
TODO: Licensing info

 @since 3.8.0
-}
applyScript :: Script -> Script -> Script
applyScript f a =
  if fVer /= aVer
    then error "apply: Plutus Core version mismatch"
    else
      Script
        Program
          { _progTerm = UplcType.Apply () fTerm aTerm
          , _progVer = fVer
          , _progAnn = ()
          }
  where
    (Script Program {_progTerm = fTerm, _progVer = fVer}) = f
    (Script Program {_progTerm = aTerm, _progVer = aVer}) = a

{- | Get the sum of all values belonging to a particular CurrencySymbol.

Vendored from LPE
TODO: Licensing info

   @since 1.1.0
-}
psymbolValueOf ::
  forall (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (s :: S).
  Term s (PCurrencySymbol :--> PValue keys amounts :--> PInteger)
psymbolValueOf =
  phoistAcyclic $
    plam $ \sym value'' -> unTermCont $ do
      PValue value' <- pmatchC value''
      PMap value <- pmatchC value'
      m' <-
        pexpectJustC 0 (plookupAssoc # pfstBuiltin # psndBuiltin # pdata sym # value)
      PMap m <- pmatchC (pfromData m')
      pure $ pfoldr # plam (\x v -> pfromData (psndBuiltin # x) + v) # 0 # m

{- | Treats a list-like structure as an assoc list. More precisely, given a
 list-like structure of key-value pairs, a method of extracting the key and
 the value, and a \'target\' key, returns the corresponding value, or
 'PNothing' if there isn't one.

 = Note

 There may be multiple mappings for a specific key; in such a situation, only
 the /first/ match is returned. In general, this requires time proportional to
 the length of the list-like structure, as we may have to check every entry.

 Vendored from LPE
 TODO: Licensing info

 @since 3.6.0
-}
plookupAssoc ::
  forall
    (k :: S -> Type)
    (v :: S -> Type)
    (kv :: S -> Type)
    (ell :: (S -> Type) -> S -> Type)
    (s :: S).
  (PElemConstraint ell kv, PListLike ell, PEq k) =>
  Term s ((kv :--> k) :--> (kv :--> v) :--> k :--> ell kv :--> PMaybe v)
plookupAssoc = phoistAcyclic $
  plam $ \getKey getVal target kvs ->
    pmatch (pfindJust # (go # getKey # target) # kvs) $ \case
      PNothing -> pcon PNothing
      PJust kv -> pcon . PJust $ getVal # kv
  where
    go ::
      forall (s' :: S).
      Term s' ((kv :--> k) :--> k :--> kv :--> PMaybe kv)
    go = phoistAcyclic $
      plam $ \getKey target kv ->
        pif
          (target #== (getKey # kv))
          (pcon . PJust $ kv)
          (pcon PNothing)

{- | A combination of 'pmap' and 'pfind', but without needing an intermediate
 structure. More precisely, searched for the first element in a list-like
 structure that produces a 'PJust' argument, returning it if found; otherwise,
 produces 'PNothing'.

 Vendored from LPE
 TODO: Licensing info

 @since 3.6.0
-}
pfindJust ::
  forall (b :: S -> Type) (ell :: (S -> Type) -> S -> Type) (a :: S -> Type) (s :: S).
  (PElemConstraint ell a, PListLike ell) =>
  Term s ((a :--> PMaybe b) :--> ell a :--> PMaybe b)
pfindJust = phoistAcyclic $ plam $ \f -> precList (go f) (const $ pcon PNothing)
  where
    go ::
      forall (s' :: S).
      Term s' (a :--> PMaybe b) ->
      Term s' (ell a :--> PMaybe b) ->
      Term s' a ->
      Term s' (ell a) ->
      Term s' (PMaybe b)
    go f self x xs = pmatch (f # x) $ \case
      PNothing -> self # xs
      PJust v -> pcon $ PJust v
