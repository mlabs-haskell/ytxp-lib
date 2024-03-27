{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Vendored utilities from open source libraries.
See the appropriate License for details on usage.
-}
module Cardano.YTxP.Control.Vendored (
  DerivePConstantViaEnum (..),
  EnumIsData (..),
  PlutusTypeEnumData,
  applyScript,
  psymbolValueOf,
) where

import Data.Coerce (coerce)
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
import Plutarch.Internal.Generic (PGeneric)
import Plutarch.Internal.PlutusType (
  PlutusTypeStrat (
    DerivedPInner,
    PlutusTypeStratConstraint,
    derivedPCon,
    derivedPMatch
  ),
 )
import Plutarch.Lift (
  PConstantDecl (PConstantRepr, PConstanted, pconstantFromRepr, pconstantToRepr),
 )
import Plutarch.Script (Script (Script))
import PlutusTx (
  FromData (fromBuiltinData),
  ToData (toBuiltinData),
  UnsafeFromData (unsafeFromBuiltinData),
 )
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

--------------------------------------------------------------------------------
-- PEnumData

{- |
  Wrapper for deriving 'ToData', 'FromData' using an Integer representation via 'Enum'.

  Vendored from LPE
  TODO: Licensing info

  @since 1.1.0
-}
newtype EnumIsData (a :: Type) = EnumIsData a

-- | @since 1.1.0
instance forall (a :: Type). (Enum a) => ToData (EnumIsData a) where
  toBuiltinData = coerce $ toBuiltinData . toInteger . fromEnum @a

-- | @since 1.1.0
instance forall (a :: Type). (Enum a) => FromData (EnumIsData a) where
  fromBuiltinData = coerce $ fmap (toEnum @a . fromInteger) . fromBuiltinData @Integer

-- | @since 1.1.0
instance forall (a :: Type). (Enum a) => UnsafeFromData (EnumIsData a) where
  unsafeFromBuiltinData = coerce . toEnum @a . fromInteger . unsafeFromBuiltinData @Integer

{- |
  PlutusTypeEnumData

  Vendored from LPE
  TODO: Licensing info
-}
data PlutusTypeEnumData

class
  ( PGeneric p
  , forall s. Enum (p s)
  , forall s. Bounded (p s)
  ) =>
  IsPlutusTypeEnumData (p :: S -> Type)
instance
  ( PGeneric p
  , forall s. Enum (p s)
  , forall s. Bounded (p s)
  ) =>
  IsPlutusTypeEnumData p

instance PlutusTypeStrat PlutusTypeEnumData where
  type PlutusTypeStratConstraint PlutusTypeEnumData = IsPlutusTypeEnumData
  type DerivedPInner PlutusTypeEnumData _ = PInteger
  derivedPCon = fromInteger . toInteger . fromEnum
  derivedPMatch = pmatchEnum

{- |
  Pattern match over the integer-repr of a Bounded Enum type.

  Vendored from LPE
  TODO: Licensing info

  @since 1.1.0
-}
pmatchEnum ::
  forall (a :: Type) (b :: S -> Type) (s :: S).
  (Bounded a, Enum a) =>
  Term s PInteger ->
  (a -> Term s b) ->
  Term s b
pmatchEnum x f = unTermCont $ do
  x' <- pletC x

  let branch :: a -> Term s b -> Term s b
      branch n =
        pif
          (x' #== (fromInteger . toInteger . fromEnum $ n))
          (f n)

  pure $ foldr branch (f maxBound) safeCases

-- | Safely enumerate all the cases.
safeCases :: forall (a :: Type). (Bounded a, Enum a) => [a]
safeCases = enumFrom minBound

{- |
  Wrapper for deriving `PConstantDecl` using an Integer representation via 'Enum'.

  Vendored from LPE
  TODO: Licensing info

  @since 1.1.0
-}
newtype DerivePConstantViaEnum (h :: Type) (p :: S -> Type)
  = DerivePConstantEnum h

-- | @since 1.1.0
instance
  forall (p :: S -> Type) (h :: Type).
  ( PLift p
  , Enum h
  , DerivePlutusType p
  , DPTStrat p ~ PlutusTypeEnumData
  ) =>
  PConstantDecl (DerivePConstantViaEnum h p)
  where
  type PConstantRepr (DerivePConstantViaEnum h p) = Integer
  type PConstanted (DerivePConstantViaEnum h p) = p

  pconstantToRepr = toInteger . fromEnum @h . coerce
  pconstantFromRepr = Just . coerce . toEnum @h . fromInteger
