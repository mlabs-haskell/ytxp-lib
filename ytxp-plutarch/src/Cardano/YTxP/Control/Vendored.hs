{-# LANGUAGE QuantifiedConstraints #-}

{- | Vendored utilities from open source libraries.
See the appropriate License for details on usage.
-}
module Cardano.YTxP.Control.Vendored (
  DerivePConstantViaEnum (..),
  PlutusTypeEnumData,
) where

import Data.Coerce (coerce)
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
