{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Vendored utilities from open source libraries.
See the appropriate License for details on usage.
-}
module Cardano.YTxP.Control.Vendored (
  DerivePConstantViaEnum (..),
  EnumIsData (..),
  ProductIsData (..),
  DerivePConstantViaDataList (..),
  PlutusTypeEnumData,
  PlutusTypeDataList,
  applyScript,
  psymbolValueOf,
) where

import Data.Coerce (coerce)
import Data.Kind (Constraint)
import GHC.TypeLits (ErrorMessage (ShowType, Text, (:$$:), (:<>:)), TypeError)
import Generics.SOP qualified as SOP
import Plutarch.Api.V1.Value (
  PCurrencySymbol,
  PValue,
 )
import Plutarch.Api.V2 (
  AmountGuarantees,
  KeyGuarantees,
 )
import Plutarch.Internal.Generic (PCode, PGeneric, gpfrom, gpto)
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
  PLifted,
 )
import Plutarch.List (pfoldl')
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
Modified version of LPE function with same purpose.
-}
psymbolValueOf ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term s (PCurrencySymbol :--> PValue keys amounts :--> PInteger)
psymbolValueOf = phoistAcyclic $
  plam $ \policyId value ->
    let valueMap = pto (pto value)
        go = pfix #$ plam $ \self valueMap' ->
          pelimList
            ( \symbolAndTokens rest ->
                pif
                  (pfromData (pfstBuiltin # symbolAndTokens) #== policyId)
                  ( let tokens = pto (pto (pfromData (psndBuiltin # symbolAndTokens)))
                     in pfoldl'
                          ( \acc tokenAndAmount ->
                              (pfromData $ psndBuiltin # tokenAndAmount) + acc
                          )
                          # 0
                          # tokens
                  )
                  (self # rest)
            )
            0
            valueMap'
     in go # valueMap

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

--------------------------------------------------------------------------------
-- ProductIsData
-- Vendored from LPE

{- | Wrapper for deriving 'ToData', 'FromData' using the List
     constructor of Data to represent a Product type.

     It is recommended to use 'PlutusTypeDataList' when deriving
     'PlutusType' as it provides some basic safety by ensuring
     Plutarch types have an Inner type of 'PDataRecord'.

     Uses 'gProductToBuiltinData', 'gproductFromBuiltinData'.

 = Example
@
import qualified Generics.SOP as SOP

data Foo =
  Foo Integer [Integer]
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (FromData, ToData) via (ProductIsData Foo)
  deriving (PConstantDecl) via (DerivePConstantViaDataList Foo PFoo)

instance PUnsafeLiftDecl PFoo where type PLifted PFoo = Foo

newtype PFoo s
    = PFoo
      ( Term s
          ( PDataRecord
              '[ "abc" ':= PInteger
               , "def" ':= PBuiltinList (PAsData PInteger)
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PFoo where
   type DPTStrat _ = PlutusTypeDataList
@

  @since 3.8.0
-}
newtype ProductIsData (a :: Type) = ProductIsData a

-- | Variant of 'PConstantViaData' using the List repr from 'ProductIsData'
newtype DerivePConstantViaDataList (h :: Type) (p :: S -> Type)
  = DerivePConstantViaDataList h

type family GetPRecord' (a :: [[S -> Type]]) :: [PLabeledType] where
  GetPRecord' '[ '[PDataRecord a]] = a

type family GetPRecord (a :: S -> Type) :: S -> Type where
  GetPRecord a = PDataRecord (GetPRecord' (PCode a))

type family GetRecordTypes (n :: [[Type]]) :: [S -> Type] where
  GetRecordTypes '[x ': xs] = PConstanted x ': GetRecordTypes '[xs]
  GetRecordTypes '[ '[]] = '[]

type family UD' (p :: S -> Type) :: S -> Type where
  UD' (p x1 x2 x3 x4 x5) = p (UD' x1) (UD' x2) (UD' x3) (UD' x4) (UD' x5)
  UD' (p x1 x2 x3 x4) = p (UD' x1) (UD' x2) (UD' x3) (UD' x4)
  UD' (p x1 x2 x3) = p (UD' x1) (UD' x2) (UD' x3)
  UD' (p x1 x2) = p (UD' x1) (UD' x2)
  UD' (p x1) = p (PAsData (UD' x1))
  UD' p = p

type family UD (p :: [S -> Type]) :: [S -> Type] where
  UD (x ': xs) = UD' x ': UD xs
  UD '[] = '[]

type family PUnlabel (n :: [PLabeledType]) :: [S -> Type] where
  PUnlabel ((_ ' := p) ': xs) = p ': PUnlabel xs
  PUnlabel '[] = '[]

type family MatchTypes' (n :: [S -> Type]) (m :: [S -> Type]) :: Bool where
  MatchTypes' '[] '[] = 'True
  MatchTypes' (x ': xs) (x ': ys) = MatchTypes' xs ys
  MatchTypes' (_x ': _xs) (_y ': _ys) = 'False
  MatchTypes' '[] _ = 'False
  MatchTypes' _ '[] = 'False

type family MatchTypesError (n :: [S -> Type]) (m :: [S -> Type]) (a :: Bool) :: Constraint where
  MatchTypesError _ _ 'True = ()
  MatchTypesError n m 'False =
    ( 'True ~ 'False
    , TypeError
        ( 'Text "Error when deriving 'PlutusTypeDataList':"
            -- Note(Nigel): Added space between tick and operator to avoid the error:
            -- `The suffix use of a ‘:$$:’ might be repurposed as special syntax'
            -- Not sure if there's a flag that can help here?
            ' :$$: 'Text "\tMismatch between constituent Haskell and Plutarch types"
            ' :$$: 'Text "Constituent Haskell Types: "
            ' :$$: 'Text "\t"
               ' :<>: 'ShowType n
            ' :$$: 'Text "Constituent Plutarch Types: "
            ' :$$: 'Text "\t"
              ' :<>: 'ShowType m
        )
    )

type MatchTypes (n :: [S -> Type]) (m :: [S -> Type]) =
  (MatchTypesError n m (MatchTypes' n m))

class
  ( PGeneric p
  , PCode p ~ '[ '[GetPRecord p]]
  ) =>
  IsPlutusTypeDataList (p :: S -> Type)
instance
  forall (p :: S -> Type).
  ( PGeneric p
  , PCode p ~ '[ '[GetPRecord p]]
  , MatchTypes
      (UD (GetRecordTypes (SOP.Code (PLifted p))))
      (PUnlabel (GetPRecord' (PCode p)))
  ) =>
  IsPlutusTypeDataList p

{- | Vendored from LPE
@since 3.5.0
-}
data PlutusTypeDataList

instance PlutusTypeStrat PlutusTypeDataList where
  type PlutusTypeStratConstraint PlutusTypeDataList = IsPlutusTypeDataList
  type DerivedPInner PlutusTypeDataList a = GetPRecord a
  derivedPCon x = case gpfrom x of
    SOP.SOP (SOP.Z (x' SOP.:* SOP.Nil)) -> x'
    SOP.SOP (SOP.S x') -> case x' of {}
  derivedPMatch x f = f (gpto $ SOP.SOP $ SOP.Z $ x SOP.:* SOP.Nil)
