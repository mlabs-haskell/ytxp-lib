{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Laws (aesonLaws, aesonLawsWith) where

import Data.Aeson (FromJSON, ToJSON (toJSON), decode, encode)
import Prettyprinter (Pretty (pretty), defaultLayoutOptions, layoutSmart,
                      viaShow, (<+>))
import Prettyprinter.Render.String (renderString)
import Test.QuickCheck (Arbitrary (arbitrary, shrink), Gen, Property,
                        counterexample, forAllShrinkShow)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Type.Reflection (Typeable, tyConName, typeRep, typeRepTyCon)

{- | Verifies the laws that any valid type implementing both 'ToJSON' and
'FromJSON' should follow. More precisely, this checks:

1. Roundtripping: @'decode' '.' 'encode'@ @=@ @'Just'@
2. 'toJSON' and 'toEncoding' must agree: @'decode' '.' 'encode'@ @=@ @'Just'
'.' 'toJSON'@

@since 0.1.0
-}
aesonLaws ::
  forall (a :: Type).
  (Typeable a, ToJSON a, FromJSON a, Arbitrary a, Pretty a, Eq a) =>
  TestTree
aesonLaws = aesonLawsWith @a arbitrary shrink

{- | As 'aesonLaws', but allows explicit control of generationa and shrinking.

@since 0.1.0
-}
aesonLawsWith ::
  forall (a :: Type).
  (Typeable a, ToJSON a, FromJSON a, Eq a, Pretty a) =>
  Gen a ->
  (a -> [a]) ->
  TestTree
aesonLawsWith gen shr =
  testGroup
    groupName
    [ testProperty "decode . encode = Just" . forAllShrinkShow gen shr stringify $ \x ->
        equiv (Just x) (decode . encode $ x)
    , testProperty "decode . encode = Just . toJSON"
        . forAllShrinkShow gen shr stringify
        $ \x ->
          equiv
            (ViaShow <$> (decode . encode $ x))
            (ViaShow <$> (Just . toJSON $ x))
    ]
  where
    groupName :: String
    groupName = "Aeson laws for " <> typeName @a

-- Helpers

-- Helper for turning a Pretty thing into a String
stringify ::
  forall (a :: Type).
  (Pretty a) =>
  a ->
  String
stringify = renderString . layoutSmart defaultLayoutOptions . pretty

-- Like ===, but uses Pretty instead of Show
equiv ::
  forall (a :: Type).
  (Eq a, Pretty a) =>
  a ->
  a ->
  Property
equiv x y = counterexample go $ x == y
  where
    go :: String
    go =
      let operator = if x == y then "==" else "/="
       in renderString . layoutSmart defaultLayoutOptions $
            pretty x <+> operator <+> pretty y

-- Helper for turning the name of an arbitrary Typeable into a String
typeName ::
  forall (a :: Type).
  (Typeable a) =>
  String
typeName = tyConName . typeRepTyCon $ typeRep @a

-- Helper newtype for avoiding 'Pretty' instances (usually orphans) when an
-- autogenerated 'Show' exists.
newtype ViaShow (a :: Type) = ViaShow a
  deriving (Eq) via a

instance (Show a) => Pretty (ViaShow a) where
  {-# INLINEABLE pretty #-}
  pretty (ViaShow x) = viaShow x
