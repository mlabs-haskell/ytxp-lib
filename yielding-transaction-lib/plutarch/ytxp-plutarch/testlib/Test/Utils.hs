module Test.Utils (noShrink) where

{- | More meaningful version of @'const []'@.

@since 0.1.0
-}
noShrink ::
  forall (a :: Type).
  a ->
  [a]
noShrink = const []
