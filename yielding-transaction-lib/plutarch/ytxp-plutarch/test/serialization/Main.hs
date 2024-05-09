module Main (main) where

import Cardano.YTxP.Control.ParametersInitial (
  ControlParametersInitial (ControlParametersInitial),
 )
import Cardano.YTxP.Control.Stubs (
  alwaysSucceedsTwoArgumentScript,
  alwaysSucceedsValidator,
 )
import Control.Monad (guard)
import Data.Aeson (encode)
import Data.ByteString.Short (ShortByteString)
import Data.Text (unpack)
import GHC.Exts (fromList, toList)
import Plutarch.Internal (
  Config (Config),
  TracingMode (DetTracing, DoTracing, DoTracingAndBinds, NoTracing),
 )
import Test.Laws (aesonLawsWith)
import Test.QuickCheck (
  Gen,
  NonNegative (NonNegative),
  arbitrary,
  counterexample,
  elements,
  forAllShrinkShow,
  shrink,
  (===),
 )
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)
import Test.Utils (noShrink)

main :: IO ()
main =
  defaultMain . adjustOption go . testGroup "serialization" $
    [ testProperty "Hex encoding of ShortByteString roundtrips"
        . forAllShrinkShow genSBS shrinkSBS (show . toList)
        $ \sbs ->
          let converted = sbsToHexText sbs
           in counterexample ("Converted: " <> unpack converted) $
                Just sbs === (hexTextToSBS . sbsToHexText $ sbs)
    , aesonLawsWith @(ControlParametersInitial Integer) genCPI noShrink
    , goldenVsString
        "ControlParametersInitial Integer"
        "goldens/ControlParametersInitialInteger.golden"
        (pure . encode $ sampleYLS)
    ]
  where
    go :: QuickCheckTests -> QuickCheckTests
    go = max 10_000

-- Golden data

sampleYLS :: ControlParametersInitial Integer
sampleYLS =
  ControlParametersInitial
    1
    [1, 2]
    [1, 2, 3]
    alwaysSucceedsTwoArgumentScript
    alwaysSucceedsValidator
    (Config NoTracing)

-- Generators and shrinkers

-- TODO: This definitely needs more thought.
genCPI :: Gen (ControlParametersInitial Integer)
genCPI = do
  NonNegative myls' <- arbitrary
  let myls = fromInteger myls'
  stakingValsNonceList <- arbitrary
  mintingPoliciesNonceList <- arbitrary
  tm <- elements [NoTracing, DetTracing, DoTracing, DoTracingAndBinds]
  pure $
    ControlParametersInitial
      myls
      stakingValsNonceList
      mintingPoliciesNonceList
      alwaysSucceedsTwoArgumentScript
      alwaysSucceedsValidator
      (Config tm)

genSBS :: Gen ShortByteString
genSBS = fromList <$> arbitrary

shrinkSBS :: ShortByteString -> [ShortByteString]
shrinkSBS sbs = do
  let asList = toList sbs
  shrunk <- shrink asList
  guard (not . null $ shrunk)
  pure . fromList $ shrunk
