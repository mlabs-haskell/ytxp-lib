module Main (main) where

import Cardano.YTxP.Control.ParametersInitial (ControlParametersInitial (ControlParametersInitial))
import Cardano.YTxP.Control.Stubs (alwaysSucceedsTwoArgumentScript,
                                   alwaysSucceedsValidator)
import Data.Aeson (encode)
import Plutarch.Internal (Config (Config),
                          TracingMode (DetTracing, DoTracing, DoTracingAndBinds, NoTracing))
import Test.Laws (aesonLawsWith)
import Test.QuickCheck (Gen, arbitrary, elements)
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.QuickCheck (QuickCheckTests)
import Test.Unit (unitSpec)
import Test.Utils (noShrink)

main :: IO ()
main = defaultMain . adjustOption go . testGroup "serialization" $ [
  aesonLawsWith @(ControlParametersInitial Integer) genCPI noShrink,
  goldenVsString "ControlParametersInitial Integer"
                 "goldens/ControlParametersInitialInteger.golden"
                 (pure . encode $ sampleYLS)
  ] <> unitSpec
  where
    go :: QuickCheckTests -> QuickCheckTests
    go = max 10_000

-- Golden data

sampleYLS :: ControlParametersInitial Integer
sampleYLS = ControlParametersInitial 1
                                     [1, 2]
                                     alwaysSucceedsTwoArgumentScript
                                     alwaysSucceedsValidator
                                     (Config NoTracing)

-- TODO: This definitely needs more thought.
genCPI :: Gen (ControlParametersInitial Integer)
genCPI = do
  myls <- arbitrary
  nl <- arbitrary
  tm <- elements [NoTracing, DetTracing, DoTracing, DoTracingAndBinds]
  pure $ ControlParametersInitial myls
                                  nl
                                  alwaysSucceedsTwoArgumentScript
                                  alwaysSucceedsValidator
                                  (Config tm)
