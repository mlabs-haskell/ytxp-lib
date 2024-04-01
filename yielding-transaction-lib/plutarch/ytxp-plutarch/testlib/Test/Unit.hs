module Test.Unit (unitSpec) where

import Cardano.YTxP.Control.Vendored (psymbolValueOf)
import PlutusLedgerApi.V1.Value (CurrencySymbol (CurrencySymbol), Value,
                                 singleton)
import PlutusLedgerApi.V2 (getLedgerBytes)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, testCase)

-- | Sample symbol for tests
dummySymbol :: CurrencySymbol
dummySymbol =
  CurrencySymbol $
    getLedgerBytes "00000000000000000000000000000000000000000000000000000000"

-- | Make a sample `Value` for tests
mkSomeValue :: Integer -> Value
mkSomeValue =
  singleton
    ( CurrencySymbol $
        getLedgerBytes "00000000000000000000000000000000000000000000000000000000"
    )
    "Some token"

-- | Expected result based on sample used
psymbolValueOfExpectedResult :: forall s. Term s PInteger
psymbolValueOfExpectedResult = pconstant 1

-- | Check that expected result is equal to function applied to sample
psymbolValueOfShouldBeEqual :: forall s. Term s PBool
psymbolValueOfShouldBeEqual =
  (psymbolValueOf # (pconstant dummySymbol) # (pconstant $ mkSomeValue 1))
    #== psymbolValueOfExpectedResult

unitSpec :: [TestTree]
unitSpec =
  [ testCase "psymbolValueOf - Utils" $
      assertBool "Should be equal" $
        plift psymbolValueOfShouldBeEqual
  ]
