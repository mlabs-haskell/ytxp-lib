module Test.Unit.Spec (unitSpec) where

import Cardano.YTxP.Control.Vendored (psymbolValueOf)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, testCase)
import Test.Unit.Transaction (sampleTxInInfoList)
import Test.Unit.Values (
  dummySymbolOne,
  dummySymbolTwo,
  dummyValueOne,
 )
import Utils (phasNoScriptInputWithToken)

pexpectedResultFalse :: Term s PBool
pexpectedResultFalse = pconstant False

pexpectedResultTrue :: Term s PBool
pexpectedResultTrue = pconstant True

-- | Expected result based on sample used
psymbolValueOfExpectedResult :: forall s. Term s PInteger
psymbolValueOfExpectedResult = pconstant 1

-- | Check that expected result is equal to function applied to sample
psymbolValueOfShouldBeEqual :: forall s. Term s PBool
psymbolValueOfShouldBeEqual =
  (psymbolValueOf # (pconstant dummySymbolOne) # (pconstant dummyValueOne))
    #== psymbolValueOfExpectedResult

-- | Token is in list, so this should fail
phasNoScriptInputWithTokenTestOne :: Term s PBool
phasNoScriptInputWithTokenTestOne =
  phasNoScriptInputWithToken sampleTxInInfoList
    # (pconstant dummySymbolOne)
    #== pexpectedResultFalse

-- | Token is not in list, so this should succeed
phasNoScriptInputWithTokenTestTwo :: Term s PBool
phasNoScriptInputWithTokenTestTwo =
  phasNoScriptInputWithToken sampleTxInInfoList
    # (pconstant dummySymbolTwo)
    #== pexpectedResultTrue

unitSpec :: [TestTree]
unitSpec =
  [ testCase "psymbolValueOf - Utils" $
      assertBool "Should pass" $
        plift psymbolValueOfShouldBeEqual
  , testCase "phasNoScriptInputWithToken - Utils" $
      assertBool "Should pass" $
        plift phasNoScriptInputWithTokenTestOne
  , testCase "phasNoScriptInputWithToken - Utils" $
      assertBool "Should pass" $
        plift phasNoScriptInputWithTokenTestTwo
  ]
