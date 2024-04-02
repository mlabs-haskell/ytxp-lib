module Test.Unit.Spec (unitSpec) where

import Cardano.YTxP.Control.Vendored (psymbolValueOf)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, testCase)
import Test.Unit.Transaction (
  dummyTxInInfoSingletonList,
  dummyTxInInfoSingletonListTwo,
  dummyTxInInfoTwoElementList,
 )
import Test.Unit.Values (
  dummySymbolOne,
  dummySymbolTwo,
  dummyValueOne,
 )
import Utils (
  phasNoScriptInputWithToken,
  phasOnlyOneInputWithExactlyOneTokenWithSymbol,
 )

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

-- | Token is in list, so should be false
phasNoScriptInputWithTokenTestOne :: Term s PBool
phasNoScriptInputWithTokenTestOne =
  phasNoScriptInputWithToken dummyTxInInfoSingletonList
    # (pconstant dummySymbolOne)
    #== pexpectedResultFalse

-- | Token is not in list, so this should be true
phasNoScriptInputWithTokenTestTwo :: Term s PBool
phasNoScriptInputWithTokenTestTwo =
  phasNoScriptInputWithToken dummyTxInInfoSingletonList
    # (pconstant dummySymbolTwo)
    #== pexpectedResultTrue

-- | Empty list, so token is not in list, so should be true
phasNoScriptInputWithTokenTestThree :: Term s PBool
phasNoScriptInputWithTokenTestThree =
  phasNoScriptInputWithToken (pconstant [])
    # (pconstant dummySymbolTwo)
    #== pexpectedResultTrue

-- | Empty list, so should be false
phasOnlyOneInputWithExactlyOneTokenWithSymbolTestOne :: Term s PBool
phasOnlyOneInputWithExactlyOneTokenWithSymbolTestOne =
  phasOnlyOneInputWithExactlyOneTokenWithSymbol (pconstant [])
    # (pconstant dummySymbolOne)
    #== pexpectedResultFalse

{- | Singleton list, with token that corresponds to symbol passed as argument
Should be true
-}
phasOnlyOneInputWithExactlyOneTokenWithSymbolTestTwo :: Term s PBool
phasOnlyOneInputWithExactlyOneTokenWithSymbolTestTwo =
  phasOnlyOneInputWithExactlyOneTokenWithSymbol dummyTxInInfoSingletonList
    # (pconstant dummySymbolOne)
    #== pexpectedResultTrue

{- | Singleton list, but token does not correspond to symbol passed as argument
Should be false
-}
phasOnlyOneInputWithExactlyOneTokenWithSymbolTestThree :: Term s PBool
phasOnlyOneInputWithExactlyOneTokenWithSymbolTestThree =
  phasOnlyOneInputWithExactlyOneTokenWithSymbol dummyTxInInfoSingletonList
    # (pconstant dummySymbolTwo)
    #== pexpectedResultFalse

{- | Two element list, contains one token with symbol passed as arugment
but needs to be only one element in list, so should be false
-}
phasOnlyOneInputWithExactlyOneTokenWithSymbolTestFour :: Term s PBool
phasOnlyOneInputWithExactlyOneTokenWithSymbolTestFour =
  phasOnlyOneInputWithExactlyOneTokenWithSymbol dummyTxInInfoTwoElementList
    # (pconstant dummySymbolOne)
    #== pexpectedResultFalse

{- | Singleton list, and token corresponds to the symbol passed as arugment,
but the `Value` amount is 4 not 1, so should be false
-}
phasOnlyOneInputWithExactlyOneTokenWithSymbolTestFive :: Term s PBool
phasOnlyOneInputWithExactlyOneTokenWithSymbolTestFive =
  phasOnlyOneInputWithExactlyOneTokenWithSymbol dummyTxInInfoSingletonListTwo
    # (pconstant dummySymbolOne)
    #== pexpectedResultFalse

unitSpec :: [TestTree]
unitSpec =
  [ -- Test for `psymbolValueOf` helper
    testCase "psymbolValueOf - Utils" $
      assertBool "Should pass" $
        plift psymbolValueOfShouldBeEqual
  , -- Tests for `phasNoScriptInputWithToken` helper
    testCase "phasNoScriptInputWithToken - Utils" $
      assertBool "Should be false" $
        plift phasNoScriptInputWithTokenTestOne
  , testCase "phasNoScriptInputWithToken - Utils" $
      assertBool "Should be true" $
        plift phasNoScriptInputWithTokenTestTwo
  , testCase "phasNoScriptInputWithTokenTestThree - Utils" $
      assertBool "Should be true" $
        plift phasNoScriptInputWithTokenTestThree
  , -- Tests for `phasOnlyOneInputWithExactlyOneTokenWithSymbol` helper
    testCase "phasOnlyOneInputWithExactlyOneTokenWithSymbol - Utils" $
      assertBool "Should be false" $
        plift phasOnlyOneInputWithExactlyOneTokenWithSymbolTestOne
  , testCase "phasOnlyOneInputWithExactlyOneTokenWithSymbol - Utils" $
      assertBool "Should be true" $
        plift phasOnlyOneInputWithExactlyOneTokenWithSymbolTestTwo
  , testCase "phasOnlyOneInputWithExactlyOneTokenWithSymbol - Utils" $
      assertBool "Should be false" $
        plift phasOnlyOneInputWithExactlyOneTokenWithSymbolTestThree
  , testCase "phasOnlyOneInputWithExactlyOneTokenWithSymbol - Utils" $
      assertBool "Should be false" $
        plift phasOnlyOneInputWithExactlyOneTokenWithSymbolTestFour
  , testCase "phasOnlyOneInputWithExactlyOneTokenWithSymbol - Utils" $
      assertBool "Should be false" $
        plift phasOnlyOneInputWithExactlyOneTokenWithSymbolTestFive
  ]
