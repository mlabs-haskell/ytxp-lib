module Test.Unit.Spec (unitSpec) where

import Cardano.YTxP.Control.Vendored (psymbolValueOf)
import Plutarch.Api.V2 (KeyGuarantees (Unsorted), PMap)
import PlutusTx.AssocMap qualified as PlutusMap
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (expectFailBecause)
import Test.Tasty.HUnit (assertBool, testCase)
import Test.Unit.Transaction (
    pdummyTxInInfoSingletonList,
    pdummyTxInInfoSingletonListTwo,
    pdummyTxInInfoThreeElementList,
    pdummyTxInInfoTwoElementList,
    pdummyTxOutRefOne,
    pdummyTxOutRefTwo,
    pdummyTxOutSingletonList,
    pdummyTxOutTwoElementList,
 )
import Test.Unit.Values (
    dummySymbolOne,
    dummySymbolThree,
    dummySymbolTwo,
    dummyValueOne,
 )
import Utils (
    phasNoScriptInputWithToken,
    phasOneScriptInputAtValidatorWithExactlyOneToken,
    phasOnlyOneInputWithExactlyOneTokenWithSymbol,
    phasOnlyOneValidScriptOutputWithToken,
    pmember,
    poutputsDoNotContainToken,
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
    (psymbolValueOf # pconstant dummySymbolOne # pconstant dummyValueOne)
        #== psymbolValueOfExpectedResult

-- | Token is in list, so should be false
phasNoScriptInputWithTokenTestOne :: Term s PBool
phasNoScriptInputWithTokenTestOne =
    phasNoScriptInputWithToken pdummyTxInInfoSingletonList
        # pconstant dummySymbolOne
        #== pexpectedResultFalse

-- | Token is not in list, so this should be true
phasNoScriptInputWithTokenTestTwo :: Term s PBool
phasNoScriptInputWithTokenTestTwo =
    phasNoScriptInputWithToken pdummyTxInInfoSingletonList
        # pconstant dummySymbolTwo
        #== pexpectedResultTrue

-- | Empty list, so token is not in list, so should be true
phasNoScriptInputWithTokenTestThree :: Term s PBool
phasNoScriptInputWithTokenTestThree =
    phasNoScriptInputWithToken (pconstant [])
        # pconstant dummySymbolTwo
        #== pexpectedResultTrue

-- | Empty list, so should be false
phasOnlyOneInputWithExactlyOneTokenWithSymbolTestOne :: Term s PBool
phasOnlyOneInputWithExactlyOneTokenWithSymbolTestOne =
    phasOnlyOneInputWithExactlyOneTokenWithSymbol (pconstant [])
        # pconstant dummySymbolOne
        #== pexpectedResultFalse

{- | Singleton list, with token that corresponds to symbol passed as argument
Should be true
-}
phasOnlyOneInputWithExactlyOneTokenWithSymbolTestTwo :: Term s PBool
phasOnlyOneInputWithExactlyOneTokenWithSymbolTestTwo =
    phasOnlyOneInputWithExactlyOneTokenWithSymbol pdummyTxInInfoSingletonList
        # pconstant dummySymbolOne
        #== pexpectedResultTrue

{- | Singleton list, but token does not correspond to symbol passed as argument
Should be false
-}
phasOnlyOneInputWithExactlyOneTokenWithSymbolTestThree :: Term s PBool
phasOnlyOneInputWithExactlyOneTokenWithSymbolTestThree =
    phasOnlyOneInputWithExactlyOneTokenWithSymbol pdummyTxInInfoSingletonList
        # pconstant dummySymbolTwo
        #== pexpectedResultFalse

{- | Two element list, contains one token with symbol passed as argument
but needs to be only one element in list, so should be false
-}
phasOnlyOneInputWithExactlyOneTokenWithSymbolTestFour :: Term s PBool
phasOnlyOneInputWithExactlyOneTokenWithSymbolTestFour =
    phasOnlyOneInputWithExactlyOneTokenWithSymbol pdummyTxInInfoTwoElementList
        # pconstant dummySymbolOne
        #== pexpectedResultFalse

{- | Singleton list, and token corresponds to the symbol passed as argument
but the `Value` amount is 4 not 1, so should be false
-}
phasOnlyOneInputWithExactlyOneTokenWithSymbolTestFive :: Term s PBool
phasOnlyOneInputWithExactlyOneTokenWithSymbolTestFive =
    phasOnlyOneInputWithExactlyOneTokenWithSymbol pdummyTxInInfoSingletonListTwo
        # pconstant dummySymbolOne
        #== pexpectedResultFalse

-- | Empty list, so token is not in list, so should be true
poutputsDoNotContainTokenTestOne :: Term s PBool
poutputsDoNotContainTokenTestOne =
    poutputsDoNotContainToken (pconstant [])
        # pconstant dummySymbolOne
        #== pexpectedResultTrue

{- | Singleton list, but contains token corresponding to the passed CurrencySymbol
Should be false
-}
poutputsDoNotContainTokenTestTwo :: Term s PBool
poutputsDoNotContainTokenTestTwo =
    poutputsDoNotContainToken pdummyTxOutSingletonList
        # pconstant dummySymbolOne
        #== pexpectedResultFalse

{- | Two element list, but one of the outputs contains token
corresponding to the passed CurrencySymbol, should be false
-}
poutputsDoNotContainTokenTestThree :: Term s PBool
poutputsDoNotContainTokenTestThree =
    poutputsDoNotContainToken pdummyTxOutTwoElementList
        # pconstant dummySymbolTwo
        #== pexpectedResultFalse

{- | Two element list, and none of the outputs contain token
corresponding to the passed CurrencySymbol, should be true
-}
poutputsDoNotContainTokenTestFour :: Term s PBool
poutputsDoNotContainTokenTestFour =
    poutputsDoNotContainToken pdummyTxOutTwoElementList
        # pconstant dummySymbolThree
        #== pexpectedResultTrue

-- | Empty list, so should be false
phasOneScriptInputAtValidatorWithExactlyOneTokenTestOne :: Term s PBool
phasOneScriptInputAtValidatorWithExactlyOneTokenTestOne =
    phasOneScriptInputAtValidatorWithExactlyOneToken
        (pconstant [])
        # pconstant dummySymbolOne
        # pdummyTxOutRefOne
        #== pexpectedResultFalse

{- | Singleton list, with single token corresponding to passed symbol
and passed TxOutRef corresponding to TxOutRef in TxInInfo in list, so should be true
-}
phasOneScriptInputAtValidatorWithExactlyOneTokenTestTwo :: Term s PBool
phasOneScriptInputAtValidatorWithExactlyOneTokenTestTwo =
    phasOneScriptInputAtValidatorWithExactlyOneToken
        pdummyTxInInfoSingletonList
        # pconstant dummySymbolOne
        # pdummyTxOutRefOne
        #== pexpectedResultTrue

{- | Singleton list, with single token corresponding to passed symbol
but passed TxOutRef does not correspond to TxOutRef in TxInInfo in list, so should be false
-}
phasOneScriptInputAtValidatorWithExactlyOneTokenTestThree :: Term s PBool
phasOneScriptInputAtValidatorWithExactlyOneTokenTestThree =
    phasOneScriptInputAtValidatorWithExactlyOneToken
        pdummyTxInInfoSingletonList
        # pconstant dummySymbolOne
        # pdummyTxOutRefTwo
        #== pexpectedResultFalse

-- | Two element list, that should be true
phasOneScriptInputAtValidatorWithExactlyOneTokenTestFour :: Term s PBool
phasOneScriptInputAtValidatorWithExactlyOneTokenTestFour =
    phasOneScriptInputAtValidatorWithExactlyOneToken
        pdummyTxInInfoTwoElementList
        # pconstant dummySymbolTwo
        # pdummyTxOutRefTwo
        #== pexpectedResultTrue

{- | Should be false, as a token with the given symbol appears at more
than one of the inputs
-}
phasOneScriptInputAtValidatorWithExactlyOneTokenTestFive :: Term s PBool
phasOneScriptInputAtValidatorWithExactlyOneTokenTestFive =
    phasOneScriptInputAtValidatorWithExactlyOneToken
        pdummyTxInInfoThreeElementList
        # pconstant dummySymbolOne
        # pdummyTxOutRefOne
        #== pexpectedResultFalse

{- | Same list as above but should be true as the token with the given
symbol differs from above, and only one of them appears with amount 1
at one if the inputs
-}
phasOneScriptInputAtValidatorWithExactlyOneTokenTestSix :: Term s PBool
phasOneScriptInputAtValidatorWithExactlyOneTokenTestSix =
    phasOneScriptInputAtValidatorWithExactlyOneToken
        pdummyTxInInfoThreeElementList
        # pconstant dummySymbolTwo
        # pdummyTxOutRefTwo
        #== pexpectedResultTrue

-- | Empty list so should be false
phasOnlyOneValidScriptOutputWithTokenTestOne :: Term s PBool
phasOnlyOneValidScriptOutputWithTokenTestOne =
    phasOnlyOneValidScriptOutputWithToken
        3
        (pconstant [])
        # pconstant dummySymbolOne
        #== pexpectedResultFalse

-- | Should fail, won't decode datum
phasOnlyOneValidScriptOutputWithTokenTestTwo :: Term s PBool
phasOnlyOneValidScriptOutputWithTokenTestTwo =
    phasOnlyOneValidScriptOutputWithToken
        3
        pdummyTxOutSingletonList
        # pconstant dummySymbolOne
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
    , -- Tests for `poutputsDoNotContainToken` helper
      testCase "poutputsDoNotContainToken - Utils" $
        assertBool "Should be true" $
            plift poutputsDoNotContainTokenTestOne
    , testCase "poutputsDoNotContainToken - Utils" $
        assertBool "Should be false" $
            plift poutputsDoNotContainTokenTestTwo
    , testCase "poutputsDoNotContainToken - Utils" $
        assertBool "Should be false" $
            plift poutputsDoNotContainTokenTestThree
    , testCase "poutputsDoNotContainToken - Utils" $
        assertBool "Should be true" $
            plift poutputsDoNotContainTokenTestFour
    , -- Tests for `phasOneScriptInputAtValidatorWithExactlyOneTokenTestOne` helper
      testCase "phasOneScriptInputAtValidatorWithExactlyOneToken - Utils" $
        assertBool "Should be false" $
            plift phasOneScriptInputAtValidatorWithExactlyOneTokenTestOne
    , testCase "phasOneScriptInputAtValidatorWithExactlyOneToken - Utils" $
        assertBool "Should be true" $
            plift phasOneScriptInputAtValidatorWithExactlyOneTokenTestTwo
    , testCase "phasOneScriptInputAtValidatorWithExactlyOneToken - Utils" $
        assertBool "Should be false" $
            plift phasOneScriptInputAtValidatorWithExactlyOneTokenTestThree
    , testCase "phasOneScriptInputAtValidatorWithExactlyOneToken - Utils" $
        assertBool "Should be true" $
            plift phasOneScriptInputAtValidatorWithExactlyOneTokenTestFour
    , testCase "phasOneScriptInputAtValidatorWithExactlyOneToken - Utils" $
        assertBool "Should be false" $
            plift phasOneScriptInputAtValidatorWithExactlyOneTokenTestFive
    , testCase "phasOneScriptInputAtValidatorWithExactlyOneToken - Utils" $
        assertBool "Should be true" $
            plift phasOneScriptInputAtValidatorWithExactlyOneTokenTestSix
    , -- Tests for `phasOnlyOneValidScriptOutputWithToken` helper
      testCase "phasOnlyOneValidScriptOutputWithToken - Utils" $
        assertBool "Should be false" $
            plift phasOnlyOneValidScriptOutputWithTokenTestOne
    , expectFailBecause
        ( mconcat
            [ "Will fail to decode datum with (UnListData (I 1)) error"
            , " as the datum is just an Integer not the required YieldListDatum with a list"
            ]
        )
        $ testCase "phasOnlyOneValidScriptOutputWithToken - Utils"
        $ assertBool "Should be false"
        $ plift phasOnlyOneValidScriptOutputWithTokenTestTwo
    , -- Test for `pmember` helper
      testGroup
        "pmember - Utils"
        [ testCase "pmember # 1 # emptyMap #== False" $
            assertBool "Should be false" $
                plift pmemberTestOne
        , testCase "pmember # 1 # [(1, 1)] #== True" $
            assertBool "Should be true" $
                plift pmemberTestTwo
        , testCase "pmember # 2 # [(1, 1)] #== False" $
            assertBool "Should be false" $
                plift pmemberTestThree
        , testCase "pmember # 1 # [(1, 1), (2, 2)] #== True" $
            assertBool "Should be true" $
                plift pmemberTestFour
        , testCase "pmember # 2 # [(1, 1), (2, 2)] #== True" $
            assertBool "Should be true" $
                plift pmemberTestFive
        ]
    ]

-----------------------------------------------
-- Tests for `pmember` helper

pmemberTestOne :: Term s PBool
pmemberTestOne =
    pmember
        # (pconstant 1 :: Term s PInteger)
        # (pconstant (PlutusMap.fromList []) :: Term s (PMap 'Unsorted PInteger PInteger))
        #== pexpectedResultFalse

pmemberTestTwo :: Term s PBool
pmemberTestTwo =
    pmember
        # (pconstant 1 :: Term s PInteger)
        # ( pconstant (PlutusMap.fromList [(1, 1)]) ::
                Term s (PMap 'Unsorted PInteger PInteger)
          )
        #== pexpectedResultTrue

pmemberTestThree :: Term s PBool
pmemberTestThree =
    pmember
        # (pconstant 2 :: Term s PInteger)
        # ( pconstant (PlutusMap.fromList [(1, 1)]) ::
                Term s (PMap 'Unsorted PInteger PInteger)
          )
        #== pexpectedResultFalse

pmemberTestFour :: Term s PBool
pmemberTestFour =
    pmember
        # (pconstant 1 :: Term s PInteger)
        # ( pconstant (PlutusMap.fromList [(1, 1), (2, 2)]) ::
                Term s (PMap 'Unsorted PInteger PInteger)
          )
        #== pexpectedResultTrue

pmemberTestFive :: Term s PBool
pmemberTestFive =
    pmember
        # (pconstant 2 :: Term s PInteger)
        # ( pconstant (PlutusMap.fromList [(1, 1), (2, 2)]) ::
                Term s (PMap 'Unsorted PInteger PInteger)
          )
        #== pexpectedResultTrue
