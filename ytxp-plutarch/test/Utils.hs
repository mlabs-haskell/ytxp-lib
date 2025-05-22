{-# LANGUAGE PackageImports #-}

module Utils (tests) where

import Plutarch.LedgerApi.V3 (KeyGuarantees (Unsorted), PMap)
import PlutusTx.AssocMap qualified as PlutusMap
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import "ytxp-plutarch" Utils (pmember)

pexpectedResultFalse :: Term s PBool
pexpectedResultFalse = pconstant False

pexpectedResultTrue :: Term s PBool
pexpectedResultTrue = pconstant True

tests :: TestTree
tests =
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

-----------------------------------------------
-- Tests for `pmember` helper

pmemberTestOne :: Term s PBool
pmemberTestOne =
  pmember
    # (pconstant 1 :: Term s PInteger)
    # ( pconstant (PlutusMap.safeFromList []) ::
          Term s (PMap 'Unsorted PInteger PInteger)
      )
    #== pexpectedResultFalse

pmemberTestTwo :: Term s PBool
pmemberTestTwo =
  pmember
    # (pconstant 1 :: Term s PInteger)
    # ( pconstant (PlutusMap.safeFromList [(1, 1)]) ::
          Term s (PMap 'Unsorted PInteger PInteger)
      )
    #== pexpectedResultTrue

pmemberTestThree :: Term s PBool
pmemberTestThree =
  pmember
    # (pconstant 2 :: Term s PInteger)
    # ( pconstant (PlutusMap.safeFromList [(1, 1)]) ::
          Term s (PMap 'Unsorted PInteger PInteger)
      )
    #== pexpectedResultFalse

pmemberTestFour :: Term s PBool
pmemberTestFour =
  pmember
    # (pconstant 1 :: Term s PInteger)
    # ( pconstant (PlutusMap.safeFromList [(1, 1), (2, 2)]) ::
          Term s (PMap 'Unsorted PInteger PInteger)
      )
    #== pexpectedResultTrue

pmemberTestFive :: Term s PBool
pmemberTestFive =
  pmember
    # (pconstant 2 :: Term s PInteger)
    # ( pconstant (PlutusMap.safeFromList [(1, 1), (2, 2)]) ::
          Term s (PMap 'Unsorted PInteger PInteger)
      )
    #== pexpectedResultTrue
