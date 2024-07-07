{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Cardano.YTxP.Test.Control.Yielding.Scripts (tests) where

import Cardano.YTxP.SDK.SdkParameters (AuthorisedScriptsSTCS (AuthorisedScriptsSTCS))
import Cardano.YTxP.Test.Control.Yielding.Scripts.Attacks (testAttacksR)
import Cardano.YTxP.Test.Control.Yielding.Scripts.NominalCases (testNominalCasesR)
import Cardano.YTxP.Test.Control.Yielding.Scripts.Utils (
  ScriptsTestsParams (
    ScriptsTestsParams,
    authorisedScriptHash,
    authorisedScriptsManagerHash,
    authorisedScriptsSTCS
  ),
 )
import Control.Monad.Reader (Reader, runReader)
import Test.Tasty (TestTree, testGroup)

dummyParams :: ScriptsTestsParams
dummyParams =
  ScriptsTestsParams
    { authorisedScriptHash = "22222222222222222222222222222222222222222222222222222222"
    , authorisedScriptsSTCS = AuthorisedScriptsSTCS "33333333333333333333333333333333333333333333333333333333"
    , authorisedScriptsManagerHash = "11111111111111111111111111111111111111111111111111111111"
    }

tests :: TestTree
tests = runReader testsR dummyParams
testsR :: Reader ScriptsTestsParams TestTree
testsR =
  let tests' = [testNominalCasesR, testAttacksR]
   in testGroup "YieldingScripts" <$> sequence tests'
