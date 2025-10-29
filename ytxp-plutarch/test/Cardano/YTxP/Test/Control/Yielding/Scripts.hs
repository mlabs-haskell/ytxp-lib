module Cardano.YTxP.Test.Control.Yielding.Scripts (tests) where

import Cardano.YTxP.SDK.SdkParameters (
  AuthorisedScriptsSTCS (AuthorisedScriptsSTCS),
 )
import Cardano.YTxP.Test.Control.Yielding.Scripts.Attacks (testAttacksR)
import Cardano.YTxP.Test.Control.Yielding.Scripts.NominalCases (
  testNominalCasesR,
 )
import Cardano.YTxP.Test.Control.Yielding.Scripts.Utils (
  ScriptsTestsParams (
    ScriptsTestsParams,
    authorisedScriptHash,
    authorisedScriptsManagerHash,
    authorisedScriptsSTCS,
    oneshotUtxo
  ),
 )
import Control.Monad.Reader (Reader, runReader)
import PlutusLedgerApi.V3 (CurrencySymbol (CurrencySymbol), TxOutRef (TxOutRef))
import Test.Tasty (TestTree, testGroup)

dummyParams :: ScriptsTestsParams
dummyParams =
  ScriptsTestsParams
    { authorisedScriptHash =
        "22222222222222222222222222222222222222222222222222222222"
    , authorisedScriptsSTCS =
        AuthorisedScriptsSTCS $
          CurrencySymbol "33333333333333333333333333333333333333333333333333333333"
    , authorisedScriptsManagerHash =
        "11111111111111111111111111111111111111111111111111111111"
    , oneshotUtxo =
        TxOutRef "d44c22ef78ab49fd975ef4f07e0c8440ede296efca48eeed425096ab783c41d1" 0
    }

tests :: TestTree
tests = runReader testsR dummyParams

testsR :: Reader ScriptsTestsParams TestTree
testsR =
  let tests' =
        [ testNominalCasesR
        , testAttacksR
        ]
   in testGroup "YieldingScripts" <$> sequence tests'
