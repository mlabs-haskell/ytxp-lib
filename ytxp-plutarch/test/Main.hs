module Main (main) where

import Cardano.YTxP.Control.Yielding.Helper (yieldingHelper)
import Cardano.YTxP.SDK.Redeemers (AuthorisedScriptIndex (AuthorisedScriptIndex), AuthorisedScriptProofIndex (AuthorisedScriptProofIndex), AuthorisedScriptPurpose (Minting), YieldingRedeemer (YieldingRedeemer))
import Data.String (IsString)
import Plutarch.Context (Builder, MintingBuilder, SpendingBuilder, buildMinting, mintSingletonWith, mkOutRefIndices, referenceInput, script, withMinting, withRefTxId, withReferenceScript, withSpendingUTXO, withValue)
import Plutarch.Test.Precompiled (
  tryFromPTerm,
  (@>),
 )
import PlutusLedgerApi.V2 (
  CurrencySymbol,
  ScriptContext,
  ScriptHash,
  singleton,
 )
import PlutusTx qualified
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = do
  defaultMain $
    testGroup
      "test suite"
      [ yieldingHelperTest
      ]

yieldingHelperTest :: TestTree
yieldingHelperTest = tryFromPTerm "yielding helper" (yieldingHelper # pconstant authorisedScriptSTCS) $ do
  [PlutusTx.toData testRedeemer, PlutusTx.toData mintFromAuthorisedScript] @> "It should mint a token from the yielding script"

testRedeemer :: YieldingRedeemer
testRedeemer = YieldingRedeemer (AuthorisedScriptIndex 0) (AuthorisedScriptProofIndex (Minting, 0))

authorisedScriptValidator :: ScriptHash
authorisedScriptValidator = "11111111111111111111111111111111111111111111111111111111"

authorisedScript :: (IsString a) => a
authorisedScript = "22222222222222222222222222222222222222222222222222222222"

authorisedScriptSTCS :: CurrencySymbol
authorisedScriptSTCS = "bb"

commonContext :: (Monoid a, Builder a) => a -> a
commonContext rest =
  mkOutRefIndices $
    mconcat
      [ referenceInput $
          script authorisedScriptValidator
            <> withValue (singleton authorisedScriptSTCS "" 1)
            <> withReferenceScript authorisedScript
            <> withRefTxId "eeff"
      , rest
      ]

mintTokenCtx :: MintingBuilder
mintTokenCtx = mintSingletonWith testRedeemer authorisedScript "hello" 333 <> withMinting authorisedScript

mintingContext :: MintingBuilder
mintingContext = commonContext mintTokenCtx

_spendingContext :: SpendingBuilder
_spendingContext = commonContext $ withSpendingUTXO (withValue (singleton "cc" "hello" 123))

mintFromAuthorisedScript :: ScriptContext
mintFromAuthorisedScript = buildMinting mempty mintingContext
