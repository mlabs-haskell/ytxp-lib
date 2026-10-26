module Main (main) where

import Cardano.YTxP.SDK.Redeemers (
  AuthorisedScriptIndex (AuthorisedScriptIndex),
  AuthorisedScriptProofIndex (AuthorisedScriptProofIndex),
  AuthorisedScriptPurpose (Minting),
  YieldingRedeemer (YieldingRedeemer),
 )
import Cardano.YTxP.SDK.SdkParameters (
  AuthorisedScriptsSTCS (AuthorisedScriptsSTCS),
  SdkParameters (SdkParameters),
 )
import Data.Aeson (encode)
import GHC.Exts (fromString)
import PlutusLedgerApi.V3 (CurrencySymbol (CurrencySymbol), getLedgerBytes)
import PlutusTx qualified
import Test.Laws (aesonLawsWith, plutusTxDataLaws)
import Test.QuickCheck (
  Gen,
  NonNegative (getNonNegative),
  arbitrary,
 )
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.QuickCheck (QuickCheckTests)

main :: IO ()
main =
  defaultMain . adjustOption go . testGroup "serialization" $
    [ aesonLawsWith @SdkParameters genSdkParams (const [])
    , plutusTxDataLaws @AuthorisedScriptIndex
    , plutusTxDataLaws @AuthorisedScriptPurpose
    , plutusTxDataLaws @AuthorisedScriptProofIndex
    , plutusTxDataLaws @YieldingRedeemer
    , goldenVsString
        "AuthScriptIndex"
        "goldens/AuthScriptIndex.golden"
        (pure . fromString . show . PlutusTx.toBuiltinData $ sampleAuthScriptIndex)
    , goldenVsString
        "AuthorisedScriptPurpose"
        "goldens/AuthorisedScriptPurpose.golden"
        ( pure . fromString . show . PlutusTx.toBuiltinData $
            sampleAuthorisedScriptPurpose
        )
    , goldenVsString
        "AuthorisedScriptProofIndex"
        "goldens/AuthorisedScriptProofIndex.golden"
        ( pure . fromString . show . PlutusTx.toBuiltinData $
            sampleAuthorisedScriptProofIndex
        )
    , goldenVsString
        "YieldingRedeemer"
        "goldens/YieldingRedeemer.golden"
        (pure . fromString . show . PlutusTx.toBuiltinData $ sampleYieldingRedeemer)
    , goldenVsString
        "SdkParameters"
        "goldens/SdkParameters.golden"
        (pure . encode $ sampleYLS)
    ]
  where
    go :: QuickCheckTests -> QuickCheckTests
    go = max 10_000

-- Golden data

sampleYLS :: SdkParameters
sampleYLS =
  SdkParameters
    [9, 10, 11]
    (AuthorisedScriptsSTCS dummySymbolOne)

sampleAuthScriptIndex :: AuthorisedScriptIndex
sampleAuthScriptIndex = AuthorisedScriptIndex 0

sampleAuthorisedScriptPurpose :: AuthorisedScriptPurpose
sampleAuthorisedScriptPurpose = Minting

sampleAuthorisedScriptProofIndex :: AuthorisedScriptProofIndex
sampleAuthorisedScriptProofIndex = AuthorisedScriptProofIndex (sampleAuthorisedScriptPurpose, 0)

sampleYieldingRedeemer :: YieldingRedeemer
sampleYieldingRedeemer = YieldingRedeemer sampleAuthScriptIndex sampleAuthorisedScriptProofIndex

-- Generators and shrinkers

genSdkParams :: Gen SdkParameters
genSdkParams = do
  nonceList <- map (fromInteger . getNonNegative) <$> arbitrary
  pure $
    SdkParameters
      nonceList
      (AuthorisedScriptsSTCS dummySymbolOne)

-- | Sample symbol for tests
dummySymbolOne :: CurrencySymbol
dummySymbolOne =
  CurrencySymbol $
    getLedgerBytes "00000000000000000000000000000000000000000000000000000000"
