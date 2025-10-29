{-# OPTIONS_GHC -Wno-deprecations #-}

module Main (main) where

import Cardano.YTxP.SDK.ControlParameters (
  ControlParameters (ControlParameters),
  HexStringScript (HexStringScript),
  YieldingScripts (
    YieldingScripts,
    yieldingMintingPolicies,
    yieldingStakingValidators,
    yieldingValidator
  ),
  hexTextToSbs,
  sbsToHexText,
 )
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
import Control.Monad (guard)
import Data.Aeson (encode)
import Data.ByteString.Short (ShortByteString)
import Data.Text (unpack)
import GHC.Exts (fromList, fromString, toList)
import PlutusLedgerApi.V3 (CurrencySymbol (CurrencySymbol), getLedgerBytes)
import PlutusTx qualified
import Test.Laws (aesonLawsWith, plutusTxDataLaws)
import Test.QuickCheck (
  Gen,
  NonNegative (getNonNegative),
  arbitrary,
  counterexample,
  shrink,
  (===),
 )
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.QuickCheck (QuickCheckTests, forAllShrinkShow, testProperty)

main :: IO ()
main =
  defaultMain . adjustOption go . testGroup "serialization" $
    [ testProperty "Hex encoding of ShortByteString roundtrips"
        . forAllShrinkShow genSBS shrinkSBS (show . toList)
        $ \sbs ->
          let converted = sbsToHexText sbs
           in counterexample ("Converted: " <> unpack converted) $
                Just sbs === (hexTextToSbs . sbsToHexText $ sbs)
    , aesonLawsWith @SdkParameters genSdkParams (const [])
    , aesonLawsWith @ControlParameters genControlParams (const [])
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

genControlParams :: Gen ControlParameters
genControlParams = do
  sdkParameters <- genSdkParams
  yieldingMP <- genSBS
  yieldingValidator <- genSBS
  yieldingSV <- genSBS
  pure $
    ControlParameters
      ( YieldingScripts
          { yieldingMintingPolicies = [HexStringScript @"YieldingMP" yieldingMP]
          , yieldingValidator = HexStringScript @"YieldingValidator" yieldingValidator
          , yieldingStakingValidators = [HexStringScript @"YieldingSV" yieldingSV]
          }
      )
      sdkParameters

genSBS :: Gen ShortByteString
genSBS = fromList <$> arbitrary

shrinkSBS :: ShortByteString -> [ShortByteString]
shrinkSBS sbs = do
  let asList = toList sbs
  shrunk <- shrink asList
  guard (not . null $ shrunk)
  pure . fromList $ shrunk

-- TODO: Make proper generator for cs

-- | Sample symbol for tests
dummySymbolOne :: CurrencySymbol
dummySymbolOne =
  CurrencySymbol $
    getLedgerBytes "00000000000000000000000000000000000000000000000000000000"
