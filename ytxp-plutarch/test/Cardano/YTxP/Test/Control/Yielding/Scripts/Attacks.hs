{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.YTxP.Test.Control.Yielding.Scripts.Attacks (testAttacksR) where

import Cardano.TestUtils (
  PreProcessor,
  TxFCEKInput (TxFCEKInput),
  attackCaseBasicRegex,
  mkPreProcessor,
  txfCEKUnitCase,
 )
import Cardano.YTxP.SDK.Optics qualified as SDKOptics
import Cardano.YTxP.SDK.Redeemers (
  AuthorisedScriptPurpose (Minting, Rewarding, Spending),
 )
import Cardano.YTxP.SDK.SdkParameters (
  AuthorisedScriptsSTCS (AuthorisedScriptsSTCS),
 )
import Cardano.YTxP.Test.Control.Yielding.Scripts.NominalCases (
  mintNominalCaseBuilderR,
  rewardNominalCaseBuilderR,
  spendNominalCaseBuilderR,
 )
import Cardano.YTxP.Test.Control.Yielding.Scripts.ScriptsBuilders (
  yieldingMPScriptR,
  yieldingSVScriptR,
  yieldingVScriptR,
 )
import Cardano.YTxP.Test.Control.Yielding.Scripts.Utils (
  ScriptsTestsParams (
    authorisedScriptHash,
    authorisedScriptsSTCS
  ),
  toLedgerRedeemer,
 )
import Control.Lens (over, set, traversed, view, (&), _1, _2, _Wrapped)
import Control.Monad.Reader (Reader, asks)
import Data.Monoid (Endo (Endo, appEndo))
import Optics qualified as PlutusLedgerApiOptics
import PlutusLedgerApi.V3 (
  Credential (ScriptCredential),
  CurrencySymbol (CurrencySymbol),
  Lovelace,
  ScriptContext (scriptContextRedeemer),
  ScriptHash,
  ToData (toBuiltinData),
  TxInInfo,
  Value (Value, getValue),
  getScriptHash,
  unsafeFromBuiltinData,
 )
import PlutusLedgerApi.V3.MintValue (
  MintValue (UnsafeMintValue),
  mintValueToMap,
 )
import PlutusTx.AssocMap qualified as PTx.Map
import PlutusTx.Eq qualified
import Test.Tasty (TestTree, testGroup)
import Text.RE.TDFA.Text (re)

testAttacksR :: Reader ScriptsTestsParams TestTree
testAttacksR = do
  -- Scripts
  ympScript <- yieldingMPScriptR
  yvScript <- yieldingVScriptR
  ysvScript <- yieldingSVScriptR

  -- Redeemers and contexts
  mintNominalContext <- mintNominalCaseBuilderR
  spendNominalContext <- spendNominalCaseBuilderR
  rewardNominalContext <- rewardNominalCaseBuilderR

  -- Attacks
  ppNoRefInput <- mkAttack attackRefInputNotPresent
  ppRefInputNoAuth <- mkAttack attackRefInputNotAuthorised
  ppAuthorisedScriptIndexInvalid <- mkAttack attackAuthorisedScriptIndexInvalid

  ppAttackAuthorisedMPProofIndexInvalid <-
    mkAttack attackAuthorisedProofIndexInvalidIndex
  ppAttackAuthorisedMPProofMismatch <-
    mkAttack attackAuthorisedMPProofIndexMismatch

  ppAttackAuthorisedVProofIndexInvalid <-
    mkAttack attackAuthorisedProofIndexInvalidIndex
  ppAttackAuthorisedVProofMismatch <- mkAttack attackAuthorisedVProofIndexMismatch

  ppAttackAuthorisedSVProofIndexInvalid <-
    mkAttack attackAuthorisedProofIndexInvalidIndex
  ppAttackAuthorisedSVProofMismatch <-
    mkAttack attackAuthorisedSVProofIndexMismatch

  pure $
    testGroup
      "Attacks"
      [ txfCEKUnitCase $
          attackCaseBasicRegex
            "ref input not present"
            [re|^(.*)$|]
            mintNominalContext
            ympScript
            ppNoRefInput
      , txfCEKUnitCase $
          attackCaseBasicRegex
            "ref input present but not authorised"
            [re|Reference input does not contain AuthorisedScriptsSTCS|]
            mintNominalContext
            ympScript
            ppRefInputNoAuth
      , txfCEKUnitCase $
          attackCaseBasicRegex
            "attackAuthorisedScriptIndex does not points to valid reference input"
            [re|^(.*)$|]
            mintNominalContext
            ympScript
            ppAuthorisedScriptIndexInvalid
      , txfCEKUnitCase $
          attackCaseBasicRegex
            "(MP) AuthorisedScriptProofIndex does not index to valid proof"
            [re|^(.*)$|]
            mintNominalContext
            ympScript
            ppAttackAuthorisedMPProofIndexInvalid
      , txfCEKUnitCase $
          attackCaseBasicRegex
            "(MP) AuthorisedScriptProofIndex point to a wrong script"
            [re|Minting policy does not match expected authorised minting policy|]
            mintNominalContext
            ympScript
            ppAttackAuthorisedMPProofMismatch
      , txfCEKUnitCase $
          attackCaseBasicRegex
            "(V) AuthorisedScriptProofIndex does not index to valid proof"
            [re|^(.*)$|]
            spendNominalContext
            yvScript
            ppAttackAuthorisedVProofIndexInvalid
      , txfCEKUnitCase $
          attackCaseBasicRegex
            "(V) AuthorisedScriptProofIndex point to a wrong script"
            [re|Input does not match expected authorised validator|]
            spendNominalContext
            yvScript
            ppAttackAuthorisedVProofMismatch
      , txfCEKUnitCase $
          attackCaseBasicRegex
            "(SV) AuthorisedScriptProofIndex does not index to valid proof"
            [re|^(.*)$|]
            rewardNominalContext
            ysvScript
            ppAttackAuthorisedSVProofIndexInvalid
      , txfCEKUnitCase $
          attackCaseBasicRegex
            "(SV) AuthorisedScriptProofIndex point to a wrong script"
            [re|Withdrawal does not match expected authorised staking validator|]
            rewardNominalContext
            ysvScript
            ppAttackAuthorisedSVProofMismatch
      ]

-----------------------------------------------------------------------
-- Attacks helpers

mkAttack ::
  Reader ScriptsTestsParams (Endo ScriptContext) ->
  Reader ScriptsTestsParams (PreProcessor () TxFCEKInput)
mkAttack attack = mkPreProcessor . attackHelper <$> attack

attackHelper ::
  Endo ScriptContext ->
  TxFCEKInput ->
  Either () TxFCEKInput
attackHelper attack (TxFCEKInput sc script) =
  let
    sc' = appEndo attack sc
   in
    Right (TxFCEKInput sc' script)

replaceIfPresent ::
  (PlutusTx.Eq.Eq k) => k -> k -> PTx.Map.Map k v -> PTx.Map.Map k v
replaceIfPresent oldKey newKey m =
  case PTx.Map.lookup oldKey m of
    Just v ->
      PTx.Map.insert newKey v $
        PTx.Map.delete oldKey m
    Nothing -> m

{- | Replace all tokens with the given currency symbol from a value with a different currency symbol
Does nothing if the CS to replace is not found
-}
replaceTokenByCS :: CurrencySymbol -> CurrencySymbol -> MintValue -> MintValue
replaceTokenByCS oldCS newCS = UnsafeMintValue . replaceIfPresent oldCS newCS . mintValueToMap

-- | Remove all tokens with the given currency symbol from a value
removeTokenByCS :: CurrencySymbol -> Value -> Value
removeTokenByCS cs v =
  Value $ v & getValue & PTx.Map.delete cs

-- | Replace an @ScriptHash@ credentials with a new one if a match is found
updateScriptCredential :: ScriptHash -> ScriptHash -> Credential -> Credential
updateScriptCredential oldScriptHash newScriptHash cred
  | cred == ScriptCredential oldScriptHash = ScriptCredential newScriptHash
  | otherwise = cred

-- | Replace an @ScriptHash@ credentials for input with a provided one (if present)
replaceInput :: ScriptHash -> ScriptHash -> TxInInfo -> TxInInfo
replaceInput oldScriptHash newScriptHash =
  over
    ( PlutusLedgerApiOptics.txOut
        . PlutusLedgerApiOptics.address
        . PlutusLedgerApiOptics.credential
    )
    (updateScriptCredential oldScriptHash newScriptHash)

-- | Replace an @ScriptHash@ for withdrawal with a provided one (if present)
replaceWdrl ::
  ScriptHash ->
  ScriptHash ->
  PTx.Map.Map Credential Lovelace ->
  PTx.Map.Map Credential Lovelace
replaceWdrl oldScript newScript =
  let
    oldCredential = ScriptCredential oldScript
    newCredential = ScriptCredential newScript
   in
    replaceIfPresent oldCredential newCredential

-----------------------------------------------------------------------
-- Attacks

attackRefInputNotPresent ::
  Reader ScriptsTestsParams (Endo ScriptContext)
attackRefInputNotPresent =
  pure
    . Endo
    $ set
      (PlutusLedgerApiOptics.txInfo . PlutusLedgerApiOptics.referenceInputs)
      []

attackRefInputNotAuthorised ::
  Reader ScriptsTestsParams (Endo ScriptContext)
attackRefInputNotAuthorised = do
  AuthorisedScriptsSTCS authorisedScriptsSTCS' <- asks authorisedScriptsSTCS
  pure $
    Endo $
      over
        ( PlutusLedgerApiOptics.txInfo
            . PlutusLedgerApiOptics.referenceInputs
            . traversed
            . PlutusLedgerApiOptics.txOut
            . PlutusLedgerApiOptics.value
        )
        (removeTokenByCS authorisedScriptsSTCS')

attackAuthorisedScriptIndexInvalid ::
  Reader ScriptsTestsParams (Endo ScriptContext)
attackAuthorisedScriptIndexInvalid =
  pure $
    Endo $
      \context ->
        let
          redeemer = unsafeFromBuiltinData (toBuiltinData $ scriptContextRedeemer context)
          tooLargeIndex =
            fromIntegral $ length $ view PlutusLedgerApiOptics.referenceInputs context
          attackRedeemer =
            set
              (SDKOptics.authorisedScriptIndex . _Wrapped)
              tooLargeIndex
              redeemer
          context' = context {scriptContextRedeemer = toLedgerRedeemer attackRedeemer}
         in
          context'

getTooLargeIndexForProof ::
  AuthorisedScriptPurpose ->
  ScriptContext ->
  Integer
getTooLargeIndexForProof purpose =
  let
    accessor = case purpose of
      Minting -> length . PTx.Map.toList . mintValueToMap . view PlutusLedgerApiOptics.mint
      Spending -> length . view PlutusLedgerApiOptics.inputs
      Rewarding -> length . PTx.Map.toList . view PlutusLedgerApiOptics.wdrl
   in
    fromIntegral . accessor

attackAuthorisedProofIndexInvalidIndex ::
  Reader ScriptsTestsParams (Endo ScriptContext)
attackAuthorisedProofIndexInvalidIndex =
  pure $
    Endo $
      \scriptContext ->
        let
          redeemer = unsafeFromBuiltinData (toBuiltinData $ scriptContextRedeemer scriptContext)
          tooLargeIndex =
            getTooLargeIndexForProof
              (view (SDKOptics.authorisedScriptProofIndex . _Wrapped . _1) redeemer)
              scriptContext
          attackRedeemer =
            set
              (SDKOptics.authorisedScriptProofIndex . _Wrapped . _2)
              tooLargeIndex
              redeemer
          scriptContext' = scriptContext {scriptContextRedeemer = toLedgerRedeemer attackRedeemer}
         in
          scriptContext'

attackAuthorisedMPProofIndexMismatch ::
  Reader ScriptsTestsParams (Endo ScriptContext)
attackAuthorisedMPProofIndexMismatch = do
  authorisedMintingPolicy <-
    asks $ CurrencySymbol . getScriptHash . authorisedScriptHash
  let
    -- NOTE: this is arbitrary but different from the one in the test params
    differentMintingPolicy :: CurrencySymbol
    differentMintingPolicy = CurrencySymbol "42424242424242424242424242424242424242424242424242424242"
  pure $
    Endo $
      over
        (PlutusLedgerApiOptics.txInfo . PlutusLedgerApiOptics.mint)
        (replaceTokenByCS authorisedMintingPolicy differentMintingPolicy)

attackAuthorisedVProofIndexMismatch ::
  Reader ScriptsTestsParams (Endo ScriptContext)
attackAuthorisedVProofIndexMismatch = do
  authorisedValidator <- asks authorisedScriptHash
  let
    -- NOTE: this is arbitrary but different from the one in the test params
    differentValidator :: ScriptHash
    differentValidator = "42424242424242424242424242424242424242424242424242424242"
  pure $
    Endo $
      over
        (PlutusLedgerApiOptics.txInfo . PlutusLedgerApiOptics.inputs . traversed)
        (replaceInput authorisedValidator differentValidator)

attackAuthorisedSVProofIndexMismatch ::
  Reader ScriptsTestsParams (Endo ScriptContext)
attackAuthorisedSVProofIndexMismatch = do
  authorisedValidator <- asks authorisedScriptHash
  let
    -- NOTE: this is arbitrary but different from the one in the test params
    differentValidator :: ScriptHash
    differentValidator = "42424242424242424242424242424242424242424242424242424242"
  pure $
    Endo $
      over
        (PlutusLedgerApiOptics.txInfo . PlutusLedgerApiOptics.wdrl)
        (replaceWdrl authorisedValidator differentValidator)
