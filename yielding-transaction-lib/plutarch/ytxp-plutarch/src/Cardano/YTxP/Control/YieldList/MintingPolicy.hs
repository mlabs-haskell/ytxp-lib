module Cardano.YTxP.Control.YieldList.MintingPolicy (
  -- * Script
  YieldListSTMPScript,
  compileYieldListSTMP,

  -- * Currency Symbol
  YieldListSTCS,
  mkYieldListSTCS,
) where

-- import Cardano.YTxP.Control.Stubs (alwaysSucceedsTwoArgumentScript,
--                                    noncedTwoArgumentScriptWrapper)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import LambdaBuffers.Runtime.Plutarch.LamVal (pfromPlutusDataPTryFrom)
import LambdaBuffers.YieldList.Plutarch (
  YieldListPolicyRedeemer (
    YieldListPolicyRedeemer'Burn,
    YieldListPolicyRedeemer'Mint
  ),
 )
import Numeric.Natural (Natural)
import Plutarch (Config, compile)
import Plutarch.Api.V1.Value (
  padaToken,
 )
import Plutarch.Api.V2 (PScriptContext, PScriptPurpose (PMinting), scriptHash)
import Plutarch.Script (Script)
import PlutusLedgerApi.V2 (CurrencySymbol (CurrencySymbol), getScriptHash)
import Prettyprinter (Pretty)
import Utils (
  phasOnlyOnePubKeyInputAndNoTokenWithSymbol,
  phasOnlyOnePubKeyOutputAndNoTokenWithSymbol,
  phasOnlyOneValidScriptOutputWithToken,
  phasTokenOfCurrencySymbolTokenNameAndAmount,
  poutputsDoNotContainTokenWithSymbol,
 )

--------------------------------------------------------------------------------
-- YieldListSTMPScript

-- | @since 0.1.0
newtype YieldListSTMPScript = YieldListSTMPScript Script
  deriving
    ( -- | @since 0.1.0
      ToJSON
    , -- | @since 0.1.0
      FromJSON
    )
    via (HexStringScript "YieldListSTMPScript")
  deriving
    ( --- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Pretty
    )
    via SerialForm

compileYieldListSTMP ::
  -- | Plutarch compilation configuration
  Config ->
  Natural ->
  (forall (s :: S). Term s (PData :--> PScriptContext :--> POpaque)) ->
  Either Text YieldListSTMPScript
compileYieldListSTMP config maxYieldListSize scriptToWrap = do
  let
    yieldListSTMPWrapper ::
      ( Term
          s
          ( (PData :--> PScriptContext :--> POpaque)
              :--> PData
              :--> PScriptContext
              :--> POpaque
          )
      )
    yieldListSTMPWrapper = mkYieldListSTMPWrapper maxYieldListSize

  script <- compile config (yieldListSTMPWrapper # scriptToWrap)

  pure $ YieldListSTMPScript script

--------------------------------------------------------------------------------
-- YieldListCTCS

-- | Opaque, semantic newtype for the YieldList state thread currency symbol
newtype YieldListSTCS = YieldListSTCS CurrencySymbol

mkYieldListSTCS :: YieldListSTMPScript -> YieldListSTCS
mkYieldListSTCS (YieldListSTMPScript script) =
  YieldListSTCS $ CurrencySymbol (getScriptHash $ scriptHash script)

--------------------------------------------------------------------------------
-- Helpers (Unexported)

{- | A YieldListSTMP unapplied to its arguments: this script wraps another
script
-}
mkYieldListSTMPWrapper ::
  forall (s :: S).
  Natural ->
  Term
    s
    ( (PData :--> PScriptContext :--> POpaque)
        :--> PData
        :--> PScriptContext
        :--> POpaque
    )
mkYieldListSTMPWrapper
  _maxYieldListSize =
    plam $ \_scriptToWrap redeemer context' -> unTermCont $ do
      let txInfo = pfromData $ pfield @"txInfo" # context'
          purpose = pfromData $ pfield @"purpose" # context'
          mint = pfromData $ pfield @"mint" # txInfo
          inputs = pfromData $ pfield @"inputs" # txInfo
          outputs = pfromData $ pfield @"outputs" # txInfo

      PMinting ((pfield @"_0" #) -> yieldListSymbol) <- pmatchC purpose

      yieldPolicyRedeemer <-
        pletC $ pfromData $ pfromPlutusDataPTryFrom @YieldListPolicyRedeemer # redeemer

      pure $ pmatch yieldPolicyRedeemer $ \case
        YieldListPolicyRedeemer'Mint -> unTermCont $ do
          pguardC "Only one token with yield list symbol and empty token name is minted" $
            phasTokenOfCurrencySymbolTokenNameAndAmount
              # mint
              # yieldListSymbol
              # padaToken
              # 1

          -- TODO: Check just for yieldListSymbol not token too, update helper for that
          pguardC
            "Only one wallet input, that does not contain yield list symbol, allowed"
            $ phasOnlyOnePubKeyInputAndNoTokenWithSymbol
              # inputs
              # yieldListSymbol
              # padaToken

          -- TODO: Same as above
          pguardC
            "Only one wallet output, that does not contain yield list symbol, allowed"
            $ phasOnlyOnePubKeyOutputAndNoTokenWithSymbol
              # outputs
              # yieldListSymbol
              # padaToken

          pguardC
            "Contains valid script output"
            $ phasOnlyOneValidScriptOutputWithToken
              # outputs
              # yieldListSymbol
              # padaToken

          pure $ popaque $ pconstant ()
        YieldListPolicyRedeemer'Burn -> unTermCont $ do
          pguardC "Only one token with yield list symbol and empty token name is burned" $
            phasTokenOfCurrencySymbolTokenNameAndAmount
              # mint
              # yieldListSymbol
              # padaToken
              # (-1)

          pguardC "Outputs must not contain token with YieldListSTCS" $
            poutputsDoNotContainTokenWithSymbol
              # outputs
              # yieldListSymbol

          pure $ popaque $ pconstant ()

{-
use maxYieldListSize and yieldListMPWrapper to build a minting policy that
takes the wrapper and add in logic according to the semantics in
the spec; copied below

3.  Semantics

    The minting policy only succeeds when exactly one of the following conditions
    hold:

    -   Exactly one token with an empty token name and the YieldListSTCS (as fetched from the \`ScriptPurpose\`)
        is minted, and
        -   Only a single wallet input UTxO is present
            - This input does not contain a YieldListSTT
        -   The minted token is sent to a UTxO at a script address, and
        -   The UTxO receiving the minted token carries a valid `YieldList` in it's datum.
            &ldquo;Validity&rdquo; here means that the type fully decodes (i.e., use \`TryFrom\` in plutarch
            or `TryFromData` in plutus-tx). In particular, AT LEAST the following conditions must be checked:
            -   Hashes must be well-formed (of the correct length)
            -   The yield list must be no larger than the max list size (to defend against large datum attacks)
            -   Any additional checks determined by further security and safety analysis
        - No tokens besides the ada and the YieldListSTMP are present at the recipient UTxO
        - Only a single wallet output is present, and the wallet output does not contain a YieldListSTT
    -   Exactly one token is burned, and
        -   Exactly one UTxO carrying a token with the `YieldListSTCS` appears at the input
        -   Exactly zero UTxOs carrying tokens with the `YieldListSTCS` appear at the output

    Note that the YLSTMP does not provide any additional security guarantees by itself. These
    <span class="underline">must</span> be library-user-defined via wrapping scripts. Examples include initialSpend (one-shot),
    multisig, admin sig, or governance wrappers.

-}
