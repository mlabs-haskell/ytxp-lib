module Cardano.YTxP.Control.YieldList.MintingPolicy (
  -- * Script
  YieldListSTMPScript,
  compileYieldListSTMP,

  -- * Currency Symbol
  YieldListSTCS,
  mkYieldListSTCS,
) where

import Cardano.YTxP.Control.YieldList (
  PYieldListMPWrapperRedeemer (PBurn, PMint),
 )
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
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

{- |
  The `mkYieldListSTMPWrapper` is a YieldListST minting policy unapplied to its arguments,
  the script wraps another script, while performing additional checks of its own, which are outlined below.

  There are two potential validation branches: `Mint` and `Burn`.

  When the supplied redeemer is `Mint` the policy checks:
    - Exactly one token with an empty token name and the YieldListSTCS
      (as fetched from the `ScriptPurpose`) is minted
    - Only a single wallet input UTxO is present and this input does not contain a YieldListSTT
    - The minted token is sent to a UTxO at a script address
    - The UTxO receiving the minted token carries a valid `YieldList` in it's datum.
      In particular the following is checked:
        * The type fully decodes (using `PTryFrom`)
        * Hashes must be well-formed (of the correct length)
        * The yield list must be no larger than the max list size (provided as an argument)
        * No tokens except Ada and the YieldListSTMP are present at the recipient UTxO
        * Only a single wallet output is present
        * That wallet output does not contain a YieldListSTT

  When the supplied redeemer is `Burn` the policy checks:
    - Exactly one token is burned
    - Exactly one UTxO carrying a token with the `YieldListSTCS` appears at the input
    - Exactly zero UTxOs carrying tokens with the `YieldListSTCS` appear at the output

  Note that the YLSTMP does not provide any additional security guarantees by itself.
  These must be library-user-defined via wrapping scripts.
  Some examples include:
    - initialSpend (one-shot)
    - multisig
    - admin sig
    - governance wrappers
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
        pfromData . fst <$> ptryFromC @(PAsData PYieldListMPWrapperRedeemer) redeemer

      pure $ pmatch yieldPolicyRedeemer $ \case
        PMint -> unTermCont $ do
          pguardC "Only one token with yield list symbol and empty token name is minted" $
            phasTokenOfCurrencySymbolTokenNameAndAmount
              # mint
              # yieldListSymbol
              # padaToken
              # 1

          pguardC
            "Only one wallet input, that does not contain yield list symbol, allowed"
            $ phasOnlyOnePubKeyInputAndNoTokenWithSymbol
              # inputs
              # yieldListSymbol

          pguardC
            "Only one wallet output, that does not contain yield list symbol, allowed"
            $ phasOnlyOnePubKeyOutputAndNoTokenWithSymbol
              # outputs
              # yieldListSymbol

          -- TODO: Finish the valid datum part
          pguardC
            "Contains valid script output"
            $ phasOnlyOneValidScriptOutputWithToken
              # outputs
              # yieldListSymbol
              # padaToken

          pure $ popaque $ pconstant ()
        PBurn -> unTermCont $ do
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
