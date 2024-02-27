module Cardano.YTxP.Control.YieldList.MintingPolicy (
  -- * Script
  YieldListSTMPScript,
  compileYieldListSTMP,

  -- * Currency Symbol
  YieldListSTCS,
  mkYieldListSTCS,
) where

import Cardano.YTxP.Control.Stubs (
  alwaysSucceedsTwoArgumentScript,
  noncedTwoArgumentScriptWrapper,
 )
import Cardano.YTxP.Control.Vendored (applyScript)
import Data.Text (Text)
import Numeric.Natural (Natural)
import Plutarch (Config, compile)
import Plutarch.Api.V2 (PScriptContext, scriptHash)
import Plutarch.Script (Script)
import PlutusLedgerApi.V2 (CurrencySymbol (CurrencySymbol), getScriptHash)

--------------------------------------------------------------------------------
-- YieldListSTMPScript

newtype YieldListSTMPScript = YieldListSTMPScript Script

compileYieldListSTMP ::
  forall (nonceType :: Type).
  -- | Plutarch compilation configuration
  Config ->
  Natural ->
  Script ->
  Either Text YieldListSTMPScript
compileYieldListSTMP config maxYieldListSize scriptToWrap = do
  let
    yieldListSTMPWrapper ::
      forall (s :: S).
      ( Term
          s
          ( (PData :--> PScriptContext :--> POpaque)
              :--> PData
              :--> PScriptContext
              :--> POpaque
          )
      )
    yieldListSTMPWrapper = mkYieldListSTMPWrapper maxYieldListSize

  wrapper <- compile config yieldListSTMPWrapper

  pure $ YieldListSTMPScript $ applyScript wrapper scriptToWrap

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
    plam $ \_scriptToWrap ->
      noncedTwoArgumentScriptWrapper @PString
        "YieldListSTMP"
        alwaysSucceedsTwoArgumentScript

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
