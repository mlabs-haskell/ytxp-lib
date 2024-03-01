module Cardano.YTxP.Control.Yielding.MintingPolicy (
  -- * Minting Policy
  YieldingMPScript (getYieldingMPScript),
  compileYieldingMP,

  -- * Currency Symbol
  YieldingMPCS,
  mkYieldingMPCS,
) where

import Cardano.YTxP.Control.Stubs (alwaysSucceedsTwoArgumentScript,
                                   noncedTwoArgumentScriptWrapper)
import Cardano.YTxP.Control.YieldList.MintingPolicy (YieldListSTCS)
import Data.Text (Text)
import Plutarch (Config, compile)
import Plutarch.Api.V2 (PScriptContext, scriptHash)
import Plutarch.Script (Script)
import PlutusLedgerApi.V2 (CurrencySymbol (CurrencySymbol), getScriptHash)

--------------------------------------------------------------------------------
-- Yielding Minting Policy Script

newtype YieldingMPScript = YieldingMPScript {getYieldingMPScript :: Script}

compileYieldingMP ::
  Config ->
  YieldListSTCS ->
  Either
    Text
    YieldingMPScript
compileYieldingMP config ylstcs = do
  let
    yieldingMP ::
      forall (s :: S).
      ( Term s (PData :--> PScriptContext :--> POpaque)
      )
    yieldingMP = mkYieldingMP ylstcs

  script <- compile config yieldingMP
  pure $ YieldingMPScript script

-------------------------------------------------------------------------------
-- Yielding Minting Policy Currency Symbol

-- | Opaque, semantic newtype for the YieldList state thread currency symbol
newtype YieldingMPCS = YieldingMPCS CurrencySymbol

mkYieldingMPCS :: YieldingMPScript -> YieldingMPCS
mkYieldingMPCS (YieldingMPScript script) =
  YieldingMPCS $ CurrencySymbol (getScriptHash $ scriptHash script)

--------------------------------------------------------------------------------
-- Helpers (Unexported)

mkYieldingMP ::
  forall (s :: S).
  YieldListSTCS ->
  Term s (PData :--> PScriptContext :--> POpaque)
mkYieldingMP _ylstcs =
  noncedTwoArgumentScriptWrapper @PString
    "YieldingMP"
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
