module Cardano.YTxP.Control.Yielding.Scripts (
  compileYieldingMP,
  compileYieldingValidator,
  compileYieldingSV,
) where

import Cardano.YTxP.Control.Yielding.Helper (yieldingHelper)
import Cardano.YTxP.SDK.SdkParameters (AuthorisedScriptsSTCS)
import Data.Text (Text)
import Numeric.Natural (Natural)
import Plutarch (Config, compile)
import Plutarch.LedgerApi (PScriptContext)
import Plutarch.Script (Script)

--------------------------------------------------------------------------------
-- Yielding Validator Script

compileYieldingValidator ::
  Config ->
  AuthorisedScriptsSTCS ->
  Either
    Text
    Script
compileYieldingValidator config ylstcs = do
  let
    yieldingValidator ::
      forall (s :: S).
      ( Term s (PData :--> PData :--> PScriptContext :--> POpaque)
      )
    -- Takes the @yieldingHelper@ and turn it into a 3 argument script
    yieldingValidator = plam $ \_datum redeemer ctx ->
      yieldingHelper ylstcs # redeemer # ctx

  compile config yieldingValidator

--------------------------------------------------------------------------------
-- Yielding Minting Policy Script

compileYieldingMP ::
  Config ->
  AuthorisedScriptsSTCS ->
  Natural ->
  Either
    Text
    Script
compileYieldingMP config stcs nonce = do
  let
    yieldingMP ::
      forall (s :: S).
      ( Term s (PData :--> PScriptContext :--> POpaque)
      )
    yieldingMP = plet (pconstant $ toInteger nonce) (const $ yieldingHelper stcs)

  compile config yieldingMP

--------------------------------------------------------------------------------
-- Yielding Staking Validator

{- | Compile a yielding staking validator that has been nonced.
The nonce is required because each staking validator can only
be delegated to a single pool; the inclusion of the nonce will change the
script hash.
Since SVs and MPs share the same signature they share the same implementation,
this function is only provided for semantic clarity
-}
compileYieldingSV ::
  Config ->
  AuthorisedScriptsSTCS ->
  Natural ->
  Either
    Text
    Script
compileYieldingSV = compileYieldingMP
