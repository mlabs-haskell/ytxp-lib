module Cardano.YTxP.Control.Yielding.MintingPolicy (
  compileYieldingMP,
) where

import Cardano.YTxP.Control.Yielding.Helper (yieldingHelper)
import Cardano.YTxP.SDK.ControlParameters (HexStringScript (HexStringScript))
import Cardano.YTxP.SDK.SdkParameters (AuthorisedScriptsSTCS)
import Data.Text (Text)
import Numeric.Natural (Natural)
import Plutarch (Config, compile)
import Plutarch.Api.V2 (PScriptContext)
import Plutarch.Script (serialiseScript)

--------------------------------------------------------------------------------
-- Yielding Minting Policy Script

compileYieldingMP ::
  Config ->
  AuthorisedScriptsSTCS ->
  Natural ->
  Either
    Text
    (HexStringScript "YieldingMP")
compileYieldingMP config ylstcs nonce = do
  let
    yieldingMP ::
      forall (s :: S).
      ( Term s (PData :--> PScriptContext :--> POpaque)
      )
    yieldingMP = plet (pconstant $ toInteger nonce) (const $ yieldingHelper ylstcs)
  script <- compile config yieldingMP
  pure $ HexStringScript (serialiseScript script)
