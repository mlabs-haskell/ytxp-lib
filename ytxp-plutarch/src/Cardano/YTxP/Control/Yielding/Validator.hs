module Cardano.YTxP.Control.Yielding.Validator (
  compileYieldingValidator,
) where

import Cardano.YTxP.Control.Yielding.Helper (yieldingHelper)
import Cardano.YTxP.SDK.ControlParameters (HexStringScript (..))
import Cardano.YTxP.SDK.SdkParameters (YieldListSTCS)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Plutarch (Config, Script, compile)
import Plutarch.Api.V2 (PScriptContext)
import Plutarch.Script (serialiseScript)

--------------------------------------------------------------------------------
-- Yielding Validator Script

compileYieldingValidator ::
  Config ->
  YieldListSTCS ->
  Either
    Text
    (HexStringScript "YieldingValidator")
compileYieldingValidator config ylstcs = do
  let
    yieldingValidator ::
      forall (s :: S).
      ( Term s (PData :--> PData :--> PScriptContext :--> POpaque)
      )
    -- Takes the @yieldingHelper@ and turn it into a 3 argument script
    yieldingValidator = plam $ \_datum redeemer ctx ->
      yieldingHelper ylstcs # redeemer # ctx

  script <- compile config yieldingValidator
  pure $ HexStringScript (serialiseScript script)
