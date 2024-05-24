module Cardano.YTxP.Control.Yielding.StakingValidator (
  compileYieldingSV,
) where

import Cardano.YTxP.Control.Yielding.Helper (yieldingHelper)
import Cardano.YTxP.SDK.ControlParameters (HexStringScript (..))
import Cardano.YTxP.SDK.SdkParameters (YieldListSTCS)
import Data.Aeson (
  FromJSON,
  ToJSON,
 )
import Data.Text (Text)
import Numeric.Natural (Natural)
import Plutarch (Config, Script, compile)
import Plutarch.Api.V2 (PScriptContext)
import Plutarch.Script (serialiseScript)

--------------------------------------------------------------------------------
-- Yielding Staking Validator

{- | Compile a yielding staking validator that has been nonced.
The nonce is required because each staking validator can only
be delegated to a single pool; the inclusion of the nonce will change the
script hash.
-}
compileYieldingSV ::
  Config ->
  YieldListSTCS ->
  Natural ->
  Either
    Text
    (HexStringScript "YieldingSV")
compileYieldingSV config ylstcs nonce = do
  let
    yieldingSV ::
      Term s (PData :--> PScriptContext :--> POpaque)
    yieldingSV =
      plet (pconstant $ toInteger nonce) (const $ yieldingHelper ylstcs)

  -- Pull the "Either" through the list
  script <- compile config yieldingSV
  pure $ HexStringScript (serialiseScript script)
