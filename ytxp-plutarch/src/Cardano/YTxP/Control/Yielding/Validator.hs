module Cardano.YTxP.Control.Yielding.Validator (
  -- * Validator
  YieldingValidatorScript (getYieldingValidatorScript),
  compileYieldingValidator,
) where

import Cardano.YTxP.Control.Yielding.Helper (yieldingHelper)
import Cardano.YTxP.SDK.SdkParameters (YieldListSTCS)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Plutarch (Config, compile)
import Plutarch.Api.V2 (PScriptContext)

--------------------------------------------------------------------------------
-- Yielding Validator Script

newtype YieldingValidatorScript = YieldingValidatorScript
  { getYieldingValidatorScript :: Text
  }
  deriving newtype
    ( ToJSON
    , FromJSON
    , Eq
    , Show
    )

compileYieldingValidator ::
  Config ->
  YieldListSTCS ->
  (Either Text)
    YieldingValidatorScript
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
  pure $ YieldingValidatorScript (serialiseScript script)
