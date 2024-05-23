module Cardano.YTxP.Control.Yielding.StakingValidator (
  -- * Staking Validator
  YieldingSVScript (getYieldingSVScript),
  compileYieldingSV,
) where

import Cardano.YTxP.Control.Yielding.Helper (yieldingHelper)
import Cardano.YTxP.SDK.SdkParameters (YieldListSTCS)
import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toEncoding, toJSON),
  object,
  pairs,
  withObject,
  (.:),
  (.=),
 )
import Data.Text (Text)
import Numeric.Natural (Natural)
import Plutarch (Config, compile)
import Plutarch.Api.V2 (PScriptContext)
import Plutarch.Script (Script)

--------------------------------------------------------------------------------
-- Yielding Staking Validator

-- | A yielding staking validator
newtype YieldingSVScript = YieldingSVScript
  { getYieldingSVScript :: Text
  }
  deriving newtype
    ( ToJSON
    , FromJSON
    , Eq
    , Show
    )

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
    YieldingSVScript
compileYieldingSV config ylstcs nonce = do
  let
    yieldingSV ::
      Term s (PData :--> PScriptContext :--> POpaque)
    yieldingSV =
      plet (pconstant $ toInteger nonce) (const $ yieldingHelper ylstcs)

  -- Pull the "Either" through the list
  script <- compile config yieldingSV

  pure $
    YieldingSVScript (serialiseScript script)
