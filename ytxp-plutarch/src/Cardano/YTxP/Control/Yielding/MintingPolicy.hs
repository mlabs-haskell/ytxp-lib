module Cardano.YTxP.Control.Yielding.MintingPolicy (
  -- * Minting Policy
  YieldingMPScript (getYieldingMPScript),
  compileYieldingMP,
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
import Plutarch.Api.V2 (PScriptContext, scriptHash)
import Plutarch.Script (Script)
import PlutusLedgerApi.V2 (CurrencySymbol (CurrencySymbol), getScriptHash)

--------------------------------------------------------------------------------
-- Yielding Minting Policy Script

newtype YieldingMPScript = YieldingMPScript
  { getYieldingMPScript :: Text
  }
  deriving newtype
    ( ToJSON
    , FromJSON
    , Eq
    , Show
    )

compileYieldingMP ::
  Config ->
  YieldListSTCS ->
  Natural ->
  Either
    Text
    YieldingMPScript
compileYieldingMP config ylstcs nonce = do
  let
    yieldingMP ::
      forall (s :: S).
      ( Term s (PData :--> PScriptContext :--> POpaque)
      )
    yieldingMP = plet (pconstant $ toInteger nonce) (const $ yieldingHelper ylstcs)
  script <- compile config yieldingMP
  pure $ YieldingMPScript (serialiseScript script)
