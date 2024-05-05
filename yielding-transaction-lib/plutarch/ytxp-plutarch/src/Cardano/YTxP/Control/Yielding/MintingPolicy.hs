module Cardano.YTxP.Control.Yielding.MintingPolicy (
  -- * Minting Policy
  YieldingMPScript (getYieldingMPScript),
  compileYieldingMP,

  -- * Currency Symbol
  YieldingMPCS,
  mkYieldingMPCS,
) where

import Cardano.YTxP.Control.YieldList.MintingPolicy (YieldListSTCS)
import Cardano.YTxP.Control.Yielding.Helper (yieldingHelper)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Plutarch (Config, compile)
import Plutarch.Api.V2 (PScriptContext, scriptHash)
import Plutarch.Script (Script)
import PlutusLedgerApi.V2 (CurrencySymbol (CurrencySymbol), getScriptHash)

--------------------------------------------------------------------------------
-- Yielding Minting Policy Script

-- | @since 0.1.0
newtype YieldingMPScript = YieldingMPScript
  { getYieldingMPScript :: Script
  -- ^ @since 0.1.0
  }
  deriving
    ( -- | @since 0.1.0
      ToJSON
    , -- | @since 0.1.0
      FromJSON
    )
    via (HexStringScript "YieldingMPScript")

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
    yieldingMP = yieldingHelper ylstcs

  script <- compile config yieldingMP
  pure $ YieldingMPScript script

-------------------------------------------------------------------------------
-- Yielding Minting Policy Currency Symbol

-- | Opaque, semantic newtype for the YieldList state thread currency symbol
newtype YieldingMPCS = YieldingMPCS CurrencySymbol

mkYieldingMPCS :: YieldingMPScript -> YieldingMPCS
mkYieldingMPCS (YieldingMPScript script) =
  YieldingMPCS $ CurrencySymbol (getScriptHash $ scriptHash script)
