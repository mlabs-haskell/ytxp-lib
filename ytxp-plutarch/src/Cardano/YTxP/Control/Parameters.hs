{- | Module: Cardano.YTxP.Control.Parameters
Description: Data required to work with the compiled control scripts
-}
module Cardano.YTxP.Control.Parameters (
  -- * Types
  YieldingScripts (
    yieldingMintingPolicies,
    yieldingValidator,
    yieldingStakingValidators
  ),

  -- * Parameter Generation (script compilation)
  mkControlParameters,
) where

import Cardano.YTxP.Control.Yielding.MintingPolicy (
  compileYieldingMP,
 )
import Cardano.YTxP.Control.Yielding.StakingValidator (
  compileYieldingSV,
 )
import Cardano.YTxP.Control.Yielding.Validator (
  compileYieldingValidator,
 )
import Cardano.YTxP.SDK.ControlParameters (ControlParameters (ControlParameters, sdkParameters, yieldingScripts), YieldingScripts (YieldingScripts, yieldingMintingPolicies, yieldingStakingValidators, yieldingValidator))
import Cardano.YTxP.SDK.SdkParameters (Config (tracing), SdkParameters (SdkParameters, authorisedScriptsSTCS, compilationConfig, mintingPoliciesNonceList, stakingValidatorsNonceList), TracingMode (DetTracing, DoTracing, DoTracingAndBinds, NoTracing))
import Data.Text (Text)
import Plutarch qualified

{- | Compile all scripts, threading through the appropriate parameters and
script hashes
-}
mkControlParameters ::
  SdkParameters ->
  Either
    Text
    ControlParameters
mkControlParameters
  cpi@SdkParameters
    { stakingValidatorsNonceList
    , mintingPoliciesNonceList
    , authorisedScriptsSTCS
    , compilationConfig
    } =
    do
      let pcompilationConfig = toPlutarchConfig compilationConfig
      ------------------------------------------------------------
      -- Now compile the yielding scripts
      yieldingVal <- compileYieldingValidator pcompilationConfig authorisedScriptsSTCS

      -- Compile the staking validators, pulling any @Left@s (containing compilation
      -- error messages) through the list
      yieldingSVs <-
        mapM
          (compileYieldingSV pcompilationConfig authorisedScriptsSTCS)
          stakingValidatorsNonceList

      yieldingMPs <-
        mapM
          (compileYieldingMP pcompilationConfig authorisedScriptsSTCS)
          mintingPoliciesNonceList

      pure $
        ControlParameters
          { yieldingScripts =
              YieldingScripts
                yieldingMPs
                yieldingVal
                yieldingSVs
          , sdkParameters = cpi
          }

toPlutarchConfig :: Config -> Plutarch.Config
toPlutarchConfig conf = case tracing conf of
  DoTracing -> Plutarch.Config Plutarch.DoTracing
  NoTracing -> Plutarch.Config Plutarch.NoTracing
  DetTracing -> Plutarch.Config Plutarch.DetTracing
  DoTracingAndBinds -> Plutarch.Config Plutarch.DoTracingAndBinds
