{- | Module: Cardano.YTxP.Control.Parameters
Description: Data required to work with the compiled control scripts
-}
module Cardano.YTxP.Control.Parameters (
  -- * Types
  YieldListScripts (
    yieldListValidator,
    yieldListMintingPolicy
  ),
  YieldingScripts (
    yieldingMintingPolicy,
    yieldingValidator,
    yieldingStakingValidators
  ),
  ControlParameters (yieldListScripts, yieldingScripts, controlParametersInitial),

  -- * Parameter Generation (script compilation)
  mkControlParameters,
) where

import Data.Aeson (ToJSON (toJSON), FromJSON (parseJSON))
import Cardano.YTxP.Control.ParametersInitial (
  ControlParametersInitial (
    ControlParametersInitial,
    compilationConfig,
    maxYieldListSize,
    nonceList,
    scriptToWrapYieldListMP,
    scriptToWrapYieldListValidator
  ),
 )
import Cardano.YTxP.Control.YieldList.MintingPolicy (
  YieldListSTMPScript,
  compileYieldListSTMP,
  mkYieldListSTCS,
 )
import Cardano.YTxP.Control.YieldList.Validator (
  YieldListValidatorScript,
  compileYieldListValidator,
 )
import Cardano.YTxP.Control.Yielding.MintingPolicy (
  YieldingMPScript,
  compileYieldingMP,
 )
import Cardano.YTxP.Control.Yielding.StakingValidator (
  YieldingStakingValidatorScript,
  compileYieldingStakingValidator,
 )
import Cardano.YTxP.Control.Yielding.Validator (
  YieldingValidatorScript,
  compileYieldingValidator,
 )
import Data.Text (Text)

-- | Scripts that govern which transaction families can be "yielded to"
data YieldListScripts = YieldListScripts
  { yieldListValidator :: YieldListValidatorScript
  -- ^ The validator where we look for UTxO(s) that carry a yieldListSTT in
  -- their values and a YieldList in their datum
  , yieldListMintingPolicy :: YieldListSTMPScript
  -- ^ The minting policy for the yieldListSTT, which authenticates a
  -- UTxO as carrying a YieldList. Note that these STTs will _usually_ be
  -- at the yieldListValidator, but these is nothing intrinsically requiring this;
  -- any UTxO carrying the STT will be looked at for a YieldList.
  }

{- | Scripts that yield to transaction families described by the datums guarded
by the YieldListScripts.
-}
data YieldingScripts (nonceType :: Type) = YieldingScripts
  { yieldingMintingPolicy :: YieldingMPScript
  , yieldingValidator :: YieldingValidatorScript
  , yieldingStakingValidators :: [YieldingStakingValidatorScript nonceType]
  -- ^ We have multiple of these, because each can only be delegated to a single
  -- pool.
  }

{- | Contains the compiled scripts along with the parameters
they were compiled against. This is useful for _library consumers_
and should contain all of the information needed to work with the
library.
-}
data ControlParameters (nonceType :: Type) = ControlParameters
  { yieldListScripts :: YieldListScripts
  , yieldingScripts :: YieldingScripts nonceType
  , controlParametersInitial :: ControlParametersInitial nonceType
  }


instance ToJSON nonceType => ToJSON (ControlParameters nonceType) where
  toJSON = error "unimplemented"

instance FromJSON nonceType => FromJSON (ControlParameters nonceType) where
  parseJSON = error "unimplemented"


{- | Compile all scripts, threading through the appropriate parameters and
script hashes
-}
mkControlParameters ::
  forall (nonceType :: Type).
  ControlParametersInitial nonceType ->
  Either
    Text
    (ControlParameters nonceType)
mkControlParameters
  cpi@ControlParametersInitial
    { maxYieldListSize
    , nonceList
    , scriptToWrapYieldListMP
    , scriptToWrapYieldListValidator
    , compilationConfig
    } =
    do
      ------------------------------------------------------------
      -- Compile the yield list scripts first. We need their
      -- hashes in order to parameterize the yielding scripts.
      yieldListSTMP <-
        compileYieldListSTMP compilationConfig maxYieldListSize scriptToWrapYieldListMP
      yieldListVal <-
        compileYieldListValidator compilationConfig scriptToWrapYieldListValidator

      let
        ylstcs = mkYieldListSTCS yieldListSTMP

      ------------------------------------------------------------
      -- Now compile the yielding scripts
      yieldingMP <- compileYieldingMP compilationConfig ylstcs
      yieldingVal <- compileYieldingValidator compilationConfig ylstcs

      -- Compile the staking validators, pulling any @Left@s (containing compilation
      -- error messages) through the list
      yieldingStakingVals <-
        mapM
          (compileYieldingStakingValidator compilationConfig ylstcs)
          nonceList

      pure $
        ControlParameters
          { yieldListScripts =
              YieldListScripts
                yieldListVal
                yieldListSTMP
          , yieldingScripts =
              YieldingScripts
                yieldingMP
                yieldingVal
                yieldingStakingVals
          , controlParametersInitial = cpi
          }
