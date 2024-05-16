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
    yieldingMintingPolicies,
    yieldingValidator,
    yieldingStakingValidators
  ),
  ControlParameters (yieldListScripts, yieldingScripts, controlParametersInitial),

  -- * Parameter Generation (script compilation)
  mkControlParameters,
) where

import Cardano.YTxP.Control.ParametersInitial (
  ControlParametersInitial (
    ControlParametersInitial,
    compilationConfig,
    maxYieldListSize,
    mintingPoliciesNonceList,
    scriptToWrapYieldListMP,
    scriptToWrapYieldListValidator,
    stakingValidatorsNonceList
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
import Prettyprinter (Pretty (pretty), braces, punctuate, sep, (<+>))

{- | Scripts that govern which transaction families can be "yielded to"

@since 0.1.0
-}
data YieldListScripts = YieldListScripts
  { yieldListValidator :: YieldListValidatorScript
  -- ^ The validator where we look for UTxO(s) that carry a yieldListSTT in
  -- their values and a YieldList in their datum
  -- @since 0.1.0
  , yieldListMintingPolicy :: YieldListSTMPScript
  -- ^ The minting policy for the yieldListSTT, which authenticates a
  -- UTxO as carrying a YieldList. Note that these STTs will _usually_ be
  -- at the yieldListValidator, but these is nothing intrinsically requiring this;
  -- any UTxO carrying the STT will be looked at for a YieldList.
  -- @since 0.1.0
  }
  deriving stock
    ( -- | @since 0.1.0
      Eq
    )

-- | @since 0.1.0
instance Pretty YieldListScripts where
  {-# INLINEABLE pretty #-}
  pretty yls =
    ("YieldListScripts" <+>) . braces . sep . punctuate "," $
      [ "yieldListValidator: " <+> (pretty . yieldListValidator $ yls)
      , "yieldListMintingPolicy: " <+> (pretty . yieldListMintingPolicy $ yls)
      ]

-- | @since 0.1.0
instance ToJSON YieldListScripts where
  {-# INLINEABLE toJSON #-}
  toJSON yls =
    object
      [ "yieldListValidator" .= yieldListValidator yls
      , "yieldListMintingPolicy" .= yieldListMintingPolicy yls
      ]
  {-# INLINEABLE toEncoding #-}
  toEncoding yls =
    pairs $
      "yieldListValidator" .= yieldListValidator yls
        <> "yieldListMintingPolicy" .= yieldListMintingPolicy yls

-- | @since 0.1.0
instance FromJSON YieldListScripts where
  {-# INLINEABLE parseJSON #-}
  parseJSON = withObject "YieldListScripts" $ \obj -> do
    ylv <- obj .: "yieldListValidator"
    ylmp <- obj .: "yieldListMintingPolicy"
    pure $ YieldListScripts ylv ylmp

{- | Scripts that yield to transaction families described by the datums guarded
by the YieldListScripts.

@since 0.1.0
-}
data YieldingScripts = YieldingScripts
  { yieldingMintingPolicies :: [YieldingMPScript]
  -- ^ @since 0.1.0
  , yieldingValidator :: YieldingValidatorScript
  -- ^ @since 0.1.0
  , yieldingStakingValidators :: [YieldingStakingValidatorScript]
  -- ^ @since 0.1.0
  }

-- \^ We have multiple of these, because each can only be delegated to a single
-- pool.

-- | @since 0.1.0
instance ToJSON YieldingScripts where
  {-# INLINEABLE toJSON #-}
  toJSON ys =
    object
      [ "yieldingMintingPolicies" .= yieldingMintingPolicies ys
      , "yieldingValidator" .= yieldingValidator ys
      , "yieldingStakingValidators" .= yieldingStakingValidators ys
      ]
  {-# INLINEABLE toEncoding #-}
  toEncoding ys =
    pairs $
      "yieldingMintingPolicies" .= yieldingMintingPolicies ys
        <> "yieldingValidator" .= yieldingValidator ys
        <> "yieldingStakingValidators" .= yieldingStakingValidators ys

-- | @since 0.1.0
instance FromJSON YieldingScripts where
  {-# INLINEABLE parseJSON #-}
  parseJSON = withObject "YieldingScripts" $ \obj -> do
    ysmp <- obj .: "yieldingMintingPolicies"
    ysv <- obj .: "yieldingValidator"
    ysvs <- obj .: "yieldingStakingValidators"
    pure $ YieldingScripts ysmp ysv ysvs

{- | Contains the compiled scripts along with the parameters
they were compiled against. This is useful for _library consumers_
and should contain all of the information needed to work with the
library.

@since 0.1.0
-}
data ControlParameters = ControlParameters
  { yieldListScripts :: YieldListScripts
  -- ^ @since 0.1.0
  , yieldingScripts :: YieldingScripts
  -- ^ @since 0.1.0
  , controlParametersInitial :: ControlParametersInitial
  -- ^ @since 0.1.0
  }

-- | @since 0.1.0
instance ToJSON ControlParameters where
  {-# INLINEABLE toJSON #-}
  toJSON cp =
    object
      [ "yieldListScripts" .= yieldListScripts cp
      , "yieldingScripts" .= yieldingScripts cp
      , "controlParametersInitial" .= controlParametersInitial cp
      ]
  {-# INLINEABLE toEncoding #-}
  toEncoding cp =
    pairs $
      "yieldListScripts" .= yieldListScripts cp
        <> "yieldingScripts" .= yieldingScripts cp
        <> "controlParametersInitial" .= controlParametersInitial cp

-- | @since 0.1.0
instance FromJSON ControlParameters where
  {-# INLINEABLE parseJSON #-}
  parseJSON = withObject "ControlParameters" $ \obj -> do
    yls <- obj .: "yieldListScripts"
    ys <- obj .: "yieldingScripts"
    cpi <- obj .: "controlParametersInitial"
    pure $ ControlParameters yls ys cpi

{- | Compile all scripts, threading through the appropriate parameters and
script hashes
-}
mkControlParameters ::
  ControlParametersInitial ->
  Either
    Text
    ControlParameters
mkControlParameters
  cpi@ControlParametersInitial
    { maxYieldListSize
    , stakingValidatorsNonceList
    , mintingPoliciesNonceList
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
      yieldingVal <- compileYieldingValidator compilationConfig ylstcs

      -- Compile the staking validators, pulling any @Left@s (containing compilation
      -- error messages) through the list
      yieldingStakingVals <-
        mapM
          (compileYieldingStakingValidator compilationConfig ylstcs)
          stakingValidatorsNonceList

      yieldingMPs <-
        mapM
          (compileYieldingMP compilationConfig ylstcs)
          mintingPoliciesNonceList

      pure $
        ControlParameters
          { yieldListScripts =
              YieldListScripts
                yieldListVal
                yieldListSTMP
          , yieldingScripts =
              YieldingScripts
                yieldingMPs
                yieldingVal
                yieldingStakingVals
          , controlParametersInitial = cpi
          }