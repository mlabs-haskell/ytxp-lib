{-# LANGUAGE ImpredicativeTypes #-}

module Cardano.YTxP.Control.Yielding.StakingValidator (
  -- * Staking Validator
  YieldingStakingValidatorScript (nonce, stakingValidator),
  compileYieldingStakingValidator,

  -- * Credential and Nonce
) where

import Cardano.YTxP.Control.ParametersInitial (
  ControlParametersInitial,
  compilationConfig,
  nonceList,
 )
import Cardano.YTxP.Control.Stubs (
  alwaysSucceedsTwoArgumentScript,
  noncedTwoArgumentScriptWrapper,
 )
import Cardano.YTxP.Control.YieldList.MintingPolicy (
  YieldListSTCS,
  compileYieldListSTMP,
  mkYieldListSTCS,
 )
import Data.Text (Text)
import Plutarch (Config, compile)
import Plutarch.Api.V2 (PScriptContext, scriptHash)
import Plutarch.Lift (PConstantDecl, PConstanted, PLifted)
import Plutarch.Script (Script)

--------------------------------------------------------------------------------
-- Yielding Staking Validator

-- | A yielding staking validator together with its nonce.
data YieldingStakingValidatorScript (nonceType :: Type) = YieldingStakingValidatorScript
  { nonce :: nonceType
  , stakingValidator :: Script
  }

{- | Compile a yielding staking validator that has been nonced.
The nonce is required because each staking validator can only
be delegated to a single pool; the inclusion of the nonce will change the
script hash.
-}
compileYieldingStakingValidator ::
  forall (nonceType :: Type).
  ( PConstantDecl nonceType
  , nonceType ~ PLifted (PConstanted nonceType)
  ) =>
  Config ->
  YieldListSTCS ->
  nonceType ->
  Either
    Text
    (YieldingStakingValidatorScript nonceType)
compileYieldingStakingValidator config ylstcs nonce = do
  let
    yieldingStakingValidator ::
      forall (s :: S). Term s (PData :--> PScriptContext :--> POpaque)
    yieldingStakingValidator =
      mkYieldingStakingValidator ylstcs nonce

  -- Pull the "Either" through the list
  script <- compile config yieldingStakingValidator

  pure $
    YieldingStakingValidatorScript nonce script

--------------------------------------------------------------------------------
-- Yielding Staking Validator Credential

--------------------------------------------------------------------------------
-- Helpers (unexported)

mkYieldingStakingValidator ::
  forall (nonceType :: Type).
  ( PConstantDecl nonceType
  , nonceType ~ PLifted (PConstanted nonceType)
  ) =>
  YieldListSTCS ->
  nonceType ->
  (forall (s :: S). Term s (PData :--> PScriptContext :--> POpaque))
mkYieldingStakingValidator _ylstcs nonce =
  plet (pconstant nonce) $
    const (plam $ \_redeemer _ctx -> popaque (pconstant ()))
