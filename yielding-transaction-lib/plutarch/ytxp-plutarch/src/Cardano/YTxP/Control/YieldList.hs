{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{- | Module: Cardano.YTxP.Control.YieldList
Description: Defines shared data types and utilities for YieldList scripts
-}
module Cardano.YTxP.Control.YieldList (
  YieldList,
  YieldedToHash,
  immutableValidatorWrapper,
  adminSigValidatorWrapper,
  multiSigValidatorWrapper,
  immutableMintingPolicyWrapper,
  adminSigMintingPolicyWrapper,
  multiSigMintingPolicyWrapper,
) where

import PlutusLedgerApi.V1.Scripts (ScriptHash) -- TODO V2 for sure?
import Prelude (error)

--------------------------------------------------------------------------------
-- Types

-- | A collection of hashes that YieldingScripts can yield to
newtype YieldList = YieldList [YieldedToHash] -- FIXME

-- | A single hash that a yielding script can yield to
data YieldedToHash -- FIXME
  = YieldedToValidator ScriptHash
  | YieldedToMP ScriptHash
  | YieldedToSV ScriptHash

--------------------------------------------------------------------------------
-- Validator Wrappers

-- FIXME:
-- immutableWrapper :: Validator -> Validator

{- | A wrapper to ensure that validator can never succeed.
Useful for making an "immutable yield list UTxO", where
transaction families cannot be removed or modified once deployed
-}
immutableValidatorWrapper :: () -> ()
immutableValidatorWrapper = error "unimplemented"

-- FIXME:
-- adminSigWrapper :: PubKeyHash -> Validator -> Validator

{- | A wrapper that imposes an additional constraint on the wrapped script
such that the admin signature must be include in the signatories of a
transaction in order for the wrapped script to succeed.
-}
adminSigValidatorWrapper :: () -> () -> ()
adminSigValidatorWrapper = error "unimplemented"

-- FIXME:
-- adminSigWrapper :: Integer -> [PubKeyHash] -> Validator -> Validator

{- | A wrapper that imposes an additional constraint on the wrapped script
such that at least `n` of the keys must be signatories of a
transaction in order for the wrapped script to succeed.
-}
multiSigValidatorWrapper :: () -> () -> ()
multiSigValidatorWrapper = error "unimplemented"

--------------------------------------------------------------------------------
-- Minting Policy Wrappers
-- FIXME: update comments as well

-- FIXME:
-- immutableWrapper :: MintingPolicy -> MintingPolicy
immutableMintingPolicyWrapper :: () -> ()
immutableMintingPolicyWrapper = error "unimplemented"

-- FIXME:
-- adminSigWrapper :: PubKeyHash -> MintingPolicy -> MintingPolicy
adminSigMintingPolicyWrapper :: () -> () -> ()
adminSigMintingPolicyWrapper = error "unimplemented"

-- FIXME:
-- adminSigWrapper :: Integer -> [PubKeyHash] -> MintingPolicy -> MintingPolicy
multiSigMintingPolicyWrapper :: () -> () -> ()
multiSigMintingPolicyWrapper = error "unimplemented"
