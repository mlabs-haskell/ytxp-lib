{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{- | Module: Cardano.YTxP.Control.YieldList
Description: Defines shared data types and utilities for YieldList scripts
-}
module Cardano.YTxP.Control.YieldList (
  YieldListDatum,
  YieldedToHash,
  YieldListMPWrapperRedeemer,
  PYieldListMPWrapperRedeemer (PMint, PBurn),
  immutableValidatorWrapper,
  adminSigValidatorWrapper,
  multiSigValidatorWrapper,
  immutableMintingPolicyWrapper,
  adminSigMintingPolicyWrapper,
  multiSigMintingPolicyWrapper,
) where

-- PDataFields,

import Cardano.YTxP.Control.Vendored (
  DerivePConstantViaEnum (DerivePConstantEnum),
  -- PlutusTypeDataList,
  EnumIsData (EnumIsData),
  PlutusTypeEnumData,
  -- ProductIsData(ProductIsData),
  -- DerivePConstantViaDataList(DerivePConstantViaDataList),
 )
import GHC.Generics (Generic)
-- PLiftData,
-- PlutusTypeNewtype,
-- PShow,

-- PConstantData,

-- PBuiltinList,

import Generics.SOP qualified as SOP
import Plutarch.Api.V2 (PScriptHash)
import Plutarch.DataRepr (
  DerivePConstantViaData (
    DerivePConstantViaData
  ),
 )
import Plutarch.Lift (
  PConstantDecl,
  PLifted,
  PUnsafeLiftDecl,
 )
import Plutarch.Prelude (
  DPTStrat,
  DerivePlutusType,
  PAsData,
  PData,
  PDataRecord,
  PEq,
  PIsData,
  PLabeledType ((:=)),
  PTryFrom,
  PlutusType,
  PlutusTypeData,
  S,
  Term,
 )
import PlutusLedgerApi.V1.Scripts (ScriptHash)
import PlutusTx qualified
import Prelude (Bounded, Enum, Eq, Show, error)

--------------------------------------------------------------------------------
-- Types

{- | A single hash that a yielding script can yield to
A yielded to script can be a validator, minting policy or a stake validator
-}
data YieldedToHash -- FIXME
  = YieldedToValidator ScriptHash
  | YieldedToMP ScriptHash
  | YieldedToSV ScriptHash
  deriving stock
    ( Show
    , Generic
    , Eq
    )

PlutusTx.makeIsDataIndexed
  ''YieldedToHash
  [ ('YieldedToValidator, 0)
  , ('YieldedToMP, 1)
  , ('YieldedToSV, 2)
  ]

data PYieldedToHash (s :: S)
  = PYieldedToValidator (Term s (PDataRecord '["scriptHash" ':= PScriptHash]))
  | PYieldedToMP (Term s (PDataRecord '["scriptHash" ':= PScriptHash]))
  | PYieldedToSV (Term s (PDataRecord '["scriptHash" ':= PScriptHash]))
  deriving stock
    ( Generic
    )
  deriving anyclass
    ( PlutusType
    , PIsData
    )

instance DerivePlutusType PYieldedToHash where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PYieldedToHash

instance PUnsafeLiftDecl PYieldedToHash where
  type PLifted PYieldedToHash = YieldedToHash

deriving via
  (DerivePConstantViaData YieldedToHash PYieldedToHash)
  instance
    (PConstantDecl YieldedToHash)

{- | The `YieldListDatum` holds a collection of hashes that YieldingScripts can yield to.
The length of the datum is checked upon creation in `mkYieldListSTMPWrapper` to ensure
that the length of the list does not exceed the max list length passed as a parameter to that script.
TODO(Nigel): Get it working
-}
data YieldListDatum = YieldListDatum
  { yieldedToScripts :: [YieldedToHash]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic)

-- deriving (PlutusTx.ToData, PlutusTx.FromData) via (ProductIsData YieldListDatum)

-- deriving via
--   (DerivePConstantViaDataList YieldListDatum PYieldListDatum)
--   instance
--     (PConstantDecl YieldListDatum)
--
-- newtype PYieldListDatum (s :: S)
--   = PYieldListDatum
--       ( Term
--           s
--           ( PDataRecord
--               '[ "yieldedToScripts" ':= PBuiltinList (PAsData PYieldedToHash)
--                ]
--           )
--       )
--   deriving stock (Generic)
--   deriving anyclass (PlutusType, PIsData, PEq, PDataFields)
--
-- instance DerivePlutusType PYieldListDatum where
--   type DPTStrat _ = PlutusTypeDataList
--
-- instance PUnsafeLiftDecl PYieldListDatum where
--   type PLifted _ = YieldListDatum
--
-- instance PTryFrom PData (PAsData PYieldListDatum)

-- | Redeemer for `mkYieldListMPWrapper`.
data YieldListMPWrapperRedeemer
  = -- | Mint branch
    Mint
  | -- | Burn branch
    Burn
  deriving stock
    ( Show
    , Generic
    , Enum
    , Bounded
    )
  deriving
    ( PlutusTx.ToData
    , PlutusTx.FromData
    )
    via (EnumIsData YieldListMPWrapperRedeemer)

-- | Plutarch-level version of 'YieldListMPWrapperRedeemer'.
data PYieldListMPWrapperRedeemer (s :: S)
  = PMint
  | PBurn
  deriving stock
    ( Generic
    , Enum
    , Bounded
    )
  deriving anyclass
    ( PlutusType
    , PIsData
    , PEq
    )

instance PTryFrom PData (PAsData PYieldListMPWrapperRedeemer)

instance DerivePlutusType PYieldListMPWrapperRedeemer where
  type DPTStrat _ = PlutusTypeEnumData

instance PUnsafeLiftDecl PYieldListMPWrapperRedeemer where
  type PLifted PYieldListMPWrapperRedeemer = YieldListMPWrapperRedeemer

deriving via
  (DerivePConstantViaEnum YieldListMPWrapperRedeemer PYieldListMPWrapperRedeemer)
  instance
    (PConstantDecl YieldListMPWrapperRedeemer)

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
