{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{- | Module: Cardano.YTxP.Control.YieldList
Description: Defines shared data types and utilities for YieldList scripts
-}
module Cardano.YTxP.Control.YieldList (
  YieldListDatum(YieldListDatum),
  PYieldListDatum (PYieldListDatum),
  YieldedToHash(YieldedToValidator, YieldedToMP, YieldedToSV),
  CustomScriptHash(CustomScriptHash),
  PYieldedToHash (PYieldedToValidator, PYieldedToMP, PYieldedToSV),
  YieldListMPWrapperRedeemer,
  PYieldListMPWrapperRedeemer (PMint, PBurn),
  getYieldedToHashByIndex,
  immutableValidatorWrapper,
  adminSigValidatorWrapper,
  multiSigValidatorWrapper,
  immutableMintingPolicyWrapper,
  adminSigMintingPolicyWrapper,
  multiSigMintingPolicyWrapper,
) where

import Cardano.YTxP.Control.Vendored (DerivePConstantViaDataList (DerivePConstantViaDataList),
                                      DerivePConstantViaEnum (DerivePConstantEnum),
                                      EnumIsData (EnumIsData),
                                      PlutusTypeDataList, PlutusTypeEnumData,
                                      ProductIsData (ProductIsData))
import Control.Monad (guard)
import Generics.SOP qualified as SOP
import Plutarch.Api.V2 (PScriptHash)
import Plutarch.DataRepr (DerivePConstantViaData (DerivePConstantViaData),
                          PDataFields)
import Plutarch.Lift (PConstantDecl, PLifted, PUnsafeLiftDecl)
import PlutusTx qualified
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins.Internal qualified as BI
import PlutusTx.Prelude (traceError)

--------------------------------------------------------------------------------
-- Types

-- | We use this `CustomScriptHash` instead of `ScriptHash` in
-- order to ensure that the hash is of length 28.
newtype CustomScriptHash = CustomScriptHash {getCustomScriptHash :: Builtins.BuiltinByteString}
  deriving stock
    ( Show
    , Eq
    )


instance PlutusTx.UnsafeFromData CustomScriptHash where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData b =
    let !args = BI.snd $ BI.unsafeDataAsConstr b
        scriptHash = BI.unsafeDataAsB (BI.head args)
     in CustomScriptHash scriptHash

-- | Note(Nigel): This will likely not compile under `plutus-tx`
-- due to the use of `error` from the Haskell `Prelude`.
-- We use `error` from Prelude here as using `traceError` doesn't give back the error message.
-- See the following issue for more details: https://github.com/IntersectMBO/plutus/issues/3003
instance PlutusTx.ToData CustomScriptHash where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (CustomScriptHash scriptHash) =
    if Builtins.lengthOfByteString scriptHash == 28
      then PlutusTx.toBuiltinData scriptHash
      else error "ScriptHash must be of length 28"

instance PlutusTx.FromData CustomScriptHash where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData b = do
    CustomScriptHash scriptHash <- PlutusTx.fromBuiltinData b
    guard (Builtins.lengthOfByteString scriptHash == 28)
    pure $ unsafeCustomScriptHash scriptHash

{-# INLINEABLE unsafeCustomScriptHash #-}
unsafeCustomScriptHash :: Builtins.BuiltinByteString -> CustomScriptHash
unsafeCustomScriptHash scriptHash
  | (Builtins.lengthOfByteString scriptHash == 28) =
      traceError "unsafeCustomScriptHash: ScriptHash must have length 28"
  | otherwise = CustomScriptHash scriptHash

{- | A single hash that a yielding script can yield to
A yielded to script can be a validator, minting policy or a stake validator
-}
data YieldedToHash
  = YieldedToValidator CustomScriptHash
  | YieldedToMP CustomScriptHash
  | YieldedToSV CustomScriptHash
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
  = PYieldedToValidator (Term s (PDataRecord '["scriptHash" ' := PScriptHash]))
  | PYieldedToMP (Term s (PDataRecord '["scriptHash" ' := PScriptHash]))
  | PYieldedToSV (Term s (PDataRecord '["scriptHash" ' := PScriptHash]))
  deriving stock
    ( Generic
    )
  deriving anyclass
    ( PlutusType
    , PIsData
    )

instance DerivePlutusType PYieldedToHash where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData (PAsData PYieldedToHash)

instance PUnsafeLiftDecl PYieldedToHash where
  type PLifted PYieldedToHash = YieldedToHash

deriving via
  (DerivePConstantViaData YieldedToHash PYieldedToHash)
  instance
    (PConstantDecl YieldedToHash)

{- | The `YieldListDatum` holds a collection of hashes that YieldingScripts can yield to.
The length of the datum is checked upon creation in `mkYieldListSTMPWrapper` to ensure
that the length of the list does not exceed the max list length passed as a parameter to that script.
-}
data YieldListDatum = YieldListDatum
  { yieldedToScripts :: [YieldedToHash]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusTx.ToData, PlutusTx.FromData) via (ProductIsData YieldListDatum)

deriving via
  (DerivePConstantViaDataList YieldListDatum PYieldListDatum)
  instance
    (PConstantDecl YieldListDatum)

newtype PYieldListDatum (s :: S)
  = PYieldListDatum
      ( Term
          s
          ( PDataRecord
              '[ "yieldedToScripts" ' := PBuiltinList (PAsData PYieldedToHash)
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PDataFields)

instance DerivePlutusType PYieldListDatum where
  type DPTStrat _ = PlutusTypeDataList

instance PUnsafeLiftDecl PYieldListDatum where
  type PLifted _ = YieldListDatum

instance PTryFrom PData (PAsData PYieldListDatum)

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
-- Helpers

getYieldedToHashByIndex :: Term s (PYieldListDatum :--> PInteger :--> PYieldedToHash)
getYieldedToHashByIndex = plam $ \datum n ->
  pmatch datum $ \case
    PYieldListDatum ((pfield @"yieldedToScripts" #) -> yieldList) ->
      pfromData $ yieldList #!! n

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
