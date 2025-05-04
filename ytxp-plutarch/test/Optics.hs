{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Module: Optics

This is Convex.PlutusLedgerApi.Optics vendored from sc-tools.

Note (Peter): I deliberately chose `makeClassyFor` because I felt lit it was a bit more explicit than `makeClassy`.
Note also that the derived instances come first for the purpose of getting TH to not complain.
This is instead of my preferred organization of grouping instances by the type the act on.
-}
module Optics (
  HasScriptContext (..),
  HasTxInfo (..),
  HasTxInInfo (..),
  HasTxOutRef (..),
  HasTxOut (..),
  HasTxId (..),
  HasAddress (..),
  AsScriptPurpose (..),
  AsTxCert (..),
  AsStakingCredential (..),
  AsCredential (..),
  AsOutputDatum (..),
) where

import Control.Lens (
  Lens',
  lens,
  makeClassyFor,
  makeClassyPrisms,
 )
import PlutusLedgerApi.V3 (
  Address,
  Credential,
  OutputDatum,
  ScriptContext (scriptContextTxInfo),
  ScriptPurpose,
  StakingCredential,
  TxCert,
  TxId,
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoId),
  TxOut (txOutAddress),
  TxOutRef (txOutRefId),
 )

-- Note: the TxInfo field is missing from here because
-- we manually define "HasTxInfo ScriptContext" below to avoid
-- name collisions
makeClassyFor
  "HasScriptContext"
  "scriptContext"
  [("scriptContextPurpose", "purpose")]
  ''ScriptContext

makeClassyPrisms ''ScriptPurpose

-- Note: the Id field is missing here because we manually define
-- "HasTxId TxInfo" below
makeClassyFor
  "HasTxInfo"
  "txInfo"
  [ ("txInfoInputs", "inputs")
  , ("txInfoReferenceInputs", "referenceInputs")
  , ("txInfoOutputs", "outputs")
  , ("txInfoFee", "fee")
  , ("txInfoMint", "mint")
  , ("txInfoTxCerts", "txCert")
  , ("txInfoWdrl", "wdrl")
  , ("txInfoValidRange", "validRange")
  , ("txInfoSignatories", "signatories")
  , ("txInfoRedeemers", "redeemers")
  , ("txInfoData", "datums")
  ]
  ''TxInfo

makeClassyFor
  "HasTxInInfo"
  "txInInfo"
  [("txInInfoOutRef", "outRef")]
  ''TxInInfo

makeClassyFor
  "HasTxOutRef"
  "txOutRef"
  [("txOutRefIdx", "idx")]
  ''TxOutRef

makeClassyFor
  "HasTxOut"
  "txOut"
  [ ("txOutAddress", "address")
  , ("txOutDatum", "datum")
  , ("txOutReferenceScript", "referenceScript")
  , ("txOutValue", "value")
  ]
  ''TxOut

makeClassyFor
  "HasAddress"
  "txAddress"
  [ ("addressCredential", "credential")
  , ("addressStakingCredential", "stakingCredential")
  ]
  ''Address

makeClassyPrisms ''TxCert
makeClassyPrisms ''StakingCredential
makeClassyPrisms ''Credential
makeClassyPrisms ''OutputDatum

--------------------------------------------------------------------------------
-- Manual instances

instance HasTxInfo ScriptContext where
  txInfo = lens g s
    where
      g = scriptContextTxInfo
      s ctx info = ctx {scriptContextTxInfo = info}

----------------------------------------

class HasTxId s where
  txId :: Lens' s TxId

instance HasTxId TxInfo where
  txId = lens g s
    where
      g = txInfoId
      s tx txId' = tx {txInfoId = txId'}

instance HasTxId TxOutRef where
  txId = lens g s
    where
      g = txOutRefId
      s txOutRef' txId' = txOutRef' {txOutRefId = txId'}

----------------------------------------

instance HasTxOut TxInInfo where
  txOut = lens g s
    where
      g = txInInfoResolved
      s txInInfo' txOut' = txInInfo' {txInInfoResolved = txOut'}

instance HasAddress TxOut where
  txAddress = lens g s
    where
      g = txOutAddress
      s txOut' address' = txOut' {txOutAddress = address'}
