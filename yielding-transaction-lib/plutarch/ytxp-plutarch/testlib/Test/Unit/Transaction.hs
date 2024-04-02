module Test.Unit.Transaction (sampleTxInInfoList) where

import Plutarch.Api.V2 (PTxInInfo)
import PlutusLedgerApi.V2 (
  TxId (TxId),
  TxInInfo (TxInInfo),
  TxOut (TxOut),
  TxOutRef (TxOutRef),
  getLedgerBytes,
 )
import Test.Unit.Addresses (dummyScriptAddressOne)
import Test.Unit.Datums (dummyOutputDatumOne)
import Test.Unit.Values (dummyValueOne)

sampleTxInInfoList :: Term s (PBuiltinList PTxInInfo)
sampleTxInInfoList = pconstant [sampleTxInInfoOne]

sampleTxInInfoOne :: TxInInfo
sampleTxInInfoOne = TxInInfo dummyTxOutRefOne dummyTxOutOne

dummyTxOutOne :: TxOut
dummyTxOutOne = TxOut dummyScriptAddressOne dummyValueOne dummyOutputDatumOne Nothing

dummyTxOutRefOne :: TxOutRef
dummyTxOutRefOne = TxOutRef dummyTxIdOne 0

dummyTxIdOne :: TxId
dummyTxIdOne =
  TxId $
    getLedgerBytes
      "1111111111111111111111111111111111111111111111111111111111111111"
