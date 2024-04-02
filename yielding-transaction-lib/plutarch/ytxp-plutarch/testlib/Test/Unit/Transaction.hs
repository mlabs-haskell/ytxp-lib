module Test.Unit.Transaction (
  dummyTxInInfoSingletonList,
  dummyTxInInfoTwoElementList,
  dummyTxInInfoSingletonListTwo,
) where

import Plutarch.Api.V2 (PTxInInfo)
import PlutusLedgerApi.V2 (
  TxId (TxId),
  TxInInfo (TxInInfo),
  TxOut (TxOut),
  TxOutRef (TxOutRef),
  getLedgerBytes,
 )
import Test.Unit.Addresses (dummyScriptAddressOne, dummyScriptAddressTwo)
import Test.Unit.Datums (dummyOutputDatumOne, dummyOutputDatumTwo)
import Test.Unit.Values (dummyValueOne, dummyValueThree, dummyValueTwo)

dummyTxInInfoTwoElementList :: Term s (PBuiltinList PTxInInfo)
dummyTxInInfoTwoElementList = pconstant [dummyTxInInfoOne, dummyTxInInfoTwo]

dummyTxInInfoSingletonList :: Term s (PBuiltinList PTxInInfo)
dummyTxInInfoSingletonList = pconstant [dummyTxInInfoOne]

dummyTxInInfoSingletonListTwo :: Term s (PBuiltinList PTxInInfo)
dummyTxInInfoSingletonListTwo = pconstant [dummyTxInInfoThree]

dummyTxInInfoOne :: TxInInfo
dummyTxInInfoOne = TxInInfo dummyTxOutRefOne dummyTxOutOne

dummyTxInInfoTwo :: TxInInfo
dummyTxInInfoTwo = TxInInfo dummyTxOutRefTwo dummyTxOutTwo

dummyTxInInfoThree :: TxInInfo
dummyTxInInfoThree = TxInInfo dummyTxOutRefThree dummyTxOutThree

dummyTxOutOne :: TxOut
dummyTxOutOne = TxOut dummyScriptAddressOne dummyValueOne dummyOutputDatumOne Nothing

dummyTxOutTwo :: TxOut
dummyTxOutTwo = TxOut dummyScriptAddressTwo dummyValueTwo dummyOutputDatumTwo Nothing

-- Same as dummyTxOne except for the `Value`, which has the
-- same CurrencySymbol but different amount
dummyTxOutThree :: TxOut
dummyTxOutThree = TxOut dummyScriptAddressOne dummyValueThree dummyOutputDatumOne Nothing

dummyTxOutRefOne :: TxOutRef
dummyTxOutRefOne = TxOutRef dummyTxIdOne 0

dummyTxOutRefTwo :: TxOutRef
dummyTxOutRefTwo = TxOutRef dummyTxIdTwo 1

dummyTxOutRefThree :: TxOutRef
dummyTxOutRefThree = TxOutRef dummyTxIdThree 2

dummyTxIdOne :: TxId
dummyTxIdOne =
  TxId $
    getLedgerBytes
      "1111111111111111111111111111111111111111111111111111111111111111"

dummyTxIdTwo :: TxId
dummyTxIdTwo =
  TxId $
    getLedgerBytes
      "2222222222222222222222222222222222222222222222222222222222222222"

dummyTxIdThree :: TxId
dummyTxIdThree =
  TxId $
    getLedgerBytes
      "3333333333333333333333333333333333333333333333333333333333333333"
