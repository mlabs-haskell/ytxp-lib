module Test.Unit.Transaction (
    pdummyTxOutSingletonListWithTxOutWithEmptyYieldListDatum,
    pdummyTxInInfoSingletonList,
    pdummyTxInInfoTwoElementList,
    pdummyTxInInfoSingletonListTwo,
    pdummyTxInInfoThreeElementList,
    pdummyTxOutSingletonList,
    pdummyTxOutTwoElementList,
    pdummyTxOutRefOne,
    pdummyTxOutRefTwo,
    dummyTxOutRefOne,
) where

import Plutarch.Api.V2 (PTxInInfo, PTxOut, PTxOutRef)
import PlutusLedgerApi.V2 (
    TxId (TxId),
    TxInInfo (TxInInfo),
    TxOut (TxOut),
    TxOutRef (TxOutRef),
    getLedgerBytes,
 )
import Test.Unit.Addresses (
    dummyScriptAddressOne,
    dummyScriptAddressThree,
    dummyScriptAddressTwo,
 )
import Test.Unit.Datums (
    dummyOutputDatumOne,
    dummyOutputDatumThree,
    dummyOutputDatumTwo,
    dummyYieldListEmptyListOutputDatum,
 )
import Test.Unit.Values (dummyValueOne, dummyValueThree, dummyValueTwo)

-- * PTxOut lists
pdummyTxOutSingletonList :: Term s (PBuiltinList PTxOut)
pdummyTxOutSingletonList = pconstant [dummyTxOutOne]

pdummyTxOutTwoElementList :: Term s (PBuiltinList PTxOut)
pdummyTxOutTwoElementList = pconstant [dummyTxOutOne, dummyTxOutTwo]

pdummyTxOutSingletonListWithTxOutWithEmptyYieldListDatum ::
    Term s (PBuiltinList PTxOut)
pdummyTxOutSingletonListWithTxOutWithEmptyYieldListDatum =
    pconstant [dummyTxOutWithYieldListDatumEmptyList]

-- * PTxInInfo lists
pdummyTxInInfoThreeElementList :: Term s (PBuiltinList PTxInInfo)
pdummyTxInInfoThreeElementList = pconstant [dummyTxInInfoOne, dummyTxInInfoTwo, dummyTxInInfoFour]

pdummyTxInInfoTwoElementList :: Term s (PBuiltinList PTxInInfo)
pdummyTxInInfoTwoElementList = pconstant [dummyTxInInfoOne, dummyTxInInfoTwo]

pdummyTxInInfoSingletonList :: Term s (PBuiltinList PTxInInfo)
pdummyTxInInfoSingletonList = pconstant [dummyTxInInfoOne]

pdummyTxInInfoSingletonListTwo :: Term s (PBuiltinList PTxInInfo)
pdummyTxInInfoSingletonListTwo = pconstant [dummyTxInInfoThree]

-- * TxInInfo samples
dummyTxInInfoOne :: TxInInfo
dummyTxInInfoOne = TxInInfo dummyTxOutRefOne dummyTxOutOne

dummyTxInInfoTwo :: TxInInfo
dummyTxInInfoTwo = TxInInfo dummyTxOutRefTwo dummyTxOutTwo

dummyTxInInfoThree :: TxInInfo
dummyTxInInfoThree = TxInInfo dummyTxOutRefThree dummyTxOutThree

dummyTxInInfoFour :: TxInInfo
dummyTxInInfoFour = TxInInfo dummyTxOutRefFour dummyTxOutFour

-- * TxOut samples
dummyTxOutOne :: TxOut
dummyTxOutOne = TxOut dummyScriptAddressOne dummyValueOne dummyOutputDatumOne Nothing

dummyTxOutTwo :: TxOut
dummyTxOutTwo = TxOut dummyScriptAddressTwo dummyValueTwo dummyOutputDatumTwo Nothing

-- Same as dummyTxOne except for the `Value`, which has the
-- same CurrencySymbol but different amount
dummyTxOutThree :: TxOut
dummyTxOutThree = TxOut dummyScriptAddressOne dummyValueThree dummyOutputDatumOne Nothing

dummyTxOutFour :: TxOut
dummyTxOutFour = TxOut dummyScriptAddressThree dummyValueThree dummyOutputDatumThree Nothing

dummyTxOutWithYieldListDatumEmptyList :: TxOut
dummyTxOutWithYieldListDatumEmptyList =
    TxOut
        dummyScriptAddressOne
        dummyValueOne
        dummyYieldListEmptyListOutputDatum
        Nothing

-- * TxOutRef samples
dummyTxOutRefOne :: TxOutRef
dummyTxOutRefOne = TxOutRef dummyTxIdOne 0

pdummyTxOutRefOne :: Term s PTxOutRef
pdummyTxOutRefOne = pconstant dummyTxOutRefOne

dummyTxOutRefTwo :: TxOutRef
dummyTxOutRefTwo = TxOutRef dummyTxIdTwo 1

pdummyTxOutRefTwo :: Term s PTxOutRef
pdummyTxOutRefTwo = pconstant dummyTxOutRefTwo

dummyTxOutRefThree :: TxOutRef
dummyTxOutRefThree = TxOutRef dummyTxIdThree 2

dummyTxOutRefFour :: TxOutRef
dummyTxOutRefFour = TxOutRef dummyTxIdThree 3

-- * TxId samples
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
