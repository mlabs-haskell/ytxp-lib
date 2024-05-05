module Test.Unit.Addresses (
    dummyScriptAddressOne,
    dummyScriptAddressTwo,
    dummyScriptAddressThree,
) where

import PlutusLedgerApi.V1.Address (Address (Address))
import PlutusLedgerApi.V1.Scripts (ScriptHash (ScriptHash))
import PlutusLedgerApi.V2 (Credential (ScriptCredential), getLedgerBytes)

dummyScriptAddressOne :: Address
dummyScriptAddressOne = Address dummyScriptCredentialOne Nothing

dummyScriptAddressTwo :: Address
dummyScriptAddressTwo = Address dummyScriptCredentialTwo Nothing

dummyScriptAddressThree :: Address
dummyScriptAddressThree = Address dummyScriptCredentialThree Nothing

dummyScriptCredentialOne :: Credential
dummyScriptCredentialOne =
    ScriptCredential $
        ScriptHash $
            getLedgerBytes "11111111111111111111111111111111111111111111111111111111"

dummyScriptCredentialTwo :: Credential
dummyScriptCredentialTwo =
    ScriptCredential $
        ScriptHash $
            getLedgerBytes "22222222222222222222222222222222222222222222222222222222"

dummyScriptCredentialThree :: Credential
dummyScriptCredentialThree =
    ScriptCredential $
        ScriptHash $
            getLedgerBytes "33333333333333333333333333333333333333333333333333333333"
