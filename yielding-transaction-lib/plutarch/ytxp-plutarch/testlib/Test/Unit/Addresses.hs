module Test.Unit.Addresses (dummyScriptAddressOne) where

import PlutusLedgerApi.V1.Address (Address (Address))
import PlutusLedgerApi.V1.Scripts (ScriptHash (ScriptHash))
import PlutusLedgerApi.V2 (
  Credential (ScriptCredential),
  getLedgerBytes,
 )

dummyScriptAddressOne :: Address
dummyScriptAddressOne = Address dummyScriptCredentialOne Nothing

dummyScriptCredentialOne :: Credential
dummyScriptCredentialOne =
  ScriptCredential $
    ScriptHash $
      getLedgerBytes "11111111111111111111111111111111111111111111111111111111"
