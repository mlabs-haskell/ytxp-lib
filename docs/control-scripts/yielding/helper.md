# Table of Contents

1. [Parameters](#org0673dab)
2. [Redeemer](#org45e406a)
3. [Semantics](#org8648bdd)

This script is a "two argument script", expecting a redeemer and a script context.
It is used as the basis for implementing the yielding minting policy, yielding validator, and yielding staking validator.

<a id="org0673dab"></a>

## Parameters

- **authorisedScriptsSTCS:** CurrencySymbol

<a id="org45e406a"></a>

## Redeemer

- **authorisedScriptIndex:** Integer
    The index of the `TxInReferenceInput` that contains the authorised reference script
- **yieldedToProofIndex:** YieldedToProofIndex
    This pair indicates the yielded to script type and the index at which to find proof
    of the script execution.
    This index has a different meaning depending on if the yielded-to script is a valdiator,
    minting policy, or staking validator.

<a id="org8648bdd"></a>

## Semantics

- Look at the UTxO at the `n` th entry in the `txInfoReferenceInputs`, where `n` is equal to `authorisedScriptIndex`.
  - Call this UTxO `authorisedScriptUTxO`.
  - Check that the `authorisedScriptUTxO` is carrying exactly one token with the `authorisedScriptsSTCS`. Blow up if not.
  - Call the hash of the reference script carried by the `authorisedScriptUTxO` the `yieldToHash`.
- Obtain evidence that the script with `yieldToHash` was triggered via the `checkYieldTo` function.
    This function looks at `yieldedToProofIndex` and indexes in one of the following ways:
  - If the `ScriptPurpose` is `Spending`, it indexes into the `TxInfoInputs` field and checks whether the `ScriptAddress` of the input matches the hash (erroring if not)
  - If the `ScriptPurpose` is `Minting`, it looks at the `TxInfoMint` field and checks for the presence of the hash in the mint `Value` (error if not)
  - If the `ScriptPurpose` is `Rewarding`, it looks  at the `TxInfoWithdrawals` field and checks for the presence of the hash in the list (erroring if not)
  - If the `ScriptPurpose` is `Delegating`, it looks at the `TxInfoDCert` for -- TODO not sure what cases we can/should check here, we can also omit Delegating from scriptpurpose for now..
