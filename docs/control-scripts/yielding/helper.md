# Table of Contents

1. [Parameters](#org0673dab)
2. [Redeemer](#org45e406a)
3. [Semantics](#org8648bdd)

This is a parameterised script taking the authorised-scripts STCS and the Plutus V3 script context.
It is used as the basis for implementing the yielding minting policy, yielding validator, and yielding staking validator.

<a id="org0673dab"></a>

## Parameters

- **authorisedScriptsSTCS:** CurrencySymbol
    The currency symbol of the token that identifies authorised reference scripts.
    A reference UTxO counts as authorised when its value carries at least one token with this
    currency symbol; the reference script that the UTxO carries is then the authorised script.

Each role adds a `nonce` parameter on top of this one, so that every compiled instance gets a
distinct script hash; see the [minting policy](./minting-policy.md), [validator](./validator.md)
and [staking validator](./staking-validator.md) pages.

<a id="org45e406a"></a>

## Redeemer

- **authorisedScriptIndex:** Integer
    The index of the `TxInReferenceInput` that contains the authorised reference script.

- **authorisedScriptProofIndex:** `(AuthorisedScriptPurpose, Integer)`
    A pair: the _authorised_ script's role and the index at which to find proof of its execution.
    The role is one of `Minting`, `Spending`, `Rewarding`.
    The meaning of the index depends on the role; see [Semantics](#org8648bdd).
    Negative or out-of-range indices abort.

<a id="org8648bdd"></a>

## Semantics

- Look at the UTxO at the `n` th entry in the `txInfoReferenceInputs`, where `n` is equal to `authorisedScriptIndex`.
  - Call this UTxO `authorisedScriptUTxO`.
  - Check that the `authorisedScriptUTxO` carries at least one token with the `authorisedScriptsSTCS`. Blow up if not.
  - Call the hash of the reference script carried by the `authorisedScriptUTxO` the `AuthorisedScriptHash`.
- Obtain evidence that the script with `AuthorisedScriptHash` was triggered via the `checkAuthorisedScript` function. TODO(alberto 20240-05-24) checkAuthorisedTo function does not exist yet
    This function looks at `authorisedScriptProofIndex` and indexes in one of the following ways:
  - If the `ScriptPurpose` is `Spending`, it indexes into the `TxInfoInputs` field and checks whether the `ScriptAddress` of the input matches the hash (erroring if not)
  - If the `ScriptPurpose` is `Minting`, it looks at the `TxInfoMint` field and checks for the presence of the hash in the mint `Value` (error if not)
  - If the `ScriptPurpose` is `Rewarding`, it looks  at the `TxInfoWithdrawals` field and checks for the presence of the hash in the list (erroring if not)
