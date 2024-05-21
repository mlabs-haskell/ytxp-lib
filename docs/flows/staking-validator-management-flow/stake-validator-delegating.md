# Table of Contents

1. [TxF Parameters](#org95dbb5f)
2. [TxF Arguments](#org01f20b3)
3. [Scripts Used](#org8d9adf9)
4. [Reference Inputs](#org223f0d4)
5. [Inputs](#orga5a6957)
6. [Outputs](#org9dbbbad)
7. [Semantics](#org9888b6a)
8. [Constraints](#orgaf92db4)

To delegate a staking validator to a specific pool, one must submit a transaction with a `DCert` in the `txInfoDCert` field using the `DCertDelegDelegate` constructor.
This value must contain the `StakingCredential` of the stake validator and the PKH of the stake pool.
One or more `DCert`s can be submitted at once.

Delegation triggers the staking validator. This means that the staking validator can be used to control where the ada is staked to.
Our `yieldingStakingValidators` do not make any such assertions directly. Instead, the yielded-to script can make these assertions.
This allows maximum flexibility in determining exactly how delegation occurs.

<a id="org95dbb5f"></a>

## TxF Parameters

- ReferenceScriptLockerAddress

<a id="org01f20b3"></a>

## TxF Arguments

- **delegations:** `NonEmpty (StakingCredential, PubKeyHash)`
    A list of the staking credentials and the corresponding PKHs of the stake pools they should be delegated to

<a id="org8d9adf9"></a>

## Scripts Used

- Each script in the `delegations` argument will be triggered

<a id="org223f0d4"></a>

## Reference Inputs

- Each script in the `delegations` argument should be passed as a reference script.

<a id="orga5a6957"></a>

## Inputs

- None specified

<a id="org9dbbbad"></a>

## Outputs

- None specified

<a id="org9888b6a"></a>

## Semantics

This transaction delegates one or more staking validators to stake pools

<a id="orgaf92db4"></a>

## Constraints

Implementations can decide how to handle what happens if the credential is already delegated, such as erroring, overriding the present delegation, or omitting the already-delegated credentials from the `txInfoDCert` field.

The delegation certificates must appear in the `txInfoDCert` field.
