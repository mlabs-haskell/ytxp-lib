# Table of Contents

1. [TxF Parameters](#orgf921065)
2. [TxF Arguments](#orga6484cd)
3. [Scripts Used](#org985268e)
4. [Reference Inputs](#org6d228d4)
5. [Inputs](#org8443af5)
6. [Outputs](#orge300cae)
7. [Semantics](#org5bbb055)
8. [Constraints](#org58cb41f)

Each of the staking validators (yielding and yielded-to) must be registered prior to use.

To register a staking validator, we need to submit a transaction that:

- Has a non-empty `txInfoDCert` field
- Within that field, contains a `DCertDelegRegKey` (a data constructor of the `DCert` type) with the corresponding script hash of each staking validator that needs to be registered

One or more staking validators can be registered at once. A transaction in this family will fail if the validator is already registered.

NOTE: registering DOES NOT trigger the corresponding staking validator.

These transactions can be wallet-to-wallet transactions. The offchain code
implementing such a transaction family should check whether the staking validator is already registered.

<a id="orgf921065"></a>

## TxF Parameters

- None

<a id="orga6484cd"></a>

## TxF Arguments

- **stakingCredentials:** NonEmpty StakingCredential
    A list of the staking credentials to register

<a id="org985268e"></a>

## Scripts Used

- None specified

<a id="org6d228d4"></a>

## Reference Inputs

- None specified

<a id="org8443af5"></a>

## Inputs

- None specified

<a id="orge300cae"></a>

## Outputs

- None specified

<a id="org5bbb055"></a>

## Semantics

This transaction registers one or more staking credentials

<a id="org58cb41f"></a>

## Constraints

Implementations can decide how to handle what happens if the credential is already registered, such as erroring or omitting the already-registered credentials from the `txInfoDCert` field.

The registration certificates must appear in the `txInfoDCert` field.
