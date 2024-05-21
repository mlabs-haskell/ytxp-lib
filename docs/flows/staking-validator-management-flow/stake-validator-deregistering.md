# Table of Contents

1. [TxF Parameters](#org929b516)
2. [TxF Arguments](#org02206fa)
3. [Scripts Used](#orgb80971a)
4. [Reference Inputs](#orgec867f0)
5. [Inputs](#org8016040)
6. [Outputs](#org69ed416)
7. [Semantics](#orge82b809)
8. [Constraints](#org187c6f6)

When a yielding or yielded-to staking validator is no longer needed, we can de-register it by submitting a transaction with a `DCertDelegDeRegKey` in the `txInfoDCert` field.

Deregistering will trigger the corresponding staking validator. The offchain code implementing this family should first ensure that the
staking validator is indeed registered.

<a id="org929b516"></a>

## TxF Parameters

- None

<a id="org02206fa"></a>

## TxF Arguments

- **stakingCredentials:** NonEmpty StakingCredential
    A list of the staking credentials to deregister

<a id="orgb80971a"></a>

## Scripts Used

- Each script listed in `stakingCredentials`

<a id="orgec867f0"></a>

## Reference Inputs

- None specified

<a id="org8016040"></a>

## Inputs

- None specified

<a id="org69ed416"></a>

## Outputs

- None specified

<a id="orge82b809"></a>

## Semantics

This transaction deregisters one or more staking credentials

<a id="org187c6f6"></a>

## Constraints

Implementations can decide how to handle what happens if the credential isn&rsquo;t registered, such as erroring or omitting the non-registered credentials from the `txInfoDCert` field.

The deregistration certificates must appear in the `txInfoDCert` field.
