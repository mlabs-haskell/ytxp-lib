# Table of Contents

1. [TxF Parameters](#org2b55979)
2. [TxF Arguments](#org2e6551b)
3. [Scripts Used](#orgbd30058)
4. [Reference Inputs](#orgbef0821)
5. [Inputs](#orgc5c62ba)
6. [Outputs](#org336bfc8)
7. [Semantics](#orgfbb05de)
8. [Constraints](#org401336b)

To trigger a staking validator under normal operation, a transaction must be submitted to withdraw rewards.

The ledger specification allows for withdrawing at <span class="underline">any</span> time, and the full amount of accumulated rewards must be withdrawn. Explicitly, withdrawing is allowed <span class="underline">even if there are no accumulated rewards in the account</span> (&ldquo;withdraw 0&rdquo;). This is the primary way in which we trigger Yielded-to staking validators.

A transaction of this type will trigger the corresponding staking validator with the `Rewarding` constructor of the `ScriptPurpose` type.
This can be induced by building a transaction with the appropriate entry in the `txInfoWdrl` field of the `TxInfo`.
Because of this, the script can be used to control when and how the rewards are withdrawn &#x2013; sending the rewards to a specific address and restaking, for example.

<a id="org2b55979"></a>

## TxF Parameters

- ReferenceScriptLockerAddress

<a id="org2e6551b"></a>

## TxF Arguments

- **rewardedCredentials:** NonEmpty ScriptCredential

<a id="orgbd30058"></a>

## Scripts Used

- each script in the `rewardedCredentials` will be triggered

<a id="orgbef0821"></a>

## Reference Inputs

- Each script in the `rewardedCredentials` argument should be passed as a reference script

<a id="orgc5c62ba"></a>

## Inputs

- None specified

<a id="org336bfc8"></a>

## Outputs

- None specified

<a id="orgfbb05de"></a>

## Semantics

This delegation withdraws all rewards from one or more reward accounts

<a id="org401336b"></a>

## Constraints

Rewards withdrawals require withdrawing the <span class="underline">entire</span> balance of the rewards account.
Library implementors must ensure that transactions in this family correct examine the state of the accounts in order to construct the transaction properly.
