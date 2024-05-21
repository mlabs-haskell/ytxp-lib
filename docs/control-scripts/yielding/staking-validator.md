# Table of Contents

1. [Parameters](#org608e468)
2. [Redeemer](#org4a8e812)
3. [Semantics](#orgf460076)

A parametrised script for stake validator script to guard delegation and withdrawal operations.
Staking credentials (i.e., the hash of a staking validator) can only be delegated to a single pool at a time.
Thus, we must add a nonce to the staking validator&rsquo;s parameters in order to create a unique script hash
so that we can utilize multipool delegation.

This script is implemented via the ["yield helper"](./helper.md) script.

<a id="org608e468"></a>

## Parameters

This script has all of the parameter of the  ["yield helper"](./helper.md) script, as well as:

- **yieldingStakingValidatorNonce**: `BuiltinInteger`

<a id="org4a8e812"></a>

## Redeemer

Identical to the ["yield helper"](./helper.md) script.

<a id="orgf460076"></a>

## Semantics

Almost identical to the ["yield helper"](./helper.md) script, with the additional behavior that a "nonce" is used to change the script hash.
The nonce can be included in as a `plet` binding, similar to

```hs
mkYieldingStakingValidator ylstcs nonce =
  plet (pconstant nonce) $ const (... yielding helper ...)
```
