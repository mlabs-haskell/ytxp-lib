# Table of Contents

1. [Parameters](#org608e468)
2. [Redeemer](#org4a8e812)
3. [Semantics](#orgf460076)

A parametrised staking validator script, used to guard delegation and withdrawal operations.
Staking credentials (i.e., the hash of a staking validator) can only be delegated to a single pool at a time.
Thus, we must add a nonce to the staking validator&rsquo;s parameters in order to create a unique script hash
so that we can utilize multipool delegation.

This script is implemented via the ["yield helper"](./helper.md) script.

<a id="org608e468"></a>

## Parameters

This script has all of the parameters of the ["yield helper"](./helper.md) script, as well as:

- **nonce**: `BuiltinInteger`
    One staking validator is compiled per nonce.
    The nonce is not used by the validation logic; it exists only to give each compiled instance a
    distinct script hash, which is what makes multipool delegation possible.

<a id="org4a8e812"></a>

## Redeemer

Identical to the ["yield helper"](./helper.md) script.

<a id="orgf460076"></a>

## Semantics

Identical to the ["yield helper"](./helper.md) script, with the additional `nonce` parameter.

The nonce does not appear in the validation body, so it only affects the script hash.
