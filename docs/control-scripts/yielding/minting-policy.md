# Table of Contents

1. [Parameters](#org0673dab)
2. [Redeemer](#org45e406a)
3. [Token Names](#org62ac69c)
4. [Semantics](#org8648bdd)

A parametrised script for yielding minting policies.
In practice, this minting policy can be used to either mint "Component Tokens", other STT (possibly NFTs, when combined with one-shot authorised transactions), or general, fungible tokens such as stablecoins, liquidity tokens, and so forth.

This script is implemented via the ["yield helper"](./helper.md) script.

<a id="org0673dab"></a>

## Parameters

This script has all of the parameter of the  ["yield helper"](./helper.md) script, as well as:

- **yieldingMintingPolicyNonce**: `BuiltinInteger`

<a id="org45e406a"></a>

## Redeemer

Identical to the ["yield helper"](./helper.md) script.

<a id="org62ac69c"></a>

## Token Names

Unspecified; library-user-defined

<a id="org8648bdd"></a>

## Semantics

Almost identical to the ["yield helper"](./helper.md) script, with the additional behavior that a "nonce" is used to change the script hash.
The nonce can be included in as a `plet` binding, similar to

```hs
mkYieldingMintingPolicy ylstcs nonce =
  plet (pconstant nonce) $ const (... yielding helper ...)
```
