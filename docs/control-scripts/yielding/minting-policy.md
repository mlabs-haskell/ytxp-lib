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

This script has all of the parameters of the ["yield helper"](./helper.md) script, as well as:

- **nonce**: `BuiltinInteger`
    One minting policy is compiled per nonce.
    The nonce is not used by the validation logic; it exists only to give each compiled instance a
    distinct script hash.

<a id="org45e406a"></a>

## Redeemer

Identical to the ["yield helper"](./helper.md) script.

<a id="org62ac69c"></a>

## Token Names

Unspecified; library-user-defined

<a id="org8648bdd"></a>

## Semantics

Identical to the ["yield helper"](./helper.md) script, with the additional `nonce` parameter.
In Plutarch the minting policy is simply the exported term with both parameters applied.

The nonce does not appear in the validation body, so it only affects the script hash.
That is what lets several policies share one STCS while remaining distinguishable by currency
symbol.
