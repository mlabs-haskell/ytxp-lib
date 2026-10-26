# Table of Contents

1. [Parameters](#org373ed43)
2. [Datum](#org2e037bf)
3. [Redeemer](#org30acbe9)
4. [Semantics](#orgc9308fb)
5. [Tokens](#org4d38e18)

A yielding validator script that can be used to lock UTxOs.

In practice, these UTxOs will usually be "component UTxOs"; UTxOs that carry component tokens.
The component tokens indicate the "type" of component, and the _authorised_ scripts make use of these types in their validation logic.

It is implemented via the ["yield helper"](./helper.md) script.

<a id="org373ed43"></a>

## Parameters

Identical to the ["yield helper"](./helper.md) script, plus a `nonce` used to diversify the script hash.

<a id="org2e037bf"></a>

## Datum

The datums of UTxOs locked by this address are unspecified and library-user-defined.
Explicitly, the datums may be of many different "types".

The script does not inspect the datum.

<a id="org30acbe9"></a>

## Redeemer

Identical to the ["yield helper"](./helper.md) script.

<a id="orgc9308fb"></a>

## Semantics

The validator is the ["yield helper"](./helper.md) applied to the authorised-scripts STCS and a
nonce.

The nonce is not used by the validation logic. It exists only to give each compiled instance a
distinct hash.

This validator can lock many different "types" of components.
The "type" of component is differentiated by the presence of a "component token".

<a id="org4d38e18"></a>

## Held Tokens

The tokens held by UTxOs at this validator address are unspecified and library-user-defined.
Explicitly, the tokens may be of many different "types".
