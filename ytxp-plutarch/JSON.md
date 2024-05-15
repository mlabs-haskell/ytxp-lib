# Notes on JSON serialization

## Serialization practices

### `Script` serialization

The standard serialization method for Plutarch `Script`s effectively [wraps
`serialiseScript`](https://github.com/Plutonomicon/plutarch-plutus/blob/master/Plutarch/Script.hs#L13).
`serialiseScript` comes from the Plutus ledger API, and involves [serializing
via `Flat`, then packing into a
`ShortByteString`](https://github.com/IntersectMBO/plutus/blob/master/plutus-ledger-api/src/PlutusLedgerApi/Common/SerialisedScript.hs#L151).
Given that this appears to be a (relatively) standard form, we adopt this
approach. However, we must still decide an encoding of binary data into JSON: we
choose hex encoding, with an `0x` prefix, as this is clear to humans reading the
serialized form and relatively easy to work with.

We provide the helper newtype `HexStringScript` to automatically perform this.

## Plutarch `Term` serialization

It can be convenient for us to represent some scripts as closed Plutarch terms
(that is, something of type ``forall (s :: S) . Term s a`` for some `a`).
However, for interoperability with other YTxP implementations, these must be
serialized like other scripts, using the convention described in the previous
section. This poses some challenges:

* In order to transform a closed Plutarch term into a `Script`, we must compile
  it first, which has a chance of erroring. Aeson's `toJSON` interface assumes
  that serialization _cannot_ fail.
* In order to convert a `Script` into a closed Plutarch term, we must assume
  that the `Script` follows the signature of the term in question. Worse, we can
  never verify this without executing the result (thanks to the halting problem,
  among other things).
* We cannot use an encoding that substantially differs from other `Script`s for
  this case, or we completely lose the point of interoperability of YTxP.

To address these, we take the following approach:

* When serializing a closed Plutarch term, a term that compiles successfully
  will be serialized exactly as its equivalent `Script` would be. A term that
  fails to compile will instead be serialized as its error message, prefixed by
  `1x`.
* When we deserialize a closed Plutarch term, provided it deserializes as a
  `Script`, we assume that the signature required is the genuine one without
  checking anything. If it fails to deserialize as a `Script`, and we detect a
  `1x` prefix, our error message specifies that we tried to deserialize a
  `Script` that didn't compile, and present the error message so encoded as
  well.

This approach, while not ideal, solves the issue of representational uniformity
among `Script`s, whether they are represented as Plutarch terms or not. Since
_anything_ prefixed by `1x` cannot deserialize into a `Script`, we can use this
as a form of specialized 'NaN boxing' to deal with partiality in serialization:
we effectively 'kick the can down the road' to deserialization time, where
failure _is_ allowed. Other implementations would simply read our specialized
encoding for compilation failures as an invalid representation for a `Script`,
without having to handle the special case we need (unless they want to). Because
the final result is a JSON string, we can use the special case to store the
error message produced by compilation, which can help debugging without
interfering with anyone else's implementations.

As compiling a Plutarch closed term requires a `Config`, we cannot supply helper
`newtype`s without requiring them to also redundantly wrap a `Config`. Instead,
we provide helper functions for serialization, and deserialization, of closed
terms representing validators and minting policies:

* `toJSONPValidator` and `toJSONPMintingPolicy` for serializing validator and
  minting policy representations respectively
* `parseJSONPValidator` and `parseJSONPMintingPolicy` for deserializing
  validator and minting policy representations respectively

These should be used for defining all Aeson instances for types which have
Plutarch closed terms as fields or members.

TODO: This is a somewhat suboptimal choice, due to the _severe_ amount of
impredicative returns we have to employ (and the broken `do` notation this
results in), and is also quite inefficient due to the non-composable nature of
`Encoding` when used via a mechanism _other_ than type classes. While in this
case, it probably doesn't matter much, it'd be good to see if we hurt too much
by doing this.

## Plutarch `Config` serialization

Since Plutarch does not provide a way to serialize `Config`, and we need to in
at least one setting, we specify a serialization as a JSON string specifying
tracing mode. We provide the `WrappedConfig` newtype helper to make this easier.

## Record serialization

We serialize records as objects, whose keys are strings identical to the field
names as given by the definition, and whose values are the field values,
recursively encoded. This matches the standard practice of Aeson.

We give the following example of how to write such an encoding:

```haskell
data Foo a = Foo { bar :: a, baz :: Integer }

instance (ToJSON a) => ToJSON (Foo a) where
   toJSON foo = object ["bar" .= bar foo, "baz" .= baz foo]
   toEncoding foo = pairs $ "bar" .= bar foo <> "baz" .= baz foo

instance (FromJSON a) => FromJSON (Foo a) where
   parseJSON = withObject "Foo" $ \obj -> do
      fooBar <- obj .: "bar"
      fooBaz <- obj .: "baz"
      -- Can use record constructor syntax too
      pure $ Foo fooBar fooBaz
```

## Standards

### No use of `Generic` derivations for JSON instances

TODO: Justifications about why Aeson is absolutely terrible, why Generic is
_also_ terrible, and why their intersection of terribleness is quadratically
worse than both.

### Always define `toEncoding` in addition to `toJSON`

TODO: Explanation of why `toEncoding` exists, and why we should make efforts to
define it.

### Use labelled derivation helpers

TODO: Explain why `HexStringScript` has a `Symbol` labelling it, and how this
helps error messages while still giving us good derivations.

### Use helpers for object decoding

TODO: Explain why `withObject` is better for error reporting at least.
