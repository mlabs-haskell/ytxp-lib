# Notes on JSON serialization

## `Script` serialization

The standard serialization method for Plutarch `Script`s effectively [wraps
`serialiseScript`](https://github.com/Plutonomicon/plutarch-plutus/blob/master/Plutarch/Script.hs#L13).
`serialiseScript` comes from the Plutus ledger API, and involves [serializing
via `Flat`, then packing into a
`ShortByteString`](https://github.com/IntersectMBO/plutus/blob/master/plutus-ledger-api/src/PlutusLedgerApi/Common/SerialisedScript.hs#L151).
Given that this appears to be a (relatively) standard form, we adopt this
approach. However, we must still decide an encoding of binary data into JSON: we
choose hex encoding, with an `0x` prefix, as this is clear to humans reading the
serialized form and relatively easy to work with.

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
