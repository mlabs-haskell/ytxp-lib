# Types

This documentation for the YTxP architecture does not mandate specific types to be implemented by libraries.
However, we do benefit from some informal description of what those types should be.

For brevity, we use Haskell syntax.
The types given should be considered suggestions, and implementors are welcome to propose changes to the specification if it becomes clear that more refined, specific types are beneficial to the architecture.

## Redeemers

### AuthorisedScriptProofIndex

```hs
data AuthorisedScriptPurpose = Minting | Spending | Rewarding

type AuthorisedScriptProofIndex = (AuthorisedScriptPurpose, Integer)
```

`Delegating` (certificate) evidence is not implemented: the library implements the minting,
spending and rewarding branches only.

## Control Parameters

These parameters are used across the various control scripts and must be known before the
scripts are compiled.

<a id="orgaa06230"></a>

### SdkParameters

If your onchain framework allows it to be done efficiently, they can be passed in as a `Reader`.

```hs
-- | Parameters available during compilation (therefore not containing any script hashes).
data SdkParameters = SdkParameters
  { validatorsNonceList :: ![Natural]
    -- ^ one yielding script is compiled for each nonce
  , authorisedScriptsSTCS :: AuthorisedScriptsSTCS
    -- ^ the currency symbol of the token that identifies authorised reference scripts
  }
```
