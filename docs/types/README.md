# Types

This documentation for the YTxP architecture does not mandate specific types to be implemented by libraries.
However, we do benefit from some informal description of what those types should be.

For brevity, we use Haskell syntax.
The types given should be considered suggestions, and implementors are welcome to propose changes to the specification if it becomes clear that more refined, specific types are beneficial to the architecture.

## Redeemers

### AuthorisedScriptProofIndex

```hs
data AuthorisedScriptScriptPurpose = Minting | Spending | Delegating | Rewarding

type AuthorisedScriptProofIndex = (ScriptPurpose, Integer)
```

## Control Parameters

These parameters are used across the various control scripts.

The parameters come in &ldquo;initial&rdquo; and regular variants.
The &ldquo;initial&rdquo; variant is the minimum set of parameters required to start compilation;
the regular variant contains additional parameters derived from compilation, such
as script hashes.

The following fields are present:

<a id="orgaa06230"></a>

### ControlParametersInitial

These must be known before compiling the scripts.
If your onchain framework allows it to be done efficiently, they can be passed in as a `Reader`.

```hs
-- | Parameters available to the YieldListValidator and YieldListMP
-- during compilation (therefore not containing any script hashes)
data ControlParametersInitial = ControlParametersInitial
  { 
  , yieldingStakingValidatorNonceList :: ![Nonce]
    -- ^ a list of nonces for the yielding staking validators.
    -- One staking validator is compiled for each nonce
  , yieldingMintingPolicyNonceList :: ![Nonce]
    -- ^ a list of nonces for the yielding minting policy.
    -- One minting policy is compiled for each nonce
  }
```

### ControlParameters

Primarily useful for library consumers and offchain implementations in particular.

``` haskell
-- | Contains the compiled scripts along with the parameters
-- they were compiled against. This is useful for _library consumers_
-- and should contain all of the information needed to work with the
-- library.
data ControlParameters = ControlParameters
  { yieldingScripts :: YieldingScripts
  , controlParametersInitial :: ControlParametersInitial
  }
```

## Scripts Types

``` haskell
-- | Scripts that yield to transaction families described by the datums guarded
-- by the YieldListScripts
data YieldingScripts = YieldingScripts
  { yieldingValidator :: YieldingValidator
  , yieldingMintingPolicies :: [YieldingMintingPolicy]
  -- ^ We have multiple of these, so we can allow differentiating tokens just by their currency symbol
  -- This makes it easier for yielded-to scripts to ensure no unexpected token is being minted
  , yieldingStakingValidators :: [YieldingStakingValidator]
  -- ^ We have multiple of these, because each can only be delegated to a single
  -- pool.
  }

```
