# ytxp-sdk changelog

<!-- scriv-insert-here -->

<a id='changelog-0.2.1'></a>
## 0.2.1 — 2025-10-29

### Breaking

- `SdkParameters` refactored:
  The following fields have been replaced by a single field, `validatorsNonceList`:
  - `stakingValidatorsNonceList`
  - `mintingPoliciesNonceList`
  - `certifyingValidatorsNonceList`
  - `votingValidatorsNonceList`
  - `proposingValidatorsNonceList`

### Non-Breaking

- `YieldingRedeemer` is now an instance of `HasBlueprintDefinition`.
<!-- scriv-end-here -->
