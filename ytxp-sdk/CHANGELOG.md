# ytxp-sdk changelog

<!-- scriv-insert-here -->

<a id='changelog-0.3.0'></a>
## 0.3.0 — 2026-10-26

### Breaking

- Removed the deprecated `Cardano.YTxP.SDK.ControlParameters` module
  (`YieldingScripts`, `ControlParameters`, `HexStringScript`, `sbsToHexText`, `hexTextToSbs`).

### Non-Breaking

- The redeemer types now carry CIP-57 blueprint schemas (`HasBlueprintSchema` and
  `HasBlueprintDefinition`), so a blueprint can emit `YieldingRedeemer` and its field
  definitions instead of referring to a definition it never writes.

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
