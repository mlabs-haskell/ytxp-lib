# ytxp-plutarch changelog

<!-- scriv-insert-here -->

<a id='changelog-0.3.0'></a>
## 0.3.0 — 2026-10-26

### Breaking

- The yielding script now takes its STCS argument `Data`-encoded
  (`PAsData PCurrencySymbol`) rather than as a raw bytestring.
  All yielding script hashes change.

### Non-Breaking

- The generated blueprint now identifies each validator by its nonce in
  `validatorTitle`, and records the authorised-scripts STCS in `validatorDescription`,
  instead of giving every validator the same title.

<a id='changelog-0.2.1'></a>
## 0.2.1 — 2025-10-29

### Breaking

- `Params` refactored:
  The following fields have been replaced by a single field, `numYieldingValidators`:
  - `numYieldingSV`
  - `numYieldingMP`
  - `numYieldingCV`
  - `numYieldingVV`
  - `numYieldingPV`

### Non-Breaking

- `Params` is now an instance of `Show`.
<!-- scriv-end-here -->
