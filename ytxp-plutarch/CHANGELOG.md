# ytxp-plutarch changelog

<!-- scriv-insert-here -->

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
