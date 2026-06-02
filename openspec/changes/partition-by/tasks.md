## 1. Axis vocabulary

- [ ] 1.1 Add an internal `scenario_axis_vocab(step)` in `R/scenario.R` returning the documented per-step axis names (§5): `data ⊆ fit ⊆ hc`, with `nrow` deliberately excluded
- [ ] 1.2 Have `scenario_default_partition_by()` continue to return the §5 defaults (no change to defaults)

## 2. Validation

- [ ] 2.1 Add `validate_partition_by(partition_by, call)` requiring a named list with `data`, `fit`, `hc` entries (all three present when supplied)
- [ ] 2.2 For each step, require the entry is a character vector, unique, non-`NA`, and a subset of `scenario_axis_vocab(step)`; abort (user-facing frame) naming the offending step/axis; loop, do not `purrr::walk`
- [ ] 2.3 Reject `"nrow"` with a bespoke message (sub-truncation column, never a path axis)
- [ ] 2.4 Replace the shallow `chk::chk_null_or(partition_by, vld = chk::vld_list)` call with `validate_partition_by()` after the default is applied

## 3. Path / inner accessor

- [ ] 3.1 Add internal `scenario_partition_axes(scenario, step)` returning `list(path = ..., inner = setdiff(vocab, path))`
- [ ] 3.2 Document the path-vs-inner semantics (shard count = `Π |path axis|`; inner = Parquet columns) and that this accessor is the consumer hook for `task-tables`/`hive-partitioning`

## 4. Print path

- [ ] 4.1 Render `partition_by` path axes per step (`data`/`fit`/`hc`) in `print.ssdsims_scenario()`

## 5. Docs

- [ ] 5.1 Update `ssd_define_scenario()` roxygen `@param partition_by` to document the named-list contract, the per-step vocabularies, the `nrow` exclusion, and the all-or-nothing override rule
- [ ] 5.2 Cross-reference `hive-partitioning` for the shard-level behaviour and note the byte-identical acceptance test lands there

## 6. Tests and checks

- [ ] 6.1 `tests/testthat/test-scenario.R`: defaults populated when absent; valid override stored verbatim
- [ ] 6.2 Rejection tests: missing step entry, unknown axis, `nrow` as path axis, duplicate/`NA` axis names — each errors in the `ssd_define_scenario()` frame
- [ ] 6.3 `scenario_partition_axes()` tests: inner = complement; all-axes-in-path ⇒ empty inner; per-step vocabularies are step-specific
- [ ] 6.4 Snapshot test for `print.ssdsims_scenario()` showing `partition_by`
- [ ] 6.5 Run `devtools::document()`, `air format .`, and `devtools::check()`; update `NAMESPACE`/`man/`
