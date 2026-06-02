## 1. Axis vocabulary (reuse #80)

- [ ] 1.1 Reuse #80's internal `task_axes(step)` (in `R/task-lists.R`) as the per-step vocabulary for `sample`/`data`/`fit`/`hc`; do not add a second constant. Ensure it is reachable from `R/scenario.R` (same package namespace)
- [ ] 1.2 Update `scenario_default_partition_by()` to the four-step defaults: `sample` = `c("dataset", "sim", "replace")`, `data` = `c("dataset", "sim", "replace", "nrow")`, `fit` = `c("dataset", "sim", "rescale")`, `hc` = `c("dataset", "sim")` (`nrow` in the `data` path — high-res first level)

## 2. Validation

- [ ] 2.1 Add `validate_partition_by(partition_by, call)` requiring a named list with `sample`, `data`, `fit`, `hc` entries (all four present when supplied)
- [ ] 2.2 For each step, require the entry is a character vector, unique, non-`NA`, and a subset of `task_axes(step)`; abort (user-facing frame) naming the offending step/axis; loop, do not `purrr::walk`
- [ ] 2.3 Reject `"nrow"` **only** under the `sample` step, with a bespoke message (shared draw has no `nrow`; it truncates at the `data` step); accept `nrow` for `data`/`fit`/`hc`
- [ ] 2.4 Parent-consistency check: for each non-root step, `intersect(path[[step]], task_axes(parent))` SHALL be a subset of `path[[parent]]` (chain `sample ← data ← fit ← hc` via #80's `task_parent()`); abort otherwise
- [ ] 2.5 Replace the shallow `chk::chk_null_or(partition_by, vld = chk::vld_list)` call with `validate_partition_by()` after the default is applied

## 3. Path / inner accessor

- [ ] 3.1 Add internal `scenario_partition_axes(scenario, step)` returning `list(path = ..., inner = setdiff(task_axes(step), path))`
- [ ] 3.2 Document the path-vs-inner semantics (shard count = `Π |path axis|`; inner = Parquet columns) and that the path axes are what a partition-aware `path_key()` keys on; mark this accessor as the consumer hook for `task-tables`/`hive-partitioning`

## 4. Print path

- [ ] 4.1 Render `partition_by` path axes per step (`sample`/`data`/`fit`/`hc`) in `print.ssdsims_scenario()`

## 5. Docs

- [ ] 5.1 Update `ssd_define_scenario()` roxygen `@param partition_by` to document the four-step named-list contract, the per-step vocabularies (reused from #80's `task_axes()`), the `sample`-only `nrow` rejection, the parent-consistency rule, and the all-or-nothing override
- [ ] 5.2 Note the four-step defaults supersede §5's pre-#80 table; cross-reference `hive-partitioning` for the shard-level behaviour and that the byte-identical acceptance test lands there

## 6. Tests and checks

- [ ] 6.1 `tests/testthat/test-scenario.R`: four-step defaults populated when absent; valid override stored verbatim
- [ ] 6.2 Rejection tests: missing step entry, unknown axis, `nrow` under `sample`, duplicate/`NA` axis names, parent-inconsistent child path — each errors in the `ssd_define_scenario()` frame
- [ ] 6.3 Acceptance test: `nrow` accepted as a `data`/`fit`/`hc` path axis
- [ ] 6.4 `scenario_partition_axes()` tests: inner = complement of `task_axes(step)`; all-axes-in-path ⇒ empty inner; per-step vocabularies equal `task_axes(step)`
- [ ] 6.5 Snapshot test for `print.ssdsims_scenario()` showing the four-step `partition_by`
- [ ] 6.6 Run `devtools::document()`, `air format .`, and `devtools::check()`; update `NAMESPACE`/`man/`
