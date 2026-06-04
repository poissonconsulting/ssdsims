## 1. Axis vocabulary (reuse task_axes())

- [ ] 1.1 Reuse the internal `task_axes(step)` (in `R/task-lists.R`, as folded by `task-list-loop-baseline-fold`) as the per-step vocabulary for `sample`/`fit`/`hc`; do not add a second constant. Ensure it is reachable from `R/scenario.R` (same package namespace)
- [ ] 1.2 Update `scenario_default_partition_by()` to the three-step defaults: `sample` = `c("dataset", "sim", "replace")`, `fit` = `c("dataset", "sim", "nrow", "rescale")`, `hc` = `c("dataset", "sim")` (`nrow` shards at the `fit` level)

## 2. Validation

- [ ] 2.1 Add `validate_partition_by(partition_by, call)` requiring a named list with `sample`, `fit`, `hc` entries (all three present when supplied)
- [ ] 2.2 For each step, require the entry is a character vector, unique, non-`NA`, and a subset of `task_axes(step)`; abort (user-facing frame) naming the offending step/axis; loop, do not `purrr::walk`
- [ ] 2.3 Reject `"nrow"` **only** under the `sample` step, with a bespoke message (shared draw has no `nrow`; `fit` truncates it inline); accept `nrow` for `fit`/`hc`
- [ ] 2.4 Validation is **per-step only** — add **no** cross-step parent-consistency check (steps partition independently; m:n is resolved at the read layer); replace the shallow `chk::chk_null_or(partition_by, vld = chk::vld_list)` call with `validate_partition_by()` after the default is applied

## 3. Path / inner accessor

- [ ] 3.1 Add internal `scenario_partition_axes(scenario, step)` returning `list(path = ..., inner = setdiff(task_axes(step), path))`
- [ ] 3.2 Document the path-vs-inner semantics (shard count = `Π |path axis|`; inner = Parquet columns) and that the path axes are what a partition-aware `path_key()` keys on; mark this accessor as the consumer hook for `task-tables`/`hive-partitioning`

## 4. Print path

- [ ] 4.1 Render `partition_by` path axes per step (`sample`/`fit`/`hc`) in `print.ssdsims_scenario()`

## 5. Docs

- [ ] 5.1 Update `ssd_define_scenario()` roxygen `@param partition_by` to document the three-step named-list contract, the per-step vocabularies (reused from `task_axes()`), the `sample`-only `nrow` rejection, that validation is per-step only (no cross-step constraint; m:n accepted), and the all-or-nothing override
- [ ] 5.2 Note the three-step defaults supersede §5's pre-fold table; cross-reference `hive-partitioning` for the shard-level behaviour and that the byte-identical acceptance test lands there

## 6. Tests and checks

- [ ] 6.1 `tests/testthat/test-scenario.R`: three-step defaults populated when absent; valid override stored verbatim
- [ ] 6.2 Rejection tests: missing step entry, unknown axis, `nrow` under `sample`, duplicate/`NA` axis names — each errors in the `ssd_define_scenario()` frame. Acceptance test: a parent-inconsistent split (a child finer/coarser than its parent on a shared axis) is **accepted** (no cross-step check)
- [ ] 6.3 Acceptance test: `nrow` accepted as a `fit`/`hc` path axis
- [ ] 6.4 `scenario_partition_axes()` tests: inner = complement of `task_axes(step)`; all-axes-in-path ⇒ empty inner; per-step vocabularies equal `task_axes(step)`
- [ ] 6.5 Snapshot test for `print.ssdsims_scenario()` showing the three-step `partition_by`
- [ ] 6.6 Run `devtools::document()`, `air format .`, and `devtools::check()`; update `NAMESPACE`/`man/`
