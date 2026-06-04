## 1. Axis vocabulary (reuse task_axes())

- [x] 1.1 Reuse the internal `task_axes(step)` (in `R/task-lists.R`, as folded by `task-list-loop-baseline-fold`) as the per-step vocabulary for `sample`/`fit`/`hc`; do not add a second constant. Ensure it is reachable from `R/scenario.R` (same package namespace)
- [x] 1.2 Update `scenario_default_partition_by()` to the three-step defaults: `sample` = `c("dataset", "sim", "replace")`, `fit` = `c("dataset", "sim", "nrow", "rescale")`, `hc` = `c("dataset", "sim")` (`nrow` shards at the `fit` level)

## 2. Validation and bundle normalization

- [x] 2.1 Add `validate_partition_by(partition_by, bundle, call)` accepting two optional named lists, each keyed by `sample`/`fit`/`hc` and possibly **partial**; require each is a named list with valid step names
- [x] 2.2 For each named entry (in either arg), require the value is a character vector, unique, non-`NA`, and a subset of `task_axes(step)`; abort (user-facing frame) naming the offending step/axis; loop, do not `purrr::walk`
- [x] 2.3 Reject `"nrow"` **only** under the `sample` step's `partition_by`, with a bespoke message (shared draw has no `nrow`; `fit` truncates it inline); accept `nrow` for `fit`/`hc` (`nrow` is absent from `sample`'s vocabulary, so it is also rejected under a `sample` `bundle` as an unknown axis)
- [x] 2.4 Reject any step named in **both** `partition_by` and `bundle`, aborting with an informative error naming that step
- [x] 2.5 **Normalize** to a complete stored `partition_by`: path from `partition_by[[step]]`; from a `bundle[[step]]` take `setdiff(task_axes(step), bundle[[step]])`; default for steps named in neither. Store only the resulting `partition_by`
- [x] 2.6 Validation is **per-step only** — add **no** cross-step parent-consistency check (steps partition independently; m:n is resolved at the read layer); replace the shallow `chk::chk_null_or(partition_by, vld = chk::vld_list)` call with `validate_partition_by()` after normalization

## 3. Path / inner accessor

- [x] 3.1 Add internal `scenario_partition_axes(scenario, step)` returning `list(path = ..., inner = setdiff(task_axes(step), path))`
- [x] 3.2 Document the path-vs-inner semantics (shard count = `Π |path axis|`; inner = Parquet columns) and that the path axes are what a partition-aware `path_key()` keys on; mark this accessor as the consumer hook for `task-tables`/`hive-partitioning`

## 4. Print path

- [x] 4.1 Render **both** the `partition_by` (across-shards) path axes and the `bundle` (within-shard) inner axes per step (`sample`/`fit`/`hc`) in `print.ssdsims_scenario()`, deriving the inner axes via `scenario_partition_axes()`

## 5. Docs

- [x] 5.1 Document `@param partition_by` and the new `@param bundle` on `ssd_define_scenario()`: the per-step named-list contract, the per-step vocabularies (reused from `task_axes()`), the `sample`-only `nrow` rejection, that the two are complementary per-step entry points (at most one per step, may mix across steps, partial allowed, unnamed steps default, a step in both aborts), that they normalize to the stored `partition_by`, and that validation is per-step only (no cross-step constraint; m:n accepted)
- [x] 5.2 Note the three-step defaults supersede §5's pre-fold table; cross-reference `hive-partitioning` for the shard-level behaviour and that the byte-identical acceptance test lands there

## 6. Tests and checks

- [x] 6.1 `tests/testthat/test-scenario.R`: three-step defaults populated when absent; valid `partition_by` override stored verbatim
- [x] 6.2 Rejection tests: unknown axis, `nrow` under `sample`, duplicate/`NA` axis names, a step named in **both** `partition_by` and `bundle` — each errors in the `ssd_define_scenario()` frame. Acceptance test: a parent-inconsistent split (a child finer/coarser than its parent on a shared axis) is **accepted** (no cross-step check)
- [x] 6.3 `bundle`/mixing tests: `bundle` entry normalizes to the path complement; `partition_by` and `bundle` mixed across steps accepted; a partial spec defaults the unnamed steps; the stored `partition_by` is the complete three-step path list in each case
- [x] 6.4 Acceptance test: `nrow` accepted as a `fit`/`hc` path axis
- [x] 6.5 `scenario_partition_axes()` tests: inner = complement of `task_axes(step)`; all-axes-in-path ⇒ empty inner; per-step vocabularies equal `task_axes(step)`
- [x] 6.6 Snapshot test for `print.ssdsims_scenario()` showing **both** the path (`partition_by`) and inner (`bundle`) axes per step
- [x] 6.7 Run `devtools::document()`, `air format .`, and `devtools::check()`; update `NAMESPACE`/`man/`
