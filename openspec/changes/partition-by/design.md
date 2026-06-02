## Context

`ssd-define-scenario` landed `partition_by` as a stored field with a default fallback, but its only validation is `chk::chk_null_or(partition_by, vld = chk::vld_list)`. `task-list-loop-baseline` (#80) has since landed the four per-step task tables — `sample`, `data`, `fit`, `hc` — together with `task_axes(step)` (the cumulative cross-join axes per step) and `add_task_ids()`/`path_key()`, which mint a path-style `<step>_id` over **all** of a step's axes plus the parent's `<parent>_id` foreign key. Per `TARGETS-DESIGN.md` §5, `partition_by` generalises that fixed path key into a configurable one: a step's chosen path axes become Hive directory levels (one shard per path cell, `Π |path axis|` shards) and the rest are inner Parquet columns. This change gives the knob a real contract; `task-tables`/`hive-partitioning` then make `path_key()` key on the chosen subset.

## Goals / Non-Goals

**Goals:**

- Validate a supplied `partition_by` as a named list with `sample`/`data`/`fit`/`hc` entries (one per #80 step), each a subset of that step's `task_axes()`, unique and non-missing.
- Reuse #80's `task_axes()` as the vocabulary source of truth (no second constant); reject `nrow` only for the `sample` step, and unknown names everywhere.
- Validate parent-consistency so the `<parent>_id` foreign key stays well-defined.
- Define and expose the path-vs-inner split via an internal accessor that `task-tables`/`hive-partitioning` call; supply four-step defaults as the `NULL` fallback; render path axes in `print()`.

**Non-Goals:**

- Changing `task_axes()`, the task tables, or `add_task_ids()`/`path_key()` themselves (those are #80, and the path-key-on-subset rewiring is `task-tables`/`hive-partitioning`).
- Building shards, Hive paths, or Parquet I/O.
- The end-to-end acceptance test that *changing* `partition_by` shifts file paths while per-task results stay byte-identical: it needs shards, so it lands with `hive-partitioning`. Here the contract is validated at the scenario level only.

## Decisions

### Decision: reuse #80's `task_axes()` as the vocabulary — do not duplicate

`task-list-loop-baseline` already owns the per-step axis vocabulary as an internal `task_axes(step)` in `R/task-lists.R`:

```r
sample <- c("dataset", "sim", "replace")
data   <- c(sample, "nrow")
fit    <- c(data, "rescale", "computable", "at_boundary_ok",
            "min_pmix", "range_shape1", "range_shape2")
hc     <- c(fit, "ci", "nboot", "est_method", "ci_method", "parametric")
```

`partition_by` validation and the path/inner split read from this same function, so there is exactly one source of truth and the earlier "duplicated constant could drift" risk is gone. *Why reuse rather than re-declare?* The two must agree by construction — they describe the same task tables — and #80 is the table author. (If `task_axes()` needs to be reachable cleanly, expose it package-internally; it is already defined in the same package.)

### Decision: `nrow` is a real axis now; reject it only for the `sample` step

#80 reorganises the §5 sub-truncation property *structurally*: the single random draw is the **`sample`** task (keyed `dataset, sim, replace`, carrying `n_max = max(nrow)`), and the **`data`** task is the RNG-free `head(sample, nrow)` truncation, where `nrow` is a genuine cross-join axis. So `nrow` is valid in `data`/`fit`/`hc` path axes and is rejected only under `sample` (which has no `nrow`), with a bespoke message pointing at the shared-draw rationale. This supersedes the draft's blanket "`nrow` is never an axis".

### Decision: `partition_by` requires all four step entries when supplied

If a user supplies `partition_by` at all, they supply the full `sample`/`data`/`fit`/`hc` named list — no partial merge onto defaults. *Why:* a partial merge hides which axes are path vs inner for the omitted steps and makes the stored field ambiguous to read back. Explicit-and-complete keeps the scenario self-describing; `NULL` → documented defaults covers the common case. *Alternative considered:* per-step merge onto defaults — rejected for that ambiguity (a `modifyList()`-on-defaults helper can be added later without changing the stored contract).

### Decision: four-step defaults map §5 onto the sample/data split

The §5 default table predates #80's `sample`/`data` split, so the defaults are restated for four steps. The old "data" default (the draw shard) becomes the **`sample`** default; the new **`data`** step shards by `nrow` too, so each truncation size is its own shard:

```r
sample = c("dataset", "sim", "replace")
data   = c("dataset", "sim", "replace", "nrow")   # one shard per truncation size
fit    = c("dataset", "sim", "rescale")
hc     = c("dataset", "sim")
```

*Why `nrow` in the `data` path by default?* High-resolution sharding at the first (`data`) level is fine — finer shards cost little here and keep each truncation independently addressable, which suits selective re-runs and per-`nrow` queries. The shared draw is already de-duplicated upstream at the `sample` step, so per-`nrow` `data` shards do not re-draw. A user who wants coarser `data` shards drops `"nrow"` from the path (making it an inner column). Downstream steps (`fit`/`hc`) keep `nrow` inner by default, since they shard on their own argument axes.

### Decision: inner axes are the lazy complement, exposed via an accessor

```r
scenario_partition_axes <- function(scenario, step) {
  path  <- scenario$partition_by[[step]]
  inner <- setdiff(task_axes(step), path)
  list(path = path, inner = inner)
}
```

The single accessor `task-tables`/`hive-partitioning` call. Storing only the path axes (and computing the inner complement) keeps `partition_by` the lone source of truth and the manifest compact. The path axes are exactly what a partition-aware `path_key()` will key on.

### Decision: validate in the user-facing frame, incl. parent-consistency

Thread the constructor's `call` into `validate_partition_by(partition_by, call)`. Loop over `c("sample", "data", "fit", "hc")` (a plain loop, not `purrr::walk`, per the error-origin convention). Per step: the entry exists, is a unique, non-`NA` character vector, and `setdiff(entry, task_axes(step))` is empty (`nrow`-under-`sample` gets the bespoke message). Then a **parent-consistency** check: for each non-root step, `intersect(path[[step]], task_axes(parent))` must be a subset of `path[[parent]]`, so a child shard maps to exactly one parent shard via `<parent>_id` (#80's `task_parent()` gives the chain `sample ← data ← fit ← hc`). Without it a child could be partitioned more finely on a shared axis than its parent, breaking the foreign-key join.

## Risks / Trade-offs

- **`task_axes()` is currently internal to #80** → reuse needs it reachable from `R/scenario.R`. Mitigation: same package, so a direct internal call works; if file-load order matters, keep both in the package namespace (no export needed).
- **Parent-consistency rule adds validation surface** → more to get right and to explain. Mitigation: it is a single `subset` check per non-root step with a targeted error; tested directly (a child finer than its parent on a shared axis aborts).
- **Restated defaults differ from the §5 table** → readers of §5 may expect three steps. Mitigation: the design and roxygen state the four-step defaults explicitly and note they supersede §5's pre-#80 table.
- **Acceptance test deferred to `hive-partitioning`** → the headline "changing `partition_by` shifts shards, results byte-identical" property is not verified here. Mitigation: scope is explicit (knob contract only); `hive-partitioning` owns the end-to-end test once `path_key()` keys on the chosen subset.

## Open Questions

- **List-valued axes as path levels** (`min_pmix` name, `range_shape1/2`): #80's `path_key()` already renders list columns element-wise (`paste0(..., collapse = ",")`), so they *can* form a path segment — resolving the earlier doubt. Whether sharding by `range_shape1` is *useful* (vs. always inner) is a usage question `hive-partitioning` can revisit; the contract permits it.
- **`replace` field on the scenario**: #80 reads `scenario$replace` (defaulting to `FALSE`). `replace` is in the `sample` vocabulary regardless; if a future `replace` knob lands on `ssd_define_scenario()` it flows through unchanged.
