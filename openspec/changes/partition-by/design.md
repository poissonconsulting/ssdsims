## Context

`ssd-define-scenario` landed `partition_by` as a stored field with a default fallback, but its only validation is `chk::chk_null_or(partition_by, vld = chk::vld_list)`. `task-list-loop-baseline` (#80), as expanded by `task-list-loop-baseline-fold`, has since landed the three per-step task tables — `sample`, `fit`, `hc` (the `data` truncation is folded into `fit`) — together with `task_axes(step)` (the cumulative cross-join axes per step) and `add_task_ids()`/`path_key()`, which mint a path-style `<step>_id` over **all** of a step's axes plus the parent's `<parent>_id` foreign key. Per `TARGETS-DESIGN.md` §5, `partition_by` configures the **Hive shard path**: a step's chosen path axes become Hive directory levels (one shard per path cell, `Π |path axis|` shards) and the rest are inner Parquet columns. This change gives the knob a real contract; `task-tables`/`hive-partitioning` then render the shard path from the chosen subset. The `<step>_id`/`<parent>_id` **task-identity** keys are *not* affected — they remain `path_key()` over all of `task_axes(step)`, unique per task and `partition_by`-independent (see the decision below).

## Goals / Non-Goals

**Goals:**

- Validate a supplied `partition_by` as a named list with `sample`/`fit`/`hc` entries (one per step), each a subset of that step's `task_axes()`, unique and non-missing.
- Reuse `task_axes()` as the vocabulary source of truth (no second constant); reject `nrow` only for the `sample` step, and unknown names everywhere.
- Define and expose the path-vs-inner split via an internal accessor that `task-tables`/`hive-partitioning` call; supply three-step defaults as the `NULL` fallback; render path axes in `print()`.

**Non-Goals:**

- Changing `task_axes()`, the task tables, or `add_task_ids()`/`path_key()` themselves (those are #80; rendering the Hive **shard path** from the subset is `task-tables`/`hive-partitioning`, and it adds a shard-path key rather than replacing the `<step>_id` task identity).
- **Any cross-step parent-consistency constraint.** Steps partition independently; m:n child↔parent shard relationships are accepted and resolved at the read layer (`shard-runner-baseline`/`task-tables`), not constrained here (see the decision below).
- Building shards, Hive paths, or Parquet I/O.
- The end-to-end acceptance test that *changing* `partition_by` shifts file paths while per-task results stay byte-identical: it needs shards, so it lands with `hive-partitioning`. Here the contract is validated at the scenario level only.

## Decisions

### Decision: reuse `task_axes()` as the vocabulary — do not duplicate

`task-list-loop-baseline` (folded) owns the per-step axis vocabulary as an internal `task_axes(step)` in `R/task-lists.R`:

```r
sample <- c("dataset", "sim", "replace")
fit    <- c(sample, "nrow", "rescale", "computable", "at_boundary_ok",
            "min_pmix", "range_shape1", "range_shape2")
hc     <- c(fit, "ci", "nboot", "est_method", "ci_method", "parametric")
```

`partition_by` validation and the path/inner split read from this same function, so there is exactly one source of truth and the earlier "duplicated constant could drift" risk is gone. *Why reuse rather than re-declare?* The two must agree by construction — they describe the same task tables — and `task-list-loop-baseline` is the table author. (If `task_axes()` needs to be reachable cleanly, expose it package-internally; it is already defined in the same package.)

### Decision: `nrow` is a real `fit` axis; reject it only for the `sample` step

The §5 sub-truncation property is preserved structurally: the single random draw is the **`sample`** task (keyed `dataset, sim, replace`, carrying `n_max = max(nrow)`), and `fit` truncates it inline (`head(sample, nrow)`, RNG-free) so `nrow` is a genuine `fit` cross-join axis (inherited by `hc`). So `nrow` is valid in `fit`/`hc` path axes and is rejected only under `sample` (which has no `nrow`), with a bespoke message pointing at the shared-draw rationale. This supersedes the draft's blanket "`nrow` is never an axis".

### Decision: `partition_by` requires all three step entries when supplied

If a user supplies `partition_by` at all, they supply the full `sample`/`fit`/`hc` named list — no partial merge onto defaults. *Why:* a partial merge hides which axes are path vs inner for the omitted steps and makes the stored field ambiguous to read back. Explicit-and-complete keeps the scenario self-describing; `NULL` → documented defaults covers the common case. *Alternative considered:* per-step merge onto defaults — rejected for that ambiguity (a `modifyList()`-on-defaults helper can be added later without changing the stored contract).

### Decision: three-step defaults; `nrow` shards at the `fit` level

The §5 default table predates the `sample` split and the data fold, so the defaults are restated for three steps. The old "data" default (the draw shard) becomes the **`sample`** default; `nrow` now shards at the **`fit`** level (each truncation size its own fit shard):

```r
sample = c("dataset", "sim", "replace")
fit    = c("dataset", "sim", "nrow", "rescale")   # one shard per (nrow, rescale)
hc     = c("dataset", "sim")
```

*Why `nrow` in the `fit` path by default?* High-resolution sharding is fine — finer shards cost little (datasets are tiny) and keep each `(nrow, rescale)` fit independently addressable, which suits selective re-runs and per-`nrow` queries. The shared draw is already de-duplicated upstream at the `sample` step, so per-`nrow` fit shards do not re-draw (each just re-`head()`s the cheap sample). A user who wants coarser fit shards drops `"nrow"` from the path (making it an inner column). `hc` keeps `nrow` inner by default, sharding on its own coarser axes.

### Decision: inner axes are the lazy complement, exposed via an accessor

```r
scenario_partition_axes <- function(scenario, step) {
  path  <- scenario$partition_by[[step]]
  inner <- setdiff(task_axes(step), path)
  list(path = path, inner = inner)
}
```

The single accessor `task-tables`/`hive-partitioning` call. Storing only the path axes (and computing the inner complement) keeps `partition_by` the lone source of truth and the manifest compact. The path axes are exactly what the partition-aware **shard path** keys on — distinct from the `<step>_id` task-identity key, which stays over all of `task_axes(step)`.

### Decision: validate in the user-facing frame (per-step only)

Thread the constructor's `call` into `validate_partition_by(partition_by, call)`. Loop over `c("sample", "fit", "hc")` (a plain loop, not `purrr::walk`, per the error-origin convention). Per step: the entry exists, is a unique, non-`NA` character vector, and `setdiff(entry, task_axes(step))` is empty (`nrow`-under-`sample` gets the bespoke message). There is **no cross-step check** — each step's path axes are validated against its own vocabulary alone (see the next decision).

### Decision: no parent-consistency rule — accept m:n

An earlier draft of this change validated **parent-consistency** (`intersect(path[[step]], task_axes(parent)) ⊆ path[[parent]]`), justified as "so a child shard maps to exactly one parent shard". That rule is dropped. Three reasons:

1. **The rationale is backwards.** A child shard maps to *one* parent shard only when `path[parent] ⊆ path[child]` (the child at least as fine as the parent on shared axes) — the *opposite* inclusion to the rule. The rule as written permits exactly the m:n it claimed to forbid.
2. **The shipped defaults are m:n by design.** They *coarsen* downstream (`sample` keyed by `dataset,sim,replace`; `fit` by `dataset,sim,nrow,rescale`; `hc` by `dataset,sim`), so an `hc` shard reads every `fit` shard for its `(dataset,sim)` across `nrow`/`rescale`, and with `replace = c(F,T)` a `fit` shard reads two `sample` shards. No one-parent-shard rule (in either direction) is compatible with these defaults.
3. **m:n needs no constraint to be correct.** Splitting/over-reading a parent shard is cheap: a child reads the set of parent shards its tasks reference and filters on path/inner columns (duckplyr predicate pushdown). So the relationship is resolved at the **read layer** — `shard-runner-baseline` proves it single-core, and the `targets` layer (`task-tables`, **Option 3**: splice each child branch's computed upstream target-name set at sourcing time) keeps per-shard invalidation under m:n. `partition_by` therefore stays purely per-step. The `<parent>_id` foreign key (full parent identity, minted by #80) is always well-defined regardless of `partition_by`; only the *shard* a parent task lives in is a coarsening of it, computed downstream.

### Decision: the `<step>_id`/`<parent>_id` keys are task identity, not the shard path

The primary and foreign keys are **task identity** — `path_key()` over *all* of `task_axes(step)` (#80) — unique per task and **`partition_by`-independent**. The **Hive shard path** is a *different* projection — `path_key(path[step])` over the partition subset — shared by every task in a shard and `partition_by`-dependent. The two coincide only when each shard holds one task; under any coarser `partition_by` the shard path is a coarsening of the identity key (so it cannot itself be the primary key — many tasks share it). This change configures the **shard path** (via `scenario_partition_axes()`); it leaves the identity keys untouched. *Why this matters:* the identity key is what the per-task primer hashes and what the `<parent>_id` foreign-key join uses, so anchoring it to the full axes (never the partition subset) is what keeps `partition_by` a free re-layout — change the split and only file paths move, never identity, RNG, or results. Earlier phrasings that said `partition_by` makes `path_key()`/`<step>_id` "key on the subset" are superseded by this: the subset drives the shard path, the identity key stays over all axes. (The `<step>_id` example `dataset=boron/sim=1/replace=FALSE` happens to equal the default sample shard path only because that default puts every sample axis in the path — the one-task-per-shard case.)

### Decision: `partition_by` is orthogonal to the per-task primer

The per-task RNG primer (`task-primer`, on `claude/gifted-lovelace-VPn0D`) is computed by `task_primers(tbl, step)` over **all** of `tbl[i, task_axes(step)]` — the full canonical identity row — *not* over the path subset. So a task's `(seed, primer)` is **invariant under `partition_by`**: re-declaring an axis as path vs inner moves only its Hive location, never its RNG. This is exactly what makes the deferred acceptance test ("changing `partition_by` shifts file paths while per-task results stay byte-identical", owned by `hive-partitioning`) hold by construction. `partition_by` validation and `scenario_partition_axes()` therefore touch storage layout only and must never feed the primer. *Implication:* the path-vs-inner split is a free re-layout knob; it cannot change results.

### Decision: `shard-completeness-assert` is a third consumer of the split; `task-tables` stores `expected_rows`

Beyond `task-tables` and `hive-partitioning`, the new `shard-completeness-assert` step (`TARGETS-DESIGN.md` §6.2/§8.4) consumes the split: a shard's **expected row count** is a function of its **inner** axes (the tasks bundled into the shard), so it is derived from `scenario_partition_axes(scenario, step)$inner`. The expected count is **stored per shard in `task-tables`** (a pure sourcing-time function) and read by the assert as a constant — `partition_by` supplies the split, not the count. Caveat for `hc`: a task's output is not one row (`proportion` fan-out, and the `ci = FALSE` collapse, §1.2), so `expected_rows ≠ |tasks|` in general; the count is summed over the shard's task rows' output cardinality in `task-tables`, not inferred from inner-axis cardinalities alone. This is a `task-tables` responsibility; `partition-by` only needs to expose the path/inner split (which it already does) and note the assert as a downstream consumer.



- **`task_axes()` is internal to `task-list-loop-baseline`** → reuse needs it reachable from `R/scenario.R`. Mitigation: same package, so a direct internal call works; if file-load order matters, keep both in the package namespace (no export needed).
- **Accepting m:n defers shard-dependency resolution downstream** → no constraint here means `partition_by` alone does not tell you a child shard maps to one parent shard. Mitigation: that resolution is intentionally a read-layer concern — `shard-runner-baseline` proves it single-core and `task-tables` (Option 3) wires it under `targets`; the `<parent>_id` foreign key stays well-defined regardless.
- **Restated defaults differ from the §5 table** → readers of §5 may expect different steps. Mitigation: the design and roxygen state the three-step defaults explicitly and note they supersede §5's pre-fold table.
- **Acceptance test deferred to `hive-partitioning`** → the headline "changing `partition_by` shifts shards, results byte-identical" property is not verified here. Mitigation: scope is explicit (knob contract only); `hive-partitioning` owns the end-to-end test once the shard path keys on the chosen subset.

## Open Questions

- **List-valued axes as path levels** (`min_pmix` name, `range_shape1/2`): #80's `path_key()` already renders list columns element-wise (`paste0(..., collapse = ",")`), so they *can* form a path segment — resolving the earlier doubt. Whether sharding by `range_shape1` is *useful* (vs. always inner) is a usage question `hive-partitioning` can revisit; the contract permits it.
- **`replace` field on the scenario**: #80 reads `scenario$replace` (defaulting to `FALSE`). `replace` is in the `sample` vocabulary regardless; if a future `replace` knob lands on `ssd_define_scenario()` it flows through unchanged.
