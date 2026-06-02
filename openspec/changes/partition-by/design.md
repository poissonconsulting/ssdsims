## Context

`ssd-define-scenario` landed `partition_by` as a stored field with a `scenario_default_partition_by()` fallback, but its only validation is `chk::chk_null_or(partition_by, vld = chk::vld_list)`. Per `TARGETS-DESIGN.md` §5, `partition_by` decides the shard topology: a step's path axes become Hive directory levels (one shard per path cell, so `Π |path axis|` shards), and the remaining axes are inner Parquet columns. `task-tables` and `hive-partitioning` will consume this split. This change gives the knob a real contract before those consumers exist; it depends only on `ssd-define-scenario`.

## Goals / Non-Goals

**Goals:**

- Validate a supplied `partition_by` as a named list with `data`/`fit`/`hc` entries, each a subset of that step's documented axis vocabulary (§5), unique and non-missing.
- Encode the per-step axis vocabulary as a single source of truth, and reject `nrow` (a sub-truncation column, never an axis) and unknown names.
- Define and expose the path-vs-inner split via an internal accessor that `task-tables`/`hive-partitioning` will call.
- Keep the documented §5 defaults as the `NULL` fallback; render path axes in `print()`.

**Non-Goals:**

- Building task tables, shards, Hive paths, or Parquet I/O — those are `task-list-loop-baseline`, `task-tables`, and `hive-partitioning`.
- The end-to-end acceptance test that *changing* `partition_by` shifts file paths while per-task results stay byte-identical: it needs shards, so it lands with `hive-partitioning`. Here the contract is validated at the scenario level only.
- Validating against *materialised* task-table columns (those don't exist yet); validation is against the documented axis vocabulary constant.

## Decisions

### Decision: per-step axis vocabulary as a single internal constant

Define one internal source of truth, e.g.

```r
scenario_axis_vocab <- function(step) {
  data <- c("dataset", "sim", "replace")
  fit  <- c(data, "rescale", "computable", "at_boundary_ok",
            "min_pmix", "range_shape1", "range_shape2")
  hc   <- c(fit, "nboot", "est_method", "ci_method", "parametric")
  switch(step, data = data, fit = fit, hc = hc)
}
```

mirroring the §5 monotone grid (`data ⊆ fit ⊆ hc`). Both validation and the path/inner split read from it, so the vocabulary is defined once. *Why a constant rather than deriving from the not-yet-built task tables?* `partition-by` is upstream of `task-tables` in the DAG; deriving from task columns would invert the dependency. When `task-tables` lands it can assert its columns match this vocabulary, keeping the two in sync from the downstream side.

### Decision: `partition_by` requires all three step entries when supplied

If a user supplies `partition_by` at all, they supply the full `data`/`fit`/`hc` named list — we do not merge a partial override onto the defaults. *Why:* a partial merge hides which axes are path vs inner for the omitted steps and makes the stored field ambiguous to read back. Explicit-and-complete keeps the scenario self-describing. The all-or-nothing default (`NULL` → documented defaults) covers the common case. *Alternative considered:* per-step merge onto defaults; rejected for the ambiguity above. (If ergonomics demand it later, a `modifyList()`-on-defaults helper can be added without changing the stored contract.)

### Decision: inner axes are the lazy complement, exposed via an accessor

Rather than store the inner axes (redundant, drift-prone), compute them on demand:

```r
scenario_partition_axes <- function(scenario, step) {
  path  <- scenario$partition_by[[step]]
  inner <- setdiff(scenario_axis_vocab(step), path)
  list(path = path, inner = inner)
}
```

This is the single accessor `task-tables`/`hive-partitioning` call. *Why an accessor over stored inner axes?* The path axes already fully determine the inner set; storing both invites inconsistency and bloats the manifest. Computing keeps `partition_by` the lone source of truth.

### Decision: validate in the user-facing frame, axis-by-axis

Thread the constructor's `call` into a `validate_partition_by(partition_by, call)` helper. Loop over `c("data", "fit", "hc")` (a plain loop, not `purrr::walk`, per the error-origin convention) so the `Error in \`ssd_define_scenario()\`:` header is preserved. For each step: require the entry exists, is a character vector, is unique, non-`NA`, and `setdiff(entry, vocab)` is empty; `nrow` gets a bespoke message because it is the most likely mistake (it *looks* like a data axis but is sub-truncation).

## Risks / Trade-offs

- **Vocabulary duplicated between this constant and the future task tables** → drift if axis names change. Mitigation: `task-tables` asserts its column names are a superset of / match `scenario_axis_vocab()`; the constant is the upstream source and the test is the downstream guard.
- **All-or-nothing `partition_by` is less ergonomic than a partial override** → users must restate defaults to tweak one step. Mitigation: documented defaults cover the common case; a merge helper can be added later without breaking the stored contract.
- **Acceptance test deferred to `hive-partitioning`** → the headline "changing `partition_by` shifts shards, results byte-identical" property is not verified here. Mitigation: scope is explicit (knob contract only); the design records the deferral so `hive-partitioning` owns the end-to-end test.
- **Spec base not yet archived** → `scenario-definition` lives in the sibling `ssd-define-scenario` change. The delta here is purely `ADDED` requirements, so it composes cleanly regardless of archive order.

## Open Questions

- Should `min_pmix`, `range_shape1`, `range_shape2` be admissible **path** axes at all (they are list-valued / structured), or restricted to inner columns? Defaulting to admissible-as-path for now (a user could shard by `min_pmix` name); `hive-partitioning` may constrain this once it knows how list-valued axes serialise into a path segment. Flagged for that change.
