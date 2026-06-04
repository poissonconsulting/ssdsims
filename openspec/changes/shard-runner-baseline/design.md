## Context

The in-memory baseline runner (`ssd_run_scenario_baseline()` in `R/task-lists.R`, seeded per task by `primer-primitives`) threads step results forward as named lists keyed by `<step>_id`: `sample_out <- set_names(..., sample_id)`, then `fit` reads `sample_out[[sample_id]]`, then `hc` reads `fit_out[[fit_id]]`. It runs in-process with no shards and no Parquet. The targets design (`TARGETS-DESIGN.md` §5/§6) replaces that memory hand-off with **Hive-partitioned Parquet shards** read back by **duckplyr predicate pushdown**. `partition-by` lands the `scenario_partition_axes(scenario, step)` path/inner split; this change is its first consumer and the first place the storage hand-off — and the **m:n parent-shard dependency** it implies — runs in plain R, before `targets` static branching (`task-tables`/`hive-partitioning`) is layered on.

The m:n relationship is intrinsic to the §5 **coarsening defaults** (`sample` keyed by `dataset,sim,replace`; `fit` by `dataset,sim,nrow,rescale`; `hc` by `dataset,sim`): an `hc` shard `(dataset,sim)` reads every `fit` shard for that `(dataset,sim)` across `nrow`/`rescale`, and with `replace = c(F,T)` a `fit` shard reads two `sample` shards. The earlier `partition-by` "parent-consistency" rule tried to forbid this; it was both backwards relative to its stated one-parent-shard rationale and contradicted by the shipped defaults. The resolution is to **accept m:n and resolve it at read time** — which is exactly what this runner does (and tests).

## Goals / Non-Goals

**Goals:**

- A single-core runner that writes each step's results as one Parquet per `partition_by` path cell under a Hive path, and reads parents back via duckplyr.
- Own the m:n resolution: per child shard, read the distinct set of parent shards its tasks span, filtering on path/inner columns.
- Preserve per-task RNG, results, and reproducibility exactly — `partition_by` is a free re-layout (validated against the in-memory baseline as oracle).
- Make `partition-by`'s deferred byte-identical acceptance test landable here, without `targets`.

**Non-Goals:**

- No `targets`, `crew`, cloud upload, manifest, completeness assert, or atomic-rewrite logic (those are their own roadmap steps). This is the plain-R storage loop only.
- Not deciding the `targets` static-branching m:n wiring — that belongs to `task-tables`. This design only **records the lean** (Option 3) so the decision is not lost.
- Not modifying the in-memory baseline runner, `task_axes()`, `add_task_ids()`/`path_key()`, or `scenario_partition_axes()`. They are consumed as-is.
- Not the `partition-by` cleanup (removing the parent-consistency requirement) — tracked separately.

## Decisions

### Decision: duckplyr (→ duckdb) for both write and read, not arrow

The §6 design used duckplyr only for downstream/off-cluster reads. Using it for **intermediate inter-step storage** on tiny datasets is heavier than `arrow` (a duckdb dependency, a connection, query overhead dwarfing kilobyte shards). We choose duckplyr anyway: it is the **same engine the targets read path commits to** (`read_parquet_duckdb` over Hive globs with predicate pushdown), so proving the write→glob-read→filter loop here de-risks `hive-partitioning`/`task-tables` directly rather than validating an `arrow` path that production won't use. *Alternative considered:* `arrow::write_dataset()` + `arrow` Hive reads — lighter, but it would exercise a different reader than production, defeating the de-risking purpose. The write side MAY use whatever duckplyr/duckdb offers for partitioned writes (e.g. a `COPY ... (FORMAT parquet, PARTITION_BY ...)` or per-shard `write_parquet`); the **read** side MUST be duckplyr to mirror production.

### Decision: results are stored as serialised list-columns inside the shard Parquet

`fit` produces `fitdists` objects and `sample` produces data frames — not flat columns. The shard Parquet carries the inner-axis columns plus a result column holding the per-task object (serialised, e.g. a blob/raw column or a nested representation duckdb round-trips). The runner reads the parent shard back, deserialises the parent result for each child task, and proceeds exactly as the in-memory runner does after a list lookup. *Why:* the hand-off must reproduce the in-memory object the next step consumes (`fit` truncates `head(sample, nrow)`; `hc` consumes a `fitdists`); flattening to tidy columns is an analysis-layer concern (`summary`, §6), not required for the inter-step link. The precise serialisation is an implementation detail the tasks pin; correctness is asserted against the in-memory oracle.

### Decision: m:n parent resolution = distinct parent shard paths over the child shard's tasks

Each child task row already carries its full parent identity (the `<parent>_id` columns, `task_axes(parent)`), minted by `task-list-loop-baseline`. The **parent shard** a task lives in is that identity **projected onto `path[parent]`** = `scenario_partition_axes(scenario, parent)$path`. For a child shard, the set of parent shards to read is the **distinct** set of those projections over the shard's tasks. The runner globs/reads exactly those parent Parquets and filters their rows to the parent identities the child tasks reference (predicate pushdown on path + inner columns). This needs **no** cross-step constraint on `partition_by`: any path/inner split per step resolves, because reading a coarser parent shard and filtering is cheap. *Alternative considered:* require one parent shard per child shard (a corrected `path[parent] ⊆ path[child]` rule) to read whole parent shards — rejected: it forbids the §5 coarsening defaults and buys only the avoidance of an inner-column filter that duckplyr pushes down for free.

### Decision: link by identity join, reusing path_key()/task_axes() — not a new id scheme

Parent rows are matched to child tasks by the existing `<parent>_id` identity (`path_key(task_axes(parent))`), the same join the in-memory runner does in memory. Hive **path** rendering reuses `path_key()` over `path[step]`; the parent-shard glob is the path-key over `path[parent]`. No new identifier or hashing is introduced — the runner is a storage swap over the same identity algebra, which is what keeps results invariant.

### Decision: the single-foreign-key task model is preserved here; it generalizes only for future reduce steps

The task tables (`task-list-loop-baseline`) are a strict tree: each row has a `<step>_id` primary key and **exactly one** `<parent>_id` foreign key (n:1 child→parent). This runner **relies** on that and does not change it — the m:n shard graph is *generated* from the single key by projecting each task's `<parent>_id` onto `path[parent]` and taking the distinct set (the §3 resolution). So sharding does **not** break "exactly one foreign key"; the single key is the *input* to m:n, not a casualty.

The single key holds only because the grids grow monotonically (`task_axes(sample) ⊆ task_axes(fit) ⊆ task_axes(hc)`), so a child carries its parent's full identity. It generalizes in two ways — **neither triggered by this change, recorded so the FK contract is not mistaken for a permanent invariant**:

1. **Set-valued foreign key (reduce/aggregation).** A step whose identity *drops* an axis — an across-replicate summary — references **many** parent tasks. The foreign key becomes a match on the *shared* axes (`intersect(axes(child), axes(parent))`) = a parent-shard glob, resolved by the **same** read+filter machinery this runner uses for m:n. This applies at **all three layers**, not just `hc`: one may reduce over any axis a layer carries (`sim`/`replace` at all three; `nrow` at `fit`/`hc`; the bootstrap knobs at `hc` only). The §6 `summary` is today's monolithic, un-sharded instance.
2. **Multiple foreign-key columns (DAG).** A step consuming more than one upstream step carries one foreign key per upstream step. The PK + FK-columns model already expresses this; only the linear single-parent chain is specific to the current three steps.

This runner implements (and tests) only the cross-join chain, where every foreign key is single-valued; it just keeps the read path (shared-axis glob + filter) general enough that a future reduce step reuses it unchanged.

**Why the foreign key is an identity selector, not a stored Hive prefix.** A tempting unification is to store, per task, the Hive **path** prefix that globs all its relevant parents (one shard for tree steps, a set for reduce). We do **not**: that prefix is `path_key(path[parent])`, a function of the *parent's* `partition_by` — so storing it would couple the task table to the partition layout and break the property this design rests on, that `partition_by` is a **free re-layout** which must never rewrite task identity, the primer, or (therefore) any stored key. It is also redundant (a pure function of columns the row already carries, identical across a shard's tasks). The partition-independent form of the idea is an **identity-space** selector: the foreign key is the parent identity (the full id for tree steps; the shared-axis tuple `intersect(axes(child), axes(parent))` for reduce steps), and the Hive glob is **derived** from it by projecting onto the *current* `path[parent]` — at read time in this runner, at sourcing time for `targets`. Derived, never stored, so re-layout stays free and the "glob all my relevant parents" model still falls out for both tree and reduce.

### Decision: record the targets m:n wiring lean (Option 3) — but defer the decision to task-tables

The `targets` static-branching layer must express each child shard's dependency on a runtime-variable **set** of parent shards. Three options: **(1)** force one-parent-shard via a corrected `path[parent] ⊆ path[child]` rule — rejected, contradicts coarsening defaults; **(2)** glob-read the parent dir inside the branch body — simple but the dependency is invisible to `targets`, breaking invalidation; **(3)** compute each child shard's upstream target-name set at **sourcing time** (shards are static, so this is known before any target runs) and splice it into the branch (`tar_target(child, f(!!!syms(parents)))`) — fine-grained invalidation **and** m:n, at the cost of more `tar_map`/`tar_eval` metaprogramming. We **lean Option 3** and record it here for `task-tables`; this single-core runner needs none of it (it resolves the set at run time in a plain loop), so the decision does not block this change. Keeping Option 3 in view is what lets the parent-consistency rule stay permanently absent from `partition-by`.

### Decision: validate against the in-memory baseline as the oracle

The headline correctness claim — `partition_by` is a free re-layout — is tested by running the same scenario through `ssd_run_scenario_baseline()` (memory) and the sharded runner (disk) and asserting per-task results equal after joining on `<step>_id`. The deferred `partition-by` acceptance test (changing `partition_by` shifts paths, results byte-identical) is realised here as a two-`partition_by`-settings comparison. *Why an oracle:* it pins "storage-only swap" precisely without re-deriving expected values, and it directly retires the test `partition-by` deferred to `hive-partitioning`.

## Risks / Trade-offs

- **duckdb dependency weight for tiny datasets** → Mitigation: scoped, single `Imports` addition; the de-risking value (same engine as production reads) outweighs per-call overhead, and the runner is a baseline/validation tool, not the hot path.
- **Result serialisation round-trip fidelity** (`fitdists` through Parquet) → Mitigation: assert byte-identity of downstream results against the in-memory oracle; if a native round-trip is lossy, fall back to a raw/blob serialisation column. The contract is "next step sees the same object", verified by the oracle test.
- **m:n read amplification** (a child shard re-reading a large parent shard it mostly filters out) → Mitigation: predicate pushdown limits scanned rows; datasets are tiny; this runner targets correctness/de-risking, not throughput. Coarser-vs-finer trade-offs are a `partition_by` tuning concern documented in `partition-by`/`hive-partitioning`.
- **Drift from the production targets reader** → Mitigation: the read side is duckplyr `read_parquet_duckdb` over the same Hive glob shape the targets path uses, so the reader is shared, not parallel.
- **Overlap with `hive-partitioning`** (which also writes Hive shards) → Mitigation: this change is explicitly "without `targets`"; `hive-partitioning` layers static branching and invalidation on top of the now-validated loop, and reuses the same path/read helpers.

## Open Questions

- **Shard write mechanism**: a single duckdb `COPY ... PARTITION_BY` over the whole step table vs. a per-shard `write_parquet` loop. Both yield the same Hive layout; the loop is simpler to reason about per-shard and matches the eventual per-target write. Pinned in tasks.
- **Result column representation**: native nested/list column vs. a serialised raw/blob column. Resolved by the oracle test; tasks pick the representation that round-trips `fitdists` faithfully.
- **`dir` lifecycle**: temp dir per run vs. a caller-supplied results root. Lean: caller-supplied `dir` (so shards are inspectable and reusable), defaulting to a session temp dir; this also previews the `results/` root the targets pipeline writes to.
