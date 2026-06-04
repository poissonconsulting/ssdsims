## Context

`TARGETS-DESIGN.md` §1.1 makes datasets and `min_pmix` **name-referenced**: the scenario stores names, never values, so (a) the per-task hash (`task_primer()` over `task_axes(step)`) is stable against recompilation/JIT and cosmetic source edits, and (b) the scenario serialises to a tiny manifest. Today nothing resolves those names: `R/scenario.R` retains dataset tibbles inline as `scenario$data` (an `ssd_data()` collection), and `resolve_min_pmix()` in `R/task-lists.R` looks a `min_pmix` name up in `ssdtools` / the global env, aborting with "there is no `min_pmix` registry yet." `registry` is the **targets-only** step that resolves each referenced name once per project — datasets persisted to Parquet, `min_pmix` pinned to a function value — and is the package's first Parquet I/O.

## Goals / Non-Goals

**Goals:**

- A single registry abstraction for both payload kinds (datasets, `min_pmix`), reflecting the shared name-only indirection of §1.1.
- Persist each *referenced* dataset to `results/datasets/<name>.parquet` with a `name -> { rows, conc_col, sha256, source }` index; enforce the `Conc` invariant at registration.
- Resolve `min_pmix` names to pinned function values; make a registered function's *body* edit leave every cached fit branch's hash unmoved (the §1.1 headline property).
- Refuse re-registration under an existing name unless the bytes match (resolve §11.2).
- Rewire `resolve_min_pmix()` to consult the registry first, falling back to the current lookup so the baseline runner is unaffected when no registry exists.

**Non-Goals:**

- Regenerating generator datasets from a stored provenance (name + generator + `.seed`): that is the deferred `dataset-provenance` step (§12). Here the dataset side only *persists* the inline tibble the scenario already carries (post `scenario-input-types`).
- The per-shard Parquet write/read of results, Hive partitioning, the manifest, and the summary query layer (those are `task-tables` / `hive-partitioning` / `manifest`).
- Tracking the execution environment (R / package versions) — that is the §9 manifest's job, owned by `manifest`.

## Decisions

### Decision: one registry step, two payload kinds

Datasets and `min_pmix` share one registry because both are name-only indirection (§1.1); only the resolved payload differs — a Parquet file for data, a pinned function value for `min_pmix`. Implement them behind a common shape (`register`, `resolve`, an index) rather than two parallel subsystems. *Alternative considered:* two separate registries — rejected because it duplicates the index, the re-registration policy, and the resolver for no behavioural gain (§1.1 explicitly collapses them into "one `registry` step, not two").

### Decision: datasets are *persisted*, never regenerated

`scenario-input-types` materialises every non-data-frame input (generator, `fitdists`, `tmbfit`, function-name) to an inline tibble **in the constructor**, so by the time `registry` runs the scenario already carries the realised bytes (`scenario$data[[name]]`). The dataset side of `registry` therefore writes those bytes to Parquet — it does not re-run any generator. This keeps reproducibility "by being kept, not by regeneration" (§1.1) and defers the name-only-and-regenerate path to `dataset-provenance`. *Implication:* registration is deterministic and seed-free here; the `.seed`-driven generation lives entirely in `scenario-input-types`.

### Decision: `min_pmix` resolves by name; body edits don't move task hashes

The per-task primer hashes `task_axes("fit")`, which includes `min_pmix` as the **name** (a string), never the function value. So `ssd_register_min_pmix("strict", \(n) 0.05)` re-pointed to a different body leaves every fit task's `(seed, primer)` unchanged — the §1.1 headline. Resolution happens just before the fit call (after `dqset.seed()`), via the registry. The regression test edits a registered function's body and asserts `task_primer()` over the fit identity is byte-identical. *Why not hash the function value?* `rlang::hash()` over a function is not byte-stable across byte-compilation, `srcref`, or captured environments (§1.1) — the whole reason for name indirection.

### Decision: Parquet interaction via `duckplyr` (team preference)

The team's standing convention is to interact with Parquet through `duckplyr` (DuckDB), recorded in `AGENTS.md`: `duckplyr::read_parquet_duckdb()` for reads and `duckplyr::compute_parquet()` for writes — **not `arrow`**. The registry's per-dataset read/write therefore goes through `duckplyr`, confined behind thin `ssd_read_parquet()` / `ssd_write_parquet()` internals (one audit point). This is also the engine the off-cluster query layer (`hive-partitioning`/`summary`, §6) uses for predicate-pushdown reads, so one dependency covers both the simple one-file-per-dataset I/O here and the later query layer. *Alternatives considered:* `arrow` (the design doc's original sketch; heavier build) and `nanoparquet` (zero-system-dependency, lighter) — both rejected in favour of the team's duckplyr convention.

### Decision: `_index.json` carries `{ rows, conc_col, sha256, source }`; sha256 over the Parquet bytes

The index (§1.1) lets a consumer check a dataset's shape and integrity without opening the Parquet. `sha256` is computed over the written Parquet file bytes via a shared `ssd_file_sha256()` (also used by `manifest`'s `completed_shards`); `digest::digest(file = ..., algo = "sha256")` is the implementation. `source` records provenance as a short string (e.g. `"data.frame"` or the generator name) for human inspection only — not a regeneration recipe (that is `dataset-provenance`).

### Decision: refuse re-registration unless byte-identical

§11.2 asks whether the registry should refuse a colliding re-registration or carry a content hash on the task id. Decision: **refuse** — registering a different payload under an existing name aborts with an informative error; re-registering byte-identical content is a no-op. This keeps the name a stable key (the task-hash contract depends on it) without threading a content hash through every task id. *Alternative considered:* content-hash-on-task-id — rejected as heavier and unnecessary while names are curated; it remains the escape hatch if silent-collision risk ever materialises.

### Decision: `resolve_min_pmix()` consults the registry first, then falls back

Rewire the existing `resolve_min_pmix()` to look the name up in the registry, falling back to the current `ssdtools`/global-env lookup when no registry entry exists. This keeps `ssd_run_scenario_baseline()` working unchanged (no registry in a local run) while giving the targets path a real resolution source. The abort message changes only when *neither* a registry entry nor a namespace function resolves.

## Risks / Trade-offs

- **Parquet write byte-stability across `duckplyr`/DuckDB versions** → pin the `duckplyr`/DuckDB version in the manifest (§9) and confine Parquet I/O behind thin `ssd_read_parquet()`/`ssd_write_parquet()` internals so the engine is swappable and the byte-stability surface is one place. (`duckplyr` also pulls in DuckDB; the crew labs' ManyLinux binary path, §4, covers the binary install on cluster nodes.)
- **Name collisions are silent without the byte-identical guard** → the refuse-unless-identical policy (above) makes them loud; tested directly.
- **`scenario-input-types` is not yet applied** → the dataset side assumes `scenario$data[[name]]` holds a realised tibble. The current constructor already retains an `ssd_data()` collection inline, so the assumption holds today for data-frame inputs; generator inputs arrive with `scenario-input-types`. Registry's contract is written against the inline tibble, independent of *how* it was realised.
- **Shared `ssd_file_sha256()` is also needed by `manifest`** → the two changes are parallel in the DAG; whichever lands first introduces the helper in a shared utils file and the other reuses it. No ordering dependency between `registry` and `manifest` is created by this.

## Open Questions

- **Index format and concurrency.** `_index.json` is rewritten on each registration; registrations are a setup-time, single-writer activity (not the parallel shard write), so a plain read-modify-write is fine. If registration ever moves into a parallel target, revisit (the per-shard sidecar pattern `manifest` uses would apply).
- **`source` granularity.** Recording only a short provenance string here is deliberate; the full generator + `.seed` provenance needed for regeneration is `dataset-provenance`'s concern.
