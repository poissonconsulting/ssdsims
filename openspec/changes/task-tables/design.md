## Context

`task-list-loop-baseline` (folded) landed the three per-step task tables (`sample`/`fit`/`hc`), `task_axes(step)`, `path_key()`/`add_task_ids()`, the per-task `task_primer()`/`task_primers()`, and the `*_data_task_primer()` seed-and-run wrappers (`primer-primitives`), plus an in-process baseline runner. `partition-by` gives the scenario a validated path/inner split via `scenario_partition_axes(scenario, step)`. `scenario-accessors` exposes datasets and `min_pmix` by name (`scenario_dataset()` / `scenario_min_pmix()`), both materialised on the scenario. What remains (the §12 `task-tables` step) is to group the task rows into shards, run a shard against persisted upstream Parquets, and assemble a `targets` pipeline that `tar_make()`s — the bridge from §3's local runner to §4's cluster pipeline. (The `manifest` is deliberately *not* in this critical path — it records provenance/verification metadata about this step's outputs and is consumed downstream; see the dependency decision below.) `TARGETS-DESIGN.md` §5/§6 specify the shard grouping, the static-branching `tar_map()` shape, and the byte-identical contract.

## Goals / Non-Goals

**Goals:**

- Per-step shard tables: one row per `partition_by` path cell, with a `tasks` list-column carrying each task row plus its `(seed, primer)` and upstream partition-path columns — the `values` for `tar_map()`.
- Per-shard step runners that reuse the existing `*_data_task_primer()` wrappers (one `local_dqrng_state(seed, primer)` per task), read upstream by partition path, and write one Parquet per shard.
- A static-branching `_targets.R` template that mints one named target per shard and `tar_make()`s a tiny scenario, with results byte-identical to `ssd_run_scenario_baseline()`.
- A `duckplyr` `ssd_summarize()` fan-in over the result layers.

**Non-Goals:**

- *Partial*-failure survival — a bad task yielding a *shorter* shard, the survivor-union, the `warn = 2` muffling — is `shard-failure-survival`. (Whole-shard `error = "null"` gating **is** in scope here; see the decision below.)
- Hive predicate-pushdown queries and the deferred `data`-step decision — `hive-partitioning`.
- Per-shard cloud upload — `cloud-upload`. The crew/SLURM controller — `cluster-pipeline`. Replay — `replay-helper`.
- Changing the `task-lists` derivations or `partition_by` validation (owned by those capabilities).

## Decisions

### Decision: `(seed, primer)` live on the shard task rows, not the declarative task tables

The `task-lists` spec requires the bare derivations to carry no `seed`/`primer` columns. Computing a primer is a pure hash (`task_primer()`), not an RNG draw, so attaching `(seed, primer)` at **grouping time** — inside each shard's `tasks` list-column — honours that contract while putting the primer exactly where it is consumed (the shard body). The `*_shards()` wrappers call the existing `task_primers(tbl, step)` over `task_axes(step)` and set `seed = scenario$seed` on each row. This reconciles the §12 wording ("task tables with `(seed, primer)` on each row") with the existing RNG-free-derivation requirement, and means a task's `(seed, primer)` is identical to the baseline runner's. *Alternative considered:* add primer columns to the bare derivations — rejected; it would modify the `task-lists` contract for no benefit, since only the shard path and the runner ever read them.

### Decision: group by the path axes from `scenario_partition_axes()`

A shard is one `partition_by` path cell (§5). `ssd_scenario_<step>_shards(scenario)` derives the step's task table, then groups by `scenario_partition_axes(scenario, step)$path` (from `partition-by`), producing one row per distinct path-cell. Each shard row carries the path-axis columns (rendered to the Hive partition path by the existing `path_key()`, and used as the `tar_map` `names`) and a `tasks` list-column of the inner task rows. The inner axes (`$inner`) are exactly the columns that vary within a shard. *Why reuse `scenario_partition_axes()`?* It is `partition-by`'s single source of truth for the split; duplicating it here would risk drift (the same reasoning `partition-by` applied to `task_axes()`).

### Decision: shard bodies reuse the per-task primitives ⇒ byte-identical to baseline

Each `ssd_run_<step>_step(tasks, scenario, ...)` loops the shard's `tasks` rows and calls the same `sample_data_task_primer()` / `fit_data_task_primer()` / `hc_data_task_primer()` the baseline runner calls, under one `local_dqrng_backend()` scope. Because a task's result is fully determined by `(seed, primer)` and is order-independent (`task-lists`), the targets run and the baseline run produce byte-identical per-task results regardless of how tasks are bundled into shards or shards into jobs. This is the headline acceptance test. *Implication:* sharding and `partition_by` are a pure re-layout — they never change results (the property `partition-by` guarantees by construction).

### Decision: inter-shard linking by upstream partition path

A fit shard's tasks carry their parent `sample` shard's partition-path columns; the runner opens `results/sample/<sample-path>/part.parquet`, truncates `head(sample, nrow)` inline (RNG-free, §5), then fits. An hc shard reads its parent `fit` shard(s) likewise. This is the portable contract (§6) reused for the query layer and replay. Under static branching a shard target can *additionally* name its specific upstream shard target for a precise dependency edge; the path remains the cross-cutting link.

### Decision: static branching with `tar_map()`; scenario is a construction-time object

The shard set is a pure function of the scenario, known when `_targets.R` is sourced (a plain `ssd_define_scenario()` call, not a `tar_target`), so `tarchetypes::tar_map(values = <step>_shards, names = <path axes>)` mints one named, addressable target per shard at sourcing time (§6). `targets` still tracks the scenario as a referenced global, so editing a knob invalidates the dependent shards. *Alternative considered:* dynamic branching (`pattern = map`) — the documented escape hatch for extreme fan-outs (§6); it trades per-shard addressability for one light pattern node. Static is the default here.

### Decision: `format = "file"` step targets; `duckplyr` summary fan-in

Step targets return the shard's Parquet **path** (`format = "file"`), not its value, so targets tracks the file and downstream targets pass paths, not in-memory frames (§6). `ssd_summarize()` reads the three result directories with `duckplyr` (the team's Parquet engine, `AGENTS.md`) rather than depending on each shard target, so it sees whatever shards landed and does not pull every shard back into R (§6).

### Decision: whole-shard `error = "null"` is in scope; partial survival is not

The step targets carry `error = "null"` from the start (it is in the §6 sketch): a failing **whole shard** records its error in `tar_meta()` and goes `NULL` without aborting `tar_make()`, so the other shards still build and `ssd_summarize()` — which reads the result directories, not the target values — unions whatever landed (§6.2). This whole-shard gating is one argument on each `tar_target()` and pairs naturally with the path-reading summary, so there is no reason to defer it. What *is* deferred to `shard-failure-survival` is the finer **partial** survival: a single bad task yielding a *shorter* shard (rather than failing the shard), the explicit survivor-union semantics, and the `warn = 2` muffling of the expected end-of-pipeline warning. *Why split it here:* `error = "null"` costs nothing and makes the toy pipeline robust; the shorter-shard machinery needs the per-task try/accumulate logic that is `shard-failure-survival`'s whole point.

### Decision: the single-core baseline runner stays, and is the oracle that validates the pipeline

`ssd_run_scenario_baseline()` (the single-core, in-process, no-`targets`/no-Parquet runner from `task-list-loop-baseline`) is **not** replaced by this step — the two runners coexist as two *drivers* over one execution core. The per-task primitives (`*_data_task_primer()`) and the per-step loop body are shared: the baseline runner threads results in memory by foreign key, while `ssd_run_<step>_step()` runs a shard's task subset and reads/writes Parquet — same task bodies, different I/O and granularity. Because both install the same `(seed, primer)` and results are order-independent, **the targets pipeline is validated against the single-core runner**: the headline acceptance test runs a tiny scenario both ways and asserts byte-identical per-task `sample`/`fit`/`hc` results. This makes the baseline runner a permanent reference oracle, not throwaway scaffolding, and gives users a dependency-light path (no `targets`/`duckplyr`) for small in-process runs. *Note on failure semantics:* the baseline runner is a plain loop with no `error = "null"` gating (a task error surfaces directly, in-process); the targets pipeline adds whole-shard `error = "null"`. The byte-identity contract is about *successful* per-task results, so this difference does not weaken the oracle. Running the targets pipeline with a single worker is a valid middle ground (sequential, but shard-addressable and resumable).

**Reused vs. genuinely new** (so the targets path does not re-implement execution):

- *Reused unchanged* — the per-task **execution core**: the state-less ops and seed-and-run wrappers `sample_data_task_primer()` / `fit_data_task_primer()` / `hc_data_task_primer()` (`primer-primitives`), the RNG scoping (`local_dqrng_backend()` / `local_dqrng_state()`), the per-task primer derivation `task_primer()` / `task_primers()`, the task-table identity helpers `task_axes()` / `path_key()` / `add_task_ids()` / `task_parent()` (`task-lists`), and the `head(sample, nrow)` inline truncation. The per-step *loop body* is the baseline runner's, lifted out so both drivers share it.
- *Reused from sibling changes* — `scenario_partition_axes()` (`partition-by`) and `scenario_dataset()` / `scenario_min_pmix()` (`scenario-accessors`).
- *Genuinely new here* — the shard **grouping** wrappers `ssd_scenario_*_shards()` (group a task table by its path axes into one row per shard, with the `tasks` list-column and its `(seed, primer)` decoration); the per-shard **step runners** `ssd_run_*_step()` whose novelty is the **I/O and granularity** (operate on a shard's task subset; read the upstream shard from Parquet *by partition path* instead of threading results in memory by foreign key; write one Parquet per shard); the `ssd_read_parquet()` / `ssd_write_parquet()` internals; `ssd_summarize()`; and the static-branching `_targets.R` template (`tar_map()` + `format = "file"` + `error = "null"`). In short: **execution is reused; sharding, Parquet I/O, and the targets wiring are new.**

### Decision: ship the pipeline as an `inst/` template plus exported building blocks

The `_targets.R` is shipped as a template under `inst/targets-templates/local/` (the cluster variant is `cluster-pipeline`'s `inst/targets-templates/cluster/`), built from the exported `ssd_scenario_*_shards()` + `ssd_run_*_step()` + `ssd_summarize()`. The integration test copies the template into a `tempdir()`, sources it for a tiny scenario, runs `targets::tar_make()`, and asserts every shard target completes and the unioned results equal `ssd_run_scenario_baseline()`. *Why a template, not a generator function?* A checked-in, readable `_targets.R` is what a cluster user copies and edits; a generator can come later without changing the building blocks.

### Decision: `manifest` is not a dependency of `task-tables` (avoid the inversion)

A targets pipeline that builds shards needs nothing from the manifest, and `task-tables` reads nothing from it — so making `manifest` a prerequisite would be a dependency inversion. The two halves of the manifest point the other way or sideways: the **head** (scenario fields + session info, §8.5/§9) depends only on the scenario, and **`completed_shards`** depends on the shards *already existing* (they must be hashed), i.e. on `task-tables`' outputs. The manifest's real consumers are downstream — `replay-helper` (verifies upstream against `completed_shards`, §7) and `shard-completeness-assert` (records expected-vs-actual, §6.2/§8.4), with `cloud-upload` recording the cloud sha256 (§6.1). So the correct edges are `define → manifest`, `manifest → replay-helper`, `manifest → shard-completeness-assert` — *not* `manifest → task-tables` (TARGETS-DESIGN.md §12 DAG updated to match).

*Latest point manifest is needed:* (1) as a build dependency, just before the first consumer (`shard-completeness-assert` / `replay-helper`) — it never gates this step, `hive-partitioning`, or `cluster-pipeline`; (2) operationally, before the first **expensive cluster run whose results you intend to trust/reproduce/replay**, since exact session info and the "trusted-as-produced" sha cannot be reconstructed afterwards. For local dev and the toy pipeline here, no manifest is required.

*Why no `ssd_record_shard()` hook in the runners now?* The manifest assembler can hash the shard Parquets on disk post-hoc, so the runner needs no manifest call to make `completed_shards` possible. Recording at write time (and the cloud sha256) is the enhancement that lands with `replay-helper`/`cloud-upload`, where the "actual cluster bytes" semantics matter. Keeping it out of the runner now removes the only coupling and keeps the happy path dependency-free.

## Risks / Trade-offs

- **`partition-by` is not yet applied** → `scenario_partition_axes()` and the three-step `sample`/`fit`/`hc` defaults are prerequisites; the current `scenario_default_partition_by()` still uses the pre-fold `data` key. This change depends on `partition-by` landing first; until then the shard wrappers cannot key on the split. Stated as a prerequisite in the proposal.
- **Heavy dependency surface** (`targets`, `tarchetypes`, `duckplyr`) → these are the price of the cluster pipeline; the crew labs (§4) pinned a binary install path. The single-core baseline runner remains a `targets`/`duckplyr`-free path for small in-process runs. Parquet I/O stays behind `ssd_read_parquet()`/`ssd_write_parquet()` internals (`duckplyr`, introduced by this change) so the engine is swappable.
- **`tar_make()` in tests is slower than unit tests** → keep the integration scenario tiny (1 dataset, `nsim = 2`, `nrow = c(5, 10)`), run it in a `tempdir()`, and gate it behind `testthat::skip_on_cran()` / a `targets`-available skip so CRAN and offline checks stay fast.
- **Byte-identity hinges on `duckplyr`/DuckDB write determinism *and row order*** → assert equality on the *read-back R frames* (per-task results), not on raw Parquet bytes. `duckplyr`/DuckDB does not guarantee row order on read, so the comparison SHALL **sort both sides by a deterministic key** — the task-identity columns (the `<step>_id`, i.e. `task_axes(step)`) — before asserting equality, so a re-ordered read does not look unequal. Parquet encoding choices are likewise normalised away by comparing R values, not bytes.
- **Provenance is not captured by the happy-path pipeline** → by design `task-tables` records no sha256/session-info; a result set built by this step alone has no manifest. Mitigation: the `manifest` step's assembler can hash the shard Parquets post-hoc, and the at-write-time recording is wired into the runners when `replay-helper`/`cloud-upload` land (see the dependency decision). The operational guardrail is to land `manifest` before the first *trusted* cluster run, not before this step.

## Open Questions

- **Shard target ↔ Slurm job packing** (§11) — out of scope here (local `tar_make()` only); `cluster-pipeline` resolves the controller packing.
- **`hc` expected-row cardinality** — a task's hc output is not one row (`proportion` fan-out, the `ci = FALSE` collapse, §1.2), so any per-shard expected-row count is summed over the shard's task outputs, not inferred from inner-axis cardinalities. Stored expected counts are `shard-completeness-assert`'s concern; this step only writes the rows.
- **Whether to reinstate an explicit `data` step** — the provisional fold (sample + inline `head`) is kept; the buffering-checkpoint decision is tied to the invalidation model and settled by `hive-partitioning` (§8).

### Decision: per-layout results root for the targets pipeline (Option D)

A shard's Hive path depth/axes depend on `partition_by`/`bundle`, and the summary/readers glob `<step>/**/part.parquet`. Replaying a scenario with a changed split into one fixed `results/` root would leave stale-granularity shards beside the new ones, and the glob would union both (double-counting, since the `<step>_id` identity is `partition_by`-independent). The targets pipeline cannot simply clobber `results/` before a run (that would defeat the `targets` cache and wipe the `error = "null"` survivors of a prior partial run it means to resume). So instead each **layout** gets its own root: `scenario_results_dir(scenario)` = `<root>/layout=<hash(partition_by)>`. A `partition_by`/`bundle` change → a fresh root (the old layout's tree is orphaned but never *mixed* with the new one); the same layout → the same root (idempotent rewrite, cache preserved). Non-layout knobs (seed, grids) keep the same root and just rewrite/add shard paths at the same depth (ordinary path-axis growth, §8.1). The complementary single-core `ssd_run_scenario_shards()` uses Option A (owns and clears a fixed `dir`). *Deferred:* pruning a now-orphaned layout root, and manifest-driven reads, belong to `manifest` / `shard-atomic-rewrite` (§8).
