# task-shards Specification

## Purpose

Group a scenario's per-step task tables into shards by `partition_by`, run each shard as one Parquet via per-shard step runners, and assemble the static-branching `targets` pipeline that runs a scenario end-to-end (`TARGETS-DESIGN.md` §5/§6). The shard wrappers feed `tar_map()`'s `values`; the per-shard runners reuse the baseline's per-task primitives; a `ssd_scenario_targets()` target factory builds the whole pipeline (one `format = "file"`, `error = "null"` target per path cell, `tar_combine()` barriers, and a `summary` fan-in) under a per-layout results root, validated byte-for-byte against the single-core baseline runner.
## Requirements
### Requirement: Group a step's task rows into shards by partition_by
The package SHALL provide `ssd_scenario_sample_shards()`, `ssd_scenario_fit_shards()`, and `ssd_scenario_hc_shards()` that group the corresponding step's task table into one row per `partition_by` **path** cell, using the path/inner split from `scenario_partition_axes(scenario, step)` (`TARGETS-DESIGN.md` §5/§6). Each shard row SHALL carry the step's path-axis columns (the Hive partition path and the `tar_map` target-name suffix) and a `tasks` list-column containing the task rows whose path-axis values match that shard.

#### Scenario: One shard row per path cell
- **WHEN** a step's shard table is derived under a `partition_by` whose path axes take `P` distinct value-combinations across the step's tasks
- **THEN** the shard table SHALL have `P` rows, each carrying the path-axis columns, and the union of the `tasks` list-columns SHALL equal the step's full task table

#### Scenario: Coarser partitioning bundles more tasks per shard
- **WHEN** the same step is sharded under a `partition_by` with fewer path axes
- **THEN** the shard count SHALL decrease and the per-shard `tasks` row counts SHALL increase correspondingly, with the union of tasks unchanged

### Requirement: Each shard task row carries its seed and primer
Each task row in a shard's `tasks` list-column SHALL carry `seed = scenario$seed` and its per-task `primer` (`task_primer()` over the `task_axes(step)` identity), computed at grouping time. This decoration SHALL be RNG-free (a hash, not a draw) and SHALL match the `(seed, primer)` the baseline runner installs for the same task.

#### Scenario: Shard task rows carry (seed, primer)
- **WHEN** a step's shard table is derived
- **THEN** every task row in every shard's `tasks` SHALL carry the scenario's `seed` and a `primer` equal to `task_primer()` over that task's `task_axes(step)` identity

#### Scenario: Decoration draws no random numbers
- **WHEN** the shard tables are derived
- **THEN** no random numbers SHALL be drawn and `.Random.seed` SHALL be unchanged

### Requirement: A per-shard step runner writes one Parquet per shard
The package SHALL provide per-shard step runners `ssd_run_sample_step()`, `ssd_run_fit_step()`, and `ssd_run_hc_step()` that loop a shard's `tasks`, install each task's `(seed, primer)` exactly once via the existing `*_data_task_primer()` wrappers under an active dqrng backend, and write the shard's results to one Parquet file at the shard's partition path. The `fit` and `hc` runners SHALL read their upstream shard's Parquet by partition path (the `fit` runner truncating `head(sample, nrow)` inline, RNG-free); the `sample` runner SHALL read its dataset via `scenario_dataset()` and the `fit` runner SHALL resolve `min_pmix` via `scenario_min_pmix()` (the `scenario-accessors` change). The runners SHALL NOT depend on the `manifest`; provenance/verification metadata is recorded by the downstream `manifest` consumers.

#### Scenario: A shard runs its tasks and writes one Parquet
- **WHEN** a step runner is called on a shard with `K` tasks
- **THEN** it SHALL prime and run all `K` tasks and write a single Parquet file at the shard's partition path containing their results

#### Scenario: Downstream runners read upstream by partition path
- **WHEN** a `fit` (or `hc`) shard runner executes
- **THEN** it SHALL open the matching upstream `sample` (or `fit`) shard's Parquet by partition path and use it as the task input, rather than recomputing the upstream

### Requirement: A static-branching targets pipeline runs a scenario
The package SHALL ship a `targets` pipeline template that builds the scenario as a plain construction-time object (not a `tar_target`) and uses `tarchetypes::tar_map()` to mint one named target per shard for each step (static branching, `TARGETS-DESIGN.md` §6), so the shard set is fixed when `_targets.R` is sourced. The pipeline SHALL `targets::tar_make()` a tiny scenario end-to-end, producing one Parquet per shard across the `sample`, `fit`, and `hc` layers.

#### Scenario: tar_make builds one target per shard
- **WHEN** the template pipeline is sourced for a scenario and `tar_make()` is run
- **THEN** one named target per shard SHALL be built for each step, and one Parquet per shard SHALL be written under the step's result directory

#### Scenario: The scenario is not a target
- **WHEN** the pipeline is sourced
- **THEN** the scenario SHALL be a construction-time object and the per-step shard tables SHALL be computed at sourcing time to feed `tar_map()`'s `values`

### Requirement: A failing whole shard does not abort the run
The step targets SHALL carry `error = "null"` so that a shard whose body fails entirely records its error (readable via `tar_meta()` after the run) and yields a `NULL` target without aborting `tar_make()`; the remaining shard targets SHALL still build and `ssd_summarise()` SHALL union whatever shards landed (`TARGETS-DESIGN.md` §6.2). Finer *partial* survival (a single bad task yielding a shorter shard) is out of scope here (`shard-failure-survival`).

#### Scenario: One failing shard leaves the others built
- **WHEN** one shard's body fails entirely during `tar_make()` and the other shards succeed
- **THEN** the run SHALL NOT abort, the failed shard's error SHALL be readable via `tar_meta()`, the other shards' Parquets SHALL be written, and `ssd_summarise()` SHALL union the shards that landed

### Requirement: Targets results match the single-core baseline runner
The per-task results produced by the targets pipeline SHALL be byte-identical (as read-back R values) to those produced by the single-core `ssd_run_scenario_baseline()` for the same scenario, because both install the same per-task `(seed, primer)` via the same primitives and results are order-independent (`TARGETS-DESIGN.md` §5/§6). The baseline runner thereby serves as the reference oracle that validates the targets-based runner.

#### Scenario: Pipeline output equals baseline output
- **WHEN** a scenario is run both through the targets pipeline and through `ssd_run_scenario_baseline()`, and both sides are sorted by the task-identity key (`<step>_id`) to normalise the unordered Parquet read
- **THEN** the per-task `sample`, `fit`, and `hc` results SHALL be equal across the two runs

### Requirement: A summary fan-in reads the result layers without re-running upstream
The package SHALL provide `ssd_summarise()` that reads the `sample`, `fit`, and `hc` result directories (via `duckplyr`) and writes a combined `results/summary.parquet`, without depending on each shard target's value and without re-running upstream steps (`TARGETS-DESIGN.md` §6).

#### Scenario: Summary reads landed shards
- **WHEN** `ssd_summarise()` is run after the shard Parquets are written
- **THEN** it SHALL read the result layers from disk and write a combined summary, without recomputing any shard

#### Scenario: Summary does not pull every shard value into R via targets
- **WHEN** the summary target runs in the pipeline
- **THEN** it SHALL read the result directories directly rather than depending on every shard target's in-memory value

### Requirement: Results written under a per-layout root keyed by partition_by
Because a step's Hive shard path depth/axes are a function of `partition_by`/`bundle`, the targets pipeline SHALL write each step's shards and the summary under a results root **keyed by the scenario's layout** — `scenario_results_dir(scenario)` = `<root>/layout=<hash of partition_by>`. Re-running a scenario with a changed `partition_by`/`bundle` SHALL therefore write to a *fresh* root and never mix shard granularities (a depth-agnostic glob over one root would otherwise union stale and current shards); re-running the *same* layout SHALL reuse the root (idempotent, cache-friendly). A non-layout knob (e.g. `seed`, a grid value) SHALL NOT change the root.

#### Scenario: Different partition_by yields a different results root
- **WHEN** `scenario_results_dir()` is computed for two scenarios that differ only in `partition_by`
- **THEN** the two roots SHALL differ

#### Scenario: Same layout yields the same root
- **WHEN** `scenario_results_dir()` is computed twice for the same `partition_by` (other knobs may differ)
- **THEN** the root SHALL be identical

### Requirement: A target factory builds the whole pipeline from a scenario
The package SHALL provide `ssd_scenario_targets(scenario, ..., root, upload, cue)` that returns the complete list of `targets` objects for the static-branching pipeline — one `format = "file"`, `error = "null"` target per `partition_by` path cell per step (named by the step's path axes), the per-child upstream edges wiring each child shard to the parent shard(s) it reads, and the `summary` — written under `root` (default `scenario_results_dir(scenario)`). The factory SHALL place `...` immediately after `scenario` and call `rlang::check_dots_empty()`, so `root`, `upload`, and `cue` MUST be passed by name and a positional or misspelled argument aborts. The `upload` argument is the **remote-destination sibling of `root`** (default `NULL`): with `upload = NULL` the factory SHALL emit no `upload_<step>` targets; with a non-`NULL` upload object (`ssd_upload_azure(...)` or `ssd_upload_dryrun()`) the factory SHALL pair each step shard with an `upload_<step>` target in the same `tar_map` (per the `cloud-upload` capability). A `_targets.R` SHALL therefore reduce to building a scenario and calling the factory; the per-task results SHALL be unchanged and independent of `upload`.

#### Scenario: A `_targets.R` is just a scenario plus the factory call
- **WHEN** a `_targets.R` does `source("scenario.R"); ssd_scenario_targets(scenario)` and `targets::tar_make()` runs
- **THEN** every shard target SHALL build and the per-task results SHALL equal those of `ssd_run_scenario_baseline()` for the same scenario

#### Scenario: root, upload, and cue must be passed by name
- **WHEN** `ssd_scenario_targets()` is called with a positional argument after `scenario`, or a misspelled named argument
- **THEN** `rlang::check_dots_empty()` SHALL abort with an informative error, so `root`/`upload`/`cue` are only ever supplied by name

#### Scenario: upload defaults to no upload targets
- **WHEN** `ssd_scenario_targets(scenario, root = ...)` is called without `upload`
- **THEN** `upload` SHALL default to `NULL` and the returned target list SHALL contain no `upload_<step>` targets

#### Scenario: a non-NULL upload pairs each shard with an upload target
- **WHEN** `ssd_scenario_targets(scenario, upload = ssd_upload_dryrun())` is called
- **THEN** the returned target list SHALL contain one `upload_<step>` target per step shard, paired in the same `tar_map`, with the per-task results unchanged from the `upload = NULL` run

### Requirement: The shard invalidation model is pinned to content-hash over format = "file"
The shard step targets (`sample_step` / `fit_step` / `hc_step`, one per `partition_by` path cell) SHALL use **content-hash invalidation over their `format = "file"` Parquet outputs** as their pinned invalidation model (`TARGETS-DESIGN.md` §8), read observably as **cache-by-existence**: a shard SHALL be treated as up to date if and only if its output Parquet exists *and* the inputs its body depends on — its task-row set, the scenario fields it reads, and the parent shard target(s) it reads — are unchanged. A shard whose Parquet is missing SHALL rebuild; a shard whose inputs changed SHALL rebuild; a recomputed shard whose Parquet bytes are byte-identical to the prior write SHALL leave its dependents skipped (value-propagation). This model SHALL be a stated property of the pipeline factory, NOT an emergent `targets` default, so the downstream `path-axis-growth`, `shard-atomic-rewrite`, and `step-scenario-slice` changes finalise their expected cached-vs-rebuilt assertions against it.

#### Scenario: A shard with an existing, unchanged Parquet is a cache hit
- **WHEN** a scenario is run through the targets pipeline and `tar_make()` is run a second time with no inputs changed
- **THEN** every shard target SHALL be reported up to date and SHALL NOT be rebuilt, and no shard Parquet SHALL be rewritten

#### Scenario: A shard whose output Parquet is missing rebuilds
- **WHEN** a shard's `format = "file"` Parquet is deleted and `tar_make()` is run again
- **THEN** that shard target SHALL be rebuilt and its Parquet rewritten, while shards whose Parquets still exist and whose inputs are unchanged SHALL stay cached

### Requirement: Per-child upstream edges drive m:n fan-in invalidation
The pipeline factory SHALL wire each child shard target to the **specific parent shard target(s) its tasks read** (the Option-3 per-child upstream edges of `TARGETS-DESIGN.md` §6), computed at sourcing time as the distinct set of parent path cells the child shard's tasks project onto (`unique(path_key(tasks, partition_by[[parent]]))` — the same projection `read_parent_shards()` uses to read them). These per-child edges SHALL **replace** the coarse step-wide `tarchetypes::tar_combine()` barriers (`sample_done` / `fit_done`) as the **invalidation** mechanism between steps, so that rewriting one parent shard invalidates only the child shards that read it, NOT the whole downstream step. The set of parent shard targets a child names SHALL equal the set of parent shards `read_parent_shards()` opens for that child (one source of truth), so the dependency graph and the read are consistent. The portable partition-path read contract (the runner opening parent Parquets by path) SHALL remain unchanged.

#### Scenario: Rewriting one parent shard re-runs only the child shards that read it
- **WHEN** one `sample` shard's Parquet is rewritten with changed bytes (or invalidated) and `tar_make()` is run again
- **THEN** only the `fit` shards whose tasks read that `sample` shard SHALL be re-run, the `fit` shards that read other `sample` shards SHALL stay cached, and the same precise propagation SHALL hold for `fit → hc`

#### Scenario: A child shard names exactly the parents it reads
- **WHEN** the pipeline is sourced for a scenario whose `partition_by` makes a child shard span several parent shards (the m:n case)
- **THEN** that child shard target SHALL declare a dependency edge to each parent shard target it reads, and SHALL declare no edge to a parent shard it does not read

#### Scenario: Per-child edges replace the coarse step barrier
- **WHEN** the factory builds the step targets
- **THEN** the inter-step invalidation SHALL route through the per-child parent-shard edges rather than a step-wide `tar_combine()` barrier, so a single parent-shard change SHALL NOT mark the entire downstream step out of date

### Requirement: Code-pin and forced-refresh semantics against the pinned model
The pinned model SHALL define the `tar_cue()` pin and forced-refresh semantics the §8.3/§8.4 workflows rely on. `tar_cue(depend = FALSE)` on a shard step target SHALL pin it against upstream **dependency/code** changes (an edited per-task `_state`/primitive function or a bumped `ssdtools` version) so trusted shards are not rebuilt by such a change — with the carve-outs that the target SHALL still rebuild if its own `format = "file"` Parquet is missing, if its task-table grouping changes (so path-axis and inner-axis growth still apply under the pin), or if it previously errored (a `error = "null"` short/failed shard retries). `targets::tar_invalidate(names = ...)` (or deleting the shard's Parquet) SHALL force a refresh of the named shards, overriding the pin, so the mixed-code case (keep trusted shards, recompute a chosen few) is expressible against the pinned model.

#### Scenario: A code edit does not rebuild pinned shards
- **WHEN** a shard step target carries `tar_cue(depend = FALSE)`, a per-task primitive function it calls is edited, and `tar_make()` is run again
- **THEN** the pinned shards SHALL NOT be rebuilt, while a shard whose Parquet is missing or whose task-table grouping changed SHALL still rebuild

#### Scenario: Forced refresh overrides the pin for chosen shards
- **WHEN** `tar_invalidate()` is called on a chosen set of pinned shards (or their Parquets are deleted) and `tar_make()` is run again
- **THEN** exactly those shards SHALL be recomputed under the current code while the other pinned shards SHALL stay cached

### Requirement: The data-step-vs-fold decision is settled to keep the fold
Under the pinned content-hash model the deferred `TARGETS-DESIGN.md` §8 decision SHALL be resolved by **keeping the `fit`-inline `head(sample, nrow)` fold** (`task-list-loop-baseline-fold`): the pipeline SHALL NOT reinstate a materialised `data` step. Because a `fit` shard is keyed by `fit_id` (which includes `nrow`), extending `nrow` SHALL mint new `fit` shards and leave existing ones cached. The shared `sample` draw size is the scenario's fixed `nrow_max` setting (resolved per dataset), **not** `max(nrow)`, so extending `nrow` (within the effective draw size) SHALL NOT change the `sample` shard — the previously-feared "growing `n_max` leaves a stale short `sample` draw" hazard cannot arise, because the draw size no longer grows with `nrow`. The `sample` shard rebuilds only when an input within its slice changes (e.g. `nrow_max` or the dataset), not when `nrow` grows.

#### Scenario: Extending nrow leaves the sample shard cached
- **WHEN** a scenario is run, then re-run after adding an `nrow` value no greater than the effective draw size, and `tar_make()` is run again
- **THEN** the `sample` shard SHALL be skipped (cached) because its fixed `nrow_max` draw is unchanged, and only the new `nrow`-keyed `fit` shards (and downstream `hc`/`summary`) SHALL build — no `fit` shard SHALL read a stale shorter `sample` draw

#### Scenario: Changing nrow_max rebuilds the sample draw
- **WHEN** a scenario is run to completion, `nrow_max` is then changed, and `tar_make()` is run again
- **THEN** the `sample` shards SHALL rebuild with the new draw size (the change is within the `sample` slice), and the dependent `fit`/`hc`/`summary` shards SHALL rebuild through the per-child edge

#### Scenario: No materialised data step is introduced
- **WHEN** the pipeline factory builds the step targets for a scenario
- **THEN** the steps SHALL remain `sample` / `fit` / `hc` with the `head(sample, nrow)` truncation folded into `fit`, and no separate materialised `data` shard target SHALL be created

### Requirement: Each step target depends only on its minimal scenario slice
The `ssd_scenario_targets()` factory SHALL make each step's `tar_map()` command depend on only the **minimal slice** of the scenario that step's per-shard runner consumes — the resolved per-step inputs and the fields reaching that step's per-task body and its `shard_path()`/`read_parent_shards()` primer — rather than the bare `scenario` global. A step target's `targets` dependency hash SHALL therefore cover only its slice, so editing a scenario field outside a step's slice SHALL leave that step's shards cached. The package SHALL provide a deterministic, hashable `scenario_step_slice(scenario, step, datasets)` helper that returns this slice (preserving the `ssdsims_scenario` class so the runners' input contract is unchanged): `sample` consumes the datasets, `nrow_max`, and `partition_by$sample`; `fit` consumes `fit$dists`, the `min_pmix` functions, and `partition_by` for `sample` and `fit`; `hc` consumes `hc$proportion`, `hc$samples`, `hc$ci`, and `partition_by` for `fit` and `hc`. Because the `sample` slice carries datasets, the factory SHALL build it **per shard**, carrying only the dataset(s) that shard reads (its `unique(tasks$dataset)`) as a per-shard mapped value, so a sample shard depends on no dataset it does not draw from. The per-task results SHALL be unchanged (byte-identical to `ssd_run_scenario_baseline()`), because the slice carries exactly the fields the runner reads. The precise expected-cached set SHALL follow the invalidation model pinned by `hive-partitioning` (`TARGETS-DESIGN.md` §8).

#### Scenario: Changing an hc-only knob rebuilds only hc and summary
- **WHEN** a scenario is run to completion with `tar_make()`, an `hc`-only knob (e.g. `hc$samples` or `hc$ci`) is then changed, and `tar_make()` is run again
- **THEN** only the `hc` shards (and `summary`) SHALL rebuild, while every `sample` and `fit` shard SHALL be skipped (cached)

#### Scenario: Changing a fit-only knob leaves sample cached
- **WHEN** a scenario is run to completion with `tar_make()`, a `fit`-only knob (e.g. `fit$dists`) is then changed, and `tar_make()` is run again
- **THEN** the `sample` shards SHALL be skipped (cached), while the `fit` (and downstream `hc`/`summary`) shards SHALL rebuild

#### Scenario: Appending a dataset caches every existing shard
- **WHEN** a scenario is run to completion with `tar_make()`, a new dataset is appended (a path-axis growth that holds `partition_by` fixed), and the pipeline is re-sourced
- **THEN** only the new dataset's `sample`/`fit`/`hc` shards (and `summary`) SHALL build, while every pre-existing dataset's `sample`/`fit`/`hc` shard SHALL be skipped (cached) — the per-shard `sample` slice keeps each existing shard's command byte-identical when the dataset set grows

#### Scenario: A step's slice carries only the fields its runner reads
- **WHEN** `scenario_step_slice(scenario, step, datasets)` is computed for each of `sample`, `fit`, and `hc`
- **THEN** each slice SHALL be a deterministic, hashable `ssdsims_scenario`-classed object carrying exactly the fields that step's runner consumes (the named `datasets` + `nrow_max` / `fit` grid + `min_pmix` functions / `hc` knobs incl. `ci`, plus the step's own and parent `partition_by` axes) and SHALL omit fields no other-step runner reads, so re-sourcing the same scenario yields a byte-identical slice

### Requirement: Path-axis growth mints new shards and caches existing ones
Appending a value to an axis that **is** in a step's `partition_by` (a *path* axis — e.g. a new dataset, or a grown `nsim` adding `sim` values; both are path axes for the `sample`, `fit`, and `hc` steps) creates new partition cells for the affected steps. When such a value is appended to a scenario that has already been `tar_make()`'d into a per-layout results root, re-sourcing the pipeline SHALL mint **new** named `tar_map()` shard targets for the new path cells and `tar_make()` SHALL build **only** those new shards; every shard target that existed before SHALL be reported cached (skipped) by `targets` and its Parquet SHALL stay byte-identical (no in-place rewrite, no recomputation), because its command and `format = "file"` output are unchanged (`TARGETS-DESIGN.md` §8.1; §6 "extension is literally more named targets"). The `summary` fan-in SHALL re-run over the enlarged shard set (its input result layers gained the new shards' Parquets), without any pre-existing shard being recomputed to feed it. Because a path-axis growth keeps `partition_by` fixed, `scenario_results_dir()` SHALL be unchanged so the prior shards' outputs remain in the same root and can satisfy the cache. The precise expected cached-vs-built shard set is finalised against the invalidation model pinned by the `hive-partitioning` change — cache-by-existence over the m:n child↔parent fan-in's per-child upstream edges — together with the per-step minimal scenario slice from the `step-scenario-slice` change: the `sample` slice is built **per dataset**, so an appended dataset is a new path cell rather than a change to an existing shard's command, and `nsim` is in no step's slice, so growing it leaves every existing shard's command byte-identical. With both upstream changes landed, the contract therefore holds on the shipped default factory (no `cue`): an existing shard whose command and `format = "file"` output are unchanged is skipped.

#### Scenario: Appending a dataset builds only the new dataset's shards and caches the rest
- **WHEN** a scenario is run to completion through the targets pipeline (`ssd_scenario_targets()` + `tar_make()`), then a new dataset is appended to the scenario (a path axis for all three steps) leaving `partition_by` unchanged, and `tar_make()` is re-run into the same per-layout results root
- **THEN** new named shard targets SHALL be minted only for the new dataset's path cells across the `sample`, `fit`, and `hc` steps and only those SHALL be built; every original dataset's `sample`/`fit`/`hc` shard target SHALL be reported cached (skipped) and its Parquet SHALL be byte-identical to before; and `summary` SHALL re-run over the enlarged shard set

#### Scenario: Growing nsim builds only the new sim cells' shards and caches the rest
- **WHEN** the same completed scenario instead has `nsim` grown (adding new `sim` path cells for all three steps) leaving `partition_by` unchanged, and `tar_make()` is re-run into the same per-layout results root
- **THEN** new named shard targets SHALL be minted only for the added `sim` values' path cells across the `sample`, `fit`, and `hc` steps and only those SHALL be built; every prior `sim` value's shard target SHALL be reported cached (skipped) and its Parquet SHALL be byte-identical to before; and `summary` SHALL re-run over the enlarged shard set

### Requirement: Inner-axis growth atomically rewrites the affected shards byte-stably
Adding a value to an axis that is **not** in a step's `partition_by` (an *inner* axis — e.g. a new `min_pmix` for the `fit` step) changes the task-row set of every shard in that step's partition tree (each affected shard gains one task per added inner value). The targets pipeline SHALL react by marking exactly the affected step's shard branches stale, re-running each affected shard's body over its **full** new task set, and **overwriting** that shard's Parquet as a whole — one Parquet per shard, atomically rewritten, with **no** in-place append (`TARGETS-DESIGN.md` §8.2). Per-task RNG identity SHALL be preserved across the rewrite (each surviving task's `(seed, primer)` is unchanged), so the rows that were present in the prior Parquet SHALL re-emit **byte-identical** to that prior Parquet and the rewritten file SHALL differ from it only by the added task's row(s). Shards in steps the inner-axis growth does not touch (e.g. the `sample` shards when a `fit` inner axis grows) SHALL stay cached and SHALL NOT be rebuilt. Byte-stability of the re-emitted rows depends on deterministic Parquet writes (a fixed column order under a pinned `duckplyr`/DuckDB), which the rewrite SHALL rely on. The precise expected cached-vs-rebuilt shard set SHALL be finalised against the invalidation model pinned by the `hive-partitioning` change (the §8 cache-by-existence vs. content-hash fork); this requirement is **downstream** of that decision.

#### Scenario: Adding a min_pmix rewrites the affected fit shards byte-stably and leaves the rest cached
- **WHEN** a scenario is run through the targets pipeline, then re-run after adding one new `min_pmix` value to the scenario's `fit` grid (an inner axis for the `fit` step), and `tar_make()` is run again into the same per-layout results root
- **THEN** every `fit` shard affected by the new `min_pmix` SHALL be re-run and its Parquet overwritten with the larger task set; the rows present in that shard before the change SHALL read back byte-identical to the prior Parquet (the prior tasks' `(seed, primer)` unchanged) and the shard SHALL differ only by the added `min_pmix` task row(s); and the shards in steps not touched by the growth (the `sample` shards) SHALL be reported cached by `targets` and SHALL NOT be rebuilt

### Requirement: An optional full summary retains the dists/samples list-columns

`ssd_summarise()` SHALL accept an optional trailing `path_with_samples` argument
(default `NULL`). The compact summary written to `path` SHALL be unchanged — it
SHALL continue to project the `dists` and `samples` list-columns out at the
DuckDB level so the potentially-large per-row bootstrap draws are never pulled
into R. When `path_with_samples` is non-`NULL`, the function SHALL **additionally** write
a *full* summary to `path_with_samples` that unions the same `hc` shard glob but
**retains** the `dists`/`samples` list-columns. The full write SHALL be performed
at the DuckDB level (read the Hive glob, write Parquet) so the retained draws are
**never collected into R** — the same no-R-materialise guarantee as the compact
path. Both summaries SHALL read the result directory (`hive_partitioning =
FALSE`) rather than the shard target values, so the full summary inherits the
partial-failure-survival property and unions whatever shards landed
(`TARGETS-DESIGN.md` §6.2). When `path_with_samples` is `NULL`, `ssd_summarise()` SHALL
behave exactly as before (a single compact summary, no extra file).

#### Scenario: Full summary retains the draws the compact summary drops
- **WHEN** `ssd_summarise()` is run with a non-`NULL` `path_with_samples` over `hc` shards
  produced with `samples = TRUE`
- **THEN** the compact `path` SHALL omit the `dists`/`samples` columns, the full
  `path_with_samples` SHALL contain a populated `samples` list-column (and `dists`), and
  the estimate columns (`est`/`lcl`/`ucl`) SHALL be identical across the two files

#### Scenario: Without path_with_samples the behaviour is unchanged
- **WHEN** `ssd_summarise()` is run with `path_with_samples = NULL` (the default)
- **THEN** it SHALL write only the compact summary at `path`, projecting out
  `dists`/`samples` as before, and SHALL NOT write any second file

### Requirement: The pipeline emits the full summary only when the scenario retains draws

`ssd_scenario_targets()` SHALL pass `path_with_samples = <root>/summary-samples.parquet`
to the `summary` target's `ssd_summarise()` call **if and only if**
`scenario$hc$samples` is `TRUE` — the case where the retained draws carry
information the compact summary cannot. In that case the `summary` target SHALL
return the **vector** of both file paths so `targets` tracks both under its
`format = "file"` contract. When `scenario$hc$samples` is `FALSE`, the pipeline
SHALL be unchanged: the `summary` target writes the single compact
`summary.parquet` and returns its path.

#### Scenario: samples = TRUE yields two tracked summary files
- **WHEN** a scenario defined with `samples = TRUE` is run through the targets
  pipeline
- **THEN** the `summary` target SHALL write both `summary.parquet` and
  `summary-samples.parquet` and SHALL return both paths so `targets` tracks each
  as a `format = "file"` output

#### Scenario: samples = FALSE leaves the pipeline unchanged
- **WHEN** a scenario defined with `samples = FALSE` is run through the targets
  pipeline
- **THEN** the `summary` target SHALL write only `summary.parquet` and SHALL NOT
  write `summary-samples.parquet`

### Requirement: distset is a valid hc partition axis, bundled by default
`"distset"` SHALL be an accepted `hc` axis for both `partition_by` (path) and
`bundle` (inner), since it is a member of `task_axes("hc")`. The documented
default hc path SHALL remain `c("dataset", "sim")`, leaving `distset` as an
**inner** (bundled) hc axis by default: one hc shard then holds every declared
pool for its `(dataset, sim)` cell, so the shard runner decodes the parent union
fit once and serves all pools from it. A user MAY promote `distset` to a path
axis via `partition_by$hc` to obtain one shard per `(…, distset)` cell.

#### Scenario: distset accepted as an hc path axis
- **WHEN** `ssd_define_scenario(..., dists = list(BCANZ = ..., Iwasaki = ...), partition_by = list(hc = c("dataset", "sim", "distset")))` is called
- **THEN** the constructor SHALL accept it and the hc shards SHALL be keyed by `dataset`, `sim`, and `distset` (one shard per pool per `(dataset, sim)`)

#### Scenario: distset is bundled by default
- **WHEN** a distset scenario is constructed without an explicit `hc` `partition_by`
- **THEN** the hc path SHALL be `c("dataset", "sim")` and `distset` SHALL be an inner axis, so one hc shard carries all declared pools for each `(dataset, sim)` cell

### Requirement: The hc shard runner subsets the union fit per distset, decoding each parent fit once
The hc shard runner (`ssd_run_hc_step()`) SHALL read each distinct parent `fit`
shard the shard's tasks reference, decode each union fit **once** per `fit_id`,
and for each hc task subset that decoded fit to the task's `distset` members
before estimating the hazard concentration. Multiple `distset` tasks sharing a
`fit_id` SHALL reuse the one decoded fit (no repeated deserialisation), and each
output row SHALL be tagged with its `hc_id`, parent `fit_id`, and `distset`.

#### Scenario: One decode per fit serves every pool in the shard
- **WHEN** an hc shard bundles several `distset` tasks that share a parent `fit_id`
- **THEN** the runner SHALL decode that union fit once and subset it per `distset`, writing one Parquet for the shard with rows tagged by `hc_id`, `fit_id`, and `distset`

#### Scenario: Adding a distribution set mints only new hc shards
- **WHEN** a scenario gains an additional distribution set (with `distset` on the hc path) and the pipeline is re-run
- **THEN** only the new hc shards SHALL be built; every `sample` and `fit` shard, and every pre-existing hc shard, SHALL be served from cache (the fit layer carries no `distset` axis)

