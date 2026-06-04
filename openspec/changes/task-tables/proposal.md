## Why

The baseline runner (`task-lists`) proves the three-step data shape in-process, but the targets/cluster path needs the rows **grouped into shards** and wired into a pipeline. `TARGETS-DESIGN.md` §6 specifies the missing pieces: per-step *shard* tables that group the task rows by `partition_by` into one row per shard (a `tasks` list-column for `tar_map`'s `values`), each task row carrying its `(seed, primer)`; per-shard step runners that read the upstream Parquet by partition path, prime the RNG once per task, and write one Parquet per shard; and a **static-branching** `_targets.R` that mints one named target per shard. This change lands the first ssdsims pipeline that compiles and `tar_make()`s a tiny scenario end-to-end, reusing the validated per-task primitives so its results are byte-identical to the baseline runner.

## What Changes

- Add `ssd_scenario_sample_shards()` / `_fit_shards()` / `_hc_shards()` — group each step's task table by its `partition_by` **path** axes (via `scenario_partition_axes()`) into one row per shard. Each shard row carries the path-axis columns (the `tar_map` target-name suffix and Hive path) and a `tasks` list-column of that shard's task rows, each decorated with `seed = scenario$seed`, its `primer` (`task_primer()` over `task_axes(step)`), and the upstream partition-path columns needed to open its parent shard.
- Add per-shard step runners `ssd_run_sample_step()` / `ssd_run_fit_step()` / `ssd_run_hc_step()` — loop the shard's tasks, install each task's `(seed, primer)` once via the existing `*_data_task_primer()` wrappers, read the matching upstream shard's Parquet by partition path, and write the shard's Parquet; read datasets via `scenario_dataset()` and `min_pmix` via `scenario_min_pmix()` (the `scenario-accessors` change). (No `manifest` involvement: the pipeline produces shards; provenance/verification metadata is recorded by the downstream `manifest`/`replay-helper`/`shard-completeness-assert` steps, not here — see design.)
- Add `ssd_summarize()` — a fan-in that reads the three result layers (via `duckplyr`) without pulling every shard value back into R (§6).
- Ship a static-branching `_targets.R` template (`inst/targets-templates/local/`) using `tarchetypes::tar_map()` over the shard tables; the scenario is a plain construction-time object (no `tar_target`), so the shard set is fixed at sourcing time (§6). The step targets carry `error = "null"` so a failing **whole shard** records its error and goes `NULL` without aborting the run — the other shards still build and `ssd_summarize()` unions what landed (§6.2).
- Introduce `targets`, `tarchetypes`, and `duckplyr` (the team's Parquet engine — Parquet I/O for the shard read/write and the summary fan-in).

## Capabilities

### New Capabilities
- `task-shards`: group a step's task rows into per-shard tables keyed by `partition_by`, run a shard with the per-task RNG primitives writing one Parquet per shard, and assemble a static-branching `targets` pipeline that `tar_make()`s a scenario with results byte-identical to the baseline runner.

### Modified Capabilities
<!-- None: the `(seed, primer)` decoration lives on the shard task rows (computed at grouping time, RNG-free), so the `task-lists` derivations keep their "no seed/primer columns on derivation" contract unchanged. -->

## Impact

- **New code**: `R/task-shards.R` (the `*_shards()` grouping wrappers, reusing `task_axes()`/`path_key()`/`task_primers()` from `R/task-lists.R`); `R/targets-runner.R` (the three `ssd_run_*_step()` runners and `ssd_summarize()`); `inst/targets-templates/local/_targets.R`. Tests in `tests/testthat/test-task-shards.R` and a `tar_make()` integration test.
- **APIs**: New exports for the shard wrappers, step runners, and `ssd_summarize()`. Roxygen/`man/` and a `_pkgdown.yml` "Targets pipeline" reference group.
- **Dependencies**: adds `targets` and `tarchetypes` to `Imports` (`duckplyr` — Parquet I/O, the team preference — arrives with `registry`).
- **On-disk layout**: writes `results/{sample,fit,hc}/<partition-path>/part.parquet` and `results/summary.parquet` (§6).
- **Prerequisites**: `registry` (datasets persisted, `min_pmix` resolved), `partition-by` (the `scenario_partition_axes()` path/inner split and the three-step `sample`/`fit`/`hc` defaults), and `primer-primitives` (the `*_data_task_primer()` wrappers — already landed). **`manifest` is *not* a prerequisite** — it is provenance/verification metadata that depends on this step's outputs, not the reverse, and feeds `replay-helper` / `shard-completeness-assert` (see design). `shard-failure-survival`, `hive-partitioning`, and `cloud-upload` build on this step (the happy-path pipeline here; `error = "null"` survival, Hive predicate-pushdown, and per-shard upload land in those steps).
