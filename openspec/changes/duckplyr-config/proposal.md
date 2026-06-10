# duckplyr-config

## Why

The targets pipeline runs duckplyr/DuckDB on `crew` workers that are deliberately
small — the shipped cluster template requests **one CPU and a few GB** per SLURM
job — but duckplyr's managed DuckDB instance defaults to **all the node's cores
and ~80% of the node's RAM**. On a shared cluster node the worker therefore
oversubscribes its CPU allocation and overruns its memory cgroup (an OOM kill,
not a clean R error). Local experiments (`exploration/`) confirm the pressure
point is the nested `samples` list-column an `hc` shard carries when
`scenario$hc$samples = TRUE`: a single 50k-double cell is harmless (writes in
milliseconds under a 100 MB `memory_limit`), but a realistic shard of 1000 such
rows (~400 MB of draws) peaks at ~2.3 GB RSS and needs a 2 GB `memory_limit`
(1.5 GB still fails) — and **none** of DuckDB's documented mitigations
(`preserve_insertion_order`, `temp_directory` spilling, `ROW_GROUP_SIZE`) move
that point, because the failing allocation is the list column's entire child
array as one buffer. The control clinches it: the *same* 50M doubles written
**flat** (long format) pass under a **100 MB** limit — nesting, not payload,
is the culprit. So the pipeline must (a) pin threads and bound memory
explicitly, and (b) size the bound to the nested-shard floor rather than assume
DuckDB can spill its way under an arbitrary limit.

Separately, duckplyr's fallback telemetry is noisy around the pipeline: with the
default configuration every fallback event is logged to the user's home
directory, and the next interactive `library(duckplyr)` then greets the user
with a multi-line "fallback events can be collected and uploaded…" banner
(experiment: `exploration/experiment-duckplyr-noise.R`). This folds in the
`duckplyr-message` roadmap item ("Turn off noise from duckplyr").

## What Changes

- New internal RAII helper (`local_duckplyr_config()`, withr-style) that, for
  the duration of a pipeline scope:
  - pins DuckDB to a **single thread** (`SET threads TO 1` — the worker has one
    CPU; the experiments show the single-thread write is no slower, and was 2×
    *faster* on the large nested shard),
  - sets an explicit **`memory_limit`** from `SSDSIMS_DUCKDB_MEMORY_LIMIT`
    (unset = DuckDB's default; the cluster template sets it beside the SLURM
    memory request, the same `script_lines` channel that carries
    `SSDSIMS_AZURE_*` to workers),
  - disables duckplyr **fallback-telemetry collection and the autoupload nudge**
    (`DUCKPLYR_FALLBACK_COLLECT=0`, `DUCKPLYR_FALLBACK_AUTOUPLOAD=0`, scoped
    env vars), so pipeline-context duckplyr use never seeds the interactive
    attach-time banner,
  - and **restores** the previous settings when the scope exits, so a user's
    own duckplyr session configuration is untouched.
- The per-shard step runners (`ssd_run_sample_step()`, `ssd_run_fit_step()`,
  `ssd_run_hc_step()`), the summary fan-in (`ssd_summarise()`), and the
  single-core runner (`ssd_run_scenario_shards()`) apply the helper — duckplyr
  is configured **just in the context of the targets pipeline**, exactly as the
  roadmap item scopes it.
- Documented sizing guidance (design.md + the helper's docs): writing an `hc`
  shard whose `samples` list-column holds `P` bytes of draws needs
  `memory_limit` ≳ 5 × `P`; the limit is a knob, not a magic constant, and the
  experiments' RESULT tables are the evidence.
- Per-task results stay **byte-identical**: threads, memory limits, and
  telemetry switches change resource usage and chatter only, never values.

## Capabilities

### New Capabilities

- `duckplyr-config`: pipeline-scoped DuckDB/duckplyr configuration — single
  thread, explicit memory limit, telemetry silence, restore-on-exit — applied by
  the step runners and the summary fan-in, with the nested-list memory floor
  documented.

### Modified Capabilities

<!-- none: the runners' observable results are unchanged (byte-identical
     Parquet shards); the configuration is an additive requirement captured in
     the new capability. -->

## Impact

- **Code**: `R/targets-runner.R` (step runners, `ssd_summarise()`),
  `R/shard-runner.R` (`ssd_run_scenario_shards()`), a new internal helper file;
  tests in `tests/testthat/`.
- **Templates**: `inst/targets-templates/*` gain a comment pointing at the
  memory knob next to the controller's `memory_gigabytes_per_cpu`.
- **Dependencies**: none added — `duckplyr::db_exec()` and `withr` are already
  in use.
- **Docs**: roadmap items `duckplyr-config` and `duckplyr-message` are both
  delivered by this change.
