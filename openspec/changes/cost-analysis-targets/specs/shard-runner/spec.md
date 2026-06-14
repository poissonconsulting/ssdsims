## ADDED Requirements

### Requirement: Per-task timings are captured in-band on the fit and hc layers
The `fit` and `hc` step runners SHALL bracket each task's body
(`ssd_run_fit_step()`, `ssd_run_hc_step()`) and record three columns in the
shard's `part.parquet`: `.start` and `.end` (UTC timestamps of the task body's
start and finish) and `.host` (the machine's CPU description, the grain the
architecture-specific cost calibration pools on and the run's calibration
provenance). On the `hc` layer the values repeat across a task's `proportion`
rows. The timings SHALL be stored **in the shard Parquet itself, not in a
sidecar file** — the shard-file count is the object-count budget at deliverable
scale. The `sample` step SHALL NOT be timed and its output SHALL be unchanged.
Timing capture SHALL NOT draw random numbers or alter any per-task result value.

#### Scenario: fit and hc shards carry timing columns
- **WHEN** a `fit` or `hc` shard is written by its step runner
- **THEN** the shard Parquet SHALL contain `.start`, `.end`, and `.host` for every task, with `.end` at or after `.start`, and no additional file SHALL be created beside `part.parquet`

#### Scenario: sample shards are unchanged
- **WHEN** a `sample` shard is written
- **THEN** its Parquet SHALL contain no timing columns and its bytes SHALL remain deterministic for a fixed scenario and seed

#### Scenario: Timing capture is RNG-neutral
- **WHEN** the same scenario and seed are run with timing capture
- **THEN** every per-task result value SHALL be byte-identical to the pre-instrumentation runner's (the timing columns excluded)

### Requirement: The baseline runner carries the same timing columns
`ssd_run_scenario_baseline()` SHALL record the same `.start`/`.end`/`.host`
columns on its in-memory `fit` and `hc` result tibbles, so a run is calibratable
whatever produced it — baseline, single-core sharded, or `targets` — and the
byte-identity oracle compares result columns symmetrically. The legacy
`ssd_sim_data()` / `ssd_fit_dists_sims()` / `ssd_hc_sims()` family SHALL NOT
change under this capability.

#### Scenario: Baseline fit/hc tibbles carry timings
- **WHEN** `ssd_run_scenario_baseline()` completes
- **THEN** its `fit` and `hc` tibbles SHALL carry `.start`, `.end`, and `.host` per task, alongside unchanged result columns

### Requirement: Summaries retain the fit and hc timing columns
`ssd_summarise()` SHALL retain the `.start`/`.end`/`.host` columns in both the
compact summary and the optional `path_with_samples` full summary (it SHALL
continue to project out `dists`/`samples` from the compact summary), so the run
summary alone is sufficient input to `ssd_calibrate_cost_from_run()` — including
when the shards are remote after upload.

#### Scenario: summary.parquet carries the timing columns
- **WHEN** `ssd_summarise()` writes the compact summary for a run with timing columns
- **THEN** the summary SHALL contain `.start`/`.end`/`.host` per `hc` row while still excluding `dists`/`samples`

## MODIFIED Requirements

### Requirement: partition_by is a free re-layout — per-task results are byte-identical to the in-memory baseline
The per-task **result** values produced by the sharded runner SHALL be identical
to those of the in-memory baseline runner (`ssd_run_scenario_baseline()`) for
the same scenario and seed, because the per-task `(seed, primer)` is derived
from the task's canonical identity (`task_axes(step)`) and is invariant under
`partition_by`. Changing a step's `partition_by` SHALL change only the shard
file paths and the path/inner column placement, never the per-task result
values. The `fit` and `hc` layers additionally carry the run-specific timing
columns `.start`/`.end`/`.host`; those columns are **excluded** from the identity
contract — equality is asserted over the result columns (every column except the
timing columns), joined on the task's `<step>_id`. The `sample` layer carries no
timing columns, so its shards remain byte-deterministic at the file level for a
fixed scenario and seed.

#### Scenario: Results match the in-memory oracle
- **WHEN** a scenario is run through both `ssd_run_scenario_baseline()` and the sharded runner
- **THEN** the per-task result rows (joined on the task's `<step>_id` identity, the `.start`/`.end`/`.host` columns excluded) SHALL be equal

#### Scenario: Re-layout shifts paths but not results
- **WHEN** the same scenario is run twice through the sharded runner with two different `partition_by` settings for a step
- **THEN** the set of shard file paths SHALL differ, but the per-task result rows (joined on `<step>_id`, timing columns excluded) SHALL be byte-identical between the two runs

#### Scenario: sample shards stay file-level deterministic
- **WHEN** the same scenario and seed produce a `sample` shard twice
- **THEN** the two `part.parquet` files SHALL be byte-identical (no timing columns are written on the `sample` layer)
