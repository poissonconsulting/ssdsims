## MODIFIED Requirements

### Requirement: partition_by is a free re-layout — per-task results are byte-identical to the in-memory baseline
The per-task **result** values produced by the sharded runner SHALL be identical
to those of the in-memory baseline runner (`ssd_run_scenario_baseline()`) for
the same scenario and seed, because the per-task `(seed, primer)` is derived
from the task's canonical identity (`task_axes(step)`) and is invariant under
`partition_by`. Changing a step's `partition_by` SHALL change only the shard
file paths and the path/inner column placement, never the per-task result
values. The `fit` and `hc` layers additionally carry the run-specific timing
columns `.start`/`.end`/`.host` (the `cost-analysis` capability); those columns
are **excluded** from the identity contract — equality is asserted over the
result columns (every column except the timing columns), joined on the task's
`<step>_id`. The `sample` layer carries no timing columns, so its shards remain
byte-deterministic at the file level for a fixed scenario and seed.

#### Scenario: Results match the in-memory oracle
- **WHEN** a scenario is run through both `ssd_run_scenario_baseline()` and the sharded runner
- **THEN** the per-task result rows (joined on the task's `<step>_id` identity, the `.start`/`.end`/`.host` columns excluded) SHALL be equal

#### Scenario: Re-layout shifts paths but not results
- **WHEN** the same scenario is run twice through the sharded runner with two different `partition_by` settings for a step
- **THEN** the set of shard file paths SHALL differ, but the per-task result rows (joined on `<step>_id`, timing columns excluded) SHALL be byte-identical between the two runs

#### Scenario: sample shards stay file-level deterministic
- **WHEN** the same scenario and seed produce a `sample` shard twice
- **THEN** the two `part.parquet` files SHALL be byte-identical (no timing columns are written on the `sample` layer)
