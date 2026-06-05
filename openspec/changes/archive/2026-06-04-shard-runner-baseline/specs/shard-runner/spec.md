## ADDED Requirements

### Requirement: Single-core sharded runner materialises each step as Hive-partitioned shards
The package SHALL provide a single-core runner that executes a scenario's three task steps in dependency order — `sample`, then `fit`, then `hc` — and, for each step, groups the step's task rows into **shards** by that step's `partition_by` **path axes** (`scenario_partition_axes(scenario, step)$path`) and writes **one Parquet file per shard** under a Hive-partitioned path `<dir>/<step>/<axis=value>/.../part.parquet`. Each shard's Parquet SHALL carry the step's **inner** axes (the path complement) and the per-task results as columns. The runner SHALL run each shard's tasks through the existing per-task seed-and-run wrappers (`*_data_task_primer()`), priming the RNG once per task. The runner SHALL operate in-process on a single core with **no** `targets` dependency, **no** `crew`, and **no** cloud upload.

#### Scenario: One Parquet per shard under a Hive path
- **WHEN** the sharded runner is called on a scenario whose step `S` has path axes `path[S]`
- **THEN** it SHALL write exactly `Π |path axis value|` Parquet files for step `S`, each at `<dir>/<S>/<axis=value>/.../part.parquet` keyed by that shard's path-axis values, and each file SHALL contain the step's inner-axis columns and per-task result column(s)

#### Scenario: Shard count follows partition_by, not task count
- **WHEN** a step bundles multiple tasks into one shard (its inner axes vary within the path cell)
- **THEN** those tasks SHALL be written to a single Parquet file (one shard), so the number of files equals the path-cell count, not the task count

#### Scenario: No targets, crew, or upload
- **WHEN** the sharded runner runs
- **THEN** it SHALL complete in-process on a single core without constructing a `targets` pipeline, launching `crew` workers, or performing any cloud upload

### Requirement: Steps link by reading parent shards via duckplyr, resolving m:n dependencies
For each non-root step, the runner SHALL obtain each task's parent result by **reading the parent step's shards back from disk via duckplyr** (`duckplyr::read_parquet_duckdb()` over the parent's Hive glob) and filtering to the rows the child needs, rather than from an in-memory result list. The runner SHALL resolve the parent dependency as a **many-to-many** relationship: for a given child shard it SHALL compute the **distinct set of parent shard paths** spanned by that shard's tasks (the `<parent>_id` full-identity foreign key projected onto the parent's path axes) and read exactly those parent shards, filtering on the parent's path and inner columns via predicate pushdown. The runner SHALL NOT assume that a child shard maps to exactly one parent shard. When a child task's parent result is **absent** from the parent shards it read, the runner SHALL abort with a clear message naming the missing parent identity, rather than proceeding on empty input — symmetrically across steps (the `fit` step on a missing `sample` draw and the `hc` step on a missing or duplicated `fit` result).

#### Scenario: A child shard reading several parent shards
- **WHEN** a child shard's tasks span multiple parent shards (e.g. with `replace = c(FALSE, TRUE)` a `fit` shard whose `replace` axis is inner spans two `sample` shards, or under the default split an `hc` shard spans several `fit` shards across `nrow`/`rescale`)
- **THEN** the runner SHALL read the full set of parent shards that shard's tasks reference and produce results identical to the in-memory baseline runner for the same scenario

#### Scenario: A parent shard feeding several child shards
- **WHEN** one parent shard's rows are needed by more than one child shard (the child shards finer on a non-shared path axis)
- **THEN** each child shard SHALL read the parent shard and filter to the rows it needs, and no parent row SHALL be attributed to the wrong child

#### Scenario: Parent lookup goes through Parquet, not memory
- **WHEN** a child step resolves a parent result
- **THEN** it SHALL read the parent value from the parent step's written Parquet shard(s) via duckplyr, not from an in-process named list

#### Scenario: A missing parent result aborts with a clear message
- **WHEN** a child task's parent identity resolves to no rows in the parent shards it read (a missing `sample` draw for the `fit` step, or a missing/duplicated `fit` result for the `hc` step)
- **THEN** the runner SHALL abort with a message naming the missing parent identity, rather than fitting or estimating on empty input

### Requirement: partition_by is a free re-layout — per-task results are byte-identical to the in-memory baseline
The per-task results produced by the sharded runner SHALL be identical to those of the in-memory baseline runner (`ssd_run_scenario_baseline()`) for the same scenario and seed, because the per-task `(seed, primer)` is derived from the task's canonical identity (`task_axes(step)`) and is invariant under `partition_by`. Changing a step's `partition_by` SHALL change only the shard file paths and the path/inner column placement, never the per-task result values.

#### Scenario: Results match the in-memory oracle
- **WHEN** a scenario is run through both `ssd_run_scenario_baseline()` and the sharded runner
- **THEN** the per-task result rows (joined on the task's `<step>_id` identity) SHALL be equal

#### Scenario: Re-layout shifts paths but not results
- **WHEN** the same scenario is run twice through the sharded runner with two different `partition_by` settings for a step
- **THEN** the set of shard file paths SHALL differ, but the per-task result rows (joined on `<step>_id`) SHALL be byte-identical between the two runs

### Requirement: Sharded run is reproducible and order-independent
For a fixed `scenario$seed`, the sharded runner SHALL be reproducible without an external seed and SHALL be independent of the order in which shards are processed, because each task installs its own `(seed, primer)` exactly once via its `*_data_task_primer()` wrapper.

#### Scenario: Re-running yields identical shards
- **WHEN** the sharded runner is run twice on the same scenario with a fixed `scenario$seed`
- **THEN** the written shards SHALL contain identical per-task results across the two runs

#### Scenario: Shard processing order does not affect results
- **WHEN** the shards within a step are processed in a different order
- **THEN** the per-task results SHALL be unchanged

### Requirement: The runner owns its output tree (no stale shards across re-runs)
A shard's Hive path depth and axes are a function of `partition_by`/`bundle`, and the readers glob `<step>/**/part.parquet` (depth-agnostic). `ssd_run_scenario_shards()` SHALL therefore **own** its output tree: it SHALL clear each step's subtree (`<dir>/<sample,fit,hc>`) before writing it, so the on-disk Hive tree always reflects the current scenario's `partition_by`. Re-running a scenario with a changed `partition_by`/`bundle` into the same `dir` SHALL NOT leave shards of a prior, different-granularity layout beside the new ones. Only the three step subtrees are owned; other content under `dir` (e.g. a sibling `summary.parquet`) SHALL be left untouched.

#### Scenario: A changed partition_by leaves only the current layout's shards
- **WHEN** `ssd_run_scenario_shards()` is run into a `dir`, then run again into the same `dir` with a coarser (or finer) `partition_by`
- **THEN** each step subtree SHALL contain only the second run's shards (the prior layout's shards are removed), so a glob read unions no stale, different-granularity shards
