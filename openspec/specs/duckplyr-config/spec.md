# duckplyr-config Specification

## Purpose
TBD - created by archiving change duckplyr-config. Update Purpose after archive.
## Requirements
### Requirement: Pipeline-scoped DuckDB resource configuration
The package SHALL provide an internal, withr-style scope helper
(`local_duckplyr_config()`) that configures duckplyr's managed DuckDB
connection for pipeline work and restores the prior configuration when the
scope exits. Within the scope the helper SHALL set DuckDB `threads` from
`SSDSIMS_DUCKDB_THREADS` (defaulting to `1` when unset), SHALL set DuckDB
`memory_limit` from `SSDSIMS_DUCKDB_MEMORY_LIMIT` (defaulting to `1GB` when
unset, so a worker is never one forgotten variable away from the engine's
machine-derived default), and SHALL set DuckDB `preserve_insertion_order` to
`false` (the byte-budgeted summary write is refused while preserving order,
and the engine consults only the global setting — a per-copy `PRESERVE_ORDER`
option cannot substitute). The per-shard step runners (`ssd_run_sample_step()`,
`ssd_run_fit_step()`, `ssd_run_hc_step()`), the summary fan-in
(`ssd_summarise()`), and the single-core runner (`ssd_run_scenario_shards()`)
SHALL each apply this scope for the duration of their body. The configuration
SHALL be scoped to the pipeline: outside an active scope the user's own
duckplyr/DuckDB settings SHALL be left untouched.

#### Scenario: Single thread by default inside a runner
- **WHEN** a step runner executes with `SSDSIMS_DUCKDB_THREADS` unset
- **THEN** DuckDB `threads` SHALL be `1` while the runner body executes

#### Scenario: Memory limit follows the environment variable
- **WHEN** a step runner executes with `SSDSIMS_DUCKDB_MEMORY_LIMIT=500MB`
- **THEN** DuckDB `memory_limit` SHALL be 500 MB while the runner body
  executes, and a write that exceeds it SHALL fail as a catchable R error
  (never an uncontrolled process kill by the engine)

#### Scenario: One gigabyte by default
- **WHEN** a step runner executes with `SSDSIMS_DUCKDB_MEMORY_LIMIT` unset
- **THEN** DuckDB `memory_limit` SHALL be 1 GB while the runner body executes

#### Scenario: Prior settings are restored on scope exit
- **WHEN** the user has set custom `threads`/`memory_limit` on duckplyr's
  managed connection and then calls a step runner or `ssd_summarise()`
- **THEN** after the call returns (or errors) the connection SHALL report the
  user's custom values again, not the pipeline values and not DuckDB defaults

#### Scenario: Nested scopes are safe
- **WHEN** `ssd_run_scenario_shards()` (which opens a scope) invokes a step
  runner (which opens its own scope)
- **THEN** the inner scope's exit SHALL restore the outer scope's settings and
  the outer scope's exit SHALL restore the original settings

#### Scenario: Insertion order is relaxed within the scope and restored after
- **WHEN** a step runner or `ssd_summarise()` executes
- **THEN** DuckDB `preserve_insertion_order` SHALL be `false` while the body
  executes and SHALL be back at its pre-call value after the call returns (or
  errors)

### Requirement: Pipeline-context duckplyr telemetry silence
Within the configuration scope the package SHALL disable duckplyr's
fallback-telemetry collection and its autoupload prompt by scoping the
environment variables `DUCKPLYR_FALLBACK_COLLECT=0` and
`DUCKPLYR_FALLBACK_AUTOUPLOAD=0`, so pipeline-context duckplyr use SHALL NOT
write fallback-telemetry logs and SHALL NOT seed the interactive attach-time
telemetry banner. The user's environment values for these variables SHALL be
restored when the scope exits, and the user's persisted duckplyr fallback
configuration SHALL NOT be modified.

#### Scenario: No telemetry logs from pipeline work
- **WHEN** a duckplyr operation inside the configuration scope falls back to
  dplyr
- **THEN** no fallback-telemetry log SHALL be collected for it

#### Scenario: User telemetry preference restored
- **WHEN** the user had `DUCKPLYR_FALLBACK_COLLECT` set (or unset) before
  entering a runner
- **THEN** after the runner returns the variable SHALL hold its prior value
  (or be unset) again

### Requirement: Configuration never changes results
The pipeline-scoped configuration SHALL affect resource usage and chatter
only: for a fixed scenario, all written Parquets SHALL be value-identical
(the same multiset of rows per file) whatever `SSDSIMS_DUCKDB_THREADS` and
(sufficient) `SSDSIMS_DUCKDB_MEMORY_LIMIT` values are in effect. Under the
default single-threaded configuration the written shard Parquets and the
compact summary SHALL additionally be byte-identical across runs (one
producer writes in input order even with insertion order relaxed); with
`SSDSIMS_DUCKDB_THREADS` raised above one, row order within written files is
not guaranteed and byte-identity MAY be lost (values are unaffected).

#### Scenario: Byte-identical shards under constrained configuration
- **WHEN** the same scenario is run once with default configuration and once
  with `SSDSIMS_DUCKDB_THREADS=1` and a sufficient
  `SSDSIMS_DUCKDB_MEMORY_LIMIT`
- **THEN** every produced shard Parquet and the compact summary SHALL be
  byte-identical between the two runs, and the full summary SHALL contain the
  same multiset of rows

### Requirement: The full summary writes byte-budgeted row groups
`ssd_summarise()` SHALL write the full summary (`path_with_samples`, the
output retaining the `dists`/`samples` list-columns) with an explicit Parquet
`ROW_GROUP_SIZE_BYTES` budget (default about 100 MB, exposed as an argument),
relying on the configuration scope's `preserve_insertion_order = false` —
so the write's memory requirement is bounded by the row-group byte budget,
not by the union's total row count, and the row-group row count adapts to
the `samples` cell size (large groups for small draws, small groups for
large draws). Because insertion order is relaxed, the full summary's
guarantee is **value-identity**: re-summarising the same shards SHALL yield
the same multiset of rows, while row order and file bytes MAY differ;
consumers SHALL address rows by their keys (`hc_id`/`fit_id`), never by
position. The compact summary write (list-columns projected out) SHALL keep
the engine's default row-group sizing.

#### Scenario: Union memory is flat in total rows
- **WHEN** `ssd_summarise()` writes a full summary unioning more rows than fit
  one row-group byte budget (e.g. thousands of rows of 50k-double `samples`
  cells) under a DuckDB `memory_limit` of 1 GB
- **THEN** the write SHALL succeed, and the output's Parquet metadata SHALL
  show row groups sized to the byte budget rather than a single row group
  spanning the union

#### Scenario: Row groups adapt to the sample size
- **WHEN** the `samples` cells are small (e.g. the default `nboot = 1000`)
- **THEN** the written row groups SHALL hold correspondingly many rows
  (approximately the byte budget divided by the cell payload), not a fixed
  small row count

#### Scenario: Value-identity across re-runs
- **WHEN** the same shard inputs are summarised twice with the same
  configuration
- **THEN** the two full summaries SHALL contain the same multiset of rows
  (equal after ordering by key), and the compact summaries SHALL be
  byte-identical

#### Scenario: Insertion order is restored after the summary returns
- **WHEN** `ssd_summarise()` returns (or errors)
- **THEN** the connection's `preserve_insertion_order` setting SHALL be back
  at its pre-call value (the configuration scope's restore)

### Requirement: Nested-shard memory sizing is documented
The helper's documentation and the cluster template SHALL state the empirical
sizing rule for nested shards (measured in this change's `exploration/`
scripts, duckdb 1.5.2): writing an `hc` shard whose `samples` list-column
holds `P` bytes of draws requires `memory_limit` of at least about `5 × P`,
because the engine demands the list column's child array as a single
allocation and cannot spill or stream it (`preserve_insertion_order`,
`temp_directory`, and `ROW_GROUP_SIZE` do not lower the floor). The guidance
SHALL name the two remedies: raise `SSDSIMS_DUCKDB_MEMORY_LIMIT` (within the
job allocation) or reduce the per-shard `samples` payload (`bundle`, `nboot`).
The documentation SHALL also state how to **raise** the default 1 GB limit
(the env var, set in the controller's `script_lines` on a cluster or via
`Sys.setenv()` interactively) and the **implications** of raising it: the
limit must leave headroom for R's own footprint within the scheduler
allocation, and a higher limit addresses only the shard-payload floor — the
full summary's floor follows its row-group byte budget and never needs it.

#### Scenario: The environment variable is documented where it is set
- **WHEN** a user reads the cluster template's controller block or the
  helper's documentation
- **THEN** they SHALL find `SSDSIMS_DUCKDB_MEMORY_LIMIT`, its 1 GB default,
  how to raise it and the implications of doing so (scheduler-allocation
  headroom; shard floor only), and the nested-shard `≳ 5 × payload` rule with
  a pointer to the measurements

