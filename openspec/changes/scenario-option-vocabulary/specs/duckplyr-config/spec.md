# duckplyr-config Delta

## MODIFIED Requirements

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
