# duckplyr-config — tasks

## 1. The scope helper (`R/duckplyr-config.R`)

- [x] 1.1 Implement internal `local_duckplyr_config(.local_envir = parent.frame())`:
  snapshot `threads` and `memory_limit` via
  `duckplyr::read_sql_duckdb("SELECT current_setting('threads') …")`, apply
  `SET threads TO <SSDSIMS_DUCKDB_THREADS or 1>` and
  `SET memory_limit = '<SSDSIMS_DUCKDB_MEMORY_LIMIT or 1GB>'` via
  `duckplyr::db_exec()`, and register a
  `withr::defer(…, envir = .local_envir)` that `SET`s the snapshotted values
  back (restore, never `RESET` — design D1)
- [x] 1.2 Scope `DUCKPLYR_FALLBACK_COLLECT = "0"` and
  `DUCKPLYR_FALLBACK_AUTOUPLOAD = "0"` in the same helper via
  `withr::local_envvar(…, .local_envir = .local_envir)` (design D3)
- [x] 1.3 Validate the env knobs with `chk`-style errors raised in the caller's
  context: `SSDSIMS_DUCKDB_THREADS` must parse as a positive whole number,
  `SSDSIMS_DUCKDB_MEMORY_LIMIT` is passed through to DuckDB (its parser is the
  authority) — a malformed value should surface DuckDB's own message, not a
  silent ignore
- [x] 1.4 Roxygen-document the helper (internal, `@noRd`): the two env knobs and
  their defaults, the restore-on-exit contract, the connection-global caveat
  while a scope is open, and the nested-shard sizing rule (`memory_limit ≳ 5 ×`
  the shard's `samples` payload; engine cannot spill a LIST child array —
  evidence in the `duckplyr-config` change's `exploration/` scripts)

## 2. Wire into the pipeline entry points

- [x] 2.1 Call `local_duckplyr_config()` at the top of `ssd_run_sample_step()`,
  `ssd_run_fit_step()`, and `ssd_run_hc_step()` (beside the existing
  `local_dqrng_backend()` calls in `R/targets-runner.R`)
- [x] 2.2 Call it at the top of `ssd_summarise()` (the lazy reads must be
  computed inside the scope) and of `ssd_run_scenario_shards()`
  (`R/shard-runner.R`, beside its run-wide `local_dqrng_backend()`)
- [x] 2.3 `air format .`; `devtools::document()`

## 3. Byte-budgeted row groups for the full summary (`R/targets-runner.R`)

- [x] 3.1 Add a `samples_row_group_bytes` argument to `ssd_summarise()`
  (default `"100MB"`); pass
  `options = list(row_group_size_bytes = <value>)` to the
  `path_with_samples` `compute_parquet()` call; the compact-summary write
  keeps default options and the ordered writer
- [x] 3.2 Hold `preserve_insertion_order = false` in `local_duckplyr_config()`
  (snapshotted and restored like `threads`/`memory_limit`) rather than around
  the one write: probed in
  `exploration/experiment-preserve-order-copy-option.R`, the relaxation
  cannot be passed per write (`preserve_insertion_order` is not a COPY
  option; `PRESERVE_ORDER` is, but the `ROW_GROUP_SIZE_BYTES` Binder check
  consults only the global setting), and the engine refuses the bytes option
  without it (Binder error), so a regression fails loud, never silently; the
  interim per-write wrapper `compute_parquet_unordered()` was dropped
- [x] 3.3 Roxygen-document the argument: why a byte budget (the engine buffers
  whole row groups of nested `samples`; the default ordered writer puts the
  whole union in ONE group, so its memory floor grows with total rows —
  evidence in the `duckplyr-config` change's
  `exploration/experiment-summary-union.R` and `exploration/
  experiment-rgbytes.R`), the value-identity (not byte-identity) contract of
  the full summary (row order non-contractual; address rows by key), the
  ~5×-budget memory floor, and the pushdown-granularity trade-off

## 4. Tests (`tests/testthat/test-duckplyr-config.R`)

- [x] 4.1 Scope behaviour: inside a `local_duckplyr_config()` scope with the
  knobs unset, `threads` reports `1` and `memory_limit` reports 1 GB (the
  default); with `SSDSIMS_DUCKDB_MEMORY_LIMIT` set (via
  `withr::local_envvar()`), `memory_limit` reports it
- [x] 4.2 Restore behaviour: set custom `threads`/`memory_limit` first, run a
  scope (normal exit and error exit), assert the custom values are reported
  after; nested scopes restore layer by layer (spec scenarios "Prior settings
  are restored" / "Nested scopes are safe")
- [x] 4.3 Telemetry env vars: inside the scope `Sys.getenv()` reports the two
  `DUCKPLYR_FALLBACK_*` values as `"0"`; after the scope the prior values
  (set and unset cases) are back
- [x] 4.4 OOM is a catchable R error: under a deliberately tiny
  `SSDSIMS_DUCKDB_MEMORY_LIMIT` (e.g. `100MB`), `ssd_write_parquet()` of a
  nested frame sized just above the floor errors with DuckDB's out-of-memory
  message and the session stays usable (spec scenario "Memory limit follows
  the environment knob")
- [x] 4.5 Byte-identity: run a small `samples = TRUE` scenario through
  `ssd_run_scenario_shards()` twice — default knobs vs
  `SSDSIMS_DUCKDB_THREADS=1` + a sufficient `SSDSIMS_DUCKDB_MEMORY_LIMIT` —
  and compare shard/summary file hashes (spec "Configuration never changes
  results"); keep the scenario tiny per the test-suite speed budget
- [x] 4.6 Runners apply the scope: assert `threads == 1` is observed *during* a
  step runner body (e.g. via a mocked/wrapped seam per the test-suite AGENTS
  wrapping rule), not merely that the helper works in isolation
- [x] 4.7 Byte-budgeted summary row groups: a full summary written with a
  small `samples_row_group_bytes` shows multiple byte-sized row groups in
  `parquet_metadata()`; two identical runs yield the same multiset of rows
  (equal after ordering by `hc_id`) and byte-identical compact summaries;
  and `preserve_insertion_order` reports its pre-write value afterwards
  (spec "The full summary writes byte-budgeted row groups"); keep the fixture
  tiny (small budget, short `samples` cells — the property under test is the
  options/scoping plumbing, not the 1 GB floor itself, which stays covered by
  `exploration/experiment-summary-union.R` and
  `exploration/experiment-rgbytes.R`)

## 5. Templates and docs

- [x] 5.1 Cluster template (`inst/targets-templates/cluster/controller.R`): add
  the `export SSDSIMS_DUCKDB_MEMORY_LIMIT=…` line to the `script_lines`
  example block, sized with headroom against `memory_gigabytes_per_cpu`
  (e.g. `3GB` of a 4 GB job), with the one-line sizing rule and a pointer to
  the helper docs; mention `SSDSIMS_DUCKDB_THREADS` defaults to single-thread
  so `cpus_per_task = 1` is correct as shipped
- [x] 5.2 `large/` and `small/` templates: a brief comment that the same env
  knobs apply when running under a local controller
- [x] 5.3 ROADMAP.md: move `duckplyr-config` to Done (this change) and fold the
  `duckplyr-message` Next item into the same Done entry (delivered here);
  remove it from Next
- [x] 5.4 Document raising the 1 GB default and its implications (helper docs
  + cluster template): how — `export SSDSIMS_DUCKDB_MEMORY_LIMIT=3GB` in the
  controller's `script_lines` on a cluster, `Sys.setenv()` interactively;
  implications — keep headroom for R's own footprint within the scheduler
  allocation (R holds a shard's draws while DuckDB ingests them, so budget
  roughly half the job for DuckDB), and a higher limit only moves the
  shard-payload floor (~5 × the nested `samples` bytes) — the full summary's
  floor follows `samples_row_group_bytes`, never the limit
- [x] 5.5 NEWS via the squash-merge PR title (Conventional Commit, e.g.
  `feat: scope single-thread, memory-bounded, telemetry-silent duckplyr config
  to the targets pipeline`)
