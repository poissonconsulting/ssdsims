# duckplyr-config — tasks

## 1. The scope helper (`R/duckplyr-config.R`)

- [ ] 1.1 Implement internal `local_duckplyr_config(.local_envir = parent.frame())`:
  snapshot `threads` and `memory_limit` via
  `duckplyr::read_sql_duckdb("SELECT current_setting('threads') …")`, apply
  `SET threads TO <SSDSIMS_DUCKDB_THREADS or 1>` and (only when
  `SSDSIMS_DUCKDB_MEMORY_LIMIT` is set and non-empty)
  `SET memory_limit = '<value>'` via `duckplyr::db_exec()`, and register a
  `withr::defer(…, envir = .local_envir)` that `SET`s the snapshotted values
  back (restore, never `RESET` — design D1)
- [ ] 1.2 Scope `DUCKPLYR_FALLBACK_COLLECT = "0"` and
  `DUCKPLYR_FALLBACK_AUTOUPLOAD = "0"` in the same helper via
  `withr::local_envvar(…, .local_envir = .local_envir)` (design D3)
- [ ] 1.3 Validate the env knobs with `chk`-style errors raised in the caller's
  context: `SSDSIMS_DUCKDB_THREADS` must parse as a positive whole number,
  `SSDSIMS_DUCKDB_MEMORY_LIMIT` is passed through to DuckDB (its parser is the
  authority) — a malformed value should surface DuckDB's own message, not a
  silent ignore
- [ ] 1.4 Roxygen-document the helper (internal, `@noRd`): the two env knobs and
  their defaults, the restore-on-exit contract, the connection-global caveat
  while a scope is open, and the nested-shard sizing rule (`memory_limit ≳ 5 ×`
  the shard's `samples` payload; engine cannot spill a LIST child array —
  evidence in the `duckplyr-config` change's `exploration/` scripts)

## 2. Wire into the pipeline entry points

- [ ] 2.1 Call `local_duckplyr_config()` at the top of `ssd_run_sample_step()`,
  `ssd_run_fit_step()`, and `ssd_run_hc_step()` (beside the existing
  `local_dqrng_backend()` calls in `R/targets-runner.R`)
- [ ] 2.2 Call it at the top of `ssd_summarise()` (the lazy reads must be
  computed inside the scope) and of `ssd_run_scenario_shards()`
  (`R/shard-runner.R`, beside its run-wide `local_dqrng_backend()`)
- [ ] 2.3 `air format .`; `devtools::document()`

## 3. Tests (`tests/testthat/test-duckplyr-config.R`)

- [ ] 3.1 Scope behaviour: inside a `local_duckplyr_config()` scope with the
  knobs unset, `threads` reports `1` and `memory_limit` reports the pre-scope
  value; with `SSDSIMS_DUCKDB_MEMORY_LIMIT` set (via `withr::local_envvar()`),
  `memory_limit` reports it
- [ ] 3.2 Restore behaviour: set custom `threads`/`memory_limit` first, run a
  scope (normal exit and error exit), assert the custom values are reported
  after; nested scopes restore layer by layer (spec scenarios "Prior settings
  are restored" / "Nested scopes are safe")
- [ ] 3.3 Telemetry env vars: inside the scope `Sys.getenv()` reports the two
  `DUCKPLYR_FALLBACK_*` values as `"0"`; after the scope the prior values
  (set and unset cases) are back
- [ ] 3.4 OOM is a catchable R error: under a deliberately tiny
  `SSDSIMS_DUCKDB_MEMORY_LIMIT` (e.g. `100MB`), `ssd_write_parquet()` of a
  nested frame sized just above the floor errors with DuckDB's out-of-memory
  message and the session stays usable (spec scenario "Memory limit follows
  the environment knob")
- [ ] 3.5 Byte-identity: run a small `samples = TRUE` scenario through
  `ssd_run_scenario_shards()` twice — default knobs vs
  `SSDSIMS_DUCKDB_THREADS=1` + a sufficient `SSDSIMS_DUCKDB_MEMORY_LIMIT` —
  and compare shard/summary file hashes (spec "Configuration never changes
  results"); keep the scenario tiny per the test-suite speed budget
- [ ] 3.6 Runners apply the scope: assert `threads == 1` is observed *during* a
  step runner body (e.g. via a mocked/wrapped seam per the test-suite AGENTS
  wrapping rule), not merely that the helper works in isolation

## 4. Templates and docs

- [ ] 4.1 Cluster template (`inst/targets-templates/cluster/controller.R`): add
  the `export SSDSIMS_DUCKDB_MEMORY_LIMIT=…` line to the `script_lines`
  example block, sized with headroom against `memory_gigabytes_per_cpu`
  (e.g. `3GB` of a 4 GB job), with the one-line sizing rule and a pointer to
  the helper docs; mention `SSDSIMS_DUCKDB_THREADS` defaults to single-thread
  so `cpus_per_task = 1` is correct as shipped
- [ ] 4.2 `large/` and `small/` templates: a brief comment that the same env
  knobs apply when running under a local controller
- [ ] 4.3 ROADMAP.md: move `duckplyr-config` to Done (this change) and fold the
  `duckplyr-message` Next item into the same Done entry (delivered here);
  remove it from Next
- [ ] 4.4 NEWS via the squash-merge PR title (Conventional Commit, e.g.
  `feat: scope single-thread, memory-bounded, telemetry-silent duckplyr config
  to the targets pipeline`)
