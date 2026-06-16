# duckplyr-config — design

## Context

Every Parquet write/read in the pipeline funnels through the duckplyr seam in
`R/targets-runner.R` (`ssd_write_parquet()` / `ssd_read_parquet()` and the lazy
reads in `ssd_summarise()`), executed by the per-shard step runners on `crew`
workers. duckplyr's managed DuckDB instance configures itself from the
*machine*, not the *job*: on this 4-core/15.6 GB dev box it starts with
`threads = 4`, `memory_limit = 12.5 GiB` (80% of RAM). A SLURM worker
requesting `cpus_per_task = 1`, `memory_gigabytes_per_cpu = 4` (the shipped
cluster template) gets the same machine-derived defaults — DuckDB will happily
schedule node-many threads and plan against ~0.8 × node RAM, oversubscribing
the CPU allocation and dying by cgroup OOM kill instead of a clean R error.

Four experiments (all under `exploration/`, RESULT tables reproducible with
`Rscript <script> --matrix`) establish the facts the design rests on:

1. **`experiment-nested-parquet.R`** — the question as asked: *can we write a
   Parquet file with a nested list column containing 50k doubles?* **Yes,
   trivially**: one row whose `samples` cell holds 50k doubles writes in
   ~0.02 s to a 0.2 MB file and round-trips losslessly — even under
   `memory_limit = '100MB'`, `threads = 1`. Named cells also write (names are
   dropped by Parquet, consistent with the runner's unname `FIXME`). The cost
   is a *shard-payload* effect, not a *cell* effect: 100 such rows peak at
   ~0.4 GB RSS; 1000 rows (~0.4 GB of doubles) peak at ~2.3 GB and **OOM under
   `memory_limit = '1GB'`**. `threads = 1` costs nothing — it *halved* the
   1000-row write time (10.8 s vs 23.6 s).
2. **`experiment-nested-mitigations.R`** — none of DuckDB's own suggestions
   move the OOM point of that failing 1000-row case at all:
   `preserve_insertion_order = false`, a `temp_directory` for spilling, and
   `ROW_GROUP_SIZE 2048` all fail with the *identical* allocation. The failed
   allocation is exactly `rows × doubles × 8` bytes — the LIST column's entire
   child array demanded as **one buffer**, which streaming/spilling
   configuration options cannot split.
3. **`experiment-nested-vs-flat.R`** — nesting, not payload, is the culprit:
   the *same* 50M doubles written **flat** (long format, 50M rows) succeed
   under `memory_limit = '100MB'`, while the **nested** form needs
   `memory_limit = '2GB'` (1.5 GB still fails). Both stages contribute:
   materialising the nested frame to a DuckDB *table* (no Parquet) passes at
   1 GB but fails at 500 MB; the Parquet writer on top pushes the floor to
   2 GB. Working rule: **a nested shard needs `memory_limit` ≳ 5 × its
   `samples` payload bytes**.
4. **`experiment-summary-union.R`** — review insight: shard writes stay small
   (a shard holds few task rows), so the row-group dimension bites at the
   **full-summary union** (`ssd_summarise(path_with_samples =)`), which crosses
   any per-shard bound. Unioning >2048 one-row nested Parquets (the verbatim
   `read_parquet_duckdb(files)` |> `compute_parquet()` path, no R
   materialisation) shows the default writer accumulates the *whole union*
   toward its default 122 880-row groups: 2100 rows land in **one** row group
   and need `memory_limit = '3GB'`; 4100 rows fail even at 3 GB — the floor
   *scales with total rows*, so a relaxed summary memory budget is not a
   stable fix. But the supposed 2048-row minimum row group does **not** hold
   on duckdb 1.5.2: an explicit `ROW_GROUP_SIZE 100` is honoured exactly
   (41 × 100-row groups) and makes the 4100-file union pass at
   `memory_limit = '1GB'` with 465 MB peak RSS — **memory flat in total
   rows**, with no compression penalty (0.181 MB/row in both layouts).
   `ROW_GROUP_SIZE_BYTES '100MB'` (payload-aware; requires
   `preserve_insertion_order = false`) passes even at 500 MB. The compact
   summary (`samples` projected out) stays trivial (100 MB, 0.7 s).
5. **`experiment-rgbytes.R`** — the configuration-option choice, on reprexes: with ordering
   preserved the engine **refuses** `ROW_GROUP_SIZE_BYTES` outright (a Binder
   error naming the fix — never silently ignored); with `threads = 1` and
   ordering relaxed, repeated runs were byte-identical *and* kept the input
   row order (observed, not contractual); small 1000-double cells yielded
   13 000-row groups against 246-row groups for 50k-double cells (one budget,
   payload-adaptive); and the memory floor tracks the *budget* at the same
   ~5× rule (100 MB budget: 250 MB fails / 500 MB passes; 32 MB budget passes
   at a 150 MB limit).
6. **`experiment-duckplyr-noise.R`** — the pipeline's own surface
   (namespace-only duckplyr, as ssdsims uses it) emits *nothing*, with or
   without silencing. The noise is one step removed: duckplyr **collects
   fallback-telemetry logs by default** (`DUCKPLYR_FALLBACK_COLLECT` unset ⇒
   on), and once logs exist, the next interactive `library(duckplyr)` prints a
   multi-line "fallback events can be collected and uploaded…" banner.
   `DUCKPLYR_FALLBACK_COLLECT=0` + `DUCKPLYR_FALLBACK_AUTOUPLOAD=0` reduce the
   attach output to dplyr's own masking notes plus the one-line
   `methods_overwrite()` notice.

`duckplyr::read_sql_duckdb("SELECT current_setting('threads'), …")` is exported
and works against the managed connection, so settings can be snapshotted and
restored (verified on duckplyr 1.2.1 / duckdb 1.5.2).

## Goals / Non-Goals

**Goals:**

- Pipeline-scoped DuckDB configuration: single thread and an explicit,
  user-controllable memory limit while the step runners / summary fan-in are
  executing — and the user's prior settings restored afterwards.
- Convert worker deaths (cgroup OOM kill) into loud, per-shard R errors that
  `error = "null"` can isolate.
- Make the full-summary (`path_with_samples`) write memory-flat in the number
  of unioned rows, so the single `summary` task needs no special memory budget.
- Silence duckplyr's fallback-telemetry collection (and thus the downstream
  interactive attach banner) for pipeline-context use — the `duckplyr-message`
  roadmap item.
- Document the nested-list sizing rule so the memory environment variable can be set
  rationally.

**Non-Goals:**

- Changing the shard storage format (e.g. flattening `samples` to long form or
  a blob). The experiments show flat would erase the memory floor, but the
  `samples`/`dists` list-columns are the `dual-summary-outputs` contract;
  revisiting the format is a separate roadmap decision if real studies hit the
  floor in practice.
- Configuring duckplyr globally (e.g. in `.onLoad()`) or for the interactive
  read-back paths (`ssd_open_uploaded()`, `ssd_summarise_uploaded()`): the
  roadmap item scopes configuration to **the targets pipeline**; a user's own
  duckplyr session stays theirs.
- Tuning DuckDB's Parquet writer (row-group size etc.) — demonstrated
  ineffective for the nested shape.

## Decisions

### D1. A withr-style RAII helper, applied at the runner entry points

One internal helper, `local_duckplyr_config(.local_envir = parent.frame())`
(new file `R/duckplyr-config.R`), called at the top of `ssd_run_sample_step()`,
`ssd_run_fit_step()`, `ssd_run_hc_step()`, `ssd_summarise()`, and
`ssd_run_scenario_shards()` — mirroring how `local_dqrng_backend()` already
scopes the RNG backend there. It:

1. snapshots `threads`, `memory_limit`, and `preserve_insertion_order` via
   `duckplyr::read_sql_duckdb()`,
2. applies the pipeline settings via `duckplyr::db_exec("SET …")` (including
   `preserve_insertion_order = false` — see D4),
3. scopes `DUCKPLYR_FALLBACK_COLLECT = "0"` and
   `DUCKPLYR_FALLBACK_AUTOUPLOAD = "0"` via `withr::local_envvar()`,
4. registers a `withr::defer()` that `SET`s the snapshotted values back.

*Why at the runners, not inside `ssd_write_parquet()`/`ssd_read_parquet()`*:
the runners are the pipeline's execution boundary (one target body each), so
one snapshot/restore per target rather than per I/O call; and `ssd_summarise()`
holds *lazy* duckplyr tables whose work happens at `compute_parquet()` time —
the scope must span the whole body anyway. Nesting is harmless: inner scopes
snapshot the outer scope's values and restore them, so
`ssd_run_scenario_shards()` wrapping the per-step runners stays correct.

*Why restore rather than `RESET`*: `RESET` returns to DuckDB defaults, clobbering
any custom values the user set on the shared managed connection before calling
a runner interactively. Alternative rejected.

### D2. Environment variables: `SSDSIMS_DUCKDB_THREADS`, `SSDSIMS_DUCKDB_MEMORY_LIMIT`

- `SSDSIMS_DUCKDB_THREADS` — default **`1`** when unset. The roadmap item asks
  for a single thread outright; the experiments show it is free (faster, even,
  on the large nested shard), and it matches the template's
  `cpus_per_task = 1`.
- `SSDSIMS_DUCKDB_MEMORY_LIMIT` — default **`1GB`** when unset (decided at
  review). A machine-derived default (80% of node RAM) is exactly the
  failure mode this change removes, and an unset-means-unbounded default
  would leave cluster workers one forgotten `export` away from a cgroup
  kill. 1 GB is comfortable for every pipeline write the experiments
  measured *except* an oversized nested shard: the byte-budgeted full
  summary needs only ~500 MB, ordinary shards far less, and a nested shard
  that does not fit (payload ≳ 200 MB of draws) fails with DuckDB's loud,
  catchable OOM error naming the remedy — at which point the user either
  raises the limit or re-shards. **Raising it** is one env var
  (`export SSDSIMS_DUCKDB_MEMORY_LIMIT=3GB` in the controller's
  `script_lines`, beside `memory_gigabytes_per_cpu = 4` — headroom for R
  itself; or `Sys.setenv()` interactively). **Implications of raising it**:
  the limit must stay below the scheduler allocation minus R's own
  footprint (R holds the shard's draws while DuckDB ingests them — budget
  roughly half the allocation for DuckDB), and a higher limit only helps
  the shard-payload floor (~5 × the nested `samples` bytes); it is never
  needed for the summary, whose floor follows the row-group byte budget
  instead. This is documented on the helper and in the templates (tasks
  5.x).

*Why env vars, not R options or function arguments*: options do not cross the
process boundary to `crew` workers, and threading a `duckdb_config` argument
through `ssd_scenario_targets()` → `tar_map()` commands → runner signatures
would put resource config inside the targets dependency hash (changing the
memory limit would invalidate every cached shard — wrong: results are
byte-identical by construction). Env vars reach every worker via the
established `script_lines` channel, and a changed limit re-runs nothing.

### D3. Fallback telemetry: prevent the logs, not just the banner

`DUCKPLYR_FALLBACK_COLLECT=0` inside the scope stops log *collection* during
pipeline runs, so the interactive banner (gated on logs existing) never gains
material from pipeline work; `DUCKPLYR_FALLBACK_AUTOUPLOAD=0` additionally
pre-answers the upload ask in case logs exist from the user's other work.
Scoped via `withr::local_envvar()` — outside the pipeline the user's own
telemetry preferences are untouched. We do not set `DUCKPLYR_FALLBACK_INFO`
(per-call fallback messages are opt-in and already silent) and we do not touch
the user's persisted `fallback_config()`.

### D4. The full-summary write uses `ROW_GROUP_SIZE_BYTES`; the scope relaxes insertion order

`ssd_summarise()`'s `path_with_samples` write passes
`row_group_size_bytes = '100MB'` (a documented argument with that default) to
`duckplyr::compute_parquet()`. The required `preserve_insertion_order = false`
lives in the `local_duckplyr_config()` scope (D1) — snapshot/restored like
`threads` and `memory_limit` — because it **cannot** be expressed per write:
`exploration/experiment-preserve-order-copy-option.R` shows
`preserve_insertion_order` is not a COPY option, and while `PRESERVE_ORDER`
is one, the `ROW_GROUP_SIZE_BYTES` Binder validation consults only the
*global* setting (per-copy `PRESERVE_ORDER FALSE` + a byte budget is still
refused). A dedicated per-write SET/restore wrapper
(`compute_parquet_unordered()`, an earlier iteration) was dropped as
redundant once the scope carries the setting. The compact-summary write
keeps default row-group sizing (its rows are tiny — 4100 rows passed at a
100 MB limit).

*Why this over a relaxed summary memory budget*: the union experiment shows
the default writer's requirement grows with the union's **total row count**
(one giant row group), so "give the summary task more memory" has no stable
ceiling — it would re-break on the next bigger sweep. A bounded row group
makes the write memory-flat at zero compression cost (0.181 MB/row in every
layout measured).

*Why `ROW_GROUP_SIZE_BYTES` over a fixed `ROW_GROUP_SIZE`* (decided on the
`experiment-rgbytes.R` reprexes, with review accepting non-guaranteed row
order):

- **Payload-adaptive with no arithmetic**: one byte budget serves every
  `nboot` — 1000-double cells yielded 13 000-row groups (≈ the 104 MB
  budget), 50k-double cells yielded 246-row groups. A fixed row count would
  need scenario-derived sizing in the factory and would still mis-size mixed
  `nboot` sweeps; the bytes configuration option sizes each group as it fills.
- **The ordering requirement is loud, not a foot-gun**: with insertion order
  preserved the engine *refuses* the option outright (Binder error naming
  `SET preserve_insertion_order = false`), so a mispaired configuration can
  never silently degrade. Mechanism: the order-preserving sink must
  reassemble upstream batches into source order before emitting, cutting row
  groups on the ordered batcher's row-count boundaries — it cannot flush
  early on a byte budget while later-ordered batches are pending (which is
  also why the default ordered writer accumulated the whole union toward one
  122 880-row group). With ordering relaxed the sink flushes whenever the
  budget fills.
- **Under `threads = 1` the relaxation costs nothing in practice**: repeated
  runs produced byte-identical files (`md5` equal) that retained the input
  row order — single-producer behaviour, not an engine contract. The spec
  therefore guarantees **value-identity** (the same multiset of rows) for the
  full summary rather than byte-identity; shards and the compact summary
  remain byte-identical.
- **The floor tracks the budget at the same ~5× rule**: a 100 MB budget needs
  250–500 MB of `memory_limit` (a 32 MB budget passes at 150 MB), so the
  default budget fits comfortably under any worker allocation that can run
  the shards at all — and can be lowered arbitrarily if ever needed.

Smaller row groups slightly coarsen predicate-pushdown granularity for
readers of the full summary; that file is an archival artefact (the compact
summary is the analysis surface), so the trade is accepted.

### D5. The memory floor is documented guidance, not enforced arithmetic

The helper's docs (and the cluster template comment) state the rule: writing an
`hc` shard whose `samples` list-column holds `P` bytes needs
`memory_limit ≳ 5 × P`; size shards via `bundle`/`nboot` or raise the limit. We
do **not** pre-compute shard payloads and abort preemptively: `nboot` varies
per task row, the multiplier is an empirical bound for duckdb 1.5.2 that may
improve, and DuckDB's own OOM error (now a clean, catchable R error naming the
allocation) is the truthful enforcement. A predictive guard can be layered on
later (`cost-analysis-targets` territory).

## Risks / Trade-offs

- [Settings are connection-global while a scope is open] Any *other* duckplyr
  work the same process does mid-run sees one thread and the capped memory. →
  Acceptable: on workers the process is the pipeline; interactively the scope
  is per-call and restores on exit. Documented on the helper.
- [`read_sql_duckdb()` is marked experimental] An upstream signature change
  would break the snapshot. → Single chokepoint in one internal helper; unit
  test fails loudly on upgrade; trivial to swap for `RESET`-with-documented-
  caveat if it ever disappears.
- [`DUCKPLYR_FALLBACK_*` are duckplyr's interface, not ours] Renames upstream
  would silently re-enable telemetry. → They are documented in
  `?duckplyr::fallback`; the noise experiment doubles as a canary
  (re-run on upgrade).
- [A too-low `SSDSIMS_DUCKDB_MEMORY_LIMIT` fails shards that used to pass] →
  That is the intended trade: a loud per-shard `error = "null"` with DuckDB's
  sizing advice beats a silent cgroup kill of the whole worker. The sizing rule
  is documented next to the environment variable.
- [Memory floor is duckdb-version-specific] The 5× rule was measured on duckdb
  1.5.2. → Recorded as an empirical bound with the reproduction scripts kept in
  `exploration/`; re-measure on major engine upgrades.
- [Row order within written files is not contractual] With
  `preserve_insertion_order = false` held for the whole configuration scope
  (D4), the engine does not promise output order for *any* pipeline write;
  under the default `threads = 1` every write was observed byte-identical
  and in input order (single producer), but raising
  `SSDSIMS_DUCKDB_THREADS` may reorder rows — and a byte-unstable shard
  rewrite would also re-trigger downstream `targets` (a cache-efficiency
  cost, never a correctness one). → Accepted at review (files are queried,
  not diffed); the spec demands value-identity in general and byte-identity
  only under the single-threaded default, and consumers address rows by
  their keys, never by position.
- [Writer byte-flushing behaviour is version-observed] duckdb 1.5.2 honours
  `ROW_GROUP_SIZE_BYTES` exactly (groups of 246/79 rows as budgeted) and
  refuses it loudly when ordering is preserved; a future engine could change
  the floor or the refusal. → `experiment-rgbytes.R` and
  `experiment-summary-union.R` are the canaries (re-run on engine upgrades);
  the ultimate fallback stands: read `samples` from the per-shard Parquets
  and skip `path_with_samples` for very large `nboot`.

## Open Questions

- None blocking. (Whether to *also* surface the configuration options as arguments on
  `ssd_run_scenario_shards()` for interactive use can be decided at review; the
  env vars already work there.)
