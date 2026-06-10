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
   child array demanded as **one buffer**, which streaming/spilling knobs
   cannot split.
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
5. **`experiment-duckplyr-noise.R`** — the pipeline's own surface
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
- Document the nested-list sizing rule so the memory knob can be set
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

1. snapshots `threads` and `memory_limit` via `duckplyr::read_sql_duckdb()`,
2. applies the pipeline settings via `duckplyr::db_exec("SET …")`,
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

### D2. Env-var knobs: `SSDSIMS_DUCKDB_THREADS`, `SSDSIMS_DUCKDB_MEMORY_LIMIT`

- `SSDSIMS_DUCKDB_THREADS` — default **`1`** when unset. The roadmap item asks
  for a single thread outright; the experiments show it is free (faster, even,
  on the large nested shard), and it matches the template's
  `cpus_per_task = 1`.
- `SSDSIMS_DUCKDB_MEMORY_LIMIT` — default **unset = leave DuckDB's default**.
  A hard-coded cap (say `2GB`) would gratuitously break big interactive
  `ssd_summarise()` calls on capable machines; the right value is a function of
  the worker's actual allocation, which only the user knows. The cluster
  template documents setting it in the controller's `script_lines` (e.g.
  `export SSDSIMS_DUCKDB_MEMORY_LIMIT=3GB` beside
  `memory_gigabytes_per_cpu = 4`, headroom for R itself), the same channel that
  already carries `SSDSIMS_AZURE_*` to workers.

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

### D4. The full-summary write gets an explicit, bounded `ROW_GROUP_SIZE`

`ssd_summarise()`'s `path_with_samples` write passes an explicit
`row_group_size` to `duckplyr::compute_parquet()` (the `options` seam already
exists), sized so one row group's `samples` payload stays within a fixed byte
budget (~100 MB): `rows_per_group = clamp(budget / (max_nboot × 8), 1, 122880)`.
The factory (`ssd_scenario_targets()`) knows the scenario's largest `nboot`, so
it computes the value and threads it to the summary target;
`ssd_summarise()` exposes it as an argument with a conservative fallback for
standalone calls. The compact-summary write keeps the default options (its
rows are tiny — 4100 rows passed at a 100 MB limit).

*Why this over a relaxed summary memory budget*: the experiment shows the
default writer's requirement grows with the union's **total row count** (one
giant row group), so "give the summary task more memory" has no stable
ceiling — it would re-break on the next bigger sweep. Bounded row groups make
the write memory-flat (465 MB peak RSS for a 1.6 GB-payload union under a 1 GB
limit) at zero compression cost.

*Why `ROW_GROUP_SIZE` over `ROW_GROUP_SIZE_BYTES`*: the bytes knob is
payload-aware but only takes effect with `preserve_insertion_order = false`,
which surrenders deterministic row order — and with it the byte-identity
property the spec demands of re-runs. A row count computed *from* the
scenario's `nboot` is equally payload-aware in practice and keeps insertion
order. Smaller row groups slightly coarsen predicate-pushdown granularity for
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
  is documented next to the knob.
- [Memory floor is duckdb-version-specific] The 5× rule was measured on duckdb
  1.5.2. → Recorded as an empirical bound with the reproduction scripts kept in
  `exploration/`; re-measure on major engine upgrades.
- [Sub-2048 `ROW_GROUP_SIZE` honoured is version-observed behaviour] duckdb
  1.5.2 wrote exact 100-row groups, but a future engine could clamp small row
  groups (the believed 2048 minimum), re-raising the summary floor to
  ~5 × 2048 × max_nboot × 8 bytes (~4 GB at `nboot = 50000`). →
  `experiment-summary-union.R` is the canary (re-run on engine upgrades); if a
  clamp ever lands, the fallback is the documented one: read `samples` from
  the per-shard Parquets and skip `path_with_samples` for very large `nboot`.

## Open Questions

- None blocking. (Whether to *also* surface the knobs as arguments on
  `ssd_run_scenario_shards()` for interactive use can be decided at review; the
  env vars already work there.)
