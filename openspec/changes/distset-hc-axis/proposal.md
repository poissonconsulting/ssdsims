## Why

A motivating study (the `ssdaveragerr` "iwasaki" analysis) fits one **superset**
of distributions to each simulated sample, then compares several
**model-averaging pools** drawn from that single fit — BCANZ, the Iwasaki set,
and individual distributions — scoring each against the same data. Today a
scenario's `dists` is a single fit-level setting (one model-averaged
`ssd_fit_dists()` per task), so reproducing that comparison means running one
**whole scenario per pool**, re-fitting the data from scratch each time (there is
no nested-fit reuse — `TARGETS-DESIGN.md` §"No nested reuse"). For 35 datasets ×
1000 sims × 3 `nrow` that is ~105k fits paid 7× over, when the pools share
identical per-distribution fits and differ only in the analytical re-averaging.

## What Changes

- **BREAKING — add `ssd_distset()` and require it for `dists`.** A distribution
  set is the pool of distributions model-averaged together to form one SSD (one
  `est`). `ssd_distset(BCANZ = ssd_dists_bcanz(), Iwasaki = c("burrIII3", ...),
  lnorm = "lnorm")` returns a validated `ssdsims_distset` collection (owning
  set-name and member validation **by value**, the `ssd_scenario_data()` precedent);
  `ssd_define_scenario(dists = ...)` SHALL accept **only** that collection. A
  bare character vector or a plain list SHALL abort with a message naming
  `ssd_distset()` (no silent fallback, no expression-archaeology for names). The
  set **name** — not its members — is what hashes onto the task path, mirroring
  the by-name treatment of `min_pmix` and datasets.
- **Fit the union once.** The fit step fits `sort(unique(unlist(dists)))` (the
  superset every pool needs) as a single model-averaged fit per fit task;
  `scenario$fit$dists` becomes that union. The named sets are stored separately
  (`scenario$hc$distsets`).
- **Introduce `distset` as an hc-level axis.** Add `"distset"` to
  `task_axes("hc")` (`R/task-lists.R:369`). The hc step, per task, `subset()`s
  the parent union fit down to its set's members (`strict = FALSE`) and
  re-averages — reusing one fit across all pools rather than re-fitting. By
  default `distset` is an **inner** (bundled) hc axis, so one hc shard decodes the
  union fit once and serves every pool for that `(dataset, sim)` cell; users may
  promote it to a path axis via `partition_by`.
- **Add a `scenario_distset()` accessor** (name → member vector), the twin of
  `scenario_min_pmix()`, and carry the distsets map on the hc scenario slice.
- **Subset-then-average in the shared per-task primitive**
  (`hc_data_task_primer()`, `R/task-lists.R:605`), so the single-core baseline,
  the single-core shard runner, and the `targets` pipeline stay byte-identical by
  construction.
- **Extend `partition_by`/`bundle` vocabulary** so `distset` is an accepted `hc`
  path/inner axis; the default hc path stays `c("dataset", "sim")` (distset
  bundled).
- **Empty-pool contract:** a set whose members all failed to fit yields a
  zero-length subset; the hc task emits no rows for that `(task, distset)` cell
  (the consumer's "estimable pools" filter), rather than aborting.
- **Sweep docs and the decision log.** This **refines** the settled
  `dists-simulation-setting` decision (`TARGETS-DESIGN.md` §"No nested reuse" /
  decision log ~L2275): *individual distributions still never fan out* (each axis
  value is a complete averaging pool, so the model-averaging science the prior
  decision protected is intact); what changes is that a **named set of pools** is
  now an hc-level axis over post-fit subsets of one union fit.

Point estimates (`est`) for a pool are **analytical** and identical to fitting
that pool directly (per-distribution fits are independent within
`ssd_fit_dists()`); this subset-reuse identity is the correctness oracle. When
`ci = TRUE`, each pool bootstraps from its own members and the hc primer (which
hashes the hc-grid row, now including `distset`) re-seeds per pool — desired, and
a re-baseline of any existing CI snapshots.

## Capabilities

### New Capabilities
<!-- None: this extends existing capabilities (no new spec file). -->

### Modified Capabilities
- `scenario-definition`: add `ssd_distset()` as the validated, by-value
  collection constructor for `dists`; `ssd_define_scenario()` requires an
  `ssd_distset()` collection (bare vector / plain list abort loudly), derives and
  stores the fit **union** (`scenario$fit$dists`) and the named sets
  (`scenario$hc$distsets`); the print method covers the set list.
- `task-lists`: `task_axes("hc")` gains `"distset"`; the hc task table fans out
  over the declared sets (their **names** as the axis values), each row carrying
  its set name and parent `fit_id`; the fit table is unchanged (it fits the
  union).
- `hazard-concentrations`: per hc cell, the union fit is subset to the cell's
  distribution set (`strict = FALSE`) and re-averaged; **at a fixed seed** the
  result is byte-identical to fitting that set alone and running
  `ssdtools::ssd_hc()` with the same seed; an empty subset yields no rows.
- `task-shards`: `partition_by`/`bundle` accept `"distset"` for the `hc` step
  (default: bundled/inner, path stays `c("dataset", "sim")`); the hc shard runner
  decodes each parent union fit once and subsets it per `distset` task.
- `scenario-accessors`: a `scenario_distset()` accessor isolates a set's member
  vector by name; the hc minimal scenario slice carries the distsets map.

## Impact

- **Specs**: `scenario-definition`, `task-lists`, `hazard-concentrations`,
  `task-shards`, `scenario-accessors` deltas.
- **Code**: new `R/distset.R` (`ssd_distset()` constructor, validator, print
  method), `R/scenario.R` (`dists` requires the collection; union derivation/storage
  in `fit`+`hc`, print, `partition_by` vocabulary in `validate_axis_list()`),
  `R/task-lists.R` (`task_axes("hc")`, hc task-table fan-out, `hc_data_task_primer()`
  subset), `R/accessors.R` (`scenario_distset()`, hc slice in `scenario_step_slice()`),
  `R/targets-runner.R` / `R/shard-runner.R` (`ssd_run_hc_step()` reads/decodes
  parent fit once and subsets per distset task), `R/hc-sims.R`/`R/internal.R` (the
  in-memory hc path, kept consistent), `NAMESPACE`/`man/` (new export).
- **BREAKING**: `dists` now requires an `ssd_distset()` collection; a bare
  character vector (today's form) aborts loudly. Existing call sites migrate
  `dists = ssd_dists_bcanz()` → `dists = ssd_distset(BCANZ = ssd_dists_bcanz())`.
- **Independence**: this change is independent of `pmix-constructor`; it owns the
  `dists`/`ssd_distset()` input, that change owns `min_pmix`/`ssd_pmix()`. They
  extend `scenario-definition` in disjoint requirements (no ordering between them).
- **RNG / re-baseline**: adding `distset` to `task_axes("hc")` makes it part of
  the hc primer, re-seeding every hc task; `est` is unchanged (analytical),
  bootstrap CIs change numerically (statistically equivalent). No stored-CI
  migration is implied.
- **Correctness gate**: a same-seed subset-reuse invariant test —
  `hc(subset(union_fit, set))` equals `hc(fit(data, dists = set))` seeded with the
  same primer — backed by `exploration/distset-subset-invariance.R`. Plus a
  path-axis-growth test that adding a set mints only new hc shards and leaves all
  fit shards (and other hc shards) cached.
- **Cost**: hc task count becomes `fit-tasks × |distsets|`, but each added pool is
  an analytical re-average of an already-fit superset (`ci = FALSE`) rather than a
  fresh fit — the iwasaki comparison drops from ~7× fits to one union fit plus
  cheap per-pool hc. `ssd_estimate_cost()` recalibration is consumed from the
  task-table counts (no `cost-estimation` spec change).
- **Out of scope (follow-up):** surfacing `ssd_gof()` diagnostics
  (`at_bound`/`computable`/`dropped`/top-dist/`delta`) per `(sample × distset)` —
  a separate, complementary change to the hc/summary output.
