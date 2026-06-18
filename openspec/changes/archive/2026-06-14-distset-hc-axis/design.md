## Context

`ssdsims` expands a declarative `ssdsims_scenario` into per-step task tables
(`sample`/`fit`/`hc`), shards them by `partition_by`, and runs them as a
Hive-partitioned `targets` pipeline whose per-task results are byte-identical to
the single-core baseline (`TARGETS-DESIGN.md` §§1–8). Today `dists` is a single
fit-level **scenario setting** — one model-averaged `ssd_fit_dists()` call per
fit task — settled by the `dists-scenario-setting` change precisely so that
*individual distributions never fan out* (fanning out per distribution would
dissolve the model averaging that defines a fit).

The motivating `ssdaveragerr` "iwasaki" study needs the opposite of one pool: it
fits a **superset** once and compares several model-averaging **pools** (BCANZ,
the Iwasaki set, single distributions) carved from that one fit via
`subset.fitdists()` — the pools share identical per-distribution fits and differ
only in the analytical re-averaging over their members. Reproducing that on
`ssdsims` today means one whole scenario per pool, re-fitting from scratch each
time (no nested-fit reuse, `TARGETS-DESIGN.md` §"No nested reuse"): ~7× the fit
cost for results that should share one fit.

## Goals / Non-Goals

**Goals:**
- Let one scenario declare several distribution **sets** (pools) and compute each
  pool's hc by subsetting **one** union fit — matching the iwasaki "fit superset,
  subset" efficiency.
- Keep the engine's invariants: name-only task hashing, byte-identity across the
  three runners, path-axis-growth caching, and a side-effect-free target factory.
- Supply sets through a validated `ssd_distset()` collection (the `ssd_scenario_data()`
  precedent, naming by value); the legacy bare-vector `dists` form fails loudly
  (BREAKING, pre-release).

**Non-Goals:**
- Fanning out over *individual* distributions (that dissolves averaging — the
  `dists-scenario-setting` guard stands; an axis value is always a whole pool).
- Surfacing `ssd_gof()` diagnostics (`at_bound`/`computable`/`dropped`/top-dist/
  `delta`) per `(sample × distset)` — a complementary follow-up change.
- Nested-fit reuse across *different unions* (a wider union still re-fits; only
  pools *within* one declared union share a fit).

## Decisions

### 1. The axis value is a set, supplied via `ssd_distset()` and keyed by name
`dists` accepts an **`ssd_distset()` collection** — a validated, named list of
character vectors. An axis value must be a complete averaging pool, so each value
is itself a vector; the collection is a named list. The constructor owns naming
(from `...` names) and validation (members ⊆ `ssd_dists_all()`, names unique and
filesystem-safe) **by value**, mirroring `ssd_scenario_data()` — so `ssd_define_scenario()`
does no expression-archaeology to name sets. The set **name** (not its members) is
what enters the `distset=` Hive path segment and the per-task primer — the by-name
pattern already used for `min_pmix` and datasets (the members ride on the scenario
for execution, isolated by the new `scenario_distset()` accessor, and never enter
a hash). The legacy bare-vector / plain-list forms abort loudly with a message
naming `ssd_distset()` (BREAKING, pre-release).

*Alternative considered:* accept a bare named list (names via `names()`, no
constructor). Rejected: it skips up-front member validation and splits the input
surface; a typed constructor is the single, teachable, validated entry point
(the `ssd_scenario_data()` precedent). *Also considered:* a `list of lists` carrying
per-distribution options — rejected: members are bare distribution names with no
per-member structure, so a list-of-character-vectors is the precise shape.

### 2. `distset` is an **hc** axis over a post-fit subset — not a fit axis
The fit step fits the **union** `sort(unique(unlist(dists)))` once; `distset`
enters `task_axes("hc")` only. Per hc task, the runner subsets the parent union
fit to the task's members and re-averages.

*Why hc, not fit:* a fit-level `distset` would mint one fit task per pool and
re-fit each (≈7× cost, the very thing we are removing — `ssdsims`/`ssdtools` do
not reuse nested `dists` fits). An hc-level axis fits once and re-averages per
pool. With `ci = FALSE` (the iwasaki case) the re-average is purely analytical —
no bootstrap, no RNG — so the per-pool cost is negligible. It also gives a clean
caching property: the fit layer carries no `distset`, so adding a pool mints only
new hc shards and caches every fit shard.

### 3. The subset happens in the shared per-task primitive
`hc_data_task_primer()` (the primitive both the baseline and the shard runners
call) gains the set members and performs
`subset(fits, select = members, strict = FALSE)` before `ssd_hc()`. Putting the
subset at this single chokepoint makes the three runners byte-identical *by
construction* — the same property the existing primers already guarantee — rather
than duplicating subset logic in each runner. `strict = FALSE` tolerates a member
that dropped out of the union fit (boundary/failed); an all-dropped set yields a
zero-length subset → zero hc rows (the §6.2 survivor model), not an abort.

### 4. Default `distset` to an inner (bundled) axis
The default hc path stays `c("dataset", "sim")`, so `distset` is inner by
default: one hc shard holds every pool for a `(dataset, sim)` cell, the runner
decodes that cell's union fit **once**, and subsets it N ways in memory (a second
layer of reuse on top of the one union fit). Users who want per-pool shards
promote `distset` to `partition_by$hc`. This keeps shard counts sane (35 datasets
× 1000 sims × 7 pools would be 245k tiny Parquets if `distset` were on the path
by default).

### 5. Correctness oracle: same-seed subset-reuse identity
The premise — re-averaging a subset of the union equals fitting that subset alone
— is asserted at a **fixed seed**: `ssd_hc(subset(union_fit, set))` byte-equals
`ssd_hc(ssd_fit_dists(data, dists = set))` seeded identically (per-distribution
fits are independent within `ssd_fit_dists()`). This is proven in
`exploration/distset-subset-invariance.R` and asserted in a unit test. It is
explicitly **not** an old-vs-new pipeline equality: `distset` joins the hc primer,
so it re-seeds hc tasks (point `est` analytical/unchanged; CIs re-seeded,
statistically equivalent) — the same re-baseline pattern as `est-method-setting`.

## Risks / Trade-offs

- **Re-baselining existing CI snapshots** → `distset` in the hc primer re-seeds
  every hc task even for a single-set collection. Mitigation: the single-set path
  has exactly one `distset` value, and `est` is analytical/unchanged; only
  `ci = TRUE` CI snapshots move, and those re-record like any primer change.
  Document in the proposal Impact; the single-pool *estimates* are preserved,
  not the *bootstrap byte-streams*.
- **Refining a settled decision** → the `dists-scenario-setting` decision said
  "dists is not an axis". Mitigation: this does not contradict it — *individual
  distributions still never fan out*; a named set of *pools* becomes an axis over
  post-fit subsets. The TARGETS-DESIGN decision log and §"No nested reuse" get an
  addendum stating the refinement (reuse now holds *within* one union).
- **`subset.fitdists` semantics drift** → relying on `strict = FALSE` and on
  subset-equals-direct-fit. Mitigation: the same-seed oracle test guards it and
  `ssdtools (>= 2.6.0)` is already a hard dependency; no `ssdtools` change is
  required.
- **Shard explosion if a user paths on `distset`** → mitigated by the bundled
  default and a vignette note; promoting `distset` to the path is an explicit,
  documented opt-in.

## Migration Plan

Back-compatible: a bare character `dists` keeps today's behaviour. Named-list
`dists` is purely additive. The only re-baseline is `ci = TRUE` CI snapshots
(re-seeded by the primer change) and printed-scenario snapshots (the print method
renders the set list). No data migration; results live under the per-layout
`scenario_results_dir()`, so a changed scenario writes to a fresh layout root.

## Open Questions

- Should a single-element named list (`dists = list(BCANZ = ...)`) still carry a
  `distset` axis of one (path segment `distset=BCANZ`), or collapse to the
  anonymous-set path? Leaning toward **keep the named segment** (explicit label is
  useful in the summary), with the bare-vector form as the only "no distset axis"
  case.
- Output column vs path-only: when `distset` is bundled (inner), should the hc
  Parquet carry an explicit `distset` column (it must, to disambiguate rows within
  a shard) — confirm the summary schema names it consistently with the path form.
