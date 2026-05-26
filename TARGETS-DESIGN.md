# Targets design — `ssdsims`

End-to-end workflow for running an `ssdsims` scenario on a cluster.
Three primary goals, each a hard constraint:

- **Reproducibility.** A scenario's results are bit-stable across
  reruns and machines (RNG state is explicit and serializable; §1, §2).
- **Debuggability.** Any single failed branch on the cluster must be
  replayable locally — with no `targets`, no orchestrator — using
  the same inputs the cluster used. This is what the stream-only
  primitives (`slice_sample_stream()`, `fit_dists_stream()`,
  `hc_stream()`) and the per-Parquet content-addressed manifest are
  *for* (§7).
- **Extensibility.** Content-addressed Parquet files *are* the cache:
  a larger scenario reuses Parquets whose task IDs match, computes
  the rest, and unions everything. Two named exceptions
  (`parent_alias` for renames, dag-of-dags for mixed-code re-runs)
  are documented in §8.

Parallelism is assumed throughout. The document covers, in order:
the **scenario object** and central registries (§1), the
**per-task dqrng+hash RNG mechanism** (§2), running locally (§3),
assembling the cluster pipeline (§4), the three step grids and
their fan-outs (§5), the concrete target graph and the **cloud
upload hook** (§6), **debugging** a cluster failure (§7), and
**extension** (§8). §9 lists known limitations, §10 maps gaps from
`RNG-FLOW.md` §5 to resolutions, §11 collects open questions, and
§12 is the implementation roadmap.

Background and the list of gaps this design closes are in `RNG-FLOW.md`
§5. This is a forward-looking design; it does not document the existing
PoC (PR #59).

---

## 1. Scenario object

The scenario is **purely declarative**. It does not carry the
materialized task grid; expansion happens at run time via
`ssd_scenario_tasks(scenario)` (§2). An S3 object holding:

```
ssdsims_scenario
├── seed         ← scalar integer; root of the per-task RNG (§2)
├── nsim         ← number of replicate sims per dataset
├── datasets     ← character vector of dataset names referencing
│                  the central dataset registry (§1.1)
├── nrow         ← integer vector of sample sizes; subset property (§5)
├── fit          ← list of ssd_fit_dists() argument vectors;
│                  min_pmix uses NAME references into the
│                  min_pmix registry (§1.1)
├── hc           ← list of ssd_hc() argument vectors; the ci-FALSE
│                  collapse means bootstrap-only knobs (nboot,
│                  ci_method, parametric) are stored as NA on tasks
│                  where ci = FALSE (§1.2)
├── upload       ← NULL (no upload) or list(backend, url, …) (§6.1)
└── parent       ← NULL, or a previous results dir referenced for
                   parent_alias / mixed-code use (§8). Plain extension
                   (more datasets, more nsim) does NOT need a parent
                   reference — content-addressed Parquet is the cache.
```

Three design points distinguish this from the current code:

1. **`seed`, a scalar integer.** Re-running a scenario with a
   different RNG means changing this one number. The L'Ecuyer-CMRG
   `root_state` (length-7 vector) of the previous design is gone;
   dqrng + hash (§2) makes it unnecessary. Two scenarios with the
   same `seed` and the same task parameters produce identical RNG
   sequences.
2. **Datasets and `min_pmix` are referenced by name.** Both live in
   central registries; the scenario stores only names. This keeps the
   scenario serializable as a tiny manifest (a few names + numeric
   knobs) and lets the per-task hash (§2) ignore function-body
   contents — so a non-behaviour-changing code edit to a registered
   function does *not* invalidate cached results. See §1.1.
3. **No `parent` for plain extension.** Adding tasks (new dataset,
   more `nsim`, more `nrow` values, …) gives them new task hashes
   and new streams; existing tasks' hashes are unchanged so their
   Parquets are reused automatically. The `parent` reference is
   only for the two cases content-addressing alone can't handle:
   renaming/reordering datasets (`parent_alias`, §8) and mixing
   old + new code after a fix (§8).

### 1.1 Central registries: datasets and min_pmix

Both datasets and `min_pmix` functions live in **central registries**
keyed by name; the scenario stores names, not values. Two motivations:

1. **Hash stability under code edits.** Function-valued parameters
   would otherwise force a content-hash invalidation on any source
   edit, even cosmetic ones. By hashing the *name* and looking the
   function up at run time, an edit to (e.g.) `ssdtools::ssd_min_pmix`
   does not move tasks across streams. Correctness under such edits
   is the user's contract — pin `ssdtools` versions in the manifest.
2. **Compact, portable scenario manifests.** A scenario serializes
   to a small JSON/Parquet sidecar containing names + numeric knobs;
   data.frame contents and function bodies live in their own files.

#### Dataset registry

```
   results/datasets/<name>.parquet     # one file per registered dataset
   results/datasets/_index.json        # name -> { rows, conc_col, sha256, source }
```

Registration:

```r
ssd_register_dataset("boron",   ssddata::ccme_boron)
ssd_register_dataset("cadmium", ssddata::ccme_cadmium)
# Synthetic / function-generated datasets are materialized at
# registration time, not lazily; the scenario hashes the name only.
ssd_register_dataset(
  "rlnorm_n100",
  generator = function(seed) {
    dqrng::dqset.seed(seed); tibble::tibble(Conc = ssdtools::ssd_rlnorm(100))
  },
  seed = 1L                       # captured so registration is deterministic
)
```

Invariant: every registered dataset has a `Conc` column (the SSD
convention; verified at registration). Other columns are passed
through.

Scenarios reference datasets by name:

```r
ssd_scenario(datasets = c("boron", "cadmium"), nsim = 100, …)
```

Synthetic datasets are **materialized at registration time**, not on
demand: they live as Parquet files in the registry alongside
real-world data. Tasks reading them via name go through the same
content-addressed path as for empirical data. Trade-off: a
function-generated dataset must fit in memory at registration; for
large ones, generate directly to disk and register the resulting
Parquet path.

#### `min_pmix` registry

```r
ssd_register_min_pmix("default", ssdtools::ssd_min_pmix)
ssd_register_min_pmix("strict",  function(n) 0.05)
```

The scenario's `fit$min_pmix` entries are names from this registry;
the task-stream hash uses the name, not the function. The actual
function is looked up just before the call, after `dqset.seed()`.

### 1.2 The `ci = FALSE` collapse

The hc-arg cross-join treats `nboot`, `ci_method`, and `parametric`
as irrelevant when `ci = FALSE` — those knobs only affect bootstrap.
Concrete rules:

- If `ci = FALSE` is the only value, `nboot` / `ci_method` /
  `parametric` are ignored with a one-line message at scenario
  construction (and the scenario's `print()` records the ignore so
  it's visible in tracing).
- If both `ci = c(FALSE, TRUE)`, the `ci = FALSE` row collapses to a
  single task per upstream fit (bootstrap knobs are NA in the grid),
  while `ci = TRUE` rows fan out across `nboot × ci_method ×
  parametric` as usual.

In the task grid:

| sim | nrow | rescale | ci    | nboot | ci_method        | parametric |
| --: | ---: | :------ | :---- | ----: | :--------------- | :--------- |
| 1   | 5    | FALSE   | FALSE | NA    | NA               | NA         |
| 1   | 5    | FALSE   | TRUE  | 100   | weighted_samples | TRUE       |
| 1   | 5    | FALSE   | TRUE  | 1000  | weighted_samples | TRUE       |

The hash of an `NA`-bearing row is well-defined as long as `NA` is
encoded canonically — `task_stream_id()` does this via
`rlang::hash()` on the named list. The collapse therefore stops
phantom streams from being allocated to combinations that don't
exist in practice.

---

## 2. Per-task RNG via dqrng + hash

Validated by `scripts/experiment-dqrng-hash.R`. Each task gets its own
RNG stream, identified by a 31-bit hash of its task parameters. The
scenario carries a single integer `seed`; per-task RNG is

```r
dqrng::dqset.seed(seed = scenario$seed,
                  stream = task_stream_id(task_params))
```

where `task_stream_id(p)` is a length-2 integer vector packing
**62 bits** of `rlang::hash(p)`:

```r
task_stream_id <- function(p) {
  h <- rlang::hash(p)                 # 32-char xxhash128 hex
  c(
    pack_int31(substr(h,  1L,  8L)),  # hi: first 31 bits
    pack_int31(substr(h,  9L, 16L))   # lo: next 31 bits
  )
}
```

`dqset.seed()`'s `stream` argument accepts a length-2 integer vector
interpreted as a 64-bit `(hi32, lo32)` pair; we use 31 of each half
(sign bit zero to stay inside signed int32, the type `dqset.seed()`
coerces to). 62 effective bits.

`dqrng::register_methods()` is called once at pipeline init so that
base R's `runif()`, `rnorm()`, `rbinom()`, `rexp()`, `rgamma()`,
`rpois()`, `sample.int()`, `sample()` (and therefore
`dplyr::slice_sample()` and `ssdtools::ssd_r*()`) all consume RNG via
dqrng's PCG64 with the configured (seed, stream). The experiment
script verifies this end-to-end.

```
   ┌────────────────────────────────────────────────────────────┐
   │  task replay primitive                                     │
   │  ────────────────────                                      │
   │  dqRNGkind("pcg64")                                         │
   │  dqrng::register_methods()                                  │
   │  dqset.seed(seed = scenario$seed,                           │
   │             stream = task$stream_id)                        │
   │  …                          # run the step body            │
   │  dqrng::restore_methods()   # process-global restore on exit│
   └────────────────────────────────────────────────────────────┘
```

### Why this replaces the L'Ecuyer-CMRG sub-stream lattice

- **No precomputed lattice.** Each task's RNG is fully specified by
  the pair `(seed, stream_id)`. Both are small integers; the task row
  carries them as ordinary columns, not length-7 state vectors.
- **Extension is implicit.** Adding tasks (new datasets, more `sim`
  values, …) gives them new hashes and therefore new streams.
  Existing tasks' streams are unaffected — their hashes don't change.
- **Re-running a scenario with a different RNG** means changing
  `scenario$seed`. All task streams are re-rooted automatically.
- **Debuggability.** The task row carries `(seed, stream_id)`; a
  failing branch replays locally as a one-liner (see §7).

### What goes into the hash

`task_stream_id(p)` hashes a canonical, name-keyed representation of
the task's parameters. For a data task: `(dataset_name, sim, replace,
nrow_max)` — see §5 for the subset-property note on `nrow`. For a
fit task: data-task identity plus the fit-arg-grid row (`rescale`,
`computable`, `at_boundary_ok`, `min_pmix_name`, `range_shape1`,
`range_shape2`). For an hc task: fit-task identity plus the hc-arg-
grid row (`nboot`, `est_method`, `ci_method`, `parametric` — modulo
the `ci = FALSE` collapse documented in §1).

Function-valued parameters (`min_pmix`) are referenced **by name**
(§1.1) so a code edit inside the function does not change the hash;
correctness is the user's contract.

### Collision probability

62-bit stream id ⇒ 50% birthday collision around `sqrt(2^62) ≈ 2.1
billion` tasks. Empirically (script §4): 0 collisions at 1 k, 10 k,
and 100 k tasks (theory: order of 10⁻⁹ at 100 k). For typical
ssdsims scenarios (10²–10⁴ tasks) this is overwhelmingly safe.

The restart property (`dqset.seed(seed, stream) → same sequence`)
is exercised in `scripts/experiment-dqrng-hash.R`; the older
sub-stream restart check
`scripts/experiment-substream-restart.R` documents the L'Ecuyer
property that motivated the previous design and is kept for
reference.

---

## 3. Local run

```r
scenario <- ssd_scenario(
  ssddata::ccme_boron,
  nsim       = 100L,
  nrow       = c(5L, 10L, 50L),
  proportion = c(0.01, 0.05),
  nboot      = 1000,
  seed       = 42
)

ssd_run_scenario(scenario)                  # sequential, in-process
ssd_run_scenario(scenario, plan = "mirai")  # in-process parallel
```

`ssd_scenario()` stores the scenario inputs (seed, dataset names,
fit/hc arg grids). It is purely declarative — it does **not** expand
the task grid. Expansion is `ssd_scenario_tasks(scenario)`, called
either by `ssd_run_scenario()` (local) or by the `tasks` target in
the cluster pipeline (§4).

```
   ssd_scenario(...) ──▶ ssdsims_scenario   (declarative; carries seed)
                              │
                              ▼
                     ssd_scenario_tasks(scenario)
                              │
                              ▼
                     three task tables (data_tasks, fit_tasks, hc_tasks),
                     each row carrying its (seed, stream_id) pair (§2)
                              │
            ┌─────────────────┴─────────────────┐
            ▼                                   ▼
   ssd_run_scenario(scenario)         tar_target(...) feeds the
   sequential or in-process parallel  cluster pipeline (§4)
```

---

## 4. From local to a cluster

The scenario object is unchanged. **Three ingredients come together**
to produce the cluster pipeline; none of them is downstream of the
others — they're equal inputs that get assembled into the final
`_targets.R`:

```
   ┌──────────────────┐  ┌──────────────────┐  ┌──────────────────┐
   │ A. Example       │  │ B. Toy pipeline  │  │ C. Working       │
   │    pipeline for  │  │    for our       │  │    scenario      │
   │    another       │  │    target        │  │    object        │
   │    cluster       │  │    cluster       │  │                  │
   └────────┬─────────┘  └────────┬─────────┘  └────────┬─────────┘
            │                     │                     │
            └─────────────────────┼─────────────────────┘
                                  │ assemble
                                  ▼
              ┌───────────────────────────────────────┐
              │ ssdsims _targets.R for our cluster    │
              │                                       │
              │   scenario ─▶ tasks ─▶ task_groups    │
              │                            │          │
              │                            ▼          │
              │                pattern = map(...) on  │
              │             crew_controller_slurm()   │
              │                            │          │
              │                            ▼          │
              │              one Slurm job per group, │
              │              one Parquet per job      │
              └───────────────────────────────────────┘
```

The three ingredients are **equally important** and gathered in
parallel; none is downstream of the others. Roles:

- **A — example pipeline for another cluster** contributes the
  *shape* of `_targets.R`: how a `crew` controller is constructed,
  how dynamic branching is wired, where results land, where the
  merge target sits. Lifted as a skeleton, not as content.
  Source: another lab's published targets+crew repo.

- **B — toy pipeline for our target cluster** contributes the
  *backend*: a `crew.cluster::crew_controller_slurm()` (or
  equivalent for the actual scheduler) configured with the right
  queue, module loads, and scratch paths. Drafted with LLM help and
  validated by submitting one trivial job end-to-end **before any
  ssdsims logic is involved** — proves the cluster wiring works.

- **C — working scenario object** contributes the *content*:
  `seed`, dataset names, fit/hc argument vectors, optional
  `upload` (§6.1), optional `parent` (§8). Already exercised
  locally with `ssd_run_scenario()` (§3) so the only remaining
  unknown when assembling the three is the cluster wiring itself.

Only the controller and resource specs (from B) change between
clusters. Pipeline shape (from A) and task content + RNG (from C)
are scheduler-independent.

---

## 5. Three grids, three fan-outs

The three RNG-touching operations consume **distinct cross-joined
parameter grids**, and the grids grow monotonically:

```
   data grid     ⊆     fit grid     ⊆     hc grid

   (dataset, sim, nrow, replace)          ┐  10 rows
        │                                 │  in the
        │ + (rescale, computable,         │  second
        │    at_boundary_ok, min_pmix,    │  example
        │    range_shape1, range_shape2)  │
        ▼                                 │
   fit grid                               │  10 rows
        │                                 │  (fit-arg
        │ + (nboot, est_method,           │   grid = 1)
        │    ci_method, parametric)       │
        ▼                                 │
   hc grid                                ┘  180 rows
                                             (10 · 6 · 3)
```

Confirmed by tracing `scripts/example.R`'s second scenario:

| step                  | grid size | fan-out                                       |
| --------------------- | --------: | --------------------------------------------- |
| `slice_sample_state()`|       10  | 2 sim · 5 nrow                                |
| `fit_dists_seed()`    |       10  | 2 sim · 5 nrow · 1 (fit-arg grid)             |
| `hc_seed()`           |      180  | 2 sim · 5 nrow · 6 nboot · 3 est_method       |

`proportion` is *inside* `ssd_hc()` (rows of the hc result tibble), not
a cross-join axis. `ci_method` and `parametric` were scalar in the
second example but are full cross-join axes in the general case.

### Stream allocation: one per task, via hash

Each task in each grid gets its own dqrng stream, keyed by the
31-bit hash of the task's parameters (§2). Tasks do not share
streams across stages or across grid axes; the only sharing is
the deliberate `nrow` subset property below.

For the small `nsim = 2, nrow = c(5, 10), rescale = c(F, T),
est_method = c("arithmetic", "multi")` example:

```
   data grid:    2 sim · 1 (effective nrow, subset trick below)  =  2 streams
   fit  grid:    data ·  2 rescale                               =  8 streams
   hc   grid:    fit  ·  2 est_method                            = 16 streams
                                                            sum = 26 streams
```

(28 in `scripts/example-expanded-grids-independent.R` if `nrow` is
treated as an independent axis; 26 with the subset trick.)

### `nrow` subset property

For empirical-data slicing, larger `nrow` values include the same
rows as smaller ones (with no resampling). This means `nrow` is
**not** an independent stream axis for the data step: the stream is
keyed by `(dataset, sim)` only, and the slice is

```r
slice_sample_stream <- function(data, n_max, n, seed, stream) {
  dqrng::dqset.seed(seed, stream = stream)
  idx <- sample.int(nrow(data), size = n_max, replace = FALSE)
  data[idx[seq_len(n)], , drop = FALSE]
}
```

with `n_max = max(scenario$nrow)` pre-computed from the scenario.
Result: `slice_sample_stream(data, n_max, 5, …)` is a prefix of
`slice_sample_stream(data, n_max, 10, …)` (same `(seed, stream)`,
same `sample.int` call, just truncated). The property is
documented as `assuming stability of sample.int()` — pin R version
in the manifest.

The trick costs one extra integer column on the data task table
(`n_max`) and lets us cut `|data grid|` from `|dataset| · |sim| ·
|nrow|` to `|dataset| · |sim|`. For the small example that's
`2 → 2` (already minimal); for a scenario with `nrow = c(5, 6, 10,
20, 50)` it cuts the data fan-out by 5×.

(`replace = TRUE` does not have the subset property; if `replace =
TRUE` appears in the scenario, the data stream reverts to keying on
`(dataset, sim, nrow, replace)`.)

### Implications for the targets pipeline

Each step needs its **own** task grouping (`data_tasks`, `fit_tasks`,
`hc_tasks`) and its own dynamic-branched target — a single shared
`task_groups` mapped lockstep through all three steps does **not**
work when the grids differ. Layers link by **content-addressed file
path**: each task row carries the upstream task IDs it depends on
(`data_id` on fit rows, `fit_id` on hc rows), and the per-branch
body opens the right upstream Parquet by that ID. §6 wires this up
concretely.

---

## 6. Target graph (small example)

Concrete pipeline matching `scripts/example-expanded-grids.R`:
`nsim = 2L`, `nrow = c(5L, 10L)`, `rescale = c(FALSE, TRUE)`,
`est_method = c("arithmetic", "multi")`, `nboot = 10`, single
dataset (`ssddata::ccme_boron`). Each of the three steps fans out
according to **its own grid** (§5):

```
   data grid:  1 dataset · 2 sim · 2 nrow                     =  4 rows
   fit  grid:  data grid · 2 rescale                          =  8 rows
   hc   grid:  fit  grid · 1 nboot · 2 est_method             = 16 rows
```

The script verifies this fan-out byte-for-byte against
`ssd_run_scenario(seed = 42L, ...)`. Each step writes a Parquet
file per branch so the data, fit, and hc layers are independently
queryable for analysis without re-running upstream steps.

```
   scenario   (declarative; carries seed)
       │
       ├──▶ data_tasks  ( 4 rows, carries data_id, data_stream)
       │         │
       │         ▼  tar_group_by(data_id), pattern = map(data_groups)
       │     data_job   ──▶ results/data/<data_id>.parquet
       │
       ├──▶ fit_tasks   ( 8 rows, carries fit_id, data_id, fit_stream)
       │         │
       │         ▼  tar_group_by(fit_id), pattern = map(fit_groups)
       │     fit_job    ──▶ results/fit/<fit_id>.parquet
       │                 reads results/data/<data_id>.parquet by path
       │
       └──▶ hc_tasks    (16 rows, carries hc_id, fit_id, hc_stream)
                 │
                 ▼  tar_group_by(hc_id), pattern = map(hc_groups)
             hc_job     ──▶ results/hc/<hc_id>.parquet
                         reads results/fit/<fit_id>.parquet by path

   summary  ──▶ results/summary.parquet
                (reads all three layers via duckplyr)
```

The link between layers is by **content-addressed path**, not by a
single shared dynamic-branch index — each task row carries its
upstream IDs (`fit_tasks$data_id`, `hc_tasks$fit_id`) and the body
opens the right upstream Parquet by that ID. This is what lets
tweaking `rescale` re-run fits + hc without re-running data (the
fit task row's `data_id` is unchanged).

`_targets.R` sketch:

```r
list(
  tar_target(scenario,
    ssd_scenario(
      ssddata::ccme_boron,
      nsim = 2L,
      nrow = c(5L, 10L),
      rescale = c(FALSE, TRUE),
      est_method = c("arithmetic", "multi"),
      nboot = 10,
      seed = 42L)),

  # Three separate task tables, one per grid (§5).
  tar_target(data_tasks, ssd_scenario_data_tasks(scenario)),
  tar_target(fit_tasks,  ssd_scenario_fit_tasks(scenario)),   # carries data_id
  tar_target(hc_tasks,   ssd_scenario_hc_tasks(scenario)),    # carries fit_id

  tar_group_by(data_groups, data_tasks, data_id),
  tar_group_by(fit_groups,  fit_tasks,  fit_id),
  tar_group_by(hc_groups,   hc_tasks,   hc_id),

  tar_target(
    data_job,
    ssd_run_data_step(data_groups, scenario, out_dir = "results/data"),
    pattern = map(data_groups), format = "file"
  ),

  tar_target(
    fit_job,
    ssd_run_fit_step(fit_groups, scenario,
                     data_dir = "results/data",
                     out_dir  = "results/fit"),
    pattern = map(fit_groups), format = "file"
  ),

  tar_target(
    hc_job,
    ssd_run_hc_step(hc_groups, scenario,
                    fit_dir = "results/fit",
                    out_dir = "results/hc"),
    pattern = map(hc_groups), format = "file"
  ),

  tar_target(
    summary,
    ssd_summarize(dir_data = "results/data",
                  dir_fit  = "results/fit",
                  dir_hc   = "results/hc",
                  path     = "results/summary.parquet"),
    format = "file"
  )
)
```

Each `ssd_run_*_step()` body reads its upstream Parquet by content-
addressed path (`fit_groups$data_id` tells the fit step which data
file to open), enters the appropriate `.state_*` from the task row,
and writes a single Parquet to `out_dir/<id>.parquet`. To keep
`fit_job` from depending on the whole `data_job` target (which would
re-run every fit branch on any data branch change), we use file-path
indirection: the fit body opens `file.path(data_dir,
sprintf("%s.parquet", fit_groups$data_id[1]))`. `targets` tracks the
*directory* by hash of all file names it contains, so adding new data
branches does not invalidate existing fit branches.

**Dependencies and what re-runs on a knob change** (applied to the
4/8/16 grid above):

| Knob change                       | data_job (4)        | fit_job (8)         | hc_job (16)         | summary |
| --------------------------------- | ------------------- | ------------------- | ------------------- | ------- |
| dataset appended (§1.1)           | new branches only   | new only            | new only            | re-run  |
| `nrow` value added                | new branches only   | new only            | new only            | re-run  |
| `nsim` grows                      | new branches only   | new only            | new only            | re-run  |
| `dists`                           | cached              | re-run all 8        | re-run all 16       | re-run  |
| `rescale` value added             | cached              | new branches only   | new only            | re-run  |
| `nboot` added                     | cached              | cached              | new only            | re-run  |
| `est_method` value added          | cached              | cached              | new only            | re-run  |
| `ci_method` / `parametric` added  | cached              | cached              | new only            | re-run  |
| `seed`                            | re-run all 4        | re-run all 8        | re-run all 16      | re-run  |
| dataset *renamed* (no alias, §8)  | re-run all          | re-run all          | re-run all          | re-run  |

Three steps as three targets is what makes this matrix possible: the
existing per-task design (data + fit + hc in one branch) cannot cache
a fit when only `nboot` changes.

**Available for analysis:**

After `tar_make()`, the three step layers are queryable independently
via duckplyr without going through `targets`:

```r
duckplyr::read_parquet_duckdb("results/data/*.parquet") |>
  dplyr::filter(nrow == 10L) |> dplyr::collect()
duckplyr::read_parquet_duckdb("results/fit/*.parquet")  |> ...
duckplyr::read_parquet_duckdb("results/hc/*.parquet")   |> ...
```

A child scenario (§8) only needs `results/hc/` *plus* the parent's
`end_state` to extend; it does not need to re-read fit or data.

### 6.1 Cloud upload hook

The data/fit/hc Parquets are the user-facing artefacts and they need
to be readable **from outside the cluster** — analysis notebooks on a
laptop, dashboards, downstream R/Python scripts. The scenario carries
an optional `upload` field describing a destination object store; each
per-step target pushes its Parquet there right after the local write.

```
   scenario$upload (NULL by default; non-NULL example):
     list(
       backend   = "azure_blob",
       url       = "https://<acct>.blob.core.windows.net",
       container = "ssdsims-results"
     )
```

Per-step flow when `upload` is non-NULL:

```
   ssd_run_<step>_step(...)
        │
        ▼ writes results/<step>/<id>.parquet  (local; targets tracks this)
        │
        ▼ pushes the same file to  <url>/<container>/<step>/<id>.parquet
                                   (cluster-side helper, e.g. AzureStor)
        │
        ▼ records the upload's sha256 in the result manifest
```

The local Parquet stays on disk so `targets`' `format = "file"`
tracking is unaffected; the cloud copy is an additional artefact.

**Auth is external.** Credentials come from environment variables
(`AZURE_STORAGE_ACCOUNT`, `AZURE_STORAGE_KEY`, or a service-principal
combo). The scenario object does **not** carry secrets — it carries
only the destination URL and container name.

**Connectivity probe up front.** `ssd_test_upload(scenario)` performs
a minimal round-trip (list the container, write and delete a small
marker blob) and either returns silently or errors with the
backend's diagnostic. The pipeline calls it once at the start of
`tar_make()` so an auth or network failure aborts before any
compute starts. Easy to run interactively too:

```r
scenario <- ssd_scenario(..., upload = list(backend = "azure_blob", ...))
ssd_test_upload(scenario)   # silent on success, throws on failure
tar_make()
```

**Failure mode.** A per-file upload error becomes the target's error;
the local Parquet remains, so `tar_make()` can be re-driven and the
upload retried. The scenario's manifest records, per `id`, the local
sha256 and the cloud sha256; a mismatch flags a corrupted transfer.

---

## 7. Debugging a cluster failure

The targets/tarchetypes layer is an abstraction, and abstractions
cost debuggability. The cluster pipeline is only debuggable if any
single failed branch can be replayed **outside targets, on any
machine**, with the same inputs and the same RNG state.

The stream-only primitives `slice_sample_stream()`,
`fit_dists_stream()`, `hc_stream()` are the contract that makes this
work. Each takes its inputs as plain values — data, a `(seed,
stream_id)` pair, scalar params — so the **task row plus the
immediate upstream Parquet is a complete reproducer**.

### Scenario

`tar_make()` against `crew_controller_slurm()` fans out N hc
branches; three error on remote workers:

```
   targets reports:
     ✗ hc_job_3ab9c7   (slurm-worker-12)
     ✗ hc_job_5fe201   (slurm-worker-04)
     ✗ hc_job_91da33   (slurm-worker-21)
```

The user wants the three failures reproduced locally, fast, without
re-running the other N−3 jobs.

### Recipe

```
   1. Identify the task row (on the cluster, or after rsync).
      ────────────────────────────────────────────────────────
      task <- tar_read(hc_tasks) |> dplyr::filter(hc_id == "3ab9c7")

      The row carries:
        - hc_stream  (length-2 integer = c(hi31, lo31), §2)
        - all hc params (nboot, est_method, ci_method, ...)
        - fit_id     (content-hash of the upstream Parquet)

   2. Locate the upstream artefact by content-addressed path.
      ───────────────────────────────────────────────────────
      results/fit/<fit_id>.parquet
      The immediate upstream is enough; no need to walk further.

   3. Sync to local.
      ──────────────
      rsync cluster:_targets/objects/hc_tasks       ./_targets/objects/
      rsync cluster:results/fit/<fit_id>.parquet    ./results/fit/

   4. Reproduce, no targets involved.
      ───────────────────────────────
      fit <- ssd_read_step("results/fit/<fit_id>.parquet")
      options(error = recover)        # or place browser() in hc_stream
      out <- ssdsims:::hc_stream(
        data       = fit,
        seed       = scenario$seed,
        stream     = task$hc_stream[[1]],
        nboot      = task$nboot,
        est_method = task$est_method,
        ci_method  = task$ci_method,
        proportion = scenario$hc$proportion,
        ci         = scenario$hc$ci,
        parametric = task$parametric,
        save_to    = NULL
      )
```

The call is the same code that ran on the worker; the `(seed,
stream)` pair is the same; the upstream Parquet is the same bytes.
A deterministic bug reproduces on the first call.

### Helper

A single helper compresses steps 1, 2 and 4:

```r
ssd_replay_task(task_id, store = "_targets", results_dir = "results")
```

infers the step from the task table the id sits in, opens the
immediate upstream Parquet, and calls the matching `_stream`
primitive with the right args. The stream-only primitives are the
supported branch-replay API.

### What makes this work

```
   ┌───────────────────────────────────────────────────────────────────┐
   │  task row + upstream Parquet = complete reproducer                │
   │  ───────────────────────────────────────────────────              │
   │  seed       scenario-scalar integer; in the manifest              │
   │  stream     length-2 integer (hi31, lo31); on the task row        │
   │  upstream   one Parquet per branch; content-addressed; tooling-   │
   │             agnostic (DuckDB, Python, R)                          │
   │  params     scalars on the task row                               │
   │  primitive  `*_stream()` takes (data, seed, stream, ...args); no  │
   │             hidden dependency on targets or the orchestrator     │
   └───────────────────────────────────────────────────────────────────┘
```

`tarchetypes` is just the orchestrator. Removing it from the
reproducer is a feature, not a workaround.

### Lightweight reproduction: skip the rsync, verify the inputs

If the prefix of the pipeline is cheap to re-run, the recipe above
collapses: drive a local `tar_make()` up to the failing target and
skip step 3 entirely. The catch is **verifying that the locally
regenerated upstream matches what the cluster's failed branch
actually consumed** — without that, you might be debugging a
phantom.

The mechanism is content-addressed hashing. Every Parquet the
cluster writes is fingerprinted with `sha256` and the value is
stored in the parent's manifest before any upload happens:

```r
manifest$completed_hashes[["<fit_id>"]]
#> "8c92…"  (sha256 of results/fit/<fit_id>.parquet at write time)
```

Local recipe:

```
   1. tar_make() locally up to fit_job, then look at the
      regenerated results/fit/<fit_id>.parquet.
   2. local_hash  <- digest::digest(file = "results/fit/<fit_id>.parquet",
                                    algo = "sha256")
      parent_hash <- parent_manifest$completed_hashes[["<fit_id>"]]
      stopifnot(identical(local_hash, parent_hash))
        ✓  the cluster's input is reproduced byte-for-byte;
           continue to step 4 of the rsync recipe.
        ✗  upstream is host-dependent (BLAS, system libs, env);
           fall back to rsync.
   3. With inputs verified, replay the failing hc_state() call
      directly (no targets involved) -- same as the rsync recipe.
```

Same primitive (`hc_state(data, state, ...)`); the only thing the
two recipes differ on is **where** the upstream Parquet came from.
Hash verification is the bridge.

### Constraints

- The bug must be deterministic in `(data, state, params)`. Non-
  determinism from BLAS, system libraries, or untracked global state
  is out of scope; capture `sessionInfo()` per-branch alongside the
  Parquet to narrow it.
- Content-addressing must be host-independent: the `<id>.parquet`
  name is the task-row hash, not a path or mtime, so the same row
  on cluster and laptop resolves to the same file name.
- The manifest must record `completed_hashes` (per-id sha256). Without
  it, lightweight reproduction can't be verified.
- Only the immediate upstream is required. To debug `hc`, pull or
  regenerate the one `fit` Parquet; you do not have to refit
  upstream of that. To debug `fit`, the one `data` Parquet; you do
  not have to resample.

Once the bug is fixed, the natural follow-up is to lock in the
surviving N−3 results and re-run only the 3 failures despite the
code change. That is the same primitive as scenario extension —
see §8 (the re-run-after-fix shape is just `desired \ completed`
with `desired = parent.desired_grid`).

---

## 8. Extension

With per-task hash-keyed streams (§2) and content-addressed Parquet
files (§6), the **default** extension story is almost trivial:

```
   For each task in scenario:
     id = task_stream_id(task_params)
     if results/<step>/<id>.parquet exists  → skip (cache hit)
     else                                    → run, write <id>.parquet
```

A "child" scenario is just a *bigger* scenario described by the same
declarative API. Tasks whose `id` already has a Parquet on disk are
not re-run; the rest are. No `parent` reference, no manifest
plumbing, no `desired \ completed` set-difference machinery — content
addressing handles all of it.

| Extension                  | Mechanism                                                          |
| -------------------------- | ------------------------------------------------------------------ |
| Append a dataset           | bigger `datasets` vector ⇒ new task IDs ⇒ new Parquets only        |
| Grow `nsim`                | bigger `nsim` ⇒ new IDs for the added `sim` values                 |
| More `nrow` values         | data step's `n_max` increases ⇒ data stream re-hashes ⇒ re-run *data*; fit/hc may reuse via subset (open question, see §11) |
| Add a `rescale` / `nboot`  | new fit / hc IDs ⇒ new Parquets only                               |

Two cases need an explicit `parent` reference because content
addressing alone doesn't cover them:

### 8.1 Rename or reorder datasets — `parent_alias`

Renaming `boron → b` changes the task ID's `dataset` field and
therefore the hash. Without help, every Parquet would look uncached.
The child scenario carries a name-mapping that's consulted *before*
the cache lookup:

```r
ssd_scenario(
  datasets     = c("b", "cd"),
  parent       = "../parent/results",
  parent_alias = c(b = "boron", cd = "cadmium"),
  …
)
```

For each new-name task, look up the parent-name task, check if its
Parquet exists in the parent's directory; if yes, re-attribute (link
or rename) under the new ID. No streams are allocated; no compute.

### 8.2 Mixed-code lock-in after a code fix — dag-of-dags

The post-§7 case: a bug in `hc_state()` is fixed, but the user wants
to lock in the surviving Parquets that ran under the buggy code and
only re-run the failed branches under the fix. Content-addressing
alone doesn't help here — the task IDs haven't changed, so deleting
the bad Parquets is the simplest move and lets `targets`' native
file-tracking re-run them automatically:

```r
# In the parent results dir, after the fix is committed:
unlink(failed_parquets)
tar_make()    # re-runs only the missing branches under new code
```

The dag-of-dags variant (separate child project pointing at the
parent's results dir) is for when the user wants the parent's
results to remain *visibly untouched* — useful for audit trails or
side-by-side comparison of old vs new code. The child writes its
re-runs to its own results dir and the `summary` target unions both.

(An alternative *within* a single `targets` project is
`targets::tar_invalidate(names = failed_branch_names)` to force
re-runs without unlinking — same end state, different bookkeeping.
See open question 4 in §11.)

### 8.3 Manifest contents

Per-scenario manifest (a small JSON sidecar to the results
directory):

- `seed` — scenario's RNG root (§2).
- `datasets` — name vector referenced from tasks (§1.1).
- `min_pmix` — name vector ditto.
- `fit`, `hc` — the argument-vector grids.
- `completed_ids` — set of `id`s whose Parquet exists and is
  trusted (recorded at write time, including the cloud copy's
  sha256 if `upload` is set; see §6.1, §7).
- `r_version`, `dqrng_version`, `ssdtools_version` — versions
  pinned for bit-stability across re-runs (§9).
- `dataset_names`, `min_pmix_names` — for `parent_alias` validation
  in 8.1.

Restart property for dqrng (same `(seed, stream)` ⇒ same draw
sequence) is verified by `scripts/experiment-dqrng-hash.R`.

---

## 9. Limitations

Constraints the design lives with rather than solves.

### `dists` and `nboot` are not fit/hc grid axes

`dists` controls *which* distributions `ssdtools::ssd_fit_dists()`
fits to a given data slice; `nboot` controls how many bootstrap
iterations `ssdtools::ssd_hc()` runs. Both iterations live inside
ssdtools and are not exposed at the ssdsims level. Adding a
distribution to `dists` therefore invalidates every fit branch (the
hash changes); raising `nboot` invalidates every hc branch. Reusing
partial results — `nboot = 100` ⊂ `nboot = 1000` — would require
either (a) wrapping each per-distribution / per-bootstrap iteration
in ssdsims with its own dqrng stream and aggregating, or (b) an
ssdtools change to expose the inner loops. Sketch only; out of
scope.

### `ssdtools` RNG flow is opaque

The internal RNG consumption of `ssdtools::ssd_fit_dists()` and
`ssd_hc()` is a black box. We install a known `(seed, stream)`
before each call, but how many uniforms the ssdtools call draws and
in what order is an ssdtools implementation detail. A breaking
change to that order in a future ssdtools release would silently
change bit-stable results even when the scenario `seed` is unchanged.
Mitigation: pin `ssdtools` (and `dqrng`, R) versions in the
scenario manifest.

### `nrow` subset property requires stable `sample.int`

§5's subset trick (`slice_sample_stream` returns a prefix of itself
for smaller `n`) holds only as long as `sample.int(N, n_max,
replace = FALSE)` is stable across R versions. Pinning the R
version in the manifest covers this. `replace = TRUE` does not have
the subset property; if `replace = TRUE` appears in the scenario
the data stream reverts to keying on `(dataset, sim, nrow,
replace)`.

### `dqrng::register_methods()` is process-global

The pipeline installs dqrng as the base R RNG backend at start-up
and must restore on exit. Tests and helper scripts that run inside
the same R session need the same discipline (`on.exit(restore_methods())`
in any function that touches the methods).

---

## 10. Gaps from `RNG-FLOW.md` §5 — how this design closes them

| Gap                                                  | Resolution                                                                                  |
| ---------------------------------------------------- | ------------------------------------------------------------------------------------------- |
| No DAG-of-DAGs primitive                             | §8.2 — child reads parent's `results/` by content-addressed path; mostly unnecessary, see below. |
| No "load previous run from Parquet" path             | §8 — content-addressed Parquet files *are* the cache; no explicit load needed.              |
| Persists fragile RNG state                           | §1, §2 — scenario stores a single integer `seed`; per-task `(seed, stream_id)` is reproducible via `dqset.seed()`. |
| Positional task IDs                                  | §2 — task IDs are content-addressed: `task_stream_id(p)` = 62-bit hash of canonical params. |
| Re-derivation cost is quadratic                      | §2 — per-task hash is O(1); no precomputed lattice.                                         |
| `nsim`-grow cache invalidation                       | §1, §2, §8 — new sim values hash to new streams; existing task IDs (and their Parquets) are untouched. |
| Three steps cached as one (no per-step re-runs)      | §5, §6 — data/fit/hc are three grids and three targets, each with its own Parquet layer.    |
| Same lattice for all steps despite grid mismatch     | §5 — each step has its own grid and stream id.                                              |
| `nrow` invalidates data states for the same `sim`    | §5 — `nrow` subset trick: data stream keyed by `(dataset, sim)`, slice truncates to `n`.    |
| Single-dataset scenarios only                        | §1.1 — datasets are name-referenced in a central registry; cross-join axis.                 |
| Function-arg edits invalidate caches                 | §1.1 — `min_pmix` referenced by name; function body edits do not move tasks across streams. |
| Bootstrap-only knobs spuriously fan out under `ci=FALSE` | §1.2 — `ci=FALSE` collapses `nboot`/`ci_method`/`parametric` to NA; one task instead of N. |
| Branch failure unreproducible off the cluster        | §7 — task row + upstream Parquet replays the failing branch via `_stream` primitives.       |
| Code fix re-runs every branch by hash invalidation   | §8.2 — `unlink()` failed Parquets (or `tar_invalidate`); content addressing leaves the rest untouched. |
| Off-cluster access to Parquet outputs                | §6.1 — `scenario$upload` pushes each Parquet to a configurable object store (e.g. Azure Blob). |
| Phantom local repros (regenerated upstream ≠ cluster's actual) | §7 — manifest's per-id sha256 lets the lightweight recipe verify the local upstream before running the failing step. |

The RNGkind side-effect bug and the independent data/fit/hc substream
issues from the original L'Ecuyer design no longer apply: dqrng with
explicit `(seed, stream)` per call has no side effects on global RNG
state of other tasks (`register_methods()` switches the backend once
per process and is restored on exit).

---

## 11. Open questions for review

1. **`nrow` subset property as a contract.** §5's `slice_sample_stream`
   relies on `base::sample.int(N, n_max, replace = FALSE)` being a
   prefix-of-itself when `n_max` is reduced. R has historically been
   stable here but it isn't a documented guarantee. Document the
   assumption in the manifest and pin R versions, or implement our
   own `sample.int`-equivalent function with an explicit contract?
2. **Dataset identity.** Datasets are keyed by **name** in the
   registry, not by `digest::digest(df)`. Two registrations under the
   same name with different bytes silently collide. Should the
   registry refuse re-registration unless byte-identical, or carry a
   content hash on the task ID alongside the name?
3. **Force re-run inside one `targets` project.** §8.2 lists two
   options for re-running after a code fix: `unlink()` the bad
   Parquets, or `tar_invalidate(names = ...)`. Which is the
   recommended path? `unlink` is simpler but loses the bookkeeping;
   `tar_invalidate` is bookkeeping-preserving but cluster-aware
   (some `targets` versions don't propagate across remote workers).
4. **Manifest format.** A scenario manifest stores `seed`, the
   `datasets` and `min_pmix` name lists, the `fit` and `hc`
   argument-vector grids, the `upload` spec, and pinned package
   versions (§9). JSON sidecar next to Parquet, or `tar_read()`
   against the project's `_targets/` store? The latter is idiomatic
   but couples a reader to the project's `targets` version.
5. **Toy pipeline shape.** Ship a single
   `inst/targets-templates/cluster/` that the LLM-authoring prompt
   edits, or only documentation pointing at `crew.cluster` examples?

---

## 12. Roadmap

In-place, step-by-step implementation. Each step lands as a coherent
working state, in order; ssdsims has no downstream dependencies so
breaking-change steps are acceptable.

| # | Step                                                                                                                  | Verifies                                              |
| - | --------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------- |
| 1 | Add `dqrng` to Imports; add `dqRNGkind("pcg64")` + `register_methods()` setup on package load with restore on unload. | `scripts/experiment-dqrng-hash.R` still passes.       |
| 2 | Replace `with_lecuyer_cmrg_state` / `local_lecuyer_cmrg_state` with `with_dqrng_stream(seed, stream, code)`.            | Smoke: existing seed-based unit tests still green by routing seed through `dqset.seed(seed)`. |
| 3 | Introduce `task_stream_id(params)` (§2 hash helper) and write `slice_sample_stream`, `fit_dists_stream`, `hc_stream` as the new state-only primitives. | Unit tests: byte-equivalence per-call given the same `(seed, stream)`. |
| 4 | Migrate `ssd_sim_data.data.frame`, `ssd_fit_dists_sims`, `ssd_hc_sims` to the `*_stream` primitives; keep the `_seed` wrappers as a thin shim for one release for migration. | `scripts/example-expanded.R` and `example-expanded-grids.R` updated to the new primitives; tests + tracing pass. |
| 5 | Add the dataset registry (`ssd_register_dataset`, `ssd_dataset()`); refactor `ssd_scenario()` constructor to accept `datasets = c("name", …)`. Synthetic datasets materialised at registration. | New scenario constructor smoke-tests; verify `Conc` column invariant. |
| 6 | Add the `min_pmix` registry (`ssd_register_min_pmix`); use names in the scenario's `fit$min_pmix`. Function-body edits no longer invalidate fit branches. | A regression test that edits the body of a registered function and re-runs without invalidation. |
| 7 | Implement `slice_sample_stream` with the `nrow` subset trick (§5); add scenario-level `n_max` derivation. | Test: `nrow = c(5, 10)` results are byte-equivalent prefixes for the same `(dataset, sim)`. |
| 8 | Implement the `ci = FALSE` collapse in the hc-task table (§1.2). | Test: cross-join with `ci = c(FALSE, TRUE)` produces the expected reduced fan-out. |
| 9 | Implement the manifest writer / reader (§8.3 fields). Each step's target writes a per-id sha256 alongside the Parquet. | `scripts/experiment-substream-restart.R` adapted to verify manifest round-trips. |
| 10 | Implement `ssd_scenario_data_tasks` / `fit_tasks` / `hc_tasks` returning the per-step task tables with `(seed, stream_id)` columns. | The targets pipeline sketch in §6 compiles and `tar_make()`s a tiny scenario. |
| 11 | Implement the cluster pipeline (§6) under `inst/targets-templates/cluster/`. Drive via `crew.cluster::crew_controller_slurm()` against a sandbox queue (or the LLM-authored toy pipeline, §4 B). | `tar_make()` end-to-end on a real cluster; one Parquet per branch. |
| 12 | Implement the cloud-upload hook (§6.1) and `ssd_test_upload()`. | Hello-Azure round trip from interactive R; tar_make's first target is the connectivity probe. |
| 13 | Implement `ssd_replay_task()` (§7 helper) and the lightweight `ssd_input_hash()` verifier. Update `scripts/example-expanded*.R` to demonstrate the debug recipes. | A test that simulates a branch failure and verifies the recipe reproduces locally. |
| 14 | Implement `parent_alias` (§8.1) and the `unlink()` / `tar_invalidate` choice (§8.2). | A test that renames a dataset and shows zero re-runs; a test that mocks a failed branch, fixes the "bug", and shows only the bad Parquet is re-run. |
| 15 | Remove the legacy L'Ecuyer-CMRG helpers and the `_seed` shims. `scripts/experiment-substream-restart.R` becomes a historical reference. | Package check + tests green; design and code in lockstep. |

Dependencies between steps: 1 is a prerequisite for 2 and the rest;
3 is a prerequisite for 4 and 7; 5 for 6 for 8; 9 for 11–14. Steps
10–14 can interleave once 9 has landed.
