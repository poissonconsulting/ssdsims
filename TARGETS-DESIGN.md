# Targets design — `ssdsims`

End-to-end workflow for running an `ssdsims` scenario on a cluster,
**reproducibly** and **extensibly**. Parallelism is assumed throughout;
this document is about the **scenario object**, **how a scenario reaches
a cluster**, and **how a scenario extends a previous one**.

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
├── master_state   ← length-7 L'Ecuyer-CMRG integer vector;
│                    the sub-stream root this scenario advances from
│                    (NOT a scalar seed — see §2)
├── nsim           ← number of replicate sims per dataset
├── generator      ← named list of data.frames (§1.1)  | function | fitdists
├── fit            ← list of ssd_fit_dists() argument vectors
├── hc             ← list of ssd_hc() argument vectors
└── parent         ← NULL, or a previous ssdsims_scenario / results path
                     this scenario extends (§7)
```

Four design points distinguish this from the current code:

1. **`master_state`, not `seed`.** The scenario stores the
   *post-`set.seed` post-`RNGkind` L'Ecuyer-CMRG state*, not the integer
   seed. Two scenarios with the same `master_state` produce identical
   task grids regardless of whether the seed→state transformation in
   `get_lecuyer_cmrg_*()` is later refactored. `ssd_scenario(seed = 42)`
   is a convenience constructor that derives and stores the state.
2. **Sub-streams only; the stream axis is reserved.** Every scenario
   (and every child) advances via `parallel::nextRNGSubStream()` from
   its `master_state`. `parallel::nextRNGStream()` is not invoked by
   any scenario primitive; the stream axis is kept available for
   future applications (e.g. statistically independent batch axes
   layered on top of scenarios).
3. **List of data.frames as the principal generator.** The data
   generator is a **named list** of data.frames; a bare data.frame is
   silently lifted to a length-1 list (named `"data"` by default).
   Each dataset is its own cross-join axis, so a scenario with three
   datasets and `nsim = 100` materializes `3 × 100 × |nrow| × …`
   tasks. The `function` and `fitdists` generators remain supported
   but are secondary; the list-of-data.frames path is the one §6 and
   §7 exercise.
4. **`parent`.** A scenario can point at an upstream scenario it
   extends. Extension always advances on the sub-stream axis (§7):

```
   scenario_v1                       scenario_v2 (parent = v1)
   master_state = M                  master_state = M_next
   nsim = 100                        nsim = 100
        │                                  │
        └── extend ────────────────────────┘
            where M_next is the first sub-stream past
            v1's last consumed sub-stream (§7).
```

### 1.1 List-of-data.frames generator

```
   generator = list(
     boron     = ssddata::ccme_boron,
     cadmium   = ssddata::ccme_cadmium,
     chloride  = ssddata::ccme_chloride
   )
```

Canonical task ordering puts the **dataset name as the outermost
axis**, then `sim`, then the remaining cross-join axes
(`nrow`, `dist_sim`, `nboot`, `est_method`, `ci_method`, …) in a
fixed lexicographic order:

```
   (dataset, sim, nrow, dist_sim, ci_method, …)
       ↑
       outermost = adding a new dataset only appends tasks at the end,
                   so existing tasks keep their sub-stream assignments
                   and stay cached.
```

This makes "extend by adding a dataset" the cheap extension path:

```
   parent: generator = list(boron, cadmium)
                       N_parent tasks consume sub-streams 1 … 3·N_parent
   child:  generator = list(boron, cadmium, chloride)  ← appended
           parent's tasks unchanged (same sub-streams)
           child's new tasks consume sub-streams 3·N_parent + 1 …
                                                3·N_child
```

Renaming or reordering existing datasets is *not* cache-preserving —
the canonical order shifts and every downstream sub-stream
re-assigns. Names are part of the scenario's content hash.

---

## 2. Pre-generated RNG states: quadratic → linear

The current per-row helpers (`fit_dists_seed()`, `hc_seed()`) call
`get_lecuyer_cmrg_stream_state(seed, stream, start_sim = k)`, which
advances `k − 1` sub-streams from the master state on every call.
Across all rows in a stage this is **O(nsim²)**; across the three
stages, **O(3 · nsim²)**.

The sub-stream lattice is computed **once, on demand**. It does not
matter whether that happens at scenario construction, at the first
call to `ssd_scenario_tasks(scenario)`, or inside the `tasks` target
of a `targets` pipeline — what matters is that all consumers see the
same answer and pay the cost only once. The natural seam is
`ssd_scenario_tasks(scenario)`: it returns the full task grid with
each task's three sub-stream states already attached. Each task gets
**three independent sub-streams**, one for each step (data, fit, hc);
two tasks never share a sub-stream:

```
   master_state          (length-7 L'Ecuyer state)
      │
      ▼  one sub-stream advance per cell, left-to-right, top-to-bottom
   ┌──────────────────────────────────────────────────────────────────┐
   │ task[1]:  .state_data ─sub─▶ .state_fit ─sub─▶ .state_hc         │
   │ task[2]:  .state_data ─sub─▶ .state_fit ─sub─▶ .state_hc         │
   │ task[3]:  .state_data ─sub─▶ .state_fit ─sub─▶ .state_hc         │
   │   ⋮                                                              │
   │ task[N]:  .state_data ─sub─▶ .state_fit ─sub─▶ .state_hc         │
   └────────────────────────────────────┬─────────────────────────────┘
                                        │  next sub-stream after task[N].hc
                                        ▼
                              end_state  ← persisted on the scenario's
                                            output manifest (§7);
                                            child's master_state = end_state.
```

Cost: 3·N sub-stream advances total when the lattice is first
computed, **O(N)**, where N = `nrow(ssd_scenario_tasks(scenario))`.
Each task row carries its three states as length-7 integer columns.
Per-task work at run time is **O(1)**: the worker reads `.state_*`
off its row and enters `local_lecuyer_cmrg_state(.state_*)`.

Task ordering must be canonical (same scenario → same row order →
same sub-stream assignment); see Open question 3 in §9.

States survive serialization — a task row sent to a Slurm worker
carries its own state — and re-running the same scenario re-derives
identical states deterministically. The restart property
(`saveRDS(state) → readRDS → nextRNGSubStream` reproduces the next
state byte-for-byte) is verified by
`scripts/experiment-substream-restart.R`.

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

`ssd_scenario()` derives and stores the `master_state`. It is purely
declarative — it does **not** expand the task grid. Expansion is
`ssd_scenario_tasks(scenario)`, called either by `ssd_run_scenario()`
(local) or by the `tasks` target in the cluster pipeline (§4).

```
   ssd_scenario(...) ──▶ ssdsims_scenario  (declarative; carries master_state)
                                │
                                ▼
                       ssd_scenario_tasks(scenario)
                                │
                                ▼
                       one row per task (carries .state_data/fit/hc)
                                │
            ┌───────────────────┼─────────────────────┐
            ▼                                         ▼
   ssd_run_scenario(scenario)              tar_target(tasks, ssd_scenario_tasks(scenario))
   sequential or in-process parallel       feeds `targets` (§4)
```

---

## 4. From local to a cluster

The scenario object is unchanged. What changes is how tasks fan out.
Three artifacts are involved:

```
   ┌─────────────────────────────────────────────────────────────────┐
   │ A. <existing pipeline targeting cluster X>                       │
   │    Published targets+crew pipeline (e.g. another lab's repo).    │
   │    Source of the _targets.R skeleton:                            │
   │      • controller construction                                   │
   │      • tar_option_set / resources                                │
   │      • results layout / merge target                             │
   └─────────────────────────────────────────────────────────────────┘
                                  │ lift skeleton, drop body
                                  ▼
   ┌─────────────────────────────────────────────────────────────────┐
   │ B. Toy pipeline for the target cluster (LLM-assisted)            │
   │    One trivial target through crew.cluster::crew_controller_slurm│
   │    (or equivalent for the target scheduler).                     │
   │    Drafted by prompting Claude Code with:                        │
   │      1. the cluster's submission docs / sbatch template          │
   │      2. the skeleton from A                                      │
   │      3. "make tar_make() submit one job and return Sys.info()"   │
   │    Iterate until one branch completes on a real submitted job.   │
   │    Goal: prove queue + scratch + module load + R version work.   │
   └─────────────────────────────────────────────────────────────────┘
                                  │ swap body for ssd_run_job(tasks, scenario)
                                  ▼
   ┌─────────────────────────────────────────────────────────────────┐
   │ C. ssdsims _targets.R for the target cluster                     │
   │    scenario  ──▶  tasks  ──▶  tar_group_by(task_id)              │
   │                                       │                          │
   │                                       ▼                          │
   │                               pattern = map(task_groups)         │
   │                                       │                          │
   │                                       ▼                          │
   │                            crew.cluster::crew_controller_slurm() │
   │                                       │                          │
   │                                       ▼                          │
   │                            one Slurm job per task group,         │
   │                            one Parquet per job                   │
   └─────────────────────────────────────────────────────────────────┘
```

Only the controller and resource specs change between clusters. Task
content, RNG, and result schema are scenario-defined.

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

### Implications for sub-stream allocation

Each step gets **its own sub-stream block**, sized to its grid:

```
   master_state
        │
        ├─ data block:  nextRNGSubStream × |data grid|
        │
        ├─ fit  block:  nextRNGSubStream × |fit  grid|
        │
        └─ hc   block:  nextRNGSubStream × |hc   grid|

   total: |data| + |fit| + |hc| sub-streams
          (200 for the second example)
```

This **replaces** the earlier "3 sub-streams per task" rule (§2): that
rule assumed one canonical task lattice, but there isn't one — there
are three. The three blocks are walked sequentially from
`master_state`; `end_state` is the sub-stream past the last block (hc),
so a child scenario picks up there regardless of which grid it grows.

### Implications for the targets pipeline

A single `task_groups` mapped lockstep through all three steps does
**not** work when the grids differ — each step needs its own task
grouping, and §6 wires this up concretely:

```
   scenario
       │
       ▼
   data_tasks   ──▶ data_jobs   pattern = map(data_groups)
       │                              │
       ▼                              ▼  per-data-row Parquet
   fit_tasks    ──▶ fit_jobs    pattern = map(fit_groups)
       │                              │  reads its data_jobs row
       ▼                              ▼  by content-addressed path
   hc_tasks     ──▶ hc_jobs     pattern = map(hc_groups)
                                      │  reads its fit_jobs row
                                      ▼
                                summary
```

The link between layers is **by content-addressed file path** (the
`<task_id>.parquet` naming from §6), not by a single shared dynamic
branch index. Each step's task row carries the task IDs of the
upstream rows it depends on (`data_id` on fit rows, `fit_id` on hc
rows), and the per-branch body opens the right upstream Parquet by
that ID. This is the only way `dists` re-running fits without
re-running data can stay correct: the fit task row's `data_id` is
unchanged.

### Implications for the stream axis

The reserved L'Ecuyer stream axis (§1, design point 2) has a natural
use here: **one stream per step family** keeps data, fit, and hc
RNG sequences ~2^127 apart by construction, eliminating any possible
overlap between the three sub-stream blocks above.

```
   master_state
      │
      ├─ nextRNGStream × 0 → data stream root → sub-streams 1..|data|
      ├─ nextRNGStream × 1 → fit  stream root → sub-streams 1..|fit|
      └─ nextRNGStream × 2 → hc   stream root → sub-streams 1..|hc|
```

This is a clean alternative to the sequential-block model: growing
one grid (e.g. adding `nboot` values) advances within hc's stream
only and never disturbs data/fit assignments. The trade-off is that
streams are then *consumed* by ssdsims rather than reserved; an
"add a new RNG-consuming step" extension would need to mint a new
stream. Open question — see §9.

---

## 6. Target graph (small example)

Concrete pipeline for a small scenario (`nsim = 4`, `nrow = c(5, 10)`,
`nboot = c(10, 50)`, two datasets). Each of the three steps fans out
according to **its own grid** (§5). For this scenario:

```
   data grid:  2 dataset · 4 sim · 2 nrow                    = 16 rows
   fit  grid:  data grid · 1 (fit-arg defaults)              = 16 rows
   hc   grid:  fit  grid · 2 nboot · 1 est_method            = 32 rows
```

Each step writes a Parquet file per branch so the data, fit, and hc
layers are independently queryable for analysis without re-running
upstream steps.

```
   scenario   (declarative; carries master_state)
       │
       ├──▶ data_tasks  (16 rows, carries .state_data, data_id)
       │         │
       │         ▼  tar_group_by(data_id), pattern = map(data_groups)
       │     data_job   ──▶ results/data/<data_id>.parquet
       │
       ├──▶ fit_tasks   (16 rows, carries .state_fit, data_id, fit_id)
       │         │
       │         ▼  tar_group_by(fit_id), pattern = map(fit_groups)
       │     fit_job    ──▶ results/fit/<fit_id>.parquet
       │                 reads results/data/<data_id>.parquet by path
       │
       └──▶ hc_tasks    (32 rows, carries .state_hc, fit_id, hc_id)
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
tweaking `dists` re-run fits without re-running data (the fit task
row's `data_id` is unchanged).

`_targets.R` sketch:

```r
list(
  tar_target(scenario,
    ssd_scenario(
      list(boron   = ssddata::ccme_boron,
           cadmium = ssddata::ccme_cadmium),
      nsim = 4L, nrow = c(5L, 10L), nboot = c(10L, 50L),
      seed = 42)),

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

**Dependencies and what re-runs on a knob change:**

| Knob change             | data_job          | fit_job        | hc_job         | summary |
| ----------------------- | ----------------- | -------------- | -------------- | ------- |
| dataset appended (§1.1) | new branches only | new only       | new only       | re-run  |
| `nrow` value added      | new branches only | new only       | new only       | re-run  |
| `nsim` grows            | new branches only | new only       | new only       | re-run  |
| `dists`                 | cached            | re-run all     | re-run all     | re-run  |
| `nboot` added           | cached            | cached         | new only       | re-run  |
| `est_method` added      | cached            | cached         | new only       | re-run  |
| `ci_method` / `parametric` added | cached   | cached         | new only       | re-run  |
| `seed`                  | re-run all        | re-run all     | re-run all     | re-run  |
| dataset *renamed*       | re-run all        | re-run all     | re-run all     | re-run  |

Three steps as three targets is what makes this matrix possible: the
existing per-task design (data + fit + hc in one branch) cannot cache
a fit when only `nboot` changes.

**Available for analysis:**

After `tar_make()`, the three step layers are queryable independently
via duckplyr without going through `targets`:

```r
duckplyr::df_from_parquet("results/data/*.parquet") |>
  dplyr::filter(nrow == 10L) |> dplyr::collect()
duckplyr::df_from_parquet("results/fit/*.parquet")  |> ...
duckplyr::df_from_parquet("results/hc/*.parquet")   |> ...
```

A child scenario (§7) only needs `results/hc/` *plus* the parent's
`end_state` to extend; it does not need to re-read fit or data.

---

## 7. Extension: scenario → scenario

Extension is always on the sub-stream axis. The parent's run emits an
**end_state**: the L'Ecuyer-CMRG sub-stream that comes immediately
after the parent consumed its last task's `.state_hc`. The child uses
this as its `master_state`. Two extension modes share the same
mechanism:

- **Append a dataset** (the principal mode, §1.1). The child's
  generator list extends the parent's; the child's task grid
  contains only the new dataset's tasks.
- **Grow `nsim`.** The child reuses the parent's generator but
  declares additional sims.

Both produce the same picture:

```
   parent run                                child run
   ──────────                                ─────────
   master_state = M                          master_state = end_state(parent)
   ┌──────────────────────┐                  ┌──────────────────────────┐
   │ task[1]: data/fit/hc │                  │ task'[1]: data/fit/hc    │
   │ task[2]: data/fit/hc │                  │ task'[2]: data/fit/hc    │
   │   ⋮                  │                  │   ⋮                      │
   │ task[N]: data/fit/hc │                  │ task'[N']: data/fit/hc   │
   └──────────┬───────────┘                  └──────────┬───────────────┘
              │ end_state                               │ end_state
              ▼                                         ▼
   persisted on parent's manifest          persisted; available for grandchild
              │                                         │
              └──────────────────┬──────────────────────┘
                                 ▼
                         child inherits, no re-derivation from seed
```

The child does **not** re-derive from the original seed; it inherits
`end_state` from the parent's persisted manifest. The child's task
grid contains only the **new** tasks.

```
   tar_make() in child project
        │
        ▼
   read parent manifest  ──▶  end_state(parent)
        │
        ▼
   build child scenario:  master_state = end_state(parent)
        │
        ▼
   child task grid       ──▶  child Parquet (parent files untouched)
        │
        ▼
   merge target reads BOTH child's and parent's Parquet (duckplyr)
```

`parent` may be:

- An `ssdsims_scenario` in memory.
- A previous project's `_targets/` store
  (`tar_read(scenario, store = "../parent/_targets")`).
- A Parquet results directory plus a sidecar manifest (language-
  agnostic).

The merge sits *downstream of all child targets*; the child writes
only what is new.

Restartability of sub-stream enumeration from a persisted length-7
integer state is verified by
`scripts/experiment-substream-restart.R` (saveRDS round-trip, restart
concatenation, no-mutation under use, draw-level equivalence — all
PASS on R 4.5).

---

## 8. Gaps from `RNG-FLOW.md` §5 — how this design closes them

| Gap                                              | Resolution                                                                                  |
| ------------------------------------------------ | ------------------------------------------------------------------------------------------- |
| No `ssd_extend_scenario()`                       | §7 — `parent` field + `ssd_extend_scenario(parent, ...)` constructor.                       |
| No "load previous run from Parquet" path         | §7 — `parent` may be a results dir; manifest stores `end_state` (length-7 integer).         |
| No DAG-of-DAGs primitive                         | §7 — child reads parent via `tar_read(..., store = "../parent/_targets")`.                  |
| Persists only `seed`, not master L'Ecuyer state  | §1, §7 — scenario stores `master_state`; `seed` is a constructor convenience.               |
| Positional task IDs                              | §2 — task IDs are content-addressed: hash of the canonical task row.                        |
| Re-derivation cost is quadratic                  | §2 — 3·N sub-stream advances at grid materialization; per-task work O(1).                   |
| `nsim`-grow cache invalidation                   | §1, §2 — scenario is declarative; downstream targets depend on individual task rows.        |
| `stream` axis conflated with extension axis      | §1, §7 — extension lives on sub-streams only; the stream axis is reserved.                  |
| Three steps cached as one (no per-step re-runs)  | §5, §6 — data/fit/hc are three grids and three targets, each with its own Parquet layer.    |
| Same lattice for all steps despite grid mismatch | §5 — each step has its own grid; the lattice has three blocks rather than one shared one.   |
| data/fit/hc states collided in PoC               | §2 — strided enumeration: each task takes three consecutive sub-streams, no overlap.        |
| Single-dataset scenarios only                    | §1.1 — generator is a named list of data.frames; datasets are the outermost cross-join.     |

The RNGkind side-effect bug and the independent data/fit/hc substreams
are assumed already merged from the PoC and are not re-derived here.

---

## 9. Open questions for review

1. **Sub-streams sequential vs. one-stream-per-step.** §5 sketches
   both: (a) three sub-stream blocks walked sequentially from
   `master_state`, vs. (b) one L'Ecuyer stream per step (data, fit,
   hc) with sub-streams within each. (a) preserves the "stream axis
   reserved" decision but couples block sizes (growing data shifts
   fit and hc). (b) makes the three step families ~2^127 apart by
   construction but consumes three streams. Which is the contract?
2. **Manifest format.** Cross-project parent linking needs a manifest
   that stores at minimum `end_state` (length-7 integer) and the
   parent's task-grid digest. Sidecar JSON next to Parquet, or
   `tar_read()` against the parent's `_targets/` store? The latter is
   idiomatic but couples the child to the parent's `targets` version.
3. **Loss of per-sim comparability.** Allocating one sub-stream per
   (task, step) means two tasks with the same `sim` but different
   `nrow` no longer share a data state — the current package keys
   `.Random.seed` on `(stream, sim)` so different `nrow` values
   within one sim see the same initial state. Is that comparability
   wanted, or is full per-task independence the new contract?
4. **Canonical task ordering.** Sub-stream assignment depends on row
   order in `ssd_scenario_data_tasks`/`fit_tasks`/`hc_tasks`. The
   proposed order is `(dataset, sim, nrow, …)` for data and so on,
   with `dataset` outermost so appended datasets keep existing cache.
   Should this ordering be documented as part of the public contract,
   or only the *content* (hash of the unordered task set) be stable?
5. **Dataset identity.** Datasets are keyed by **name**, not by
   `digest::digest(df)`, so two scenarios that point at semantically
   identical but separately-loaded data.frames under the same name
   collide. Should the name + a content hash both participate in the
   task ID, or only the name?
6. **Child cache stability.** Re-running the child unchanged should be
   a full cache hit, but the child also reads the parent's Parquet —
   does `targets` track those files as dependencies, and is their
   hash stable across hosts (mtime, path)?
7. **Toy pipeline shape.** Ship a single
   `inst/targets-templates/cluster/` that the LLM-authoring prompt
   edits, or only documentation pointing at `crew.cluster` examples?
