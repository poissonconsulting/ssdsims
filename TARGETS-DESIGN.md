# Targets design — `ssdsims`

End-to-end workflow for running an `ssdsims` scenario on a cluster.
Three primary goals, each a hard constraint:

- **Reproducibility.** A scenario's results are bit-stable across
  reruns and machines (RNG state is explicit and serializable; §1, §2).
- **Debuggability.** Any single failed branch on the cluster must be
  replayable locally — with no `targets`, no orchestrator — using
  the same inputs the cluster used. This is what the state-only
  primitives (`slice_sample_state()`, `fit_dists_state()`,
  `hc_state()`) and the per-Parquet content-addressed manifest are
  *for* (§7).
- **Extensibility.** A scenario can declare a desired task grid that
  references a parent; the child runs only the difference between
  its desired grid and the parent's completed grid (§8).

Parallelism is assumed throughout. The document is about the
**scenario object**, **how a scenario reaches a cluster**, **how a
failed branch is debugged locally**, and **how a scenario extends a
previous one**.

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
├── root_state   ← length-7 L'Ecuyer-CMRG integer vector;
│                    the sub-stream root this scenario advances from
│                    (NOT a scalar seed — see §2)
├── nsim           ← number of replicate sims per dataset
├── generator      ← named list of data.frames (§1.1)  | function | fitdists
├── fit            ← list of ssd_fit_dists() argument vectors
├── hc             ← list of ssd_hc() argument vectors
├── upload         ← NULL (no upload) or list(backend, url, container, …) (§6.1)
└── parent         ← NULL, or a previous ssdsims_scenario / results path
                     this scenario extends (§8)
```

Four design points distinguish this from the current code:

1. **`root_state`, not `seed`.** The scenario stores the
   *post-`set.seed` post-`RNGkind` L'Ecuyer-CMRG state*, not the integer
   seed. Two scenarios with the same `root_state` produce identical
   task grids regardless of whether the seed→state transformation in
   `get_lecuyer_cmrg_*()` is later refactored. `ssd_scenario(seed = 42)`
   is a convenience constructor that derives and stores the state.
2. **Sub-streams only; the stream axis is reserved.** Every scenario
   (and every child) advances via `parallel::nextRNGSubStream()` from
   its `root_state`. `parallel::nextRNGStream()` is not invoked by
   any scenario primitive; the stream axis is kept available for
   future applications (e.g. statistically independent batch axes
   layered on top of scenarios).
3. **Named list of data.frames as the principal generator.** The data
   generator is a **named list** of data.frames; a bare data.frame is
   silently lifted to a length-1 list (named `"data"` by default).
   Each dataset is its own cross-join axis, so a scenario with three
   datasets and `nsim = 100` materializes `3 × 100 × |nrow| × …`
   tasks. The `function` and `fitdists` generators remain supported
   but are secondary; the named-list-of-data.frames path is the one
   §6 and §8 exercise.
4. **`parent`.** A scenario can point at an upstream scenario it
   extends. Extension always advances on the sub-stream axis (§8):

```
   scenario_v1                       scenario_v2 (parent = v1)
   root_state = M                  root_state = M_next
   nsim = 100                        nsim = 100
        │                                  │
        └── extend ────────────────────────┘
            where M_next is the first sub-stream past
            v1's last consumed sub-stream (§8).
```

### 1.1 Named-list-of-data.frames generator

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

Renaming or reordering existing datasets is *not* cache-preserving
**at the scenario level** — the canonical order shifts and every
downstream sub-stream re-assigns. Names are part of the scenario's
content hash. The dag-of-dags extension primitive (§8) recovers
cache hits across a rename or reorder by carrying an explicit
`parent_alias` from new dataset names to parent dataset names; the
parent's Parquets are then re-attributed (not re-computed) under
the new labels.

---

## 2. Pre-generated RNG states: quadratic → linear

The current per-row helpers (`fit_dists_seed()`, `hc_seed()`) call
`get_lecuyer_cmrg_stream_state(seed, stream, start_sim = k)`, which
advances `k − 1` sub-streams from the root state on every call.
Across all rows in a stage this is **O(nsim²)**.

The sub-stream lattice is computed **once, on demand**. It does not
matter whether that happens at scenario construction, at the first
call to `ssd_scenario_tasks(scenario)`, or inside the `tasks` target
of a `targets` pipeline — what matters is that all consumers see the
same answer and pay the cost only once. The natural seam is
`ssd_scenario_tasks(scenario)`: it returns the three task tables
(one per step, §5) with one sub-stream state already attached to
each row. **Each stochastic call gets its own sub-stream**, so the
total count is `|data grid| + |fit grid| + |hc grid|`, not three
per task (§5 explains why a single "task lattice" doesn't exist —
data, fit and hc have different grids).

```
   root_state          (length-7 L'Ecuyer state)
       │
       ▼  one sub-stream advance per row, by canonical task order
   ┌──────────────────────────────────────────────────────────────────┐
   │  data grid:  data[1] ─sub─▶ data[2] ─sub─▶ … ─sub─▶ data[|D|]    │
   │  fit  grid:  fit[1]  ─sub─▶ fit[2]  ─sub─▶ … ─sub─▶ fit[|F|]     │
   │  hc   grid:  hc[1]   ─sub─▶ hc[2]   ─sub─▶ … ─sub─▶ hc[|H|]      │
   └──────────────────────────────────────┬───────────────────────────┘
                                          │ next sub-stream after hc[|H|]
                                          ▼
                              end_state  ──▶  persisted on the scenario's
                                              output manifest (§8);
                                              child's root_state = end_state.
```

For the 16-fan-out example in
`scripts/example-expanded-grids-independent.R`: `|D| = 4`, `|F| = 8`,
`|H| = 16`, total **28** sub-streams in a single
`get_lecuyer_cmrg_stream_states(nsim = 28L, ...)` call.

Cost: `|data| + |fit| + |hc|` sub-stream advances at first lattice
computation, **O(N)** where N is that sum. Each row of the
respective task table carries its single length-7 state column.
Per-task work at run time is **O(1)**: the worker reads `.state` off
its row and enters `local_lecuyer_cmrg_state(.state)`.

Task ordering within each grid must be canonical (same scenario →
same row order → same sub-stream assignment); see Open question 3
in §11.

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

`ssd_scenario()` derives and stores the `root_state`. It is purely
declarative — it does **not** expand the task grid. Expansion is
`ssd_scenario_tasks(scenario)`, called either by `ssd_run_scenario()`
(local) or by the `tasks` target in the cluster pipeline (§4).

```
   ssd_scenario(...) ──▶ ssdsims_scenario   (declarative; carries root_state)
                              │
                              ▼
                     ssd_scenario_tasks(scenario)
                              │
                              ▼
                     three task tables (data_tasks, fit_tasks, hc_tasks),
                     each row carrying one length-7 .state column
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
  `root_state`, generator, fit/hc argument vectors, optional
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

### Sub-stream allocation: current vs. aspirational

The package allocates one L'Ecuyer-CMRG sub-stream per `(stream,
sim)` tuple and **reuses it across all tasks for that pair** —
across data/fit/hc stages, and across the fit-grid and hc-grid
cross-join axes (`nrow`, `rescale`, `nboot`, `est_method`, …).
Multiple datasets are handled by running separate scenarios with
distinct `stream` values, so the existing convention is **one
(sub-)stream per dataset, reused across tasks within that
dataset**:

```
   stream = 1            stream = 2          ← current convention:
       │                     │                  one L'Ecuyer stream
       ▼                     ▼                  per dataset
   sub-streams 1..nsim   sub-streams 1..nsim ← one sub-stream per sim
       │                     │                  within that stream
       ▼                     ▼                ← that sub-stream is REUSED
   all task cells        all task cells         across data, fit, hc,
   for that sim          for that sim           and every cross-join cell
                                                (nrow, rescale, nboot, …)
```

The **aspirational allocation** — what TARGETS-DESIGN proposes,
**to be confirmed** before adoption — gives each grid element its
own sub-stream:

```
   root_state
        │
        ├─ data block:  nextRNGSubStream × |data grid|
        │
        ├─ fit  block:  nextRNGSubStream × |fit  grid|
        │
        └─ hc   block:  nextRNGSubStream × |hc   grid|

   total: |data| + |fit| + |hc| sub-streams
          (200 for the second example, 28 for the small one in
           scripts/example-expanded-grids-independent.R)
```

The aspirational model is exercised by
`scripts/example-expanded-grids-independent.R`. Switching the
production code to it is a design decision pending review — see
Open question 2 in §11 ("Loss of per-sim comparability"): the
aspirational allocation breaks the property that two tasks with
the same `sim` but different `nrow` share a data state.

The L'Ecuyer **stream axis** stays reserved in both allocations.
`parallel::nextRNGStream()` is not invoked by any scenario
primitive of the aspirational design; the wide-jump (~2^127) axis
is kept available for future applications (e.g. statistically
independent batch axes layered on top of scenarios).

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
   scenario   (declarative; carries root_state)
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

The state-only primitives `slice_sample_state()`,
`fit_dists_state()`, `hc_state()` are the contract that makes this
work. Each takes its inputs as plain values — data, a length-7
integer state, scalar params — so the **task row plus the immediate
upstream Parquet is a complete reproducer**.

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
        - .state_hc (length-7 integer)
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
      options(error = recover)        # or place browser() in hc_state
      out <- ssdsims:::hc_state(
        data       = fit,
        state      = task$.state_hc[[1]],
        nboot      = task$nboot,
        est_method = task$est_method,
        ci_method  = task$ci_method,
        proportion = scenario$hc$proportion,
        ci         = scenario$hc$ci,
        parametric = task$parametric,
        save_to    = NULL
      )
```

The call is the same code that ran on the worker; the state is the
same integer vector; the upstream Parquet is the same bytes. A
deterministic bug reproduces on the first call.

### Helper

A single helper compresses steps 1, 2 and 4:

```r
ssd_replay_task(task_id, store = "_targets", results_dir = "results")
```

infers the step from the task table the id sits in, opens the
immediate upstream Parquet, and calls the matching `_state`
primitive with the right args. The state-only primitives are the
supported branch-replay API; the `_seed` wrappers stay as
convenience for inline scripting.

### What makes this work

```
   ┌───────────────────────────────────────────────────────────────────┐
   │  task row + upstream Parquet = complete reproducer                │
   │  ───────────────────────────────────────────────────              │
   │  state      length-7 integer; survives saveRDS / rsync / git      │
   │  upstream   one Parquet per branch; content-addressed; tooling-   │
   │             agnostic (DuckDB, Python, R)                          │
   │  params     scalars on the task row                               │
   │  primitive  `*_state()` takes (data, state, ...args); no hidden   │
   │             dependency on targets or the orchestrator             │
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

## 8. Extension: scenario → scenario

Extension is **one primitive**: the child declares its desired task
grid by reference to a parent, and runs the tasks that are missing
from the parent's successfully-completed grid. The parent's surviving
Parquets are immutable inputs to the child; nothing already on disk
is recomputed.

```
   child.tasks_to_run = child.desired_grid \ parent.completed_grid
                                                  (set difference)
```

Four common shapes are particular cases of the same primitive:

| Shape                               | `child.desired_grid`                | Missing tasks         | Sub-streams for missing tasks  |
| ----------------------------------- | ----------------------------------- | --------------------- | ------------------------------ |
| Append a dataset (§1.1, principal)  | parent ∪ {new dataset's tasks}      | new dataset's tasks   | fresh, from `parent.end_state` |
| Grow `nsim`                         | parent ∪ {extra sim rows}           | extra sim rows        | fresh, from `parent.end_state` |
| Re-run after a code fix (post-§7)   | parent's grid (unchanged)           | parent's failed IDs   | same as parent attempted       |
| Rename / reorder datasets (§1.1)    | parent's grid with renamed labels   | ∅ (all aliased)       | none — pure re-attribution     |

In all three the mechanism is identical:

```
   parent
   ┌────────────────────────────────────────────┐
   │ desired_grid:     N rows                   │
   │ completed_grid:   N-k Parquets             │
   │ manifest carries  root_state, end_state    │
   └─────────────────────┬──────────────────────┘
                         │ parent = path / scenario object
                         ▼
   child  (some N' rows of desired_grid)
       ─ if id ∈ parent.desired_grid:
             re-use parent's sub-stream
             (by canonical content-addressed allocation)
       ─ else:
             allocate from parent.end_state onward
             (preserves §2's linear cost)
                         │
                         ▼
   child runs (desired ∩ ¬completed); writes those Parquets only.
                         │
                         ▼
   summary unions BOTH result dirs via duckplyr.
```

```
   tar_make() in child project
        │
        ▼
   read parent manifest  ──▶  end_state, completed_ids, completed_hashes (§7)
        │
        ▼
   build child scenario; compute `tasks_to_run = desired \ completed_ids`
        │
        ▼
   child task table only contains `tasks_to_run`
        │
        ▼
   child Parquets land alongside parent's (parent files untouched)
        │
        ▼
   summary reads both via duckplyr
```

`parent` may be:

- An `ssdsims_scenario` in memory.
- A previous project's `_targets/` store
  (`tar_read(scenario, store = "../parent/_targets")`).
- A Parquet results directory plus a sidecar manifest (language-
  agnostic).
- A cloud URL (§6.1) plus a manifest blob — no local copy needed if
  duckplyr can read Parquet over HTTP.

The merge sits *downstream of all child targets*; the child writes
only what is missing.

**Why "re-run after a code fix" is not a separate primitive.** When
the parent's `desired_grid` equals the child's `desired_grid` (the
user isn't adding tasks, just filling gaps), `desired \ completed`
is the set of failed IDs. The same set-difference machinery —
driven by `completed_ids` on the manifest — selects which tasks to
run. The state-only primitives (§7) are pure in `(data, state,
params)`, so re-running a failed ID under patched code with the
same sub-stream gives the deliberate fix-only delta. If the fix
changes behaviour *only* inside the previously-failing case, the
surviving N−k Parquets would have been byte-identical under the new
code; if it changes behaviour everywhere, the union is intentionally
heterogeneous and the user owns that.

**Renaming / reordering datasets via `parent_alias`.** §1.1 noted
that renaming a dataset (`boron → b`) shifts the scenario's content
hash and would invalidate every Parquet — *at the scenario level*.
The dag-of-dags primitive recovers full cache via an explicit
mapping carried on the child:

```r
ssd_scenario(
  list(b = ccme_boron, cd = ccme_cadmium),
  parent       = "../parent",
  parent_alias = c(b = "boron", cd = "cadmium"),
  …
)
```

`parent_alias` is applied *before* the set-difference: a child task
identifier `<dataset = "b", sim, nrow, …>` is treated as completed
iff the parent has a completed `<dataset = "boron", sim, nrow, …>`.
The child's `summary` target unions the two result directories with
the alias mapping; the cluster does **not** re-run the renamed
tasks. Sub-streams: none allocated — the operation is pure
re-attribution of existing Parquets under new labels. The same
machinery handles reordering (the alias is the identity but
participates in the canonical-order lookup) and column-name
adjustments that don't change underlying data.

**Manifest requirement.** The parent's manifest carries:
- `root_state` and `end_state` (length-7 integers, §2).
- `desired_grid_digest` (hash of parent's full intended task grid).
- `completed_ids`: which IDs have Parquets on disk and are trusted.
- `completed_hashes`: per-id sha256 of the local Parquet at write time (§7).
- `dataset_names`: parent's dataset labels, so a child's
  `parent_alias` can be validated against them.

Without `completed_ids`, the child cannot distinguish "absent because
the run hasn't reached this task yet" from "absent because the
branch errored". Without `completed_hashes`, §7's lightweight
reproduction can't verify inputs.

Restartability of sub-stream enumeration from a persisted length-7
integer state is verified by
`scripts/experiment-substream-restart.R` (saveRDS round-trip, restart
concatenation, no-mutation under use, draw-level equivalence — all
PASS on R 4.5).

---

## 9. Limitations

Constraints the design lives with rather than solves.

### `dists` is not a fit-grid axis

`dists` controls *which* distributions `ssdtools::ssd_fit_dists()`
fits to a given data slice, but the iteration over the elements of
`dists` is buried inside ssdtools and is not exposed at the ssdsims
level. Making `dists` a fit-grid axis (so adding a distribution
causes only the new sub-fits to re-run while existing ones stay
cached) would require either wrapping each single-distribution fit
in ssdsims and aggregating the results back into a `fitdists`
object, or a change to ssdtools to expose its per-distribution loop.
Under the current contract `dists` is a scenario-scalar (a single
character vector) — adding a distribution invalidates every fit
branch.

### `ssdtools` RNG flow is opaque

The internal RNG consumption of `ssdtools::ssd_fit_dists()` and
`ssdtools::ssd_hc()` is treated as a black box. The state-only
primitives (§7) install a known sub-stream before each call, but
how many uniforms the ssdtools call draws, and in what order, is an
ssdtools implementation detail. A breaking change to that order in
a future ssdtools release would silently change bit-stable results
even when the scenario `root_state` is unchanged. The mitigation is
to pin `ssdtools` versions in the scenario manifest.

---

## 10. Gaps from `RNG-FLOW.md` §5 — how this design closes them

| Gap                                              | Resolution                                                                                  |
| ------------------------------------------------ | ------------------------------------------------------------------------------------------- |
| No `ssd_extend_scenario()`                       | §8 — `parent` field + `ssd_extend_scenario(parent, ...)` constructor.                       |
| No "load previous run from Parquet" path         | §8 — `parent` may be a results dir; manifest stores `end_state` (length-7 integer).         |
| No DAG-of-DAGs primitive                         | §8 — child reads parent via `tar_read(..., store = "../parent/_targets")`.                  |
| Persists only `seed`, not root L'Ecuyer state    | §1, §8 — scenario stores `root_state`; `seed` is a constructor convenience.               |
| Positional task IDs                              | §2 — task IDs are content-addressed: hash of the canonical task row.                        |
| Re-derivation cost is quadratic                  | §2 — `|data|+|fit|+|hc|` sub-stream advances at grid materialization; per-task work O(1).   |
| `nsim`-grow cache invalidation                   | §1, §2 — scenario is declarative; downstream targets depend on individual task rows.        |
| `stream` axis conflated with extension axis      | §1, §8 — extension lives on sub-streams only; the stream axis is reserved.                  |
| Three steps cached as one (no per-step re-runs)  | §5, §6 — data/fit/hc are three grids and three targets, each with its own Parquet layer.    |
| Same lattice for all steps despite grid mismatch | §5 — each step has its own grid; the lattice has three blocks rather than one shared one.   |
| data/fit/hc states collided in PoC               | §2 — strided enumeration: each task takes three consecutive sub-streams, no overlap.        |
| Single-dataset scenarios only                    | §1.1 — generator is a named list of data.frames; datasets are the outermost cross-join.     |
| Branch failure unreproducible off the cluster    | §7 — task row + immediate upstream Parquet replays the failing branch via the `_state` primitives. |
| Code fix re-runs every branch by hash invalidation | §8 — re-run-after-fix is the same `desired \ completed` primitive as scenario extension; child runs only the missing hc_ids. |
| Off-cluster access to Parquet outputs            | §6.1 — `scenario$upload` pushes each Parquet to a configurable object store (e.g. Azure Blob) right after the local write. |
| Phantom local repros (regenerated upstream ≠ cluster's actual) | §7 — manifest's `completed_hashes` lets the lightweight recipe verify the local upstream by sha256 before running the failing step. |

The RNGkind side-effect bug and the independent data/fit/hc substreams
are assumed already merged from the PoC and are not re-derived here.

---

## 11. Open questions for review

1. **Manifest format.** Cross-project parent linking needs a manifest
   that stores at minimum `end_state` (length-7 integer) and the
   parent's task-grid digest. Sidecar JSON next to Parquet, or
   `tar_read()` against the parent's `_targets/` store? The latter is
   idiomatic but couples the child to the parent's `targets` version.
2. **Loss of per-sim comparability.** The aspirational sub-stream
   allocation (§5, one sub-stream per grid element) means two tasks
   with the same `sim` but different `nrow` no longer share a data
   state. The current allocation reuses one sub-stream per `(stream,
   sim)` across all cells, so different `nrow` values within one sim
   see the same initial state. Adopting the aspirational allocation
   loses that comparability; the design proposal hasn't decided
   whether to.
3. **Canonical task ordering.** Sub-stream assignment depends on row
   order in `ssd_scenario_data_tasks`/`fit_tasks`/`hc_tasks`. The
   proposed order is `(dataset, sim, nrow, …)` for data and so on,
   with `dataset` outermost so appended datasets keep existing cache.
   Should this ordering be documented as part of the public contract,
   or only the *content* (hash of the unordered task set) be stable?
4. **Dataset identity.** Datasets are keyed by **name**, not by
   `digest::digest(df)`, so two scenarios that point at semantically
   identical but separately-loaded data.frames under the same name
   collide. Should the name + a content hash both participate in the
   task ID, or only the name?
5. **Child cache stability.** Re-running the child unchanged should be
   a full cache hit, but the child also reads the parent's Parquet —
   does `targets` track those files as dependencies, and is their
   hash stable across hosts (mtime, path)?
6. **Toy pipeline shape.** Ship a single
   `inst/targets-templates/cluster/` that the LLM-authoring prompt
   edits, or only documentation pointing at `crew.cluster` examples?
