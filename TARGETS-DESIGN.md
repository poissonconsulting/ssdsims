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

The scenario is the single declarative input to everything downstream.
An S3 object holding:

```
ssdsims_scenario
├── master_state   ← length-7 L'Ecuyer-CMRG integer vector
│                    (NOT a scalar seed — see §2)
├── stream         ← starting stream index
├── start_sim      ← starting substream index
├── nsim           ← number of substreams in this scenario
├── generator      ← data.frame | function | fitdists
├── fit            ← list of ssd_fit_dists() argument vectors
├── hc             ← list of ssd_hc() argument vectors
└── parent         ← NULL, or a previous ssdsims_scenario / results path
                     this scenario extends (§5)
```

Two design points distinguish this from the current code:

1. **`master_state`, not `seed`.** The scenario stores the
   *post-`set.seed` post-`RNGkind` L'Ecuyer-CMRG state*, not the integer
   seed. Two scenarios with the same `master_state` produce identical
   task grids regardless of whether the seed→state transformation in
   `get_lecuyer_cmrg_*()` is later refactored. `ssd_scenario(seed = 42)`
   is a convenience constructor that derives and stores the state.
2. **`parent`.** A scenario can point at an upstream scenario it
   extends:

```
   scenario_v1 (stream = 1, nsim = 100)
        │
        └── extend ──▶ scenario_v2 (stream = 2, nsim = 100, parent = v1)
                            │
                            └── extend ──▶ scenario_v3 (...)
```

---

## 2. Pre-generated RNG states: quadratic → linear

The current per-row helpers (`fit_dists_seed()`, `hc_seed()`) call
`get_lecuyer_cmrg_stream_state(seed, stream, start_sim = k)`, which
advances `k − 1` substreams from the master state on every call.
Across all rows in a stage this is **O(nsim²)**; across the three
stages, **O(3 · nsim²)**.

The scenario builds the lattice **once**, at construction:

```
   master_state
      │
      nextRNGStream      × (stream − 1)
      │
      ▼
   stream root
      │
      nextRNGSubStream   × (start_sim − 1)
      │
      ▼ ─────────────────────────────────────────────────────────┐
   state[1] ─sub─▶ state[2] ─sub─▶ … ─sub─▶ state[nsim]          │  O(nsim) once
                                                                  │
   per row i:                                                     │
     .state_data[i] = state[i]                                    │
     .state_fit[i]  = nextRNGSubStream(.state_data[i])            │
     .state_hc[i]   = nextRNGSubStream(.state_fit[i])             ▼
                              stored as length-7 integer vectors on the task row
```

Consequences:

- Per-task work is **O(1)**: each worker reads `.state_*` off its row
  and enters `local_lecuyer_cmrg_state(.state_*)`.
- States survive serialization — a task row sent to a Slurm worker
  carries its own state.
- Extension is free: a child scenario advances from the parent's
  lattice, no re-derivation (§5).

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

`ssd_scenario()` materializes the `master_state` and the task grid
(with `.state_*` columns); it does not run anything. `ssd_run_scenario()`
consumes the task grid. The same scenario object is what the cluster
pipeline below feeds to `targets`.

```
   ssd_scenario(...) ──▶ ssdsims_scenario ──▶ ssd_scenario_tasks()
                                │                         │
                                │                         ▼
                                │                    one row per task
                                │                    (carries .state_*)
                                │                         │
                                ├──────────────────────▶  ssd_run_scenario()   (local)
                                │                         ssd_run_job(tasks)   (per-target)
                                ▼
                         feeds the same object into `targets` (§4)
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

## 5. Extension: scenario → scenario

Two extension modes:

```
   extend by stream                  extend by sim
   ────────────────                  ─────────────
   parent: stream=1, nsim=100        parent: stream=1, nsim=100
   child:  stream=2, nsim=100        child:  stream=1,
           parent = v1                       start_sim=101, nsim=100
                                             parent = v1

   → statistically independent       → continuation of same batch;
     batch (~2^127 jump)                parent's first 100 substreams
                                        are reused as-is
```

The child's `master_state` is **inherited** from the parent (not
re-derived from a possibly-changed seed→state transformation). The
child task grid contains only the **new** tasks.

```
   tar_make() in child project
        │
        ▼
   read parent manifest  ──▶  parent's master_state, last (stream, sim)
        │
        ▼
   build child scenario from parent's state
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

---

## 6. Gaps from `RNG-FLOW.md` §5 — how this design closes them

| Gap                                              | Resolution                                                                                  |
| ------------------------------------------------ | ------------------------------------------------------------------------------------------- |
| No `ssd_extend_scenario()`                       | §5 — `parent` field + `ssd_extend_scenario(parent, ...)` constructor.                       |
| No "load previous run from Parquet" path         | §5 — `parent` may be a results dir; manifest stores `master_state` + last `(stream, sim)`.  |
| No DAG-of-DAGs primitive                         | §5 — child reads parent via `tar_read(..., store = "../parent/_targets")`.                  |
| Persists only `seed`, not master L'Ecuyer state  | §1, §5 — scenario stores `master_state`; `seed` is a constructor convenience.               |
| Positional task IDs                              | §1, §3 — task IDs are content-addressed: hash of `(master_state, stream, sim, nrow, ...)`.  |
| Re-derivation cost is quadratic                  | §2 — states pre-generated once at scenario construction; per-task work O(1).                |
| `nsim`-grow cache invalidation                   | §1 — `stable_config` excludes `nsim`; downstream targets depend on tasks, not on scenario.  |

The RNGkind side-effect bug and the independent data/fit/hc substreams
are assumed already merged from the PoC and are not re-derived here.

---

## 7. Open questions for review

1. **Manifest format.** Cross-project parent linking needs a manifest.
   Sidecar JSON next to Parquet, or `tar_read()` against the parent's
   `_targets/` store? The latter is idiomatic but couples the child to
   the parent's `targets` version.
2. **Where does `master_state` live for a path-referenced parent?**
   In the manifest (canonical), in `_targets/meta` (tar-only), or both?
3. **Task ID hash inputs.** Should `dists`, `ci_method`, etc.
   participate? If yes, adding a new `ci_method` to a scenario keeps
   existing branches cached and adds new ones. If no, two scenarios
   with the same `(master_state, stream, sim, nrow)` but different
   `ci_method` collide.
4. **Child cache stability.** Re-running the child unchanged should be
   a full cache hit, but the child also reads the parent's Parquet —
   does `targets` track those files as dependencies, and is their hash
   stable across hosts (mtime, path)?
5. **Toy pipeline shape.** Ship a single
   `inst/targets-templates/cluster/` that the LLM-authoring prompt
   edits, or only documentation pointing at `crew.cluster` examples?
