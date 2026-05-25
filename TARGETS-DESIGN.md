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
├── nsim           ← number of replicate sims in this scenario
├── generator      ← data.frame | function | fitdists
├── fit            ← list of ssd_fit_dists() argument vectors
├── hc             ← list of ssd_hc() argument vectors
└── parent         ← NULL, or a previous ssdsims_scenario / results path
                     this scenario extends (§5)
```

Three design points distinguish this from the current code:

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
3. **`parent`.** A scenario can point at an upstream scenario it
   extends. Extension always advances on the sub-stream axis (§5):

```
   scenario_v1                       scenario_v2 (parent = v1)
   master_state = M                  master_state = M_next
   nsim = 100                        nsim = 100
        │                                  │
        └── extend ────────────────────────┘
            where M_next is the first sub-stream past
            v1's last consumed sub-stream (§5).
```

---

## 2. Pre-generated RNG states: quadratic → linear

The current per-row helpers (`fit_dists_seed()`, `hc_seed()`) call
`get_lecuyer_cmrg_stream_state(seed, stream, start_sim = k)`, which
advances `k − 1` sub-streams from the master state on every call.
Across all rows in a stage this is **O(nsim²)**; across the three
stages, **O(3 · nsim²)**.

`ssd_scenario_tasks(scenario)` materializes the task grid **once**
and walks the sub-stream axis as a single sequential chain. Each
task gets **three independent sub-streams**, one for each step
(data, fit, hc); two tasks never share a sub-stream:

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
                                            output manifest (§5);
                                            child's master_state = end_state.
```

Cost: 3·N sub-stream advances total at materialization, **O(N)**, where
N = `nrow(ssd_scenario_tasks(scenario))`. Each task row carries its
three states as length-7 integer columns. Per-task work at run time is
**O(1)**: the worker reads `.state_*` off its row and enters
`local_lecuyer_cmrg_state(.state_*)`.

Task ordering must be canonical (same scenario → same row order →
same sub-stream assignment); see Open question 3 in §7.

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

## 5. Extension: scenario → scenario

Extension is always on the sub-stream axis. The parent's run emits an
**end_state**: the L'Ecuyer-CMRG sub-stream that comes immediately
after the parent consumed its last task's `.state_hc`. The child uses
this as its `master_state`:

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

## 6. Gaps from `RNG-FLOW.md` §5 — how this design closes them

| Gap                                              | Resolution                                                                                  |
| ------------------------------------------------ | ------------------------------------------------------------------------------------------- |
| No `ssd_extend_scenario()`                       | §5 — `parent` field + `ssd_extend_scenario(parent, ...)` constructor.                       |
| No "load previous run from Parquet" path         | §5 — `parent` may be a results dir; manifest stores `end_state` (length-7 integer).         |
| No DAG-of-DAGs primitive                         | §5 — child reads parent via `tar_read(..., store = "../parent/_targets")`.                  |
| Persists only `seed`, not master L'Ecuyer state  | §1, §5 — scenario stores `master_state`; `seed` is a constructor convenience.               |
| Positional task IDs                              | §2 — task IDs are content-addressed: hash of the canonical task row.                        |
| Re-derivation cost is quadratic                  | §2 — 3·N sub-stream advances at grid materialization; per-task work O(1).                   |
| `nsim`-grow cache invalidation                   | §1, §2 — scenario is declarative; downstream targets depend on individual task rows.        |
| `stream` axis conflated with extension axis      | §1, §5 — extension lives on sub-streams only; the stream axis is reserved.                  |
| data/fit/hc states collided in PoC               | §2 — strided enumeration: each task takes three consecutive sub-streams, no overlap.        |

The RNGkind side-effect bug and the independent data/fit/hc substreams
are assumed already merged from the PoC and are not re-derived here.

---

## 7. Open questions for review

1. **Manifest format.** Cross-project parent linking needs a manifest
   that stores at minimum `end_state` (length-7 integer) and the
   parent's task-grid digest. Sidecar JSON next to Parquet, or
   `tar_read()` against the parent's `_targets/` store? The latter is
   idiomatic but couples the child to the parent's `targets` version.
2. **Loss of per-sim comparability.** Allocating one sub-stream per
   (task, step) means two tasks with the same `sim` but different
   `nrow` no longer share a data state — the current package keys
   `.Random.seed` on `(stream, sim)` so different `nrow` values
   within one sim see the same initial state. Is that comparability
   wanted, or is full per-task independence the new contract?
3. **Canonical task ordering.** Sub-stream assignment depends on row
   order in `ssd_scenario_tasks(scenario)`. The ordering rule must be
   stable across R sessions and platforms (e.g. lexicographic over
   `(sim, nrow, dist_sim, ci_method, ...)` with explicit collation).
   Should the order be documented as part of the public contract, or
   only the *content* (hash of the unordered task set) be stable?
4. **Child cache stability.** Re-running the child unchanged should be
   a full cache hit, but the child also reads the parent's Parquet —
   does `targets` track those files as dependencies, and is their
   hash stable across hosts (mtime, path)?
5. **Toy pipeline shape.** Ship a single
   `inst/targets-templates/cluster/` that the LLM-authoring prompt
   edits, or only documentation pointing at `crew.cluster` examples?
