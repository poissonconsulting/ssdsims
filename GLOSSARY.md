# ssdsims Glossary

Terminology used throughout `ssdsims`.

## RNG terms

- **seed**: A scalar integer passed to `base::set.seed()` (or the
  `seed` argument of `dqrng::dqset.seed()`). Both base R and dqrng
  accept a single integer as the seed.
- **state**: The full internal state of an RNG. For L'Ecuyer-CMRG,
  the state is a length-7 integer vector assignable to
  `.Random.seed` (it cannot be passed to `set.seed()`). For dqrng,
  the state is opaque to user code but accessible via
  `dqrng::dqrng_get_state()` / `dqrng_set_state()`. ssdsims function
  names ending in `_state` (e.g. `with_lecuyer_cmrg_state`,
  `slice_sample_state`, `fit_dists_state`, `hc_state`) take a
  `state` argument that, in the new design, holds a **primer**
  (see below) — the function installs it as the running RNG state
  before executing its body.
- **stream**: An independent sequence of pseudo-random numbers
  within an RNG family. For L'Ecuyer-CMRG, streams are advanced via
  `parallel::nextRNGStream()` (~2^127 jump per stream); the L'Ecuyer
  stream selector IS a state vector. For dqrng, `stream` is a
  separate argument to `dqset.seed()` (independent of `seed`); same
  `seed` and different `stream` give statistically independent
  sequences.
- **sub-stream**: A finer subdivision within an L'Ecuyer-CMRG
  stream, advanced via `parallel::nextRNGSubStream()`. ssdsims's
  current package convention assigns one sub-stream per simulation
  index (`sim`). The dqrng-based design (TARGETS-DESIGN.md §2) does
  not use sub-streams; each task gets its own dqrng stream selected
  by its **primer**.
- **primer**: The value that, *together with* `seed`, fully
  initializes an RNG instance to a known starting point — i.e.
  picks which independent sequence to consume. Concrete type
  depends on the RNG family:
    * **dqrng PCG64**: a 64-bit integer packed as a length-2
      integer vector (hi32, lo32). The primer is the value passed
      to the `stream` argument of `dqrng::dqset.seed()`.
    * **L'Ecuyer-CMRG**: a length-7 integer state vector
      assignable to `.Random.seed`.
  In TARGETS-DESIGN.md, the per-task primer is the 64-bit
  `rlang::hash()` of the task's parameters via `task_primer(p)`
  (§2). The `state =` argument of the `_state` functions *is* the
  primer for that task. (A historical misnomer, to be fixed in a future iteration:
  `primer = ` argument of the `_primer` functions.)

## Pipeline terms

- **task**: One row of a step's task table (`data_tasks` /
  `fit_tasks` / `hc_tasks`). A task specifies the smallest unit
  of computation in the pipeline: it carries the per-task
  **primer** (§2), all parameter values for the cross-join axes
  at that step, and the upstream partition path it depends on.
  Many tasks bundle into one **shard** (below) when they share
  the step's `partition_by` column values.
- **partition**: A Hive directory level keyed by an axis value
  (e.g. `dataset=boron/sim=1/`). The Hive-partitioned layout is
  a *read-side* concept — query engines (duckplyr / DuckDB)
  inspect the directory names and skip files whose path doesn't
  match a filter (predicate pushdown). The scenario's
  `partition_by[[step]]` picks which task-table columns become
  partition levels for that step (§5).
- **shard**: One Parquet file on disk — a *write-side* concept.
  A shard is the unit of dispatch (one branch produces one
  shard), of atomic rewrite (one Parquet rewritten when the task
  set changes, §8), and of caching (file existence at the
  partition path = cache hit). One shard contains 1+ task
  results, one row per task, with `task_id` as a column. In our
  design **one shard ≡ one partition leaf** (no `part-N` style
  splitting); the leaf file is always named `part.parquet`.
- **step**: One of the three RNG-touching stages of the pipeline:
  **data** (`slice_sample_state()`), **fit** (`fit_dists_state()`),
  **hc** (`hc_state`). Each step has its own task table
  (`data_tasks` / `fit_tasks` / `hc_tasks`), its own grid, its
  own `partition_by` axes, its own dynamic-branched target
  (`data_step` / `fit_step` / `hc_step`), and its own shard
  directory. The word "step" is reserved for these three stages.
- **target**: A `targets::tar_target()` declaration in the
  `_targets.R` script. A *static* target produces one object; a
  *dynamic-branched* target with `pattern = map(grouped_tbl)`
  produces one **branch** per group of the upstream grouped
  table.
- **branch**: One sub-target of a dynamic-branched target —
  produced when `pattern = map(...)` is iterated over a grouped
  task table. **1 branch = 1 group = 1 shard out**; the branch
  body loops over the K tasks in its group, primes the RNG once
  per task, and writes one Parquet. **Independent branches run
  in parallel** — that is the unit of parallelism the design
  commits to. How branches are packed into Slurm jobs under
  `crew_controller_slurm()` is an open question
  (TARGETS-DESIGN.md §11).
- **job**: Reserved exclusively for the cluster-scheduler term —
  a Slurm (or equivalent) work unit dispatched by a `crew`
  controller. A job may host one branch or several; the precise
  branch ↔ job mapping under `crew_controller_slurm()` is an
  open question (TARGETS-DESIGN.md §11). What is fixed is that
  **independent shards run in parallel** — shard granularity is
  the unit of parallelism, regardless of how branches are
  packed into jobs. Branches and shards exist with or without a
  scheduler; jobs only exist on a cluster.

## Simulation terms

- **`sim`**: The index of a simulation replicate.
- **`nsim`**: The number of simulation replicates to perform.
- **`nrow`**: The number of rows (species) in each simulated dataset.
- **`replace`**: Whether the resampling that generates simulated data is
  performed with replacement.

## SSD terms

- **SSD**: Species sensitivity distribution: a distribution of species-level
  toxicity endpoints used in ecological risk assessment.
- **`dists`**: The parametric distributions fit to the SSD data (e.g.
  `lnorm`, `gamma`, `llogis`); see `ssdtools::ssd_fit_dists()`.
- **`fits`**: A `fitdists` object holding one or more fitted distributions.
- **`hc`**: Hazard concentration: a quantile of the SSD at a given
  `proportion` (e.g. `hc5` is the 5% quantile); see `ssdtools::ssd_hc()`.
- **`proportion`**: The proportion of species affected at which the hazard
  concentration is computed.
- **`ci`**: Whether to compute confidence intervals on hazard
  concentrations.
- **`ci_method`**: The method used to compute confidence intervals (e.g.
  `multi_fixed`, `weighted_samples`).
- **`nboot`**: The number of bootstrap replicates used when computing
  confidence intervals.
