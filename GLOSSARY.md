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
  not use sub-streams; each shard gets its own dqrng stream selected
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
  In TARGETS-DESIGN.md, the per-shard primer is the 64-bit
  `rlang::hash()` of the shard's parameters via `shard_primer(p)`
  (§2). The `state =` argument of the `_state` functions *is* the
  primer for that shard.

## Pipeline terms

- **shard**: One row of a step's shard table → one Parquet file
  on output → one branch of the corresponding dynamic-branched
  target. Each shard has its own primer (§2), its own
  Hive-partitioned Parquet path under `results/<step>/`, and is
  the unit of fan-out, caching, and replay. The 1-shard =
  1-Parquet rule is the design's central invariant. The three
  steps (data, fit, hc) each emit their own shards; shard counts
  differ across steps because the grids differ (§5).
- **step**: One of the three RNG-touching stages of the pipeline:
  **data** (`slice_sample_state`), **fit** (`fit_dists_state`),
  **hc** (`hc_state`). Each step has its own shard table
  (`data_shards` / `fit_shards` / `hc_shards`), its own grid, and
  its own dynamic-branched target (`data_step` / `fit_step` /
  `hc_step`). The word "step" is reserved for these three stages
  and is **not** used for shards, targets, or Slurm jobs.
- **target**: A `targets::tar_target()` definition in the
  `_targets.R` script. A *static* target produces one object; a
  *dynamic-branched* target with `pattern = map(...)` produces
  one branch per upstream row, where each branch ≡ one shard.
  "Target" is a targets-package term and is unrelated to the
  Slurm/cluster word "job".
- **job**: Reserved exclusively for the cluster-scheduler term —
  a Slurm (or equivalent) work unit dispatched by a `crew`
  controller. With `crew.cluster::crew_controller_slurm()` and
  `pattern = map(...)`, each branch typically becomes one Slurm
  job, so in practice one Slurm job ≈ one shard, but the two
  terms are not synonyms: shards exist regardless of scheduler;
  jobs only exist on a cluster.

## Simulation terms

- **`sim`**: The index of a simulation replicate within a `stream`.
- **`nsim`**: The number of simulation replicates to perform.
- **`start_sim`**: The starting `sim` index; sub-streams are advanced to
  this position before data is generated.
- **`stream` (argument)**: The index of the L'Ecuyer-CMRG stream to use;
  distinct `stream` values give statistically independent sequences.
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
