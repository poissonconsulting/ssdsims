# ssdsims Glossary

Terminology used throughout `ssdsims`.

## RNG terms

- **seed**: A scalar integer passed to
  [`base::set.seed()`](https://rdrr.io/r/base/Random.html) (or the
  `seed` argument of
  [`dqrng::dqset.seed()`](https://daqana.github.io/dqrng/reference/dqrng-functions.html)).
  Both base R and dqrng accept a single integer as the seed. In
  [`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md)
  it is the scenario’s RNG root — one of the three **required
  positional** arguments (`data, nsim, seed`), **not** a grid **scenario
  axis** or a **scenario setting** (below). Its canonical call-site slot
  is third, immediately after `nsim` and before any `...` scenario
  option (e.g. `nrow`).
- **state**: The full internal state of an RNG. For dqrng, the state is
  opaque to user code but accessible via
  [`dqrng::dqrng_get_state()`](https://daqana.github.io/dqrng/reference/dqrng-functions.html)
  / `dqrng_set_state()`. The dqrng-path helpers ending in `_state`
  (e.g. [`local_dqrng_state()`](https://poissonconsulting.github.io/ssdsims/reference/local_dqrng_state.md)
  /
  [`with_dqrng_state()`](https://poissonconsulting.github.io/ssdsims/reference/local_dqrng_state.md))
  take a **primer** (see below) and install it as the running RNG state
  before executing their body.
- **stream**: An independent sequence of pseudo-random numbers within an
  RNG family. For dqrng, `stream` is a separate argument to
  `dqset.seed()` (independent of `seed`); the same `seed` and a
  different `stream` give statistically independent sequences.
- **primer**: The value that, *together with* `seed`, fully initializes
  an RNG instance to a known starting point — i.e. picks which
  independent sequence to consume. For dqrng’s PCG64 it is a 64-bit
  integer packed as a length-2 integer vector (hi32, lo32), passed to
  the `stream` argument of
  [`dqrng::dqset.seed()`](https://daqana.github.io/dqrng/reference/dqrng-functions.html).
  In TARGETS-DESIGN.md, the per-task primer is the 64-bit
  [`rlang::hash()`](https://rlang.r-lib.org/reference/hash.html) of the
  task’s parameters via `task_primer(p)` (§2). The dqrng-path
  seed-and-run wrappers carry it under the `primer =` argument of the
  `_primer` functions `sample_data_task_primer()` /
  `fit_data_task_primer()` / `hc_data_task_primer()`, which install
  `(seed, primer)` once via
  [`local_dqrng_state()`](https://poissonconsulting.github.io/ssdsims/reference/local_dqrng_state.md).

## Pipeline terms

- **task**: One row of a step’s task table (`data_tasks` / `fit_tasks` /
  `hc_tasks`). A task specifies the smallest unit of computation in the
  pipeline: it carries the per-task **primer** (§2), all parameter
  values for the cross-join axes at that step, and the upstream
  partition path it depends on. Many tasks bundle into one **shard**
  (below) when they share the step’s `partition_by` column values.

- **scenario option**: The genus term for a declarative
  [`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md)
  parameter that shapes the *results*. Every scenario option is either a
  **scenario axis** (below — it multiplies tasks) or a **scenario
  setting** (below — it is consumed within each task), and the two
  species exhaust the genus. The required inputs `data`/`nsim`/`seed`
  (and the `name`) are **not** scenario options, nor are the **layout
  arguments** `partition_by`/`bundle`/`upload` — those relocate shards
  on disk without changing any task result (the byte-identity invariant,
  §5/§8). Bare “option” never introduces the term; always qualify it
  (“scenario option”, “scenario axis”, “scenario setting”).

- **scenario axis** (cross-join axis): A scenario option a step *fans
  out* over — one task per combination of the step’s axis values. The
  `sample` axes are `(dataset, sim, replace)`; `data` adds `nrow`; `fit`
  adds the fit-grid axes (`rescale`, `computable`, `at_boundary_ok`,
  `min_pmix`, `range_shape1`, `range_shape2`); `hc` adds the hc-grid
  axes (`nboot`, `ci_method`, `parametric`) and `distset` (the
  **distribution-set** name, below). `est_method`, `proportion`, `ci`,
  and `samples` are **not** hc axes — they are *scenario settings*
  (below), consumed within each task rather than multiplying it. A task
  row carries **only** its identity — the axis columns, its
  `<step>_id`/`<parent>_id` keys, and the per-row `seed`/`primer` the
  shard path attaches; a non-axis value a runner needs (the draw size,
  the `ci` flag) lives on the scenario as a setting, never as a row
  column. `nrow` is deliberately not a `sample` axis because every
  `nrow` is a sub-truncation of one shared draw sized by the `nrow_max`
  setting (TARGETS-DESIGN.md §5), truncated inline at the (RNG-free)
  `fit` step.

- **scenario setting**: A scenario option that is **not** an axis — it
  is absent from `task_axes(step)`, so it never creates a task, enters
  the per-task **primer**, or becomes a **shard**/**partition** level.
  Its effect is realised *inside* each task: it either fans out within
  the task’s own output (`est_method`, `proportion` → one HC row per
  value), is applied uniformly to every task (`ci`, `dists`, `samples`),
  or sets the shared draw size (`nrow_max`). Where an **axis**
  multiplies the *task graph*, a scenario setting only shapes the
  *contents* of a task’s result. “Scalar” is a near-synonym but a
  misnomer for `proportion` and `est_method` (vector-valued) and for
  `dists` (a character vector) — all non-scalar yet still not axes.
  Settings attach at different **steps**: `nrow_max` is the
  **sample**-level setting (the fixed shared-draw size), `dists` is a
  **fit**-level setting (the `dists` vector handed to every fit task),
  while `est_method`, `proportion`, `ci`, and `samples` are
  **hc**-level. In the
  [`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md)
  signature the **non-`ci`-gated** settings (`nrow_max`, `dists`,
  `est_method`, `proportion` — each valid and meaningful when
  `ci = FALSE`) come before `ci`; the scenario options `ci` **gates**
  then follow it — the bootstrap axes `nboot`/`ci_method`/`parametric`
  (rejected when `ci = FALSE`) and `samples` (which only retains
  bootstrap draws).

- **partition**: A Hive directory level keyed by an axis value
  (e.g. `dataset=boron/sim=1/`). The Hive-partitioned layout is a
  *read-side* concept — query engines (duckplyr / DuckDB) inspect the
  directory names and skip files whose path doesn’t match a filter
  (predicate pushdown). The scenario’s `partition_by[[step]]` picks
  which task-table columns become partition levels for that step (§5).

- **shard**: One Parquet file on disk — a *write-side* concept. A shard
  is the unit of dispatch (one branch produces one shard), of atomic
  rewrite (one Parquet rewritten when the task set changes, §8), and of
  caching (file existence at the partition path = cache hit). One shard
  contains 1+ task results, one row per task, with `task_id` as a
  column. In our design **one shard ≡ one partition leaf** (no `part-N`
  style splitting); the leaf file is always named `part.parquet`.

  > *The on-disk results tree is a separate, configurable hierarchy from
  > `design → scenario → task` above: it is keyed by `seed=`/`layout=`
  > (and, within a shard, `partition_by`), with the design root holding
  > the combined `summary.parquet`. A scenario has no directory of its
  > own — members sharing a `(seed, layout)` blend into one subtree
  > (common random numbers) and surface only via the `scenario` identity
  > column. The two hierarchies meet at the task.*

- **sidecar**: A small auxiliary metadata file written *beside* the data
  it describes (in the same directory), rather than embedded in it —
  human-readable, diffable JSON kept separate from the bulk Parquet
  results. ssdsims writes two (TARGETS-DESIGN.md §8.5): the per-scenario
  **manifest** (`<results>/manifest.json` — the scenario’s declarative
  fields, session info, and `completed_shards`), and a per-**shard**
  sha256 record (`meta.json`) written next to each shard’s
  `part.parquet` at write time, recording that shard’s
  trusted-as-produced sha256. One writer per sidecar file, so parallel
  shard targets never race on a shared manifest; the manifest assembler
  later unions the per-shard sidecars into `completed_shards`. The shard
  sidecar is uploaded with its Parquet, so a download can be verified by
  re-hashing it against the recorded sha256.

- **path axis / inner axis**: The two halves of a step’s
  `task_axes(step)` under a given partitioning. **Path axes** become
  Hive directory levels; **inner axes** are the complement — ordinary
  Parquet columns that vary row-to-row inside a shard. Together
  `path ⊎ inner = task_axes(step)`.

- **`partition_by`**: The per-step **path axes** — the canonical
  sharding argument (`scenario$partition_by`, a `sample`/`fit`/`hc`
  named list). `partition_by[[step]]` is a subset of `task_axes(step)`
  whose values become the shard’s Hive path, one shard per cell; shard
  count is `Π |path axis|`. More path axes → finer (more, smaller)
  shards. This is the stored source of truth.

- **`bundle`**: The per-step **inner axes** — the complement of
  `partition_by` (`setdiff(task_axes(step), partition_by[[step]])`): the
  axes whose tasks are *kept together* within one shard (more bundled
  axes → coarser, larger files). An equal-status **alias** entry point
  on
  [`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md):
  **per step** a caller names what to split on (`partition_by`) **or**
  what to keep together (`bundle`) — at most one of the two per step,
  but the two may be mixed across steps (e.g. `partition_by` for
  `sample`, `bundle` for `fit`). ssdsims normalizes both into a single
  complete, stored `partition_by` (defaults fill any step named in
  neither) and recomputes `bundle` for
  [`print()`](https://rdrr.io/r/base/print.html), which shows both.

- **task identity vs shard path**: A task’s **identity** — its
  `<step>_id` primary key and `<parent>_id` foreign key — is
  `path_key()` over **all** of `task_axes(step)`: unique per task,
  `partition_by`-independent, and exactly what the per-task **primer**
  hashes and the foreign-key join uses. The **shard path** is the
  coarser `path_key()` over `partition_by[[step]]` only, shared by every
  task in a shard. They coincide only one-task-per-shard; changing
  `partition_by` (or `bundle`) moves the shard path, never the identity,
  the primer, or results.

- **step**: One of the three RNG-touching stages of the pipeline:
  **data** (`sample_data_task()`), **fit** (`fit_data_task()`), **hc**
  (`hc_data_task()`). Each step has its own task table (`data_tasks` /
  `fit_tasks` / `hc_tasks`), its own grid, its own `partition_by` axes,
  its own dynamic-branched target (`data_step` / `fit_step` /
  `hc_step`), and its own shard directory. The word “step” is reserved
  for these three stages.

- **target**: A
  [`targets::tar_target()`](https://docs.ropensci.org/targets/reference/tar_target.html)
  declaration in the `_targets.R` script. A *static* target produces one
  object; a *dynamic-branched* target with `pattern = map(grouped_tbl)`
  produces one **branch** per group of the upstream grouped table.

- **branch**: One sub-target of a dynamic-branched target — produced
  when `pattern = map(...)` is iterated over a grouped task table. **1
  branch = 1 group = 1 shard out**; the branch body loops over the K
  tasks in its group, primes the RNG once per task, and writes one
  Parquet. **Independent branches run in parallel** — that is the unit
  of parallelism the design commits to. How branches are packed into
  Slurm jobs under `crew_controller_slurm()` is an open question
  (TARGETS-DESIGN.md §11).

- **job**: Reserved exclusively for the cluster-scheduler term — a Slurm
  (or equivalent) work unit dispatched by a `crew` controller. A job may
  host one branch or several; the precise branch ↔︎ job mapping under
  `crew_controller_slurm()` is an open question (TARGETS-DESIGN.md §11).
  What is fixed is that **independent shards run in parallel** — shard
  granularity is the unit of parallelism, regardless of how branches are
  packed into jobs. Branches and shards exist with or without a
  scheduler; jobs only exist on a cluster.

## Design terms

These three nest, finest to coarsest: a **scenario** is one regular
grid, a **design** unions scenarios into one pipeline run, and a
**study** aggregates design-runs across infrastructure, time, and
software versions.

- **scenario**: A single, purely declarative `ssdsims_scenario`
  ([`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md))
  — one **regular** cross-join of the **axes** at each step (a
  rectangular sub-grid). It is the construction-time root of one
  `targets` pipeline
  ([`ssd_scenario_targets()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_targets.md)),
  carrying a `seed`, the scenario options, and the dataset *names*; it
  draws no random numbers and expands no tasks (TARGETS-DESIGN.md §1).
  Vignette prose that calls a scenario “the study” is the loose gloss
  this section tightens: a scenario is one **arm** of a study, not the
  study.
- **design**: A named set of scenarios run as **one pipeline** — one
  `targets` store, one `tar_make()`, one provenance/execution context —
  built with
  [`ssd_design()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_design.md)
  and turned into targets by
  [`ssd_design_targets()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_design_targets.md).
  Where a single scenario is one regular grid, a design unions several
  into the full, possibly **non-regular** (ragged) experimental design:
  the union of regular sub-grids is exactly how an irregular design
  region is expressed. “Design” is used here in the
  **design-of-experiments** sense (the set of conditions to run); it is
  distinct from the *software*-design sense of `TARGETS-DESIGN.md` and
  the openspec `design.md` artifact. Each scenario is a member of the
  design, addressed by its collection **name** (the `scenario=<name>`
  results level and the `<name>_` target-name prefix); names enter
  addressing only, never task identity, the **primer**, or any result
  value. The design’s results table is the combined `summary.parquet`,
  with a `scenario` identity column.
- **study**: The whole investigation a design serves — the longitudinal
  aggregate that may span **multiple design-runs** executed on different
  infrastructure (laptop / cluster), at different times, or under
  different software versions. Unlike a scenario or a design, a study is
  **not a constructible object**: its members are realised in separate
  sessions, so it is reconstructed on the **read side** by unioning
  result trees (a future `study` column over several designs’
  summaries). Reserved for that umbrella level; not built by the design
  machinery. Distinct from **experiment**, which in this repo names the
  proof-of-work `scripts/experiment-*.R`.

### Mapping to design-of-experiments terminology

The vocabulary above maps onto the design-of-experiments /
simulation-study literature (Morris, White & Crowther 2019, *Statistics
in Medicine* 38:2074–2102). The mapping is a **gloss** for readers
coming from that literature — “factor”, “level”, and “study” are not
working terms in this repo (see the `## Design terms` hierarchy above
for the terms we do use):

| ssdsims term     | DoE / Morris-White-Crowther (2019) gloss     |
|------------------|----------------------------------------------|
| scenario axis    | factor                                       |
| axis value       | level                                        |
| task             | factorial cell (the literature’s “scenario”) |
| scenario setting | held-constant condition                      |
| `nsim`           | repetitions per cell                         |

Note the terminology clash the gloss resolves: the literature’s
**“scenario”** is our **task** (one factorial cell), whereas our
**scenario** is a whole regular grid of cells. “Factor”/“level”/“study”
remain glosses only.

## Simulation terms

- **`sim`**: The index of a simulation replicate.
- **`nsim`**: The number of simulation replicates to perform.
- **`nrow`**: The number of rows (species) in each simulated dataset.
- **`nrow_max`**: The fixed size of the shared `sample` draw (default
  `1000L`) — a sample-level **scenario setting**, not an axis or a
  task-row column. The effective per-dataset draw is
  `min(nrow_max, nrow(data))` when `replace = FALSE` (the high default
  draws the full permutation) and `nrow_max` rows when `replace = TRUE`;
  every `nrow` is a [`head()`](https://rdrr.io/r/utils/head.html) prefix
  of that draw, so adding `nrow` values (within the effective draw size)
  never re-draws (TARGETS-DESIGN.md §5).
- **`replace`**: Whether the resampling that generates simulated data is
  performed with replacement (default `TRUE`; `FALSE` draws a
  permutation, capping the effective draw — and so each `nrow` — at the
  dataset size).

## SSD terms

- **SSD**: Species sensitivity distribution: a distribution of
  species-level toxicity endpoints used in ecological risk assessment.
- **`dists`**: The parametric distributions fit to the SSD data (e.g.
  `lnorm`, `gamma`, `llogis`); see
  [`ssdtools::ssd_fit_dists()`](https://bcgov.github.io/ssdtools/reference/ssd_fit_dists.html).
  Supplied as an
  [`ssd_distset()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_distset.md)
  collection of **distribution sets** (below); the fit step fits the
  **union** of every set’s members (`scenario$fit$dists`) — *one*
  model-averaged fit applied uniformly to every fit task, a fit-level
  **scenario setting** (above), **not** a cross-join **axis**: it is
  absent from `task_axes("fit")`, so it never fans out, enters a
  **primer**, or becomes a **partition** level. (Fanning out
  per-distribution would dissolve the model averaging that defines a
  fit.)
- **distribution set** (`distset`): The pool of distributions
  model-averaged together to form **one** SSD (one `est`) — supplied as
  a named member of an
  \[[`ssd_distset()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_distset.md)\]
  collection. The set **name** is an hc cross-join **axis** (`distset` ∈
  `task_axes("hc")`): per hc cell the parent **union** fit is
  [`subset()`](https://rdrr.io/r/base/subset.html)-ed to the set’s
  members (`strict = FALSE`) and re-averaged, so several pools reuse
  *one* fit rather than re-fitting. The name (not its members) hashes
  onto the `distset=<name>` Hive path segment and the per-task primer
  (mirroring `min_pmix` and datasets); the members ride on the scenario
  (`scenario$hc$distsets`, isolated by
  [`scenario_distset()`](https://poissonconsulting.github.io/ssdsims/reference/scenario_distset.md))
  and never enter a hash. Bundled (inner) by default — one hc shard
  serves every pool for a `(dataset, sim)` cell — and promotable to a
  path axis via `partition_by$hc`.
- **`fits`**: A `fitdists` object holding one or more fitted
  distributions.
- **`hc`**: Hazard concentration: a quantile of the SSD at a given
  `proportion` (e.g. `hc5` is the 5% quantile); see
  [`ssdtools::ssd_hc()`](https://bcgov.github.io/ssdtools/reference/ssd_hc.html).
- **`proportion`**: The proportion of species affected at which the
  hazard concentration is computed.
- **`ci`**: Scenario-wide scalar flag for whether to compute confidence
  intervals on hazard concentrations. A *scenario setting*, not a
  cross-join axis — the point estimate is identical whether `ci` is
  `TRUE` or `FALSE`, so `ci = TRUE` is a superset of `ci = FALSE`
  (TARGETS-DESIGN.md §1.2).
- **`ci_method`**: The method used to compute confidence intervals (e.g.
  `multi_fixed`, `weighted_samples`).
- **`nboot`**: The number of bootstrap replicates used when computing
  confidence intervals. An hc cross-join **axis** (`task_axes("hc")`):
  distinct values fan out into separate tasks, primers, and shards, so
  results can be partitioned by `nboot`. It enters the per-task
  **primer**, giving each value an independent, reproducible bootstrap
  draw (the bootstrap is the only RNG consumer in hc estimation; the
  point estimate is analytic). See TARGETS-DESIGN.md §9 for why it stays
  in the primer rather than sharing one stream across `nboot` values.
