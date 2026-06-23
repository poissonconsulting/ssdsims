# Targets design — ssdsims

End-to-end workflow for running an `ssdsims` scenario on a cluster.
Three primary goals, each a hard constraint:

- **Reproducibility.** A scenario’s results are bit-stable across reruns
  and machines (RNG state is explicit and serializable; §1, §2).
- **Debuggability.** Any single failed task on the cluster must be
  replayable locally — with no `targets`, no orchestrator — using the
  same inputs the cluster used. This is what the `_state` primitives
  (`slice_sample_state()`, `fit_dists_state()`, `hc_state()`, each
  taking a per-task **primer**) and the per-shard Parquet partitions and
  their manifest are *for* (§7).
- **Extensibility.** Shards are the cache unit: a larger scenario reuses
  shards with Parquets already computed and computes the rest. Path-axis
  growth adds new shards; inner-axis growth rewrites affected shards
  atomically (§8). Pinning outputs *despite a code change* stays inside
  the one project via `tar_cue(depend = FALSE)` — no second project
  needed (§8.3).

Parallelism is assumed throughout. The document covers, in order: the
**scenario object** and its name-keyed datasets/`min_pmix` (§1), the
**per-task dqrng+hash RNG mechanism** (§2), running locally (§3),
assembling the cluster pipeline (§4), the three step grids and their
fan-outs (§5), the concrete target graph and the **cloud upload hook**
(§6), **debugging** a cluster failure (§7), and **extension** (§8). §9
lists known limitations, §10 maps gaps from `RNG-FLOW.md` §5 to
resolutions, §11 collects open questions, and §12 is the implementation
roadmap.

Terminology (per GLOSSARY.md): a **task** is one row of a
`{data,fit,hc}_tasks` table — the smallest unit of computation, carrying
a primer; a **shard** is the Parquet file produced by running one or
several tasks (depending on **partitioning**). **Step** = one of the
three stages data / fit / hc; **target** =
[`tar_target()`](https://docs.ropensci.org/targets/reference/tar_target.html)
declaration; **job** = Slurm work unit. Each shard is its own named
`*_step` target (minted by `tar_map` at construction time — **static
branching**, §6); a shard’s tasks run inside that one target, which
writes 1 shard. Independent shard targets run in parallel; how they map
to Slurm jobs under `crew_controller_slurm()` is an open question (§11).

Background and the list of gaps this design closes are in `RNG-FLOW.md`
§5. This is a forward-looking design; it does not document the existing
PoC (PR \#59).

------------------------------------------------------------------------

## Executive summary

The most consequential design choices, with section refs:

- **Per-task RNG via dqrng + hash, not an L’Ecuyer sub-stream lattice
  (§2).** Each task primes dqrng PCG64 with the scenario’s scalar `seed`
  plus a 64-bit `primer` derived from `rlang::hash(task_params)`.
  Validated end-to-end by `scripts/experiment-dqrng-hash.R`. The choice
  of pcg64 is forced because Xoroshiro128++/Xoshiro256++ hang on
  length-2 `stream` arguments. (Threefry not yet reviewed.)
- **Primer / state / seed / stream are four distinct terms
  (GLOSSARY.md).** `seed` is the scenario scalar; `primer` is the
  per-task initializer; `state` is the RNG’s internal state (the
  function-name suffix `_state` reflects the wrapper that installs the
  primer); `stream` is dqrng’s API parameter and the L’Ecuyer-CMRG
  abstraction.
- **Each task initializes the RNG once (§12 `primer-primitives`).** The
  per-task body calls `local_dqrng_state(seed, primer)` exactly once and
  then runs the (state-less) ssdtools / dplyr ops against the ambient
  RNG. No `state =` argument on the inner ops.
- **Scenario is purely declarative (§1).** Stores `seed`, scenario
  options, and *names* of datasets and `min_pmix` entries; the values
  are **materialised on the scenario, reached by name via accessors**
  (§1.1). Names enter the per-task hash, function definitions do not (so
  a code edit / JIT does not move tasks across primers, and the data
  stored in Parquet files remains simple).
- **Three step grids, one primer per task (§5).** Data, fit and hc fan
  out independently; `nrow` is **never** an axis of the data step —
  every `nrow` value is a `head(., n)` of a single fixed-size sample
  (the scenario’s `nrow_max` setting).
- **`ci` is a scalar flag, not an axis (§1.2).** `ci` is a scenario-wide
  `TRUE`/`FALSE` choice (the point estimate is identical either way;
  `ci = TRUE` merely adds the CI columns), so it is excluded from
  `task_axes("hc")` and the per-task primer. When `ci = FALSE` the
  `nboot` / `ci_method` / `parametric` axes are stored as `NA` in the
  hc-task table — no phantom branches.
- **Static branching — one named target per shard (§6).** The shard set
  is a pure function of the scenario, known when `_targets.R` is
  sourced, so
  [`tarchetypes::tar_map()`](https://docs.ropensci.org/tarchetypes/reference/tar_map.html)
  mints one named target per shard (no `pattern = map` over a runtime
  task-table target). The scenario is a plain construction-time object,
  not a
  [`tar_target()`](https://docs.ropensci.org/targets/reference/tar_target.html).
  Dynamic branching stays available as a documented fallback for
  extreme-scale fan-outs (§6). The targets lab exercised both styles
  head to head and confirmed the rule, including the cheap-extension
  payoff static branching relies on (§6).
- **Per-shard Parquet shards, Hive-style (§6).** Each shard target
  writes one Parquet file under `results/<step>/dataset=.../sim=.../`;
  downstream shards open the upstream shard by partition path
  (`format = "file"`), and duckplyr predicate-pushes filters into the
  partition columns.
- **Cloud upload as a separate target (§6.1).** When the runner’s
  `upload` argument is set (`ssd_scenario_targets(..., upload = ...)`),
  an `upload_<step>` target sits downstream of each shard file and
  pushes it to Azure Blob (or another object store). Because it is its
  own `format = "file"` target, content-hashing skips re-uploading
  shards that did not change;
  [`ssd_test_upload()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_test_upload.md)
  is the user’s explicit preflight probe (the factory itself does no
  network I/O and never runs it), and
  [`ssd_upload_dryrun()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_azure.md)
  builds and dry-runs the DAG with no credentials.
- **Partial failures survive and stay visible (§6.2).** The pipeline
  runs **keep-going by default** — the shipped `_targets.R` templates
  set `tar_option_set(error = "continue")`, the `make -k` analogue, so
  one errored target skips only its dependents while every other
  reachable shard still builds (a long parallel run is never lost to one
  bad branch). On top of that, a shard body writes as many rows as ran
  successfully, so a bad task yields a *shorter* shard, not an abort;
  the step target errors (and carries `error = "null"`, stronger still —
  its NULL flows downstream) only in exceptional cases. A downstream
  `assert_<step>` target — sibling to `upload_<step>` — compares the
  shard’s row count to the expected count and goes red on any shortfall,
  making incompleteness a first-class DAG node and the refresh set a
  [`tar_meta()`](https://docs.ropensci.org/targets/reference/tar_meta.html)
  query (§8.4). Errors are read *after*
  [`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html)
  returns (a target cannot call
  [`tar_meta()`](https://docs.ropensci.org/targets/reference/tar_meta.html)
  mid-run).
- **Debug = task row + one upstream shard (§7).** Any failed branch
  replays locally with `local_dqrng_state(seed, primer)` + the
  immediate-upstream shard, no `targets` needed; the lightweight recipe
  verifies the upstream against `manifest$completed_shards` (sha256)
  before running the failing step.
- **Extension is mostly implicit (§8).** Shards *are* the cache: a
  larger scenario reuses shards whose partition path already has a
  Parquet, and adds shards (path-axis growth, §8.1) or atomically
  rewrites them (inner-axis growth, §8.2) as needed. Pinning shards
  *despite a code change* (the opposite of invalidation) is
  `tar_cue(depend = FALSE)` on the step targets, all in the one project
  — no child project, no `parent` reference (§8.3).
- **Roadmap with parallel work streams (§12).** Kebab-slugged steps with
  a Mermaid DAG showing where branches open. Two ground-up entries —
  `ssd-define-scenario` and the `task-list-loop-baseline` runner — land
  before any RNG / dqrng machinery so the data shape is settled first.
  Retiring the legacy public API is the terminal `cleanup-lecuyer` step
  (which folds in the former `migrate-public-api`): the targets-only
  plumbing never waited on it, and datasets / `min_pmix` are
  materialised on the scenario and reached by name through
  `scenario-accessors` — no registry.

------------------------------------------------------------------------

## 1. Scenario object

The scenario is **purely declarative**. It does not carry the
materialized task grid; expansion happens via
`ssd_scenario_tasks(scenario)` (§2). It is a **plain construction-time
object** built at the top of `_targets.R`, not a
[`tar_target()`](https://docs.ropensci.org/targets/reference/tar_target.html)
— so the shard set it expands to is known when the pipeline is sourced,
which is what lets the steps use static branching (§6). An S3 object
holding:

    ssdsims_scenario
    ├── seed         ← scalar integer; root of the per-task RNG (§2)
    ├── nsim         ← number of replicate sims per dataset
    ├── datasets     ← character vector of dataset names; the tibbles are
    │                  materialised on the scenario, reached by name (§1.1)
    ├── nrow         ← integer vector of sample sizes; subset property (§5)
    ├── fit          ← list of ssd_fit_dists() argument vectors;
    │                  min_pmix is a NAME; the function is materialised on
    │                  the scenario, reached by name (§1.1)
    ├── hc           ← list of ssd_hc() argument vectors; ci is a scalar
    │                  flag (not an axis, §1.2) — when ci = FALSE the
    │                  bootstrap-only scenario options (nboot, ci_method,
    │                  parametric) are stored as NA on the hc tasks
    └── partition_by ← list(data = ..., fit = ..., hc = ...) of character
                       vectors picking the Hive partition axes per step;
                       one shard per (step, partition-cell). Default:
                       data=(dataset,sim,replace), fit=(dataset,sim,rescale),
                       hc=(dataset,sim). See §5.

There is no `upload` field. The upload destination is a **runner
argument** (`ssd_scenario_targets(..., upload = ...)`), the sibling of
`root`, not part of the scenario’s declarative identity (the single-core
runner deliberately has no upload; §6.1). There is no `parent` field.
Extension never needs one: plain growth (more datasets, more `nsim`)
reuses shards by file existence (`file existence ⇒ cache hit`, §8.1),
inner-axis growth rewrites the affected shards (§8.2), and pinning
shards against a code change is done in-project with
`tar_cue(depend = FALSE)` (§8.3) rather than by pointing a second
project at this one’s outputs.

Three design points distinguish this from the current code:

1.  **`seed`, a scalar integer.** Re-running a scenario with a different
    RNG means changing this one number. The L’Ecuyer-CMRG `root_state`
    (length-7 vector) of the previous design is gone; dqrng + hash (§2)
    makes it unnecessary. Two scenarios with the same `seed` and the
    same task parameters produce identical RNG sequences.
2.  **Datasets and `min_pmix` are keyed by name.** Both are materialised
    on the scenario and reached by name via accessors (§1.1); the
    scenario stores the names as its identity surface (a few names +
    numeric scenario options) and the per-task hash (§2) keys on the
    name, ignoring function-body contents — so a non-behavior-changing
    code edit to a `min_pmix` function does *not* invalidate cached
    results. See §1.1.
3.  **No `parent` reference at all.** Adding tasks on a *path* axis (new
    dataset, more `nsim`) creates new shards; existing shards are
    unaffected. Adding tasks on an *inner* axis (new `min_pmix`, new
    `nboot`) rewrites the affected shards atomically (§8.2). The one
    case that once seemed to need a second project pointing back at this
    one — pinning shards despite a code change — is handled in-project
    with `tar_cue(depend = FALSE)` (§8.3), so the scenario carries no
    `parent`.

### 1.1 Datasets and min_pmix: materialised on the scenario, accessed by name

The scenario carries datasets and `min_pmix` entries as **values
materialised at construction, keyed by name** — the dataset tibbles
(`scenario-input-types` realises every input via `ssd_data()`, `Conc`
verified) and the `min_pmix` functions (a supplied function kept under
its derived name; a name-string resolved to a function at construction).
A consumer **isolates a value by name** with a thin accessor —
`scenario_dataset(scenario, name)` / `scenario_min_pmix(scenario, name)`
(the `scenario-accessors` step, §12) — so there is **no registry**: no
per-project resolution target, no `results/datasets/*.parquet`, no
pinned function value. The scenario is a referenced global in
`_targets.R` transported to workers, and datasets/min_pmix functions are
tiny, so the materialised values simply ride along.

The crucial split is **hash by name, carry value for execution**: the
per-task primer hashes the *name* (`task_axes`), never the value, so a
recompile/JIT or a cosmetic edit can’t move a task to a different RNG
stream — while the value travels on the scenario purely to be run. Two
reasons the hash keys on the name:

1.  **Function-value hashes are not stable.** This is the technical
    reason and the primary one.
    [`rlang::hash()`](https://rlang.r-lib.org/reference/hash.html) over
    a function serializes its representation, which is **not
    byte-stable** across apparently-equivalent forms:

    - Byte-compilation changes the hash. `compiler::cmpfun(f)` has a
      different
      [`rlang::hash()`](https://rlang.r-lib.org/reference/hash.html)
      from the source-form `f` even though they evaluate identically.
      R’s own auto-JIT triggers this unpredictably (the compiler may
      apply after a few calls).
    - Loading a function from source vs. from an installed package can
      pin different `srcref` and `environment(f)` payloads — same code,
      different hash.
    - Closures that capture different parent environments hash
      differently.

    Hashing the *name* and looking the function up at call time bypasses
    all of these. (Source edits that change behavior are the user’s
    contract — pin `ssdtools` and R versions in the manifest, §9.)

2.  **A stable identity surface.** The names (plus numeric scenario
    options) are what the §9 manifest records and what task identities
    are built from, independent of the materialised values that ride on
    the scenario for execution. (Datasets are tiny, so carrying them
    inline is cheap; if one ever grows too large to transport, the
    deferred `dataset-provenance` step (§12) stores a generator + seed
    instead of the bytes.)

#### Datasets

`scenario-input-types` materialises every input — a data frame, a
generator function, a `fitdists`/`tmbfit`, a function-name string — to a
validated tibble (numeric `Conc` verified) **at construction**, stored
inline on the scenario under its derived name. A consumer reads one by
name:

``` r

scenario_dataset(scenario, "boron")   # the materialised tibble
```

Generation is seeded independently of the scenario (the dataset *name*
as the dqrng stream), so the realised bytes are reproducible by being
**kept, not regenerated**. There is no on-disk dataset cache: the
scenario already carries the bytes and is transported to workers.
Trade-off: an inline dataset must fit in memory; the deferred
`dataset-provenance` step (§12) is the escape hatch for one too large to
transport, storing a generator + seed to rebuild from instead.

#### `min_pmix`

`min_pmix` is materialised the same way: supplied as an
[`ssd_pmix()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_pmix.md)
collection, the scenario stores the **name** (in `fit$min_pmix`, which
feeds the per-task primer and the task path) *and* the single-argument
**function**, keyed by name. Names come from the
[`ssd_pmix()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_pmix.md)
argument names or symbol capture; there is no string-to-function
resolution. A consumer reads it by name:

``` r

scenario_min_pmix(scenario, "ssd_min_pmix")   # the materialised function
```

The primer hashes the *name*, never the function value (which is why
storing the value is safe — see the two reasons above); the function is
fetched via the accessor just before the call, after `dqset.seed()`.

### 1.2 `ci` is a scalar flag (not an axis)

`ci` is a scenario-wide scalar flag (`TRUE`/`FALSE`, default `FALSE`),
not a grid/task axis (delivered by the `scalar-ci-flag` change, §12).
The point estimate is the reason:
[`ssdtools::ssd_hc()`](https://bcgov.github.io/ssdtools/reference/ssd_hc.html)
computes the estimate (`est`) analytically from the fit, independent of
the bootstrap and the RNG, so it is **byte-identical** whether
`ci = TRUE` or `ci = FALSE` (verified across every `ssd_ci_methods()`).
A `ci = TRUE` run is therefore a strict superset of `ci = FALSE` — same
`est`, plus the populated `se` / `lcl` / `ucl`. Running both in one
scenario would only emit a redundant point-estimate row per fit-task, so
`ci` is a single either/or choice: `ci = FALSE` for cheap,
bootstrap-free point estimates, or `ci = TRUE` for estimates plus CIs.
This makes `ci` a **scenario setting** (GLOSSARY) — a non-axis scenario
option consumed within each task — alongside `samples` (applied
uniformly) and `proportion` (which fans out within the task’s own
output). The three are grouped together in the
[`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md)
signature, after the axes.

Because the estimate carries no `ci`-dependence, `ci` is **excluded from
`task_axes("hc")` and the per-task primer** and applied uniformly to
every hc task. The bootstrap-only scenario options (`nboot`,
`ci_method`, `parametric`) only affect the bootstrap:

- If `ci = FALSE`, those scenario options are meaningless: supplying any
  of them aborts at scenario construction (set `ci = TRUE` or omit
  them), and the hc-task table stores them as `NA`. `est_method` is an
  hc scenario setting (not an axis), so this leaves no fan-out axis at
  all — exactly one hc row per fit task.
- If `ci = TRUE`, the hc grid fans out across
  `nboot × ci_method × parametric`.

In the hc task table (here a `ci = TRUE` scenario with two `nboot`
values):

| sim | nrow | rescale | ci   | nboot | ci_method        | parametric |
|----:|-----:|:--------|:-----|------:|:-----------------|:-----------|
|   1 |    5 | FALSE   | TRUE |   100 | weighted_samples | TRUE       |
|   1 |    5 | FALSE   | TRUE |  1000 | weighted_samples | TRUE       |

A `ci = FALSE` scenario instead yields exactly one hc row per fit task
with `nboot` / `ci_method` / `parametric` all `NA` (every requested
`est_method` is summarised within that single task). The hash of an
`NA`-bearing row is well-defined as long as `NA` is encoded canonically
—
[`task_primer()`](https://poissonconsulting.github.io/ssdsims/reference/task_primer.md)
does this via
[`rlang::hash()`](https://rlang.r-lib.org/reference/hash.html) on the
named list — so the `NA` bootstrap scenario options never allocate
phantom streams to combinations that don’t exist in practice.

------------------------------------------------------------------------

## 2. Per-task RNG via dqrng + hash

Validated by `scripts/experiment-dqrng-hash.R`. Each task (= row of a
`{data,fit,hc}_tasks` table) gets its own **primer**, a length-2 integer
derived from the hash of its task parameters. The scenario carries a
single integer `seed`; per-task RNG is configured by passing the primer
into dqrng’s `stream` argument:

``` r

dqrng::dqset.seed(seed   = scenario$seed,
                  stream = task_primer(task_params))   # task's primer
```

A **primer** is the value that, together with `seed`, fully specifies an
RNG instance’s starting point — see GLOSSARY.md. For the dqrng path it
is a 64-bit integer (length-2 int); for the legacy L’Ecuyer-CMRG path it
was the length-7 state vector. The function argument `state =` of
`slice_sample_state()`, `fit_dists_state()`, `hc_state()` carries the
primer; the `_state` suffix reflects that the function *installs the
primer as the running state* before its body executes.

(The dqrng API parameter happens to be named `stream` — that name
belongs to dqrng, not to ssdsims. In our terminology the value passed
there is a primer.)

where `task_primer(p)` is a length-2 integer vector packing **64 bits**
of `rlang::hash(p)`:

``` r

task_primer <- function(p) {
  h <- rlang::hash(p)                 # 32-char xxhash128 hex
  c(
    hex8_to_int32(substr(h, 1L,  8L)),  # hi32: 0x80000000 -> NA_integer_
    hex8_to_int32(substr(h, 9L, 16L))   # lo32: ditto
  )
}
```

**64 bits effective.** dqrng’s docs document that `stream` accepts a
length-2 integer vector representing a 64-bit value
([`?dqrng::dqRNGkind`](https://daqana.github.io/dqrng/reference/dqrng-functions.html)).
In R each integer is signed int32 and the bit pattern `0x80000000`
(INT_MIN) is reserved as `NA_integer_`. dqrng accepts `NA_integer_` in
`stream` and treats it as INT_MIN, so we encode the full 64 bits by
mapping the one INT_MIN bit pattern to `NA_integer_` in the task primer.
Validated in `scripts/experiment-dqrng-hash.R` (4): 0 empirical
collisions at 100 k tasks; theoretical 50% collision around
`sqrt(2^64) ≈ 4.3 billion` tasks. Vastly safe for ssdsims’ 10²–10⁴-task
scenarios.

**Which RNG.** dqrng exposes pcg64, Xoroshiro128++/Xoshiro256++, and
Threefry. Empirically (`scripts/experiment-dqrng-hash.R`) only pcg64 and
Threefry handle a length-2 `stream` argument without hanging —
Xoroshiro/Xoshiro hang. We pick **pcg64**: well tested, fast, supports
stream by construction (each stream is a distinct LCG increment ⇒
statistically independent sequences). `dqRNGkind("pcg64")` is set
explicitly at pipeline init; the package’s `dqRNGkind` default
(Xoroshiro128++) is overridden.

Per scenario execution, `dqRNGkind("pcg64")` is set and
[`dqrng::register_methods()`](https://daqana.github.io/dqrng/reference/user-supplied-rng.html)
is called at the start (wrapping the step runner with
`on.exit(restore_methods())`). This routes base R’s
[`runif()`](https://rdrr.io/r/stats/Uniform.html),
[`rnorm()`](https://rdrr.io/r/stats/Normal.html),
[`rbinom()`](https://rdrr.io/r/stats/Binomial.html),
[`rexp()`](https://rdrr.io/r/stats/Exponential.html),
[`rgamma()`](https://rdrr.io/r/stats/GammaDist.html),
[`rpois()`](https://rdrr.io/r/stats/Poisson.html),
[`sample.int()`](https://rdrr.io/r/base/sample.html),
[`sample()`](https://rdrr.io/r/base/sample.html) (and therefore
[`dplyr::slice_sample()`](https://dplyr.tidyverse.org/reference/slice.html)
and `ssdtools::ssd_r*()`) through dqrng’s pcg64 with the configured
(seed, primer). The experiment script verifies this end-to-end.

       ┌────────────────────────────────────────────────────────────┐
       │  task execution within a scenario step                     │
       │  ────────────────────────────────────────                  │
       │  dqRNGkind("pcg64")                                         │
       │  dqrng::register_methods()                                  │
       │  on.exit(dqrng::restore_methods())   # cleanup guarantee    │
       │  for each task:                                             │
       │    dqset.seed(seed   = scenario$seed,                       │
       │               stream = task$primer)   # dqrng's stream arg  │
       │    …                          # run the task body          │
       └────────────────────────────────────────────────────────────┘

### Why this replaces the L’Ecuyer-CMRG sub-stream lattice

- **No precomputed lattice.** Each task’s RNG is fully specified by the
  pair `(seed, state)`. Both are small integers; the task row carries
  them as ordinary columns, not length-7 state vectors.
- **Extension is implicit.** Adding tasks (new datasets, more `sim`
  values, …) gives them new hashes and therefore new primers. Existing
  tasks’ states are unaffected — their hashes don’t change.
- **Re-running a scenario with a different RNG** means changing
  `scenario$seed`. All task primers are re-rooted automatically.
- **Debuggability.** The task row carries `(seed, state)`; a failing
  branch replays locally as a one-liner (see §7).

### What goes into the hash

`task_primer(p)` hashes a canonical, name-keyed representation of the
task’s parameters. For a data task: `(dataset_name, sim, replace)` only
— `nrow` is *not* in the hash because every `nrow` value is
sub-truncation of the same `n_max`-row sample (§5). For a fit task:
data-task identity plus the fit-arg-grid row (`rescale`, `computable`,
`at_boundary_ok`, `min_pmix_name`, `range_shape1`, `range_shape2`). For
an hc task: fit-task identity plus the hc-arg- grid row (`nboot`,
`ci_method`, `parametric`). `ci` and `est_method` are hc scenario
settings, **not** in the hash (§1.2; `est_method` is summarised within
the task from a single bootstrap sample set); when `ci = FALSE` the
bootstrap-only scenario options are `NA` in that row (canonically
encoded).

Function-valued parameters (`min_pmix`) are referenced **by name**
(§1.1) so that a recompile/JIT does not move the task to a different
state; the hash is over the name, not the function value.

The restart property (`dqset.seed(seed, state) → same sequence`) is
exercised in `scripts/experiment-dqrng-hash.R`. The previous design
relied on the analogous L’Ecuyer sub-stream restart property; that
lattice (and its exploratory scripts) was retired with the legacy RNG
path.

------------------------------------------------------------------------

## 3. Local run

``` r

scenario <- ssd_define_scenario(
  ssd_scenario_data(ssddata::ccme_boron),
  nsim       = 100L,
  nrow       = c(5L, 10L, 50L),
  proportion = c(0.01, 0.05),
  nboot      = 1000,
  seed       = 42
)

ssd_run_scenario_baseline(scenario)  # sequential, in-process
```

[`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md)
stores the scenario inputs (seed, dataset names, fit/hc arg grids). It
is purely declarative — it does **not** expand the task tables.
Expansion is `ssd_scenario_tasks(scenario)`, called either by
[`ssd_run_scenario_baseline()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario_baseline.md)
(local) or, in the cluster pipeline (§4), while `_targets.R` is sourced
to build the per-shard task tables that `tar_map` fans out over (§6).

       ssd_define_scenario(...) ──▶ ssdsims_scenario  (declarative; carries seed)
                                  │
                                  ▼
                         ssd_scenario_tasks(scenario)
                                  │
                                  ▼
                         three task tables (sample, fit, hc tasks; see §5),
                         each row = one task carrying its (seed, primer) pair (§2)
                                  │
                ┌─────────────────┴─────────────────┐
                ▼                                   ▼
       ssd_run_scenario_baseline(scenario)  tar_target(...) feeds the
       in-process baseline runner          cluster pipeline (§4)

------------------------------------------------------------------------

## 4. From local to a cluster

The scenario object is unchanged. **Three ingredients come together** to
produce the cluster pipeline; none of them is downstream of the others —
they’re equal inputs that get assembled into the final `_targets.R`:

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
                  │   scenario ─▶ {data,fit,hc}_shards    │
                  │                            │          │
                  │                            ▼          │
                  │              tar_map(...) targets on  │
                  │             crew_controller_slurm()   │
                  │                            │          │
                  │                            ▼          │
                  │           shards run in parallel;     │
                  │           each target writes one shard│
                  └───────────────────────────────────────┘

The three ingredients are **equally important** and gathered in
parallel; none is downstream of the others. Roles:

- **A — example pipeline for another cluster** contributes the *shape*
  of `_targets.R`: how a `crew` controller is constructed, how the
  per-shard branching is wired, where results land, where the merge
  target sits. Lifted as a skeleton, not as content. Source: another
  lab’s published targets+crew repo.

- **B — toy pipeline for our target cluster** contributes the *backend*:
  a
  [`crew.cluster::crew_controller_slurm()`](https://wlandau.github.io/crew.cluster/reference/crew_controller_slurm.html)
  (or equivalent for the actual scheduler) configured with the right
  queue, module loads, and scratch paths. Drafted with LLM help and
  validated by submitting one trivial job end-to-end **before any
  ssdsims logic is involved** — proves the cluster wiring works.

- **C — working scenario object** contributes the *content*: `seed`,
  dataset names, fit/hc argument vectors. (The optional cloud `upload`
  is a **runner argument** of
  [`ssd_scenario_targets()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_targets.md),
  not a scenario field — §6.1.) Already exercised locally with
  [`ssd_run_scenario_baseline()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario_baseline.md)
  (§3) so the only remaining unknown when assembling the three is the
  cluster wiring itself.

Only the controller and resource specs (from B) change between clusters.
Pipeline shape (from A) and task content + RNG (from C) are
scheduler-independent.

**Validated by the crew labs.** Ingredients A and B are no longer
hypothetical: the project’s crew labs (in the sister planning repo) have
assembled and run them end to end on a SLURM cluster. One lab is the
bare shape (A) — a
[`crew.cluster::crew_controller_slurm()`](https://wlandau.github.io/crew.cluster/reference/crew_controller_slurm.html)
dispatching a no-op workload — and a second drives the real ssdsims
per-sim pipeline (C) through that same controller, with a
[`crew::crew_controller_local()`](https://wlandau.github.io/crew/reference/crew_controller_local.html)
fallback for off-cluster smoke tests. The labs also pinned down the
operational backend (B): a ManyLinux binary install path so worker nodes
resolve the dependency tree without compiling, plus a login-node
prerequisite checker. `cluster-pipeline` (§12) therefore ports a proven
shape, not an untested sketch.

------------------------------------------------------------------------

## 5. Three grids, three fan-outs

The three RNG-touching operations consume **distinct cross-joined
parameter grids**, and the grids grow monotonically:

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

Confirmed by tracing `scripts/example.R`’s second scenario
(pre-`nrow`-sub-truncation counts; `slice_sample_state()` now fans out
by `2 sim · 1` since `nrow` is a sub-truncation axis, not a cross-join
axis — see “`nrow` is never an independent axis” below):

| step                   | grid size | fan-out                                 |
|------------------------|----------:|-----------------------------------------|
| `slice_sample_state()` |        10 | 2 sim · 5 nrow                          |
| `fit_dists_state()`    |        10 | 2 sim · 5 nrow · 1 (fit-arg grid)       |
| `hc_state()`           |       180 | 2 sim · 5 nrow · 6 nboot · 3 est_method |

`proportion` is *inside* `ssd_hc()` (rows of the hc result tibble), not
a cross-join axis. `ci_method` and `parametric` were scalar in the
second example but are full cross-join axes in the general case.

### State allocation: one per task, via hash

Each task in each step’s table gets its own per-task **state** — a
length-2 integer derived from the 64-bit hash of the task’s parameters
(§2), passed to dqrng via its `stream` argument. Tasks do not share
states across steps or across grid axes; the only sharing is the
deliberate `nrow` sub-truncation below.

### Tasks bundle into shards by partition

Tasks are the unit of computation (one row, one primer, one `_state`
call). Shards are the unit of *storage, dispatch and parallelism*: one
shard ≡ one Parquet file ≡ one named step target (minted by `tar_map`,
§6), and **independent shards run in parallel**. (How shard targets are
packed into Slurm jobs under `crew_controller_slurm()` is an open
question — see §11.) **One shard typically contains many tasks**: the
scenario’s `partition_by[[step]]` picks which task-table columns become
Hive directory levels, and every task row sharing those column values
goes into the same shard.

Default `partition_by`:

| step | path axes | inner axes (Parquet columns) |
|----|----|----|
| data | dataset, sim, replace | (none — the data shard is a single sample) |
| fit | dataset, sim, rescale | computable, at_boundary_ok, min_pmix, range_shape1, range_shape2 |
| hc | dataset, sim | rescale, nboot, est_method, ci_method, parametric |

`partition_by` is **scenario-configurable**; pushing more axes into the
path produces smaller, finer-grained shards; pushing fewer axes produces
coarser shards.

What this changes:

- **Shard count** is `Π |path axis|` per step, not `Π |task axis|`. For
  the §6 example (1 dataset · 2 sim · 2 nrow · 2 rescale · 2 est_method,
  default `partition_by`): 2 data shards, 4 fit shards, 2 hc shards
  (instead of 4 / 8 / 16 tasks-as-shards).
- **Per-task RNG is unchanged.** Each task in the shard runs with its
  own primer; the branch body loops over the shard’s task rows and
  primes the RNG once per task.
- **Cache granularity is per shard, not per task.** Adding a new
  *inner-axis* value (e.g. a new `min_pmix`) changes the task-row set
  for every affected shard and forces an atomic rewrite of those
  Parquets (existing branches re-run). Adding a new *path-axis* value
  (e.g. a new dataset) creates new shards and leaves existing shards
  untouched — see §8.
- **Replay (§7) is still per task.** A task’s primer, params, and the
  row’s upstream partition-path columns fully reproduce the computation;
  the shard it happens to live in is incidental.

For the small
`nsim = 2, nrow = c(5, 10), rescale = c(F, T), est_method = c("arithmetic", "multi")`
example, with the default `partition_by` (§5):

                                                     tasks (primers)    shards (Parquets)
       data:  2 sim · 1 (nrow sub-trunc) · 1 replace     =  2            =  2  (1 task per shard)
       fit :  data ·  2 rescale                          =  4            =  4  (1 task per shard)
       hc  :  fit  ·  1 nboot · 2 est_method             =  8            =  2  (4 tasks per shard)
                                                  total = 14 tasks       =  8 shards

(The legacy `scripts/example-expanded-grids-independent.R` allocated 28
by treating `nrow` as an independent axis — the design no longer does
that; see `scripts/experiment-subset-property.R` for the proof that
`nrow` is sub-truncation.)

### `nrow` is never an independent axis

For empirical-data slicing, **larger `nrow` values include the same rows
as smaller ones, byte-identically**. So `nrow` is **never** an axis of
the data state — it is just `head(., n)` of a single `n_max`-row sample.
Proven by `scripts/experiment-subset-property.R` for both
`replace = FALSE` and `replace = TRUE`. The data state is keyed by
`(dataset, sim, replace)` only, and the slice is

``` r

slice_sample_state <- function(data, n_max, n, seed, state, replace) {
  dqrng::dqset.seed(seed, stream = state)   # dqrng API
  idx <- sample.int(nrow(data), size = n_max, replace = replace)
  data[idx[seq_len(n)], , drop = FALSE]
}
```

with `n_max` the **effective draw size** resolved from the scenario’s
fixed `nrow_max` setting (`min(nrow_max, nrow(data))` for
`replace = FALSE` — the high default thus draws the full permutation —
and `nrow_max` rows for `replace = TRUE`), **not** `max(scenario$nrow)`:
deriving the draw size from the `nrow` axis meant a widened `max(nrow)`
re-drew the sample and invalidated the fit shards reading it; the
`nrow_max`-sized draw is axis-independent, so extending `nrow` (within
the effective draw size) never re-draws (the `nrow-max-setting` change).
Result: `slice_sample_state(data, n_max, 5, …)` is a prefix of
`slice_sample_state(data, n_max, 10, …)` — same `(seed, state)`, same
`sample.int` call, just truncated.

**Why the property holds for both `replace` values.**

- `replace = FALSE`: `sample.int(N, n_max, replace = FALSE)` runs
  Fisher-Yates internally; the first `n` indices are a permutation
  prefix and a valid size-`n` sample by construction.
- `replace = TRUE`: `sample.int(N, n_max, replace = TRUE)` is `n_max`
  independent uniform draws; the first `n` are a size-`n` sample drawn
  from the same RNG sequence.

Both cases assume the byte-stable behavior of
[`base::sample.int()`](https://rdrr.io/r/base/sample.html) (and
[`dplyr::slice_sample()`](https://dplyr.tidyverse.org/reference/slice.html)
which delegates to it). Pin R version in the manifest (§9).

The trick costs nothing on the task table — the draw size is the
scenario’s `nrow_max` setting, resolved by the runner per dataset, not a
row column — and cuts `|data grid|` from `|dataset| · |sim| · |nrow|` to
`|dataset| · |sim| · |replace|`. For a scenario with
`nrow = c(5, 6, 10, 20, 50)` and `replace = FALSE` only, it cuts the
data fan-out by 5×.

### Implications for the targets pipeline

Each step needs its **own** task table (`data_tasks`, `fit_tasks`,
`hc_tasks`) and its own per-shard targets (one named target per shard,
§6) — a single shared task table mapped lockstep through all three steps
does **not** work when the grids differ. Layers link via the upstream
**shard’s partition path**: each task row carries its full parent
identity (the `sample_id` / `fit_id` columns), and its parent *shard* is
that identity projected onto the parent’s path axes — **derived** at
read/sourcing time, **not** stored, so the task table stays
`partition_by`-independent and re-layout stays free (storing a Hive-path
prefix as the key would couple the table to the layout). Because steps
may partition independently — the §5 defaults deliberately *coarsen*
downstream — a child shard generally spans a **set** of parent shards
(**m:n**): an `hc` shard reads every `fit` shard for its
`(dataset, sim)` across `nrow`/`rescale`, and with `replace = c(F, T)` a
`fit` shard reads two `sample` shards. The per-shard body therefore
opens the **set** of upstream Parquets its tasks reference and filters
them (duckplyr predicate pushdown); it does **not** assume a single
upstream shard. `shard-runner-baseline` (§12) proves this read+filter
loop single-core; §6 wires it into `targets`.

At the **task** level the linkage is **n:1** — each `fit`/`hc` task has
exactly one parent task, so each table carries a single `<parent>_id`
foreign key. That single key is a *special case* that holds only while
the grids grow monotonically
(`task_axes(sample) ⊆ task_axes(fit) ⊆ task_axes(hc)`), so a child
carries its parent’s full identity and the m:n shard graph is
*generated* from the single key (project each task’s `<parent>_id` onto
`path[parent]`, take the distinct set). A future **reduce** step — an
across-replicate summary that *drops* an axis — references **many**
parent tasks (a **set-valued** foreign key = a shared-axis glob, the
same read), and this applies at **all three layers**, not just `hc`: one
may reduce over any axis a layer carries (`sim`/`replace` at all three,
`nrow` at `fit`/`hc`, the bootstrap axes at `hc` only). The §6 `summary`
is today’s monolithic, un-sharded instance of that fan-in over all three
layers; a sharded version is three reduce steps, each with a set-valued
key to its own layer. A step that consumes more than one upstream step
would simply carry more than one foreign-key column (a DAG, not a tree);
the PK + FK-columns data model already expresses both relaxations, and
neither is triggered by sharding.

------------------------------------------------------------------------

## 6. Target graph (small example)

Concrete pipeline matching `scripts/example-expanded-grids.R`:
`nsim = 2L`, `nrow = c(5L, 10L)`, `rescale = c(FALSE, TRUE)`,
`est_method = c("arithmetic", "multi")`, `nboot = 10`, single dataset
([`ssddata::ccme_boron`](https://rdrr.io/pkg/ssddata/man/ccme_boron.html)).
Under the default `partition_by` (§5), tasks fan out as:

       step  | tasks (rows)                                      | shards (Parquets)
       ------+---------------------------------------------------+------------------
       data  | 1 dataset · 2 sim · 1 (nrow sub-trunc) · 1 replace =  2 |  2 (path: dataset, sim, replace)
       fit   | data · 2 rescale                                  =  4 |  4 (path: dataset, sim, rescale)
       hc    | fit  · 1 nboot · 2 est_method                     =  8 |  2 (path: dataset, sim — 4 tasks per shard)

So in this example data and fit are 1 task : 1 shard, but hc bundles 4
tasks into each of its 2 shards (the inner axes `rescale`, `nboot`,
`est_method` become columns inside the shard’s Parquet, not partition
levels). Each step writes one Parquet per shard so the data, fit, and hc
layers are independently queryable for analysis without re-running
upstream steps.

### Implemented refinement: `sample` as a distinct draw step (`task-list-loop-baseline`)

The `task-list-loop-baseline` step splits the conflated `data` step into
the *draw* and the *truncation*, materialising the grids above as these
task tables:

| step | identity axes | RNG? | note |
|----|----|----|----|
| `sample` | `dataset, sim, replace` | yes | one draw sized by the `nrow_max` setting (resolved per dataset; no row column) |
| `fit` | `sample` axes, `nrow` `× fit grid` | yes | truncates `head(sample, nrow)` inline (RNG-free) then fits |
| `hc` | fit axes `× hc grid` (scalar `ci`, §1.2) | yes | `ci` is a scalar setting read from the scenario, not an axis or a row column; bootstrap scenario options `NA` when `ci = FALSE` |

This keeps the sub-truncation property **structural**: there is exactly
one `sample` task per `(dataset, sim, replace)`, so the expensive draw
is shared across every `nrow`, while `nrow` is an ordinary scalar
cross-join axis at the `fit` step — no compound/list column on the draw.
`partition_by` is correspondingly three-step (`sample`/`fit`/`hc`); by
default `nrow` shards at the `fit` level
(`dataset, sim, nrow, rescale`).

This fold is **provisional**: whether to reinstate an explicit `data`
step (as a buffering checkpoint) is a deferred decision tied to the
caching/invalidation model — see the note in §8.

Dependencies between tasks are **explicit**: each row carries a
path-style `<step>_id` primary key — `path_key()` over **all** of the
step’s `task_axes()`, e.g. `dataset=boron/sim=1/replace=FALSE` — plus
its parent step’s id as a foreign key (`sample_id` on `fit`, `fit_id` on
`hc`). A child references its parent by a single joinable column, and
the baseline runner threads results by that foreign key. The ids are
deterministic and stable under scenario growth (adding a dataset does
not renumber existing rows), so they compose with the cache-by-existence
story in §8.

**Identity key ≠ Hive shard path — the PK/FK are task identity, not
partitioning.** The `<step>_id` primary key and `<parent>_id` foreign
key are the **task identity**: `path_key()` over *all* of
`task_axes(step)`, unique per task, **`partition_by`-independent**, and
exactly what the per-task primer hashes (§2) and the foreign-key join
uses. The **Hive shard path** is a *separate* projection —
`path_key(path[step])` over the `partition_by` **subset** (§5) — shared
by every task in a shard and **`partition_by`-dependent**. They coincide
only in the degenerate one-task-per-shard case; under any coarser
`partition_by` the shard path is a *coarsening* of the identity key
(many tasks share it, so it is **not** a primary key), and the parent
*shard* a child reads is its `<parent>_id` projected onto `path[parent]`
— derived at read/sourcing time, not stored (§5 Implications).
`partition_by` moves the shard path; it never touches the identity key,
the primer, or results.

### Static branching: one named target per shard

The shard set is a **pure function of the scenario**, and the scenario
is known when `_targets.R` is *sourced* — it is a plain constructor call
at the top of the file, not a
[`tar_target()`](https://docs.ropensci.org/targets/reference/tar_target.html).
So the fan-out width is fixed at construction time, never data-dependent
at run time. By the standard rule of thumb — *static branching when the
branch count is known at sourcing time, dynamic branching only when it
depends on data computed during the run* — this design uses **static
branching**:
[`tarchetypes::tar_map()`](https://docs.ropensci.org/tarchetypes/reference/tar_map.html)
mints one named target per shard while the pipeline is built. The
project’s targets lab (a set of single-behaviour toy pipelines in the
sister planning repo) exercised both styles head to head — a
static-branch axis and a dynamic-branch axis as the pair to compare: the
same fan-out shape, decided at construction time vs at run time — and
confirmed the rule applies here. Its most complete pipeline (pinning +
`error = "null"` + per-shard upload, all composed) is itself built on
static branching.

Why static fits this design specifically:

- **The scenario need not live in a target.** It is a construction-time
  object; `ssd_scenario_*_shards(scenario)` evaluate at sourcing time
  and feed
  [`tar_map()`](https://docs.ropensci.org/tarchetypes/reference/tar_map.html)’s
  `values`. `targets` still tracks the scenario as a referenced global,
  so editing a scenario option still invalidates the dependent shards —
  tracking is not lost by taking it out of a target.
- **Named, addressable shards.** Each shard is its own DAG node
  (`fit_step_boron_sim1_rescaleFALSE`), so
  [`tar_read()`](https://docs.ropensci.org/targets/reference/tar_read.html)
  /
  [`tar_invalidate()`](https://docs.ropensci.org/targets/reference/tar_invalidate.html)
  (§8.3–§8.4) and the replay story (§7) can point at one shard by name,
  and
  [`tar_mermaid()`](https://docs.ropensci.org/targets/reference/tar_mermaid.html)
  shows the real per-shard graph with per-shard status colours.
- **Extension is literally “more named targets.”** Adding a dataset or
  growing `nsim` (path-axis growth, §8.1) mints new shard targets at the
  next sourcing; `targets` diffs the target set and builds only the new
  ones — exactly the §8 cache story, with no directory-hash indirection
  needed to keep existing shards from rebuilding.
- **Precise upstream edges.** A fit shard target can name its one
  upstream data shard target directly, so a changed data shard
  invalidates only its dependent fit/hc shards — strictly better than
  depending on a whole pattern target. (The Hive partition-path layout
  below is still kept, but for the off-cluster query layer and replay,
  not as the inter-target wiring crutch dynamic branching would force.)

**The bounded exception — scale.**
[`tar_map()`](https://docs.ropensci.org/tarchetypes/reference/tar_map.html)
builds one target object per shard every time `_targets.R` is sourced,
so a scenario with very many shards (thousands: many datasets × large
`nsim` under a fine-grained `partition_by`) makes sourcing and the graph
tooling heavy, where dynamic branching’s single pattern node stays
light. Branching is an implementation detail *behind the shard
abstraction* — one Parquet per shard, identical per-task RNG,
partition-path linking, and replay contract either way — so a step whose
fan-out is large enough to hurt may opt into dynamic branching
(`tar_target(pattern = map(<step>_tasks))` over a grouped task-table
target) without changing anything else. Static is the default; dynamic
is the documented escape hatch for extreme fan-outs.

**What the targets lab validated.** The lab isolates each `targets`
behaviour as its own laptop-runnable toy pipeline; three findings from
the branching axes carry back here:

- **The static default holds, and its cheap-extension payoff is real.**
  A grow-the-pipeline spike bumps the variant count `4 -> 6` and
  re-runs: the existing four variants are skipped (their commands are
  unchanged and they never depended on the count), so extending costs
  *build the new variants + redo the fan-in*, not *redo everything*.
  That is precisely the path-axis-growth story this design leans on
  (§8.1) — confirmed, not assumed.
- **Static mints separately-named, addressable nodes.**
  [`tar_map()`](https://docs.ropensci.org/tarchetypes/reference/tar_map.html)
  produces N separately-named targets, each its own
  [`tar_read()`](https://docs.ropensci.org/targets/reference/tar_read.html)-addressable
  DAG node, matching the named-shard rationale above; dynamic branching
  instead tracks branches under hashed names with a downstream target
  that row-binds them automatically. The escape hatch works; it just
  trades addressability for a single light pattern node.
- **A shard is just a multi-row target value.** A sharding spike
  confirms a shard target is simply a target that returns many rows; the
  branching axes only add ways to *generate* such targets — so branching
  stays an implementation detail behind the shard abstraction, as
  claimed.

&nbsp;

       scenario   (plain construction-time object; carries seed, partition_by)
           │
           ├──▶ data_shards  ( 2 rows ; partition cols dataset,sim,replace)
           │         │
           │         ▼  tar_map(values = data_shards, names = c(dataset, sim, replace))
           │     data_step_*  ──▶ results/data/dataset=boron/sim=1/replace=FALSE/part.parquet  (2 named shard targets)
           │
           ├──▶ fit_shards   ( 4 rows ; partition cols dataset,sim,rescale; data upstream)
           │         │
           │         ▼  tar_map(values = fit_shards, names = c(dataset, sim, rescale))
           │     fit_step_*   ──▶ results/fit/dataset=boron/sim=1/rescale=FALSE/part.parquet   (4 named shard targets)
           │                  each shard reads the matching data shard by partition path
           │
           └──▶ hc_shards    ( 2 rows ; partition cols dataset,sim; fit upstream)
                     │
                     ▼  tar_map(values = hc_shards, names = c(dataset, sim))
                 hc_step_*  ──▶ results/hc/dataset=boron/sim=1/part.parquet                  (2 named shard targets, 4 tasks each)
                             each shard reads the matching fit shard(s) by partition path

       summary  ──▶ results/summary.parquet
                    (reads all three layers via duckplyr)

The link between steps is by **upstream partition path**: each shard row
carries the partition column values of the upstream shard it needs, and
the body opens that one upstream Parquet. Under static branching a shard
target can additionally name its specific upstream shard target for a
precise dependency edge; the partition path remains the portable
contract used for the off-cluster query layer (the Hive-style layout
below) and replay (§7). This is what lets tweaking `rescale` re-run
fit + hc without re-running data (the fit shard’s `data` partition
columns are unchanged).

`_targets.R` sketch:

``` r

library(targets)
library(tarchetypes)

# The scenario is a plain object built HERE, at sourcing time — not a
# tar_target. Everything downstream is a pure function of it, so the
# shard set is fixed before any target runs (static branching).
scenario <- ssd_scenario(
  ssddata::ccme_boron,
  nsim = 2L,
  nrow = c(5L, 10L),
  rescale = c(FALSE, TRUE),
  est_method = c("arithmetic", "multi"),
  nboot = 10,
  seed = 42L
)

# One row per shard, computed now (sourcing time). Each row carries the
# step's partition_by values (which become the tar_map target-name
# suffix and the Hive path) plus a list-column `tasks` of the task rows
# that shard must run.
data_shards <- ssd_scenario_data_shards(scenario)  # 2 rows
fit_shards  <- ssd_scenario_fit_shards(scenario)   # 4 rows
hc_shards   <- ssd_scenario_hc_shards(scenario)    # 2 rows (4 tasks each)

list(
  # tar_map mints one named target per shard at sourcing time. Independent
  # shard targets run in parallel; how they pack into Slurm jobs under
  # crew_controller_slurm() is an open question (§11).
  #
  # error = "null" so one bad shard does not abort the run: the target
  # goes NULL, the error is recorded, the rest keeps building and summary
  # unions the survivors (§6.2). format = "file" passes the shard path,
  # not its value, between targets (see below).
  tar_map(
    values = data_shards, names = c(dataset, sim, replace),
    tar_target(
      data_step,
      ssd_run_data_step(tasks, scenario, out_dir = "results/data"),
      format = "file", error = "null"
    )
  ),

  tar_map(
    values = fit_shards, names = c(dataset, sim, rescale),
    tar_target(
      fit_step,
      ssd_run_fit_step(tasks, scenario,
                       data_dir = "results/data",
                       out_dir  = "results/fit"),
      format = "file", error = "null"
    )
  ),

  tar_map(
    values = hc_shards, names = c(dataset, sim),
    tar_target(
      hc_step,
      ssd_run_hc_step(tasks, scenario,
                      fit_dir = "results/fit",
                      out_dir = "results/hc"),
      format = "file", error = "null"
    )
  ),

  tar_target(
    summary,
    ssd_summarise(dir_data = "results/data",
                  dir_fit  = "results/fit",
                  dir_hc   = "results/hc",
                  path     = "results/summary.parquet"),
    format = "file"
  )
)
```

`summary` reads the three result directories directly with duckplyr
rather than depending on each shard target — so it sees whatever shards
landed (survivors included, §6.2) and does not pull every shard’s value
back into R.
[`tarchetypes::tar_combine()`](https://docs.ropensci.org/tarchetypes/reference/tar_combine.html)
is the alternative fan-in when an in-memory bind is wanted instead.

A step body (sketch):

``` r

# `tasks` is the shard row's list-column of task rows (one per task in
# this shard), supplied by tar_map from fit_shards.
ssd_run_fit_step <- function(tasks, scenario, data_dir, out_dir) {
  rows <- purrr::pmap_dfr(tasks, function(task_id, primer, data_id,
                                          rescale, min_pmix, ...) {
    upstream <- duckplyr::read_parquet_duckdb(file.path(data_dir,
                                              # partition path from upstream cols
                                              .upstream_path(...),
                                              "part.parquet"))
    local_dqrng_state(scenario$seed, primer)
    tibble::tibble(task_id = task_id,
                   rescale = rescale,
                   min_pmix = min_pmix,
                   fit = list(fit_dists_state(upstream, rescale, min_pmix, ...)))
  })
  out <- file.path(out_dir,
                   .partition_path(tasks, scenario$partition_by$fit),
                   "part.parquet")
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  duckplyr::compute_parquet(rows, out)
  out
}
```

The body loops once per task in the shard, primes the RNG with that
task’s primer, calls the (state-less) `_state` primitive, and stacks K
result rows into one Parquet at the shard’s partition path. Each fit
shard opens the upstream data shard(s) its tasks reference — here a
single shard, but in general the **set** of parent shards a child spans
(§5/§6 Linking), read and filtered via duckplyr — so a fit shard never
depends on the whole data step. Under static branching that scoping is
computed at sourcing time: a fit shard target names its upstream data
shard target(s) (Option 3, §12 `task-tables`) for precise per-shard
edges, so adding new data shards mints new targets and leaves existing
fit shards untouched — no directory-hash indirection required to avoid
spurious rebuilds.

**Dependencies and what re-runs on a scenario-option change** (applied
to the 2/4/2 shard counts above; “1 shard re-runs” = its Parquet is
rewritten with the new task set):

| Scenario-option change | data_step (2 shards) | fit_step (4 shards) | hc_step (2 shards) | summary |
|----|----|----|----|----|
| dataset appended (path axis everywhere) | new shards only | new shards only | new shards only | re-run |
| `sim` value (= `nsim` grows; path axis everywhere) | new shards only | new shards only | new shards only | re-run |
| `nrow` value added (sub-trunc; not an axis) | cached (the draw is the fixed `nrow_max` setting, not `max(nrow)`) | new shards only (`nrow` ∈ fit path) | rewrite all (`nrow` ∈ hc inner) | re-run |
| `nrow_max` changed (sample setting) | rewrite all (the draw resizes) | rewrite all (per-child edge) | rewrite all | re-run |
| `replace` value added (data path; fit/hc inner under default) | new shards only | rewrite all (`replace` ∈ fit inner) | rewrite all (`replace` ∈ hc inner) | re-run |
| `rescale` value added (fit path; hc inner under default) | cached | new shards only | rewrite all (`rescale` ∈ hc inner) | re-run |
| `dists` change (fit setting, not an axis) | cached | rewrite all 4 | rewrite all 2 | re-run |
| `min_pmix` value (fit inner) | cached | rewrite all 4 | rewrite all 2 | re-run |
| `nboot` value added (hc inner) | cached | cached | rewrite all 2 | re-run |
| `est_method` value added (hc inner) | cached | cached | rewrite all 2 | re-run |
| `ci_method` / `parametric` added (hc inner) | cached | cached | rewrite all 2 | re-run |
| `seed` | re-run all | re-run all | re-run all | re-run |

Three steps as three targets is what makes this matrix possible: a
single combined branch (data + fit + hc) cannot cache a fit shard when
only `nboot` changes.

**Layout coherence across re-runs.** A shard’s Hive path depth and axes
are a function of `partition_by`/`bundle`, while the readers glob
`<step>/**/part.parquet` (depth-agnostic). So changing the split and
re-running into one fixed `results/` would leave shards of a *different
granularity* beside the new ones, and the glob would union stale and
current shards — double-counting, since the `<step>_id` identity is
`partition_by`-independent (one task lands in both the old coarse and
the new fine shard). Two complementary fixes, by driver:

- **Single-core (`ssd_run_scenario_shards`) — own the tree.** It clears
  each `<dir>/<step>` before writing, so the tree always matches the
  current layout. It has no cache to preserve, so clobbering is the
  simplest correct fix.
- **`targets` — per-layout root.** Each layout writes under
  `scenario_results_dir(scenario)` =
  `results/layout=<hash(partition_by)>`, so a split change yields a
  fresh root (never *mixed* with the old) while the same layout reuses
  its root (cache + `error = "null"` survivors preserved). Non-layout
  arguments (seed, grids) keep the root and just rewrite shard paths at
  the same depth (§8.1). Pruning an orphaned layout root and
  manifest-driven reads are deferred to
  `manifest`/`shard-atomic-rewrite`.

**Available for analysis:**

After
[`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html),
the three step layers are queryable independently via duckplyr without
going through `targets` (Hive-partitioned reads auto-discover the
partition columns from the path):

``` r
duckplyr::read_parquet_duckdb("results/data/**/*.parquet") |>
  dplyr::filter(dataset == "boron", sim == 1L) |> dplyr::collect()
duckplyr::read_parquet_duckdb("results/fit/**/*.parquet")  |> ...
duckplyr::read_parquet_duckdb("results/hc/**/*.parquet")   |> ...
```

Path-axis filters push down to the directory tree (no irrelevant
Parquets opened); inner-axis filters (e.g. `min_pmix == "default"`) push
down to row-group metadata but still need to open the relevant Parquets.

#### Hive-style layout for predicate pushdown

Shards are written into a **Hive-partitioned** directory tree keyed by
the step’s `partition_by` columns (defaults from §5):

       results/
         data/  dataset=boron/   sim=1/  replace=FALSE/  part.parquet
                dataset=boron/   sim=2/  replace=FALSE/  part.parquet
                dataset=cadmium/ sim=1/  replace=FALSE/  part.parquet
         fit/   dataset=boron/   sim=1/  rescale=FALSE/  part.parquet
                …
         hc/    dataset=boron/   sim=1/                  part.parquet
                …

duckplyr / DuckDB read the partition columns straight out of the
directory names and use them for predicate pushdown; a query like
`filter(dataset == "boron", sim == 1L)` only opens the relevant shards,
regardless of how big the rest of the tree grows. **Inner axes** (those
not in `partition_by`) are ordinary columns inside the Parquet — still
filterable, just not via path-level pushdown. The leaf file name is the
fixed string `part.parquet`; the Hive path uniquely identifies the
shard, so no hash in the name.

#### Linking between targets

Within the pipeline, `targets` wires the dependency at **sourcing
time**: the child branch for shard *S* depends on the **set** of
upstream shards its tasks reference — computed from the static shard
tables (the scenario is a plain sourcing-time object, §6) — and gets
their **paths passed in as function arguments** (the `format = "file"`
contract). No lookup-by-hash in the body. Because partitioning is
per-step, that set is in general **many-to-many** (a child shard spans
several parent shards; a parent shard feeds several children); the
single-upstream case is just the degenerate one. To keep invalidation
**per-shard** under m:n, the wiring computes each child branch’s
upstream target-name set at sourcing time and splices it in
(`tar_target(child, f(!!!syms(parents)))`) rather than gluing the whole
parent step together or globbing the parent directory blindly — see the
`task-tables` step (§12) and `shard-runner-baseline`, which proves the
same set-of-parents read+filter on one core. The partition layout above
also serves *downstream queries* and the debug replay (§7).

#### Why `format = "file"`, not the native Parquet store

`targets` ships a native Parquet store
(`tar_option_set(format = "parquet")`) that serialises each target’s
value to Parquet with no explicit write — the obvious first reach. The
targets lab’s Parquet spike ruled it out and works the reasoning through
in full: depending on such a target **eagerly deserialises its value
back into R**, even when the downstream body only wants the store path
(a probe confirmed the loaded data frame is present even when only the
path is used), so it does *not* avoid the R roundtrip; and the store
layout (`_targets/objects/<name>`, no extension) is not a stable
contract to read against from outside `targets`. `format = "file"` is
the seam that actually passes storage between targets without the
roundtrip: the target’s value *is* the Parquet path, so a downstream
branch receives a string and hands it straight to duckplyr / the
uploader. That is why every step target here — and the `summary` and
`upload_<step>` targets — is `format = "file"`.

### 6.1 Cloud upload hook

The data/fit/hc shards are the user-facing artefacts and they need to be
readable **from outside the cluster** — analysis notebooks on a laptop,
dashboards, downstream R/Python scripts.

> **Implemented** (the `cloud-upload` capability) with three departures
> from the original sketch below: the destination is a **runner
> argument** of `ssd_scenario_targets(scenario, ..., root, upload, cue)`
> (the sibling of `root`), **not** a `scenario` field; it is a **typed,
> self-validating** object rather than an untyped list; and absent Azure
> credentials **fail loud** rather than silently dry-running. See the
> realised contract immediately below; the historical sketch follows for
> context.

**The destination is a typed runner argument.** Two constructors return
plain, serialisable, classed S3 objects carrying **only** the
destination (never credentials):
`ssd_upload_azure(url, container, ..., prefix = NULL, domain = "blob.core.windows.net")`
(class `c("ssdsims_upload_azure_blob", "ssdsims_upload")`; the storage
**account** is derived from `url`, and an optional `prefix` writes the
shards under a subdirectory of the container) and
[`ssd_upload_dryrun()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_azure.md)
(class `c("ssdsims_upload_dryrun", "ssdsims_upload")`), validated at
construction. They are passed by name to the factory — `upload = NULL`
(default; **no** `upload_<step>` nodes),
[`ssd_upload_dryrun()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_azure.md)
(no-op nodes, exercised offline/in CI), or `ssd_upload_azure(...)` (ship
to Azure). Four generics dispatch on the object’s class:
[`ssd_test_upload()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_test_upload.md)
(the front-door creds/connectivity probe, the user’s explicit preflight
— the factory never runs it), `ssd_upload_shard(path, upload)` (ship one
shard), `ssd_open_uploaded(upload, step)` (read the uploaded results
back in place), and `ssd_summarise_uploaded(upload, step, drop_samples)`
(the in-place fan-in summary, the cloud counterpart of
[`ssd_summarise()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_summarise.md)).
A new backend (S3/GCS) is a constructor plus those four methods — no
edit to existing methods.

**Fail loud on absent credentials.** Azure with missing credentials
**errors** (naming the missing `SSDSIMS_AZURE_*` variable), at probe
time and as a per-shard backstop — never a silent no-op. Intent to skip
the network is expressed only by
[`ssd_upload_dryrun()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_azure.md).

**Upload is its own target, not an inline side effect.** When `upload`
is non-NULL the factory adds one `upload_<step>` target per shard,
paired with its step target by the same `tar_map`:

``` r

tar_map(
  values = fit_shards, names = c(dataset, sim, rescale),
  tar_target(fit_step, ssd_run_fit_step(tasks, scenario, ...),
             format = "file", error = "null"),
  tar_target(upload_fit, ssd_upload_shard(fit_step, upload),
             format = "file", error = "null")   # fit_step = the shard path
)
```

The targets lab’s Azure spike showed why this beats pushing the blob
from inside `ssd_run_<step>_step()` right after the local write:

- **Content-hashing skips unchanged shards.** Because `upload_<step>`
  takes the shard’s path (`format = "file"`) as input, `targets` re-runs
  it only when that shard’s content hash changes. A re-driven
  [`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html)
  that rebuilt nothing uploads nothing; a partial extension uploads only
  the new/rewritten shards. (A combined lab pipeline composes this with
  `error = "null"` and a pinning cue: fixing one failed branch becomes a
  *minimal* re-upload, not a full redo.)
- **The graph builds and dry-runs with no credentials — explicitly.**
  Passing
  [`ssd_upload_dryrun()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_azure.md)
  gives `upload_<step>` nodes that no-op (reach no network and return
  the local path), so the same DAG shape runs offline and in CI without
  credentials. This is now an **explicit opt-in**, not a silent fallback
  on absent credentials (see *fail loud* above): `ssd_upload_azure(...)`
  with missing credentials **errors**.
- **Concerns stay separated by dependency.** The compute manifest
  depends only on the shard targets (what was produced); the upload
  manifest depends on the `upload_<step>` targets (what was shipped). A
  reader that needs only uploaded data depends on the latter.

**Read the shards back in place, no download.** The same Azure spike
closes the loop on the off-cluster read story: a query step reads the
uploaded blobs **in place on Azure** with DuckDB’s `azure` extension (a
Hive-glob `az://…/<partition>=*/part.parquet`) — the analysis layer
never downloads the Parquet, it predicate-pushes straight against blob
storage. So the Hive partition layout (above) pays off identically
whether the shards sit on the cluster filesystem or in the object store.

Per-shard flow when `upload` is non-NULL:

       <step>_step branch  ──▶ results/<step>/<partition-path>/part.parquet
            │                  (local shard; targets tracks it, format = "file")
            ▼
       upload_<step> branch ──▶ <url>/<container>/<step>/<partition-path>/part.parquet
                                (own target; runs only if the shard hash changed;
                                 with ssd_upload_dryrun() it no-ops, never touching
                                 the network)

The local shard stays on disk so `targets`’ `format = "file"` tracking
is unaffected; the cloud copy is an additional artefact.

**Auth is external.** The storage **account** is derived from the
destination `url` (not an environment variable); the **secret** comes
from the environment (one of `SSDSIMS_AZURE_STORAGE_KEY`,
`SSDSIMS_AZURE_STORAGE_SAS`, or the service-principal trio). The upload
object does **not** carry secrets — it carries only the destination
(URL, container, optional `prefix`/`domain`, and the account derived
from `url`). A missing secret is a **loud error** (naming the missing
variable), not a silent no-op — skipping the network is
[`ssd_upload_dryrun()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_azure.md)’s
job.

**Connectivity probe — the user’s explicit preflight.**
`ssd_test_upload(upload)` performs a minimal round-trip (list the
container, write and delete a small marker blob) and either returns
silently or errors with the backend’s diagnostic — resolving the
credentials first and aborting, naming the missing `SSDSIMS_AZURE_*`
variable, when one is absent. It is the one-liner the user runs at the
prompt before
[`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html)
to answer “are my credentials in the right place?”. The factory
[`ssd_scenario_targets()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_targets.md)
**does not** run it: `_targets.R` is re-sourced by every `targets`
operation
([`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html),
but also
[`tar_manifest()`](https://docs.ropensci.org/targets/reference/tar_manifest.html),
[`tar_visnetwork()`](https://docs.ropensci.org/targets/reference/tar_visnetwork.html),
[`tar_outdated()`](https://docs.ropensci.org/targets/reference/tar_outdated.html))
and on **every worker** in a `crew`/cluster run, so probing in the
factory would fire a credential resolution and marker-blob round-trip on
each of those — surprising network I/O on every graph inspection and N×
on a cluster, and a requirement that workers carry credentials merely to
*source* the pipeline. The factory therefore stays a pure,
side-effect-free assembler; fail-loud is preserved by the backstop in
[`ssd_upload_shard()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_shard.md)
(a missing credential aborts that shard’s upload branch at upload time,
leaving the rest shipping under `error = "null"`).

``` r

upload <- ssd_upload_azure(url = "https://<acct>.blob.core.windows.net",
                           container = "ssdsims-results")
ssd_test_upload(upload)   # preflight: silent on success, throws (naming the missing var) on failure
ssd_scenario_targets(scenario, upload = upload)   # pure: builds the target list, no network I/O
```

**Failure mode.** A per-shard upload error becomes that `upload_<step>`
branch’s error (and, under `error = "null"`, leaves the rest uploading);
the local shard remains, so
[`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html)
can be re-driven and only the failed uploads retried.

**No upload hashing for now.** The upload path records **no** sha256 — a
shard is shipped and read back **in place**
([`ssd_open_uploaded()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_open_uploaded.md)
/
[`ssd_summarise_uploaded()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_summarise_uploaded.md)),
with the row data itself the round-trip check. Hash-based
transfer-corruption detection (an upload sha256, or shipping the
manifest’s per-shard `meta.json` sidecar alongside the blob) is
**deferred** together with the parked `manifest` concept, to be
revisited when the manifest is picked up (the `manifest-revival` task in
`ROADMAP.md`).

### 6.2 Surviving a failed shard

A single bad shard must not abort an N-shard cluster run, and a single
bad *task* must not lose the rest of its shard. The design splits those
into two concerns — a tolerant producer and a separate completeness
gate:

- **The shard body writes as many rows as ran successfully.** Each shard
  runs its K bundled tasks, keeps the survivors, and writes them to the
  one Parquet; a task that fails is simply absent. A partial failure
  therefore yields a *shorter* shard, not an error, so the step target
  fails only in *exceptional* circumstances (the upstream shard is
  unreadable, the worker OOMs, a package won’t load). It carries at most
  a light `error = "null"` for those catastrophic cases — the ordinary
  partial-failure path no longer depends on it. `summary` unions
  whatever rows did land.
- **A downstream assertion target makes the shortfall visible.** Sibling
  to `upload_<step>` (§6.1), each shard gets an `assert_<step>` target
  that reads the shard file’s row count and compares it to the
  **expected** count — a pure function of the scenario’s task table,
  known when `_targets.R` is sourced and read from the Parquet *footer*
  (no scan). It errors (goes red) on any mismatch, so completeness is a
  first-class, coloured DAG node: green = full shard, red = short shard,
  queryable via
  [`tar_meta()`](https://docs.ropensci.org/targets/reference/tar_meta.html)
  and visible in
  [`tar_mermaid()`](https://docs.ropensci.org/targets/reference/tar_mermaid.html).
  `assert_<step>` itself carries `error = "null"` so one red assert does
  not abort the build. The expected count also catches *silent* shape
  bugs (a join that drops rows), not just task failures.

So “see partial shard failures clearly” is the assert layer’s job; “keep
going despite them” is the tolerant body’s. An errored *shard* (the
exceptional case) is always out of date, so a re-driven
[`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html)
after a fix retries it (§8.4); a short shard is found by its red
`assert_<step>` (also §8.4).

**Keep-going is the pipeline default (`make -k`), and it is layered.**
The shipped `_targets.R` templates set
`tar_option_set(error = "continue")` — the `make -k` analogue — so a
target that errors skips only its own dependents while every other
reachable shard still builds; one bad branch never tears down a long
parallel run. (It is a pipeline-wide *option*, so it sits in
`_targets.R` next to the controller block, not in the
[`ssd_scenario_targets()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_targets.md)
factory — which owns the *per-target* settings instead.) The shard,
`assert_<step>`, and `upload_<step>` targets carry the stronger
per-target `error = "null"`, which is *more* permissive than `make -k`:
the errored target’s value becomes `NULL` and its downstream still runs
(a short/`NULL` shard flows on and is reported, above), where plain
`make -k` would skip it. The two settings compose cleanly — a per-target
`error` overrides the global default — so the global `continue` only
governs the targets that carry no explicit override (e.g. an unexpected
error in `summary` or a user-added target skips its dependents instead
of aborting the run). Guards that *should* abort the whole run — the
upload/cluster pre-flight (§6.1, §4) — deliberately live **outside** the
DAG, in a separate pre-flight script the **user runs** before
[`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html)
(a documented pre-condition, not a target the pipeline enforces), so the
keep-going default never swallows them. The trade we accept: under
keep-going a *systemic* failure (every shard red because a package won’t
load) does not stop the pipeline on its own — but the operator watches
the first few minutes, where a wall of red surfaces immediately, so
optimising for *not losing a long run to one bad branch* beats
stop-on-first-error. Override with an explicit
`tar_option_set(error = "stop")` in `_targets.R` if a hard stop is ever
wanted.

Two constraints, both confirmed by the targets lab’s failure spike:

- **Errors are read *after* the run, not inside it.** A target cannot
  call
  [`tar_meta()`](https://docs.ropensci.org/targets/reference/tar_meta.html)
  (or other store functions) while the pipeline is running. So `summary`
  detects a gap from the missing shard / `NULL` value *inside* the DAG,
  and the error *messages* are pulled from metadata once
  [`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html)
  returns:

  ``` r

  tar_make()
  tar_meta(fields = "error") |> dplyr::filter(!is.na(error))
  ```

- **The end-of-pipeline warning needs handling, not silencing.** Even
  with `error = "null"`,
  [`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html)
  emits a “some targets errored” warning at the end; under
  `options(warn = 2)` that becomes an abort. The runner wraps
  [`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html)
  in a [`withCallingHandlers()`](https://rdrr.io/r/base/conditions.html)
  that muffles only that one expected warning rather than relaxing the
  global option.

**A short or `NULL` shard must flow cleanly downstream.** A shorter
shard is still a valid Parquet, so `upload_<step>` ships it and the
manifests record it as normal; only its `assert_<step>` goes red. In the
exceptional case where the whole shard errors its value is `NULL`, and
the immediate-downstream targets — `upload_<step>`, `assert_<step>`, and
the two manifests — must tolerate that rather than choke:
`ssd_upload_shard(NULL, ...)` records a skip, `assert_<step>` treats a
missing file as zero rows (red), and the manifests omit it. A combined
lab pipeline composed a construction-time-sized fan-out (static
branching), this `error = "null"` handling, and the per-shard upload
target, and found this tolerance in the upload and manifest helpers was
the *only* glue the composition needed — and that once the failing
branch is fixed under the §8.3 pin, only that one shard rebuilds,
re-uploads, and is re-queried (a minimal re-run, not a full redo).

**Why split the producer from the assertion.** Folding the row-count
check into the shard body would make a short shard *error*, which under
`error = "null"` discards its hard-won survivor rows and, under the §8.3
pin, would also force a rebuild on the next
[`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html).
Keeping the check in a separate downstream target lets the survivors
persist and stay queryable while the red `assert_<step>` flags the gap —
completeness is *reported*, not *enforced by destruction*. The cost is
that an errored downstream assert does **not** invalidate its upstream
shard, so refreshing a short shard is a deliberate step, not automatic;
§8.4 shows how the runner drives that from the red-assert set.

------------------------------------------------------------------------

## 7. Debugging a cluster failure

The targets/tarchetypes layer is an abstraction, and abstractions cost
debuggability. The cluster pipeline is only debuggable if any single
failed branch can be replayed **outside targets, on any machine**, with
the same inputs and the same RNG state.

The state-only primitives `slice_sample_state()`, `fit_dists_state()`,
`hc_state()` are the contract that makes this work. Each takes its
inputs as plain values — data, a `(seed, state)` pair, scalar params —
so the **failing task’s row plus the immediate upstream shard is a
complete reproducer**, even when the failing branch processed many other
tasks alongside it.

### Scenario

[`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html)
against `crew_controller_slurm()` fans out N hc shards; three branches
error on remote workers:

       targets reports:
         ✗ hc_step_3ab9c7   (slurm-worker-12)
         ✗ hc_step_5fe201   (slurm-worker-04)
         ✗ hc_step_91da33   (slurm-worker-21)

The user wants the failing tasks reproduced locally, fast, without
re-running every other shard.

### Recipe

       1. Identify the failing task row.
          ──────────────────────────────
          # Find the task that errored within the failing branch:
          # branch error message names the shard's partition path;
          # within the shard the task_id of the bad row is in the log.
          task <- tar_read(hc_tasks) |> dplyr::filter(task_id == "3ab9c7")

          The row carries:
            - primer    (length-2 integer = c(hi31, lo31), §2)
            - all hc params (nboot, est_method, ci_method, ...)
            - upstream fit-shard partition values (dataset, sim, rescale)
              — enough to reconstruct results/fit/dataset=.../sim=.../rescale=.../part.parquet

       2. Locate the upstream shard by partition path.
          ────────────────────────────────────────────
          results/fit/dataset=<...>/sim=<...>/rescale=<...>/part.parquet
          The immediate upstream is enough; no need to walk further.

       3. Sync to local.
          ──────────────
          rsync cluster:_targets/objects/hc_tasks   ./_targets/objects/
          rsync cluster:<upstream_path>             ./<upstream_path>

       4. Reproduce, no targets involved.
          ───────────────────────────────
          fit <- duckplyr::read_parquet_duckdb(<upstream_path>) |>
                 # the fit shard may contain multiple fit task rows;
                 # pick the one this hc task points to.
                 dplyr::filter(task_id == task$fit_id) |>
                 dplyr::pull(fit) |> .subset2(1L)
          options(error = recover)        # or place browser() in hc_state
          out <- ssdsims:::hc_state(
            data       = fit,
            seed       = scenario$seed,
            state      = task$primer[[1]],
            nboot      = task$nboot,
            est_method = task$est_method,
            ci_method  = task$ci_method,
            proportion = scenario$hc$proportion,
            ci         = scenario$hc$ci,
            parametric = task$parametric,
            save_to    = NULL
          )

The call is the same code that ran on the worker; the `(seed, state)`
pair is the same; the upstream Parquet is the same bytes. A
deterministic bug reproduces on the first call.

### Helper

A single helper compresses steps 1, 2 and 4:

``` r

ssd_replay_task(task_id, store = "_targets", results_dir = "results")
```

infers the step from the task table the id sits in, opens the upstream
shard at the right partition path, filters it down to the one upstream
task row, and calls the matching `_state` primitive with the right args.
The state-only primitives are the supported task-replay API.

### What makes this work

       ┌───────────────────────────────────────────────────────────────────┐
       │  task row + upstream shard = complete reproducer                  │
       │  ─────────────────────────────────────────────                    │
       │  seed       scenario-scalar integer; in the manifest              │
       │  primer     length-2 integer (hi32, lo32); on the task row;       │
       │             passed to dqset.seed()'s `stream` arg                 │
       │  upstream   one shard per partition cell; keyed by Hive path;     │
       │             tooling-agnostic (DuckDB, Python, R)                  │
       │  params     scalars on the task row                               │
       │  primitive  `*_state()` takes (data, seed, state = primer, ...);  │
       │             no hidden dependency on targets or the orchestrator   │
       └───────────────────────────────────────────────────────────────────┘

`tarchetypes` is just the orchestrator. Removing it from the reproducer
is a feature, not a workaround.

### Lightweight reproduction: skip the rsync, verify the inputs

If the prefix of the pipeline is cheap to re-run, the recipe above
collapses: drive a local
[`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html)
up to the failing target and skip step 3 entirely. The catch is
**verifying that the locally regenerated upstream matches what the
cluster’s failed branch actually consumed** — without that, you might be
debugging a phantom.

The mechanism is per-shard content-hashing (sha256). Every shard the
cluster writes is fingerprinted with `sha256` and the value is stored in
the cluster run’s manifest, keyed by the shard’s partition path, before
any upload happens:

``` r

manifest$completed_shards[["fit/dataset=boron/sim=1/rescale=FALSE"]]
#> "8c92…"  (sha256 of part.parquet at write time)
```

Local recipe:

       1. tar_make() locally up to fit_step, then look at the
          regenerated upstream shard at its partition path.
       2. local_hash   <- digest::digest(file = <upstream_path>, algo = "sha256")
          cluster_hash <- cluster_manifest$completed_shards[[<partition_key>]]
          stopifnot(identical(local_hash, cluster_hash))
            ✓  the cluster's input is reproduced byte-for-byte;
               continue to step 4 of the rsync recipe.
            ✗  upstream is host-dependent (BLAS, system libs, env);
               fall back to rsync.
       3. With inputs verified, replay the failing hc_state() call
          directly (no targets involved) -- same as the rsync recipe.

Same primitive (`hc_state(data, state, ...)`); the only thing the two
recipes differ on is **where** the upstream shard came from. Hash
verification is the bridge.

### Constraints

- The bug must be deterministic in `(data, state, params)`. Non-
  determinism from BLAS, system libraries, or untracked global state is
  out of scope; capture
  [`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html) per-branch
  alongside the Parquet to narrow it.
- Content-addressing must be host-independent: the shard’s partition
  path is derived from task-table column values, not from any host-local
  state, so the same task on cluster and laptop resolves to the same
  shard path.
- The manifest must record `completed_shards` (partition path → sha256).
  Without it, lightweight reproduction can’t be verified.
- Only the immediate upstream is required. To debug `hc`, pull or
  regenerate the one `fit` shard; you do not have to refit upstream of
  that. To debug `fit`, the one `data` shard; you do not have to
  resample.

Once the bug is fixed, the natural follow-up is to lock in the surviving
N−3 shards and re-run only the 3 failures despite the code change — see
§8.4 ([`unlink()`](https://rdrr.io/r/base/unlink.html) the bad
Parquets +
[`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html)),
or §8.3 (`tar_cue(depend = FALSE)` to pin the survivors against the
edit) depending on the scope of the change.

------------------------------------------------------------------------

## 8. Extension

Shards are the cache unit. The shard’s identity is its **partition
path** (`results/<step>/dataset=.../sim=.../…/`), and its contents are
the rows of the step’s task table that share those partition column
values. Extension is the answer to one question per extension type:
*does the new scenario change the set of shards, or the contents of an
existing shard?*

> **Deferred decision — explicit `data` step vs. inline
> [`head()`](https://rdrr.io/r/utils/head.html) in `fit`.** The §8
> examples below are written for the four-step model and still reference
> a materialised `data` shard. The implemented baseline currently
> *folds* that RNG-free `head(sample, nrow)` truncation into `fit`
> (`task-list-loop-baseline-fold`), so there is no `data` shard today.
> Whether to reinstate an explicit `data` step is **left open until the
> invalidation model here is finalised** (`task-tables` /
> `hive-partitioning`), because the answer depends entirely on that
> model: - Under **cache-by-existence** (this section’s model), a `fit`
> shard is keyed by `fit_id`, which includes `nrow`. Extending `nrow`
> only mints new `fit` shards and leaves existing ones cached — the fold
> is sufficient and a `data` shard would be redundant. - Under
> **content-hash invalidation** (targets’ native default), `fit` depends
> on the `sample` *value*; growing `n_max` (e.g. extending `nrow`
> upward) changes that value and cascades a rerun into every `fit`
> branch — even those whose `head(sample, nrow)` is byte-identical. A
> materialised `data` step is then a *buffering checkpoint*:
> `data_n = head(sample, n)` recomputes byte-identically, so targets
> prunes the expensive `fit` rerun. It is also the natural place to
> handle the dual hazard that, under cache-by-existence, growing `n_max`
> does **not** invalidate the `sample` shard (keyed
> `dataset, sim, replace`), risking a stale short draw.
>
> When `task-tables`/`hive-partitioning` pin the invalidation model,
> revisit this and either keep the fold or restore the `data` step — and
> reconcile the four-step examples below at the same time.

### 8.1 Path-axis growth — new shards, existing shards untouched

Adding a value to an axis that is **in `partition_by`** for a step
creates new partition cells and therefore new shards; existing shards’
Parquets stay byte-identical and are reused. No bookkeeping needed
beyond `targets`’ usual branch-level dependency tracking. This
minimal-rebuild-on-growth property is asserted end-to-end by the
`path-axis-growth` step (§12).

| What you added | Path axis for… | Effect |
|----|----|----|
| new dataset name | data, fit, hc | new shards for that dataset; rest cached |
| `nsim` grows | data, fit, hc | new shards for the added `sim` values |
| `replace` value (default `partition_by`) | data only (path); fit/hc (inner; if `replace` not in their path) | new data shards; fit/hc not affected unless they include `replace` in path |
| `rescale` value (default) | fit only (path) | new fit shards; data cached; hc rewrites (rescale is hc *inner*) — see §8.2 |

### 8.2 Inner-axis growth — atomic shard rewrite

Adding a value to an axis that is **not in `partition_by`** for a step
changes the task-row set of every shard in that step’s partition tree
(each shard now has one more row’s worth of tasks). Each affected shard
is **atomically rewritten** with the new task set: targets sees the
grouped task table change, marks those branches stale, the body re-runs
all K tasks in each shard and overwrites the Parquet.

Per-task RNG identity is preserved (each task’s primer is unchanged), so
the *rows that were there before* come out byte-identical to the
previous Parquet — but the file itself is new. There is no in-place
append: one Parquet per shard, rewritten as a whole. Trade-off accepted
(Q2 in the design dialogue): simpler cache semantics in exchange for
redoing the work of every task already in the shard.

**Whether `summary` rebuilds depends on the bytes, not the rewrite.**
The targets lab’s invalidation spike confirmed `targets` propagates on
*value*, not on the act of rebuilding: a recomputed target whose value
is byte-identical leaves its dependents skipped. Here the shard *did*
gain a row, so its Parquet bytes change and `summary` (which reads the
shard) re-runs. But the corollary matters for §8.4 and for any no-op
rewrite: if a rewrite reproduced the exact same bytes, `format = "file"`
content-hashing would see no change and `summary` would not rebuild.
Byte-stable Parquet writes (pinned `duckplyr`/DuckDB, deterministic
column order) are what make that hash comparison meaningful.

| What you added | Inner axis for… | Cost |
|----|----|----|
| new `min_pmix` name | fit | rewrite all fit shards (was K tasks each → now K+1) |
| `dists` change | fit (setting, not an axis — content rewrite) | rewrite all fit shards |
| `nboot` value | hc | rewrite all hc shards |
| `est_method` value | hc | rewrite all hc shards |
| `ci_method` / `parametric` | hc | rewrite all hc shards |

**To avoid the rewrite cost**, move the *axis* into `partition_by` for
that step. The trade is shard count vs. cache reuse: pushing an axis
into the path means future growth on that axis adds new shards instead
of rewriting old ones, at the cost of producing more (smaller) Parquets
up front. This escape hatch applies only to the genuine inner *axes*
(`min_pmix`, `nboot`, `est_method`, `ci_method`, `parametric`): they are
in `task_axes(step)`, so they can be promoted to path axes. `dists` is
in the table for its cost, not its category — it is a fit-level
**scenario setting**, not an axis, so it cannot be a partition level and
its rewrite is intrinsic (changing `dists` re-fits the *contents* of
every fit task, rather than adding a row).

### 8.3 Pinning shards despite a code change — `tar_cue(depend = FALSE)`

The user edited an `_state` function (or `ssdtools` ticked over a
version); `targets`’ own hash-graph would now flag every dependent
branch as out-of-date, forcing a re-run the user does *not* want. The
question is the **opposite** of invalidation — how to keep `targets`
from re-running things whose shards are still trusted.

This stays inside the one project. The targets lab’s invalidation spike
settled it: `tar_cue(depend = FALSE)` does exactly this. It tells a
target to ignore changes to its upstream dependencies — including the
function bodies it calls — so editing an `_state` function (or bumping
an `ssdtools` version) rebuilds *nothing*:

``` r

tar_map(
  values = fit_shards, names = c(dataset, sim, rescale),
  tar_target(
    fit_step,
    ssd_run_fit_step(tasks, scenario, data_dir = "results/data",
                     out_dir = "results/fit"),
    format = "file", error = "null",
    cue = tar_cue(depend = FALSE)   # pin against upstream code edits
  )
)
```

`tar_invalidate(names = ...)` still forces a rebuild of the specific
shards the user *does* want refreshed — it deletes the metadata the cue
compares against, overriding the pin per-target (§8.4). So the
mixed-code case — keep the trusted shards, recompute a few under the new
code — is two single-project moves: pin everything with the cue, then
[`tar_invalidate()`](https://docs.ropensci.org/targets/reference/tar_invalidate.html)
the handful to refresh. No second project, no `parent` reference, no
cross-project file-input wiring.

(`tar_cue(depend = FALSE)` pins against *dependency/code* changes only;
the target still rebuilds if its own `format = "file"` output goes
missing or if its task-table grouping changes — which is what makes
path-axis and inner-axis growth in §8.1–§8.2 still work while the pin is
in place. The lab also found one case the pin does **not** hold: an
already-errored target reruns regardless, so a shard left broken under
`error = "null"` retries on the next
[`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html)
even when pinned — exactly what you want for the §8.4 fix-and-refresh
case.)

### 8.4 Forced re-run after a code fix

The dual case: a bug in `hc_state()` is fixed; the user wants the
*buggy* shards rewritten under the patched code. Partition paths are
unchanged (the fix doesn’t alter `partition_by` columns), so file
existence still says “cache hit”. Easiest move:

``` r

unlink(failed_shards)        # the specific Parquets to refresh
tar_make()                   # targets sees the missing files, re-runs only those
```

`targets::tar_invalidate(names = failed_branch_names)` is the
bookkeeping-preserving alternative — same end state, and the same call
that overrides a §8.3 pin on the shards you do want refreshed. Either
way the patched code produces different bytes, so the rewritten shards’
content hashes change and `summary` rebuilds from them
(value-propagation, §8.2); the shards left alone stay cached.

**Finding the set to refresh — the `assert_<step>` targets.** The hard
part of this workflow used to be *naming* `failed_branch_names`: a short
shard from a partial failure (§6.2) looks up to date to `targets`, so a
code fix under the §8.3 pin will not recompute it on its own. The
completeness asserts turn that list into a query — the short shards are
exactly the red `assert_<step>` targets:

``` r

short <- tar_meta(fields = "error") |>
  dplyr::filter(!is.na(error), startsWith(name, "assert_"))
tar_invalidate(names = sub("^assert_", "", short$name))  # the shard targets
tar_make()                                                # only those recompute
```

The runner closes this loop automatically:
[`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html),
read the red asserts,
[`tar_invalidate()`](https://docs.ropensci.org/targets/reference/tar_invalidate.html)
their shard targets,
[`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html)
again — a two-pass refresh that needs no hand-maintained shard list and
works through the §8.3 pin
([`tar_invalidate()`](https://docs.ropensci.org/targets/reference/tar_invalidate.html)
overrides it). A shard whose shortfall is *deterministic* (a task that
errors on every run) stays red and is the signal to investigate, not to
loop on; deciding when to stop re-running is the runner’s policy, not
the pipeline’s.

### 8.5 Manifest contents

Per-scenario manifest (a small JSON sidecar to the results directory):

- `seed` — scenario’s RNG root (§2).
- `datasets` — name vector referenced from tasks (§1.1).
- `min_pmix` — name vector ditto.
- `fit`, `hc` — the argument-vector grids.
- `partition_by` — the per-shard path axes (§5).
- `completed_shards` — set of shard partition paths whose Parquet exists
  and is trusted, with each shard’s sha256 (the trusted-as-produced
  value, recorded at write time in a per-shard `meta.json` sidecar; see
  §6.1, §7). No separate cloud-copy sha256: the sidecar is uploaded with
  the shard and a download is verified by re-hashing against this sha256
  (§6.1).
- `r_version`, `dqrng_version`, `ssdtools_version` — versions pinned for
  bit-stability across re-runs (§9).

Restart property for dqrng (same `(seed, state)` ⇒ same draw sequence)
is verified by `scripts/experiment-dqrng-hash.R`.

### 8.6 The incremental loop — run, assemble, expand, re-assemble

The pieces above compose into one repeatable cycle: **run a scenario,
assemble, expand the scenario, run only the missing pieces, assemble
again** — with no redundant recomputation. It works because every step
is a pure function of the *current* state, never an accumulation of
edits:

1.  **Run.**
    [`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html)
    writes one Parquet per shard under the scenario’s layout root (§5).
    Path-axis growth and inner-axis rewrite (§8.1–§8.2) are exactly the
    rules that decide which shards a *later* run rebuilds.
2.  **Assemble.**
    [`ssd_assemble_manifest()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_assemble_manifest.md)
    walks the results tree and rebuilds `completed_shards` from whatever
    Parquets exist (§8.5), preferring each shard’s `meta.json` sidecar
    and hashing the rest. It reads the manifest head left by
    [`ssd_write_manifest()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_write_manifest.md)
    and replaces only the tail.
3.  **Expand.** Edit the scenario object — append a dataset, grow
    `nsim`, add a `min_pmix`, widen `nrow` — and re-source `_targets.R`.
    The shard set is re-derived from the expanded scenario at sourcing
    time (§6 static branching).
4.  **Run the missing pieces.**
    [`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html)
    again. Path-axis growth mints and builds *only* the new shards and
    skips the rest (cache-by- existence, §8.1); inner-axis growth
    atomically rewrites *only* the affected shards (§8.2). Nothing else
    recomputes.
5.  **Assemble again.**
    [`ssd_assemble_manifest()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_assemble_manifest.md)
    re-walks the tree. Because it rebuilds the tail from disk every
    call, the second assembly is **idempotent and monotone under
    growth**: the new shards appear, the untouched ones keep their
    (byte-identical) entries, and the result is their union — no merge
    bookkeeping, no stale entries to prune.

**Two ordering facts make the loop safe.**

- *Write the head before you assemble.* The head is a pure function of
  the scenario (§8.5), and
  [`ssd_write_manifest()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_write_manifest.md)
  rewrites the *whole* file with the head alone — it does not preserve a
  previous tail. So after an **expansion** the head must be re-written
  from the *expanded* scenario (otherwise it still describes the smaller
  grid), and the assemble must follow it to rebuild the tail from disk.
  The runner’s contract is therefore *write-head-then-assemble-tail* on
  every (re-)run: the head always matches the current scenario, the tail
  always matches the shards on disk, and the two cannot drift across
  runs (the within-run case is §6 static branching).
- *Growth keeps the layout root; re-layout starts a fresh tree.* The
  results root is keyed by `partition_by` (`layout=<hash>`, §5), so
  growing along existing axes accretes into the *same* tree and reuses
  its cache and manifest. Changing `partition_by` itself is not growth —
  it mints a new layout root with its own (initially empty) manifest,
  leaving the old tree intact rather than migrating it.

------------------------------------------------------------------------

## 9. Limitations

Constraints the design lives with rather than solves.

### No nested reuse of ssdtools’ inner `dists` / `nboot` loops

`dists` controls *which* distributions
[`ssdtools::ssd_fit_dists()`](https://bcgov.github.io/ssdtools/reference/ssd_fit_dists.html)
fits to a given data slice; `nboot` controls how many bootstrap
iterations
[`ssdtools::ssd_hc()`](https://bcgov.github.io/ssdtools/reference/ssd_hc.html)
runs. Both loops live *inside* ssdtools and are not exposed at the
ssdsims level, so neither nesting —
`c("lnorm", "gamma") ⊂ c("lnorm", "gamma", "llogis")`,
`nboot = 100 ⊂ nboot = 1000` — can be reused incrementally: a larger run
re-fits or re-draws from scratch rather than extending the cached
smaller one. Exploiting the nesting would require either (a) wrapping
each per-distribution / per-bootstrap iteration in ssdsims with its own
dqrng stream and aggregating, or (b) an ssdtools change to expose the
inner loops. Sketch only; out of scope.

The two scenario options sit on **opposite** sides of the axis/setting
split (GLOSSARY.md), and their cache behaviour differs accordingly — the
heading is *not* “neither is an axis”:

- **`dists` is a fit-level *scenario setting*, not an axis.** It is the
  **union** of the declared distribution sets — a single character
  vector applied uniformly to every fit task (one model-averaged
  `ssd_fit_dists()` call per task); it is absent from
  `task_axes("fit")`, so it never enters a **primer** or a
  **partition**. Because a `dists` vector defines *one* model-averaged
  fit, fanning out per-distribution would change the science, so it is
  deliberately not an axis. Editing it (e.g. adding a distribution to
  the union) changes the content fed to every fit task and re-fits the
  whole slice — the “rewrite all fit shards” row of §8 — with no way to
  fit only the added distribution and merge.

  > **Addendum (`distset-hc-axis`).** *Individual distributions still
  > never fan out* — every axis value is a complete averaging pool, so
  > the model-averaging science this decision protects is intact. What
  > changed is that a **named set of pools**
  > ([`ssd_distset()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_distset.md))
  > is now an **hc-level** axis (`distset` ∈ `task_axes("hc")`) over
  > *post-fit subsets of one union fit*: the fit step fits the union
  > once, and each hc cell
  > [`subset()`](https://rdrr.io/r/base/subset.html)s that fit to its
  > pool’s members (`strict = FALSE`) and re-averages. Reuse therefore
  > now holds **within** one union — several pools share one fit instead
  > of re-fitting (the iwasaki “fit superset, subset” pattern) — while
  > the cross-*union* non-reuse above still stands (a wider union
  > re-fits). Because the fit layer carries no `distset`, adding a pool
  > whose members are already in the union mints only new hc shards and
  > caches every fit shard.

- **`nboot` *is* an hc grid axis** (`task_axes("hc")`): distinct values
  fan out into separate tasks, identities, and shards, so they are
  cached independently and `partition_by[["hc"]]` may list `nboot` to
  shard by it. Raising `nboot` does **not** invalidate the existing
  branch — the smaller value keeps its own identity and shard (the
  `nsim`-grow story, §8.1); what is unavailable is *content* reuse
  **across** nboot values.

**Why `nboot` enters the per-task primer (and `nrow` does not).**
Bootstrapping is the *only* RNG consumer in hc estimation: `ssd_hc()`
computes the point estimate `est` analytically from the fit, independent
of the bootstrap and the RNG (§1.2), so a `ci = FALSE` task draws no
random numbers at all and an hc task’s RNG use is entirely the
`nboot`-iteration bootstrap. The per-task primer
([`task_primer()`](https://poissonconsulting.github.io/ssdsims/reference/task_primer.md)
over `task_axes("hc")`, which includes `nboot`) therefore gives **each
`nboot` value its own dqrng stream** → statistically *independent*
bootstrap draws, each fully reproducible on its own. The alternative —
hashing the hc primer over `task_axes("hc")` *minus* `nboot` (decoupling
the primer from the task identity, exactly as §5 keeps `nrow` out of the
*sample* draw’s primer) — would make every `nboot` value draw from one
shared primed stream, so `nboot = 100`’s draws would be a **prefix** of
`nboot = 1000`’s (a subset property), which is the RNG precondition that
would make any future inner-loop reuse *coherent* (the extra 900 draws
continue the same stream rather than starting an independent one). We
keep `nboot` **in** the primer because, unlike `nrow` — whose
`head(sample, nrow)` truncation is *our* deterministic code, validated
by `scripts/experiment-subset-property.R` — the bootstrap loop is
*internal to ssdtools* (the opaque-RNG limitation below), so the
prefix/subset property cannot be guaranteed or easily validated.
Independent-stream-per-`nboot` is the honest, robust default; the
shared-stream nesting is revisitable only if ssdtools exposes the loop
or its prefix-stability is validated and pinned.

### `ssdtools` RNG flow is opaque

The internal RNG consumption of
[`ssdtools::ssd_fit_dists()`](https://bcgov.github.io/ssdtools/reference/ssd_fit_dists.html)
and `ssd_hc()` is a black box. We install a known `(seed, state)` before
each call, but how many uniforms the ssdtools call draws and in what
order is an ssdtools implementation detail. A breaking change to that
order in a future ssdtools release would silently change bit-stable
results even when the scenario `seed` is unchanged. Mitigation: pin
`ssdtools` (and `dqrng`, R) versions in the scenario manifest.

### `nrow` sub-truncation requires stable `sample.int`

§5’s “`nrow` is never an axis” property — `slice_sample_state` returns a
prefix of itself for smaller `n`, for both `replace` values — holds only
as long as [`base::sample.int()`](https://rdrr.io/r/base/sample.html) is
byte-stable across R versions. Validated for the current R by
`scripts/experiment-subset-property.R`; pin the R version (and `dqrng`,
`ssdtools`) in the manifest (§8.5) to guard against future behavior
changes.

### `dqrng::register_methods()` is scoped per scenario execution

Each scenario execution (via
[`ssd_run_scenario_baseline()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario_baseline.md)
or a cluster step) installs dqrng as the base R RNG backend at entry and
restores on exit via `on.exit(restore_methods())`. Tests and helper
scripts that touch the methods mid-session (not inside a scenario
runner) use the same [`on.exit()`](https://rdrr.io/r/base/on.exit.html)
discipline — documented in AGENTS.md (§RNG discipline).

------------------------------------------------------------------------

## 10. Gaps from `RNG-FLOW.md` §5 — how this design closes them

| Gap | Resolution |
|----|----|
| No DAG-of-DAGs primitive | Not needed — §8.3 pins shards against code changes in-project with `tar_cue(depend = FALSE)`; §8.1–§8.2 handle extension via the shards-as-cache. |
| No “load previous run from Parquet” path | §8 — shards are the cache; no explicit load needed. |
| Persists fragile RNG state | §1, §2 — scenario stores a single integer `seed`; per-task `(seed, state)` is reproducible via `dqset.seed()`. |
| Positional task IDs | §2 — task IDs are keyed by `task_primer(p)` = 64-bit hash of canonical params. |
| Re-derivation cost is quadratic | §2 — per-task hash is O(1); no precomputed lattice. |
| `nsim`-grow cache invalidation | §1, §2, §8.1 — new sim values hash to new primers and create new shards; existing shards untouched. |
| Three steps cached as one (no per-step re-runs) | §5, §6 — data/fit/hc are three grids and three targets, each with its own shard layer. |
| Same lattice for all steps despite grid mismatch | §5 — each step has its own grid and per-task primer. |
| `nrow` invalidates data states for the same `sim` | §5 — `nrow` is never an axis: data state keyed by `(dataset, sim, replace)`, slice truncates to `n`. |
| Single-dataset scenarios only | §1.1 — datasets are materialised on the scenario, keyed by name; cross-join axis. |
| Function-arg edits invalidate caches | §1.1 — `min_pmix` referenced by name; function body edits do not move tasks across streams. |
| Bootstrap-only scenario options spuriously fan out under `ci=FALSE` | §1.2 — `ci` is a scalar flag (not an axis); under `ci=FALSE` the bootstrap-only scenario options are rejected at construction and stored `NA`, so they never fan out. |
| Branch failure unreproducible off the cluster | §7 — task row + upstream shard replays the failing task via `_state` primitives. |
| Code fix re-runs every branch by hash invalidation | §8.3 — `tar_cue(depend = FALSE)` pins shards against the edit; §8.4 — [`tar_invalidate()`](https://docs.ropensci.org/targets/reference/tar_invalidate.html) / [`unlink()`](https://rdrr.io/r/base/unlink.html) refreshes only the chosen shards. |
| Off-cluster access to Parquet outputs | §6.1 — the runner’s `upload` argument adds a per-shard `upload_<step>` target (content-hashed, [`ssd_upload_dryrun()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_azure.md) offline) to a configurable object store (e.g. Azure Blob), and [`ssd_open_uploaded()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_open_uploaded.md) reads it back in place. |
| Phantom local repros (regenerated upstream ≠ cluster’s actual) | §7 — manifest’s per-shard sha256 lets the lightweight recipe verify the local upstream before running the failing task. |

The RNGkind side-effect bug and the independent data/fit/hc substream
issues from the original L’Ecuyer design no longer apply: dqrng with
explicit `(seed, state)` per task has no side effects on global RNG
state of other tasks (the backend is switched per scenario execution and
restored on exit, not process-global).

------------------------------------------------------------------------

## 11. Open questions for review

1.  **`nrow` sub-truncation as a contract.** §5’s `slice_sample_state`
    relies on [`base::sample.int()`](https://rdrr.io/r/base/sample.html)
    being a prefix-of-itself when `n_max` is reduced (for both `replace`
    values). Validated by `scripts/experiment-subset-property.R`; R has
    historically been stable here but it isn’t a documented guarantee.
    Document the assumption in the manifest and pin R versions, or
    implement our own `sample.int`-equivalent function with an explicit
    contract?
2.  **Dataset identity.** Datasets are keyed by **name** on the
    scenario, not by `digest::digest(df)`. Within one scenario each name
    is materialised once, so there is no registration to collide; the
    open question is *across* scenarios sharing a results tree — two
    scenarios using the same dataset name for different bytes would
    point at the same partition path. Should a shared-tree run carry a
    content hash on the task ID alongside the name, or is name
    uniqueness the user’s contract?
3.  **Force re-run inside one `targets` project.** §8.4 lists two
    options for re-running after a code fix:
    [`unlink()`](https://rdrr.io/r/base/unlink.html) the bad shards, or
    `tar_invalidate(names = ...)`. Which is the recommended path?
    `unlink` is simpler but loses the bookkeeping; `tar_invalidate` is
    bookkeeping-preserving but cluster-aware (some `targets` versions
    don’t propagate across remote workers).
4.  **Manifest format.** A scenario manifest stores `seed`, the
    `datasets` and `min_pmix` name lists, the `fit` and `hc`
    argument-vector grids, the `upload` spec, and pinned package
    versions (§9). JSON sidecar next to Parquet, or
    [`tar_read()`](https://docs.ropensci.org/targets/reference/tar_read.html)
    against the project’s `_targets/` store? The latter is idiomatic but
    couples a reader to the project’s `targets` version.
5.  **Toy pipeline shape.** Ship a single
    `inst/targets-templates/cluster/` that the LLM-authoring prompt
    edits, or only documentation pointing at `crew.cluster` examples?
6.  **Shard-target ↔︎ Slurm-job mapping under
    `crew_controller_slurm()`.** `targets` + `crew` dispatch the
    per-shard targets across Slurm jobs but the precise mapping — one
    shard target per job vs several packed into one job — is
    configurable and depends on `crew` settings. The design commits only
    to **shards being the unit of parallelism**: independent shard
    targets run concurrently, regardless of job packing. Resolve by
    prototyping with the `cluster-pipeline` step (§12) and document the
    chosen packing convention; until then `shard target ↔︎ job` should be
    read as “many to one or one to one, depending on configuration”.

------------------------------------------------------------------------

## 12. Roadmap

This section is the **landed-work record and dependency graph** for the
in-place, step-by-step implementation. Each step is a kebab-case slug
that lands as a coherent working state; ssdsims has no downstream
dependencies, so breaking-change steps are fine, and **parallel work
streams are preferred** — the dependency DAG below shows where branches
open and close.

The **forward-looking backlog** (the not-yet-landed steps, their
priorities, and the independent tidy-ups) now lives in
[`ROADMAP.md`](https://poissonconsulting.github.io/ssdsims/ROADMAP.md),
in the `initiative`-template `Now` / `Next` / `Later` / `Bluesky` style.
This section retains the steps that have **landed and been archived**
(`### Archived`, in the dependency order they were implemented) and the
dependency DAG. The DAG now holds **only** those landed (green) nodes —
the dependency record of what shipped; not-yet-landed work lives as
prose in `ROADMAP.md` and re-enters the DAG as a `proposed` (red) node
only when a new DAG step is proposed. Keep the two in step: when a
change lands, move its DAG node to green (and into `archived_box`), add
its `### Archived` bullet here, and move its `ROADMAP.md` line to
`## Done`.

**De-risked by the labs.** The remaining targets-mechanics steps —
`shard-failure-survival`, `mixed-code-lockin`, and the `format = "file"`
choice underneath them — each have a working single-behaviour spike in
the project’s targets lab, and the now-landed `hive-partitioning`,
`cloud-upload`, and `cluster-pipeline` ported a crew + SLURM shape the
crew labs already ran end to end (see §4, §6). These steps are therefore
*porting* validated behaviour into the package, not discovering it.

### Archived

Completed steps that have landed and been archived (full artifacts under
`openspec/changes/archive/`; their requirements are synced into
`openspec/specs/`). Listed in the dependency order they were
implemented; the Mermaid graph below colours these nodes green inside
`archived_box`.

- **`ssd-define-scenario`** — Public constructor for the scenario object
  (S3); replaces the PoC’s `data2`-prefixed names. Signature along the
  lines of
  `ssd_define_scenario(data, ..., nsim, nrow, rescale, est_method, nboot, ci, ...)`,
  forwarding the input data through `ssd_data()` (a tiny normaliser that
  validates the `Conc` column and tibble shape). Stores only declarative
  fields (seed, scenario options, dataset names — the datasets
  themselves are materialised on the scenario, reached by name via
  `scenario-accessors`). No RNG, no tasks, no targets yet. **Scoped to
  data-frame input only** (single or list); the other generator inputs
  are `scenario-input-types`, below.

- **`task-list-loop-baseline`** — Derive three task lists (data, fit, hc
  rows; one column per cross-join axis; no RNG, no shards, no targets)
  from a scenario, and a runner that is just three
  [`purrr::pmap()`](https://purrr.tidyverse.org/reference/pmap.html)
  loops. Establishes the data shape and a working baseline that
  subsequent steps swap pieces of, one at a time. *Expanded by
  `task-list-loop-baseline-fold`* (applied; not a new DAG node): the
  steps are `sample`/`fit`/`hc` — `fit` truncates `head(sample, nrow)`
  inline rather than via a separate `data` step.

- **`dqrng-init`** — Add `dqrng` to `Imports`; `dqRNGkind("pcg64")`

  - `register_methods()` on package load, `restore_methods()` on unload.
    Verifies: `scripts/experiment-dqrng-hash.R` still passes.

- **`local-dqrng-state`** —
  `local_dqrng_state(seed, state, .local_envir = parent.frame())` thin
  wrapper around `dqset.seed(seed, stream = state)` with a `withr`-style
  restore on exit. Prefer `local_*` over `with_*` when touching code.
  Replaces `local_lecuyer_cmrg_state()` for the dqrng path.

- **`task-primer`** — `task_primer(params)` per §2 (64-bit hash,
  NA-as-INT_MIN encoding). Unit tests verify reproducibility and
  collision-resistance on the validated examples from
  `scripts/experiment-dqrng-hash.R`.

- **`primer-primitives`** — Refactor `slice_sample_state`,
  `fit_dists_state`, `hc_state` around the new contract: **each per-task
  body calls `local_dqrng_state(seed, primer)` exactly once**, then
  invokes the (state-less) operation against the ambient RNG. The
  `_state` suffix marks the wrapper that installs the primer; the inner
  ssdtools / dplyr calls consume RNG from the now-set state. No
  `state =` argument on the inner ops. (The seeded `n_max` draw lives
  here as `slice_sample_state`; the `head(sample, nrow)` truncation is
  the `fit` step’s inline, RNG-free step — there is no separate
  sub-truncation step.)

- **`scenario-accessors`** — Datasets and `min_pmix` are **materialised
  on the scenario, accessed by name** — no registry (§1.1). Materialise
  the `min_pmix` functions on the scenario at construction (keyed by
  name, resolving name-strings then), and add
  `scenario_dataset(scenario, name)` /
  `scenario_min_pmix(scenario, name)` accessors; `resolve_min_pmix()`
  becomes the accessor. Hashing stays name-only, so a function-body edit
  does not move any cached fit branch (regression test). Adds no
  dependency (no Parquet here — that is `task-tables`). Persisting a
  *large* dataset to disk instead of carrying it inline is the deferred
  `dataset-provenance` step.

- **`task-tables`** — `ssd_scenario_data_tasks` / `_fit_tasks` /
  `_hc_tasks` returning the per-step task tables with `(seed, primer)`
  on each row, plus the `ssd_scenario_*_shards` wrappers that group
  those rows by `partition_by` into one row per shard (a `tasks`
  list-column) for `tar_map`’s `values`. The §6 sketch compiles and
  [`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html)s
  a tiny scenario. **Static branching**: the scenario is a plain
  construction-time object and `tar_map` mints one named target per
  shard (§6) — no `pattern = map` over a runtime target.

- **`path-axis-growth`** — §8.1 — assert **end-to-end that a minimal
  scenario change on a path axis rebuilds only the necessary shards**.
  Appending a dataset (or growing `nsim`) to a working, already-
  [`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html)-d
  scenario mints new shard targets and leaves every existing shard
  cached — the cheap-extension payoff the extensibility goal leans on,
  validated in the targets lab’s `static-extend` axis (§6) and ported
  here into the package’s own
  `ssd_scenario → ssd_scenario_*_shards → tar_map → tar_make` path.
  Split out of `task-tables` so that step stays a build-and-compile
  checkpoint while this one owns the incremental-rebuild contract; it is
  the path-axis counterpart to `shard-atomic-rewrite`’s inner-axis test.
  **Depends on** `task-tables`. Note: the precise expected-cached set
  follows the invalidation model pinned by `hive-partitioning` (the §8
  cache-by-existence vs. content-hash fork), so the assertion is
  finalised once that decision lands. Test: run a tiny scenario to
  completion; append a dataset (a path axis for all three steps),
  [`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html)
  again, and assert only the new dataset’s shards build while the
  original dataset’s shard targets are skipped (and `summary` re-runs);
  repeat for `nsim` growth.

- **`step-scenario-slice`** — *Caveat (pre-existing) that this step
  closes.* The
  [`ssd_scenario_targets()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_targets.md)
  factory leaves `scenario` as a bare symbol in every step command
  (`ssd_run_<step>_step(tasks, scenario, …)`), so the **whole scenario
  object is a dependency of every shard target** across all three steps.
  Editing *any* scenario field — even a scenario option that feeds only
  one step, or only the output layer (e.g. `samples`, which is
  `hc`-only) — therefore invalidates and rebuilds **all** shards, not
  just the affected step’s. Project each step’s command onto the
  **minimal slice** of the scenario it actually consumes (the resolved
  per-step inputs / the fields that reach that step’s per-task body and
  primer), so a change to a step-irrelevant field leaves the other
  steps’ shards cached. Test: change an `hc`-only scenario option on a
  [`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html)-d
  scenario and assert only `hc` (and `summary`) rebuild while
  `sample`/`fit` shards are skipped; change a `fit`-only scenario option
  and assert `sample` stays cached. **Depends on** `task-tables` (the
  factory it refines); pairs with `path-axis-growth` (the path-axis
  counterpart of the same minimal-rebuild contract) and is finalised
  against the invalidation model pinned by `hive-partitioning` (§8).

- **`shard-runner-baseline`** — §5 / §6 — a **single-core** runner that
  materialises each step as Hive-partitioned Parquet shards (one per
  `partition_by` path cell) and links steps by reading parent shards
  back with duckplyr predicate pushdown, resolving the **m:n**
  child-shard ← parent-shards dependency *at run time* (the distinct set
  of parent shard paths over a child shard’s tasks). “Shards without
  `targets`”, mirroring `task-list-loop-baseline`’s “tasks without
  shards”. First consumer of `partition-by`’s
  `scenario_partition_axes()`; owns the Hive path/read helpers and
  proves the write→glob-read→filter loop + m:n resolution in plain R.
  Makes `partition-by`’s deferred acceptance test (changing
  `partition_by` shifts paths, per-task results byte-identical) landable
  here, **without `targets`**, against the in-memory baseline as oracle.
  Adds `duckplyr` to `Imports` (the same engine the targets read path
  uses, so this de-risks `hive-partitioning` directly). Depends on
  `primer-primitives` + `partition-by`.

- **`hive-partitioning`** — §6 / §8 — wire the (now-validated) Hive
  write/read into the `targets` `task-tables` branches as
  `format = "file"` outputs and **pin the invalidation model**
  (`tar_cue` / content-hash, §8), so it also settles the deferred
  `data`-step-vs-fold decision recorded in §8. Because the child↔︎parent
  shard relationship is **m:n**, the invalidation model must propagate
  over the fan-in: rewriting one parent shard invalidates the **set** of
  child shards that read it — which rides on `task-tables`’ per-child
  upstream edges (Option 3, computed at sourcing time), not on a single
  named edge. Reuses `shard-runner-baseline`’s path/read helpers — the
  layout and predicate-pushdown read are already proven there, so this
  step is the `targets`-integration + caching half only (its smoke test
  moves to `shard-runner-baseline`). Depends on `task-tables` and
  `shard-runner-baseline`.

- **`partition-by`** — §5 / §1 — scenario argument `partition_by` (named
  list per step) picks which task-table columns become Hive path levels
  and which become Parquet columns. Default per step ships as documented
  in §5. Test: changing `partition_by` for a step shifts shards (file
  paths) but per-task results inside are byte-identical.

- **`shard-atomic-rewrite`** — §8.2 — inner-axis growth rewrites
  affected shards. Test: adding a new `min_pmix` to the scenario causes
  targets to re-run the affected fit branches and overwrite the shards’
  Parquets; rows that were there before come out byte-identical to the
  previous Parquet.

- **`scalar-ci-flag`** — Demote `ci` from a grid/task axis to a **scalar
  flag** (`chk_flag`, default `FALSE`), mirroring `samples`. The point
  estimate `est` is byte-identical whether `ci = TRUE` or `ci = FALSE`
  (verified across every `ssd_ci_methods()`; the estimate is analytic
  and RNG-independent), so a single `ci = TRUE` run is a superset of
  `ci = FALSE` (same `est`, plus `se`/`lcl`/`ucl`) and running
  `ci = c(FALSE, TRUE)` only doubles the hc work for a redundant
  point-estimate row. Removes `"ci"` from `task_axes("hc")` and the
  per-task primer, **retires the §1.2 `ci = FALSE` collapse** (the
  `hc_grid_tbl()` branching collapses to one grid keyed by the scalar
  `ci`), and keeps the bootstrap-only scenario option guard with
  `ci = TRUE` as the enablement path. `ci = FALSE` stays the cheap,
  bootstrap-free, point-estimate mode — a scenario-wide either/or, not
  combinable with `TRUE`. Removing `ci` from the primer shifts the hc
  bootstrap stream, so CIs re-baseline (estimates unchanged); acceptable
  pre-release. The legacy public `ssd_hc_sims()` primer this once had to
  reconcile against is removed outright by `cleanup-lecuyer` (which
  folds in the former `migrate-public-api`). Surfaced verifying the `ci`
  axis against `ssdtools`. Independent tidy-up with no dependants; not
  on the dependency DAG.

- **`blob-storage-format`** — Evaluated how the `fit` step’s non-tabular
  per-task result (a `fitdists` object) is stored in its shard Parquet.
  The interim `encode_obj()`/`decode_obj()` seam (`R/targets-runner.R`)
  carries the object as an `serialize(ascii = TRUE)` **ASCII string** in
  a `VARCHAR` column, because duckplyr cannot store a `raw`/list column
  and an ASCII serialisation round-trips losslessly. Benchmarked the
  alternatives against the three constraints (byte-identity oracle,
  duckplyr/Parquet column type, §6 projectable-blob read path): binary
  `serialize(ascii = FALSE)` as base64 text proved **larger** (~1.5× on
  disk — the object is mostly compact doubles and Parquet already
  compresses the `VARCHAR` for free) and
  [`jsonlite::serializeJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/serializeJSON.html)
  was **not lossless** on the embedded model fits, so neither cleared
  the swap gate. **Decision: keep the interim ASCII-`VARCHAR` encoding**
  and instead tighten the `shard-runner` spec — the byte-identity,
  string-column, and projectable-blob contracts are now stated
  explicitly. No code change beyond the spec; the benchmark is preserved
  in the change’s `exploration` (the `benchmark-blob-encoding.R`
  script). Independent tidy-up with no dependants; not on the dependency
  DAG.

- **`dists-scenario-setting`** — Reconcile `dists`’s classification
  across the spec, signature, and docs. `dists` is absent from
  `task_axes("fit")` — a fit-level **scenario setting** (one
  model-averaged `ssd_fit_dists()` per task, applied uniformly), not a
  cross-join axis — but the `scenario-definition` role-grouping
  requirement listed it among the axes and the signature wedged it in
  the fit-axis block. This change moved `dists` to lead the contiguous
  simulation-settings block
  (`… parametric, dists, proportion, ci, samples`), corrected the spec,
  and swept call sites. Behaviour-preserving (no task-graph, primer, or
  shard change); paired with the §9 / GLOSSARY corrections that also
  fixed the stale *“`dists` and `nboot` are not fit/hc grid axes”*
  heading (`nboot` **is** an hc axis). Independent tidy-up; not on the
  dependency DAG.

- **`est-method-setting`** — Reclassify `est_method` from an hc
  cross-join axis to an hc-level **scenario setting** (the same shape as
  `dists-scenario-setting` / `scalar-ci-flag`: `scenario-definition` +
  `task-lists` + `hazard-concentrations` deltas). `est_method` is
  removed from `task_axes("hc")`; the hc fan-out becomes
  `nboot × ci_method × parametric` and a single bootstrap per cell
  yields every requested `est_method` (the analytical `est` differs; the
  CI is est_method-invariant — verified at a fixed seed in the change’s
  `exploration/`). Unlike the other reclassifications this is **not**
  byte-preserving: because the hc primer hashes the hc-grid row
  including `est_method` (§2), dropping the axis **re-seeds** every hc
  task, so bootstrap CIs change numerically (point estimates unchanged).
  ~3× cost reduction on the `est_method` axis. Independent tidy-up; not
  on the dependency DAG.

- **`distset-hc-axis`** — Refine `dists-scenario-setting`: `dists`
  becomes an
  [`ssd_distset()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_distset.md)
  collection of named **distribution sets** (pools), and the set *name*
  becomes an **hc** cross-join axis (`distset` ∈ `task_axes("hc")`). The
  fit step fits the **union** `sort(unique(unlist(dists)))` once
  (`scenario$fit$dists`); the hc step
  [`subset()`](https://rdrr.io/r/base/subset.html)s that union fit to
  each cell’s pool (`strict = FALSE`) and re-averages — so several pools
  share one fit (the iwasaki “fit superset, subset” pattern) rather than
  re-fitting (~7× → one fit). *Individual distributions still never fan
  out* — an axis value is a whole pool, so the model-averaging science
  `dists-scenario-setting` protects is intact; what is new is reuse
  **within** one union (a wider union still re-fits). The subset happens
  in the shared `hc_data_task_primer()` chokepoint, so baseline, shard,
  and targets paths stay byte-identical by construction; an all-dropped
  set yields zero rows (the survivor model). `distset` is bundled
  (inner) by default — one hc shard serves every pool for a
  `(dataset, sim)` cell, decoding the union fit once — and may be
  promoted to `partition_by$hc`. **BREAKING** (pre-release): a
  bare-vector / plain-list `dists` aborts, pointing to
  [`ssd_distset()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_distset.md).
  Not byte-preserving: `distset` joins the hc primer, so it re-seeds
  every hc task (point `est` analytical/unchanged, bootstrap CIs
  re-seeded). `scenario-definition` + `task-lists` +
  `hazard-concentrations` + `task-shards` + `scenario-accessors` deltas;
  correctness oracle in the change’s
  `exploration/distset-subset-invariance.R`. Independent of
  `pmix-constructor` (disjoint inputs); not on the dependency DAG.

- **`cost-estimation`** — New `cost-estimation` capability: a
  calibration harness
  ([`ssd_calibrate_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_calibrate_cost.md))
  that re-measures a per-task cost model on the target architecture and
  an estimator
  ([`ssd_estimate_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_estimate_cost.md))
  that reads a scenario’s hc task expansion (read-only, no run) to
  predict ballpark total cost and the longest single task. Model: the hc
  bootstrap dominates `ci = TRUE`; per-call time ≈
  `base + slope(ci_method) × max(nboot, n0)`, with
  `proportion`/`est_method` free and a bounded non-monotonic `nrow`
  factor (calibrated this session at ~430 single-core hours for the
  motivating scenario). Ships a default calibration with provenance; the
  model-form discovery is preserved in the change’s `exploration/`.
  Independent new capability; not on the dependency DAG (reads the
  archived `task-tables` expansion, no dependants).

- **`manifest`** — Per-scenario manifest
  writer/reader/recorder/assembler with the §8.5 field set (the head
  records **complete session info**, not just the three named version
  pins); the recorder writes one per-shard `meta.json` sidecar and the
  assembler unions them (hashing shards without one). **Not on the
  `task-tables` critical path** — the runner reads nothing from it.
  Landed in `R/manifest.R` (#114). **Subsequently un-exported** (kept
  internal, no live consumer yet — the shard runner does not depend on
  it and its readers `replay-helper` / `shard-completeness-assert` are
  not built): the four functions are now `@keywords internal`, dropped
  from `NAMESPACE` and the pkgdown reference; the code, tests, and
  `manifest` spec stay. See `ROADMAP.md` *Decisions* and the
  `manifest-revival` task.

- **`cluster-pipeline`** — Editable SLURM `crew.cluster` targets
  template (`inst/targets-templates/cluster/`) reusing the
  [`ssd_scenario_targets()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_targets.md)
  factory and scenario shape verbatim from `large/`, with a standalone
  connectivity + worker-prerequisite `preflight.R` (run before
  [`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html)),
  shards as the unit of parallelism dispatched to SLURM jobs, and a
  zero-to-running-job guide shipped as both the template `README.md` and
  the `cluster-pipeline.qmd` vignette (#115). `crew.cluster` is in
  `Suggests`; a scheduler-free test exercises the preflight probe and
  asserts the pipeline graph stays clean. The **real-SLURM end-to-end
  run** (tasks 4.1/4.2) remains a documented manual/lab step (the
  byte-identity comparison runs green off-cluster via `large/`).

- **`cloud-upload`** — §6.1 — typed, self-validating destination objects
  (`ssd_upload_azure(url, container, ..., prefix, domain)`,
  [`ssd_upload_dryrun()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_azure.md);
  class `ssdsims_upload`) dispatched by
  [`ssd_upload_shard()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_shard.md)
  /
  [`ssd_test_upload()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_test_upload.md)
  /
  [`ssd_open_uploaded()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_open_uploaded.md)
  /
  [`ssd_summarise_uploaded()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_summarise_uploaded.md)
  generics. The destination is a **runner argument**, the sibling of
  `root` on `ssd_scenario_targets(..., upload, cue)` (with
  [`rlang::check_dots_empty()`](https://rlang.r-lib.org/reference/check_dots_empty.html)
  forcing named args), **not** a `scenario` field (so it is dropped from
  [`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md)
  and `ssdsims_scenario` — both **BREAKING**). Fail-loud credentials
  (Azure with absent creds errors; skip-the-network intent is expressed
  only by
  [`ssd_upload_dryrun()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_azure.md));
  per-shard content-hashed `upload_<step>` targets (`format = "file"`,
  `error = "null"`); in-place read-back and summary via DuckDB’s `azure`
  extension
  ([`ssd_open_uploaded()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_open_uploaded.md)
  /
  [`ssd_summarise_uploaded()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_summarise_uploaded.md),
  no download). Landed as `R/upload.R`, the `upload` wiring in
  [`ssd_scenario_targets()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_targets.md),
  the `cloud-upload.qmd` vignette, and `AzureStor`/`AzureRMR` in
  `Suggests` (#114/#129). Upload sha256 recording was dropped, deferred
  with the parked `manifest` (see `ROADMAP.md`).

- **`dual-summary-outputs`** — Optional second
  [`ssd_summarise()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_summarise.md)
  output: a trailing `path_with_samples = NULL` that, when supplied,
  **also** writes a *full* hc union **retaining** the `dists`/`samples`
  list-columns (read lazily in DuckDB, never materialised in R),
  alongside the unchanged compact `summary.parquet`.
  [`ssd_scenario_targets()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_targets.md)
  passes `path_with_samples = <root>/summary-samples.parquet` **iff**
  `scenario$hc$samples` is `TRUE`, and the `summary` target returns the
  path vector so `targets` tracks both files; with `samples = FALSE` the
  pipeline is unchanged. Additive and backward-compatible; `task-shards`
  delta. Independent tidy-up; not on the dependency DAG (#140).

- **`task-rng-postcheck`** — Per-task RNG-backend **postcondition**.
  Each `*_data_task_primer()` wrapper, when the task *ends*, verifies
  that dqrng (not merely *some* user-supplied RNG) still held base R’s
  `user_unif_rand` slot for the whole body, via a non-destructive
  state-advance witness; a frozen state means a foreign RNG hijacked the
  slot, so it **aborts** (chk-style), symmetric with the
  [`local_dqrng_state()`](https://poissonconsulting.github.io/ssdsims/reference/local_dqrng_state.md)
  entry guard. Closes the gap that the
  [`RNGkind()`](https://rdrr.io/r/base/Random.html) probe is satisfied
  by a *foreign* user-supplied RNG. Depends on `primer-primitives`;
  orthogonal to `task-primer`. *Treated as merged for the 2026-06-07
  roadmap split; its change directory is not yet physically archived
  (see `ROADMAP.md`).*

### Dependency DAG (parallel streams)

Mermaid (renders inline on GitHub):

``` mermaid
flowchart TD
    %% This DAG is the dependency record of the **landed (archived)** steps —
    %% every node here is green and lives in `archived_box`. Not-yet-landed work
    %% is tracked as prose in ROADMAP.md; when a new DAG step is proposed it is
    %% added here as a `proposed` (red) node and moves into the box on archive.
    subgraph archived_box [archived]
        define[ssd-define-scenario]
        baseline[task-list-loop-baseline]
        dqinit[dqrng-init]
        dqstate[local-dqrng-state]
        primer[task-primer]
        prims[primer-primitives]
        partby[partition-by]
        acc[scenario-accessors]
        shardrun[shard-runner-baseline]
        tt[task-tables]
        hive[hive-partitioning]
        slice[step-scenario-slice]
        rewrite[shard-atomic-rewrite]
        pathgrow[path-axis-growth]
        manif[manifest]
        cluster[cluster-pipeline]
        cloud[cloud-upload]
        postcheck[task-rng-postcheck]
    end

    define --> baseline
    define --> partby
    define --> acc
    dqinit --> dqstate
    dqstate --> primer
    baseline --> prims
    primer --> prims

    prims --> postcheck
    baseline --> partby

    acc --> tt
    prims --> tt
    partby --> tt

    prims --> shardrun
    partby --> shardrun
    shardrun --> hive

    tt --> hive
    tt --> cluster
    tt --> cloud
    tt --> rewrite
    tt --> pathgrow
    tt --> slice

    %% manifest is provenance/verification metadata: it depends on the
    %% scenario (head) and feeds the verification layer; it does NOT gate
    %% task-tables (see the manifest roadmap bullet).
    define --> manif
    manif --> cloud

    %% hive-partitioning pins the invalidation model (§8); the three
    %% minimal-rebuild contracts finalise their cached-vs-rebuilt
    %% assertions against it, so they depend on it (soft edge, dotted).
    hive -.-> rewrite
    hive -.-> pathgrow
    hive -.-> slice

    %% step-scenario-slice's per-dataset `sample` slice is what makes appending
    %% a dataset leave the existing shards cached; path-axis-growth's
    %% dataset-growth contract therefore depends on the slice landing (hard edge).
    slice --> pathgrow

    %% --- node status colouring (palette kept for future proposed nodes) ---
    %% green = archived, yellow = done (implemented, not yet archived),
    %% red = proposed (artifacts exist, not implemented),
    %% blue = ready (every prerequisite landed — ready to propose),
    %% unfilled = open (still blocked by an un-landed prerequisite).
    %% Every current node is archived (green); add a new DAG step as a red
    %% `proposed` node, then move it into `archived_box` (green) on archive.
    classDef archived fill:#c8e6c9,stroke:#2e7d32,color:#1b5e20
    classDef done fill:#fff9c4,stroke:#f9a825,color:#5f4300
    classDef proposed fill:#ffcdd2,stroke:#c62828,color:#7f1414
    classDef ready fill:#bbdefb,stroke:#1565c0,color:#0d3c61
    classDef open fill:#ffffff,stroke:#90a4ae,color:#37474f

    class define,baseline,dqinit,dqstate,primer,prims,acc,partby,tt,shardrun,hive,slice,rewrite,pathgrow,manif,cluster,cloud,postcheck archived
```

**Every node here is archived (green)** — the DAG is the dependency
record of the landed steps. The colour palette (green = archived, yellow
= done, red = proposed, blue = ready, unfilled = open) and the
`archived_box` rule still apply **when a new DAG step is proposed**: add
it as a red `proposed` node, then move it to green inside `archived_box`
on archive. Not-yet-landed steps are not drawn here — they live in
[`ROADMAP.md`](https://poissonconsulting.github.io/ssdsims/ROADMAP.md)
until they are proposed.

**Status snapshot (2026-06-07).** The dependency-DAG steps that have
**landed and been archived** are green inside `archived_box`: the
scenario/RNG/task-table foundation (`ssd-define-scenario` …
`task-tables`), the shard machinery (`shard-runner-baseline`,
`hive-partitioning`, `partition-by`), the three minimal-rebuild
contracts (`step-scenario-slice`, `path-axis-growth`,
`shard-atomic-rewrite`), the `manifest`, and now `cluster-pipeline`,
`cloud-upload`, and `task-rng-postcheck` (the last treated as merged for
this split). Off the DAG, the independent tidy-ups `scalar-ci-flag`,
`blob-storage-format`, `dists-scenario-setting`, `est-method-setting`,
`cost-estimation`, and `dual-summary-outputs` are likewise archived
(prose bullets above, no Mermaid nodes).

The **not-yet-landed** steps are no longer drawn in this DAG. Their
dependency shape: `shard-failure-survival` is unblocked by
`cluster-pipeline`, and `mixed-code-lockin` by `shard-atomic-rewrite`;
`replay-helper` is unblocked (`task-tables` + `manifest` archived);
`shard-completeness-assert` waits on `shard-failure-survival`, and
`cleanup-lecuyer` — which retires the legacy public API and folds in the
former `migrate-public-api` — carries artifacts and is unblocked (the
replacement surface is shipped; the removed code is in no shard’s
command closure, so no `mixed-code-lockin` pin is needed). Priorities
and queue position for all of these live in
[`ROADMAP.md`](https://poissonconsulting.github.io/ssdsims/ROADMAP.md).
