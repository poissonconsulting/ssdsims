# Define a Simulation Scenario

Constructs a purely declarative `ssdsims_scenario` object: the root of
the targets-based pipeline (see `TARGETS-DESIGN.md` section 1). The
object stores only declarative fields - a scalar `seed`, the replicate
count `nsim`, the sample sizes `nrow`, the dataset *names*, and the
`fit` and `hc` argument grids. It performs **no** random-number
generation, **no** task expansion, and has **no** dependency on
`targets`.

## Usage

``` r
ssd_define_scenario(
  data,
  nsim,
  seed,
  ...,
  nrow = 6L,
  replace = FALSE,
  rescale = FALSE,
  computable = FALSE,
  at_boundary_ok = TRUE,
  min_pmix = list(ssdtools::ssd_min_pmix),
  range_shape1 = list(c(0.05, 20)),
  range_shape2 = list(c(0.05, 20)),
  dists = ssdtools::ssd_dists_bcanz(),
  est_method = "multi",
  proportion = 0.05,
  ci = FALSE,
  nboot = 1000,
  ci_method = "weighted_samples",
  parametric = TRUE,
  samples = FALSE,
  partition_by = NULL,
  bundle = NULL
)
```

## Arguments

- data:

  An
  [`ssd_scenario_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_data.md)
  collection: a validated, named collection of `Conc` tibbles assembled
  from data frames and/or
  [`ssd_gen()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_gen.md)
  generator datasets.

- nsim:

  A count of the number of data sets to generate.

- seed:

  A scalar whole number; the scenario's RNG root. Required - changing it
  fully re-roots the scenario's random-number draws.

- ...:

  Unused; must be empty.

- nrow:

  A positive whole number of the minimum number of non-missing rows.

- replace:

  A logical vector specifying whether to sample with replacement.

- rescale:

  A flag specifying whether to leave the values unchanged (FALSE) or to
  rescale concentration values by dividing by the geometric mean of the
  minimum and maximum positive finite values (TRUE) or a string
  specifying whether to leave the values unchanged ("no") or to rescale
  concentration values by dividing by the geometric mean of the minimum
  and maximum positive finite values ("geomean") or to logistically
  transform ("odds").

- computable:

  A flag specifying whether to only return fits with numerically
  computable standard errors.

- at_boundary_ok:

  A flag specifying whether a model with one or more parameters at the
  boundary should be considered to have converged (default = TRUE).

- min_pmix:

  The `min_pmix` function(s), referenced **by name**. Supply either a
  character vector of names, or a function (or list of functions) with a
  single argument that inputs the number of rows of data and returns a
  proportion between 0 and 0.5 - in which case the name is derived from
  the argument expression (e.g.
  [`ssdtools::ssd_min_pmix`](https://bcgov.github.io/ssdtools/reference/ssd_min_pmix.html)
  gives `"ssd_min_pmix"`), mirroring dataset name derivation. The name
  is what the task path hashes; the resolved single-argument function is
  additionally materialised on the scenario (keyed by name) for
  execution and isolated via
  [`scenario_min_pmix()`](https://poissonconsulting.github.io/ssdsims/reference/scenario_min_pmix.md).
  A name-string is resolved to a function at construction (from
  `ssdtools` or the caller's environment), failing fast if it cannot be
  resolved to a single-argument function.

- range_shape1:

  A list of numeric vectors of length two of the lower and upper bounds
  for the shape1 parameter.

- range_shape2:

  A list of numeric vectors of length two of the lower and upper bounds
  for the shape2 parameter.

- dists:

  A character vector of the distribution names.

- est_method:

  A string specifying whether to estimate directly from the
  model-averaged cumulative distribution function
  (`est_method = 'multi'`) or to take the arithmetic mean of the
  estimates from the individual cumulative distribution functions
  weighted by the AICc derived weights (`est_method = 'arithmetic'`) or
  or to use the geometric mean instead (`est_method = 'geometric'`).

- proportion:

  A numeric vector of proportion values to estimate hazard
  concentrations for.

- ci:

  A flag specifying whether to estimate confidence intervals (by
  bootstrapping).

- nboot:

  A count of the number of bootstrap samples to use to estimate the
  confidence limits. A value of 10,000 is recommended for official
  guidelines.

- ci_method:

  A string specifying which method to use for estimating the standard
  error and confidence limits from the bootstrap samples. The default
  and recommended value is still `ci_method = "weighted_samples"` which
  takes bootstrap samples from each distribution proportional to its
  AICc based weights and calculates the confidence limits (and SE) from
  this single set. `ci_method = "multi_fixed"` and
  `ci_method = "multi_free"` generate the bootstrap samples using the
  model-averaged cumulative distribution function but differ in whether
  the model weights are fixed at the values for the original dataset or
  re-estimated for each bootstrap sample dataset. The value
  `ci_method = "MACL"` (was `ci_method = "weighted_arithmetic"`), which
  is only included for historical reasons, takes the weighted arithmetic
  mean of the confidence limits while `ci_method = GMACL` which takes
  the weighted geometric mean of the confidence limits was added for
  completeness but is also not recommended. Finally
  `ci_method = "arithmetic_samples"` and
  `ci_method = "geometric_samples"` take the weighted arithmetic or
  geometric mean of the values for each bootstrap iteration across all
  the distributions and then calculate the confidence limits (and SE)
  from the single set of samples.

- parametric:

  A flag specifying whether to perform parametric bootstrapping as
  opposed to non-parametrically resampling the original data with
  replacement.

- samples:

  A logical scalar (default `FALSE`): retain the bootstrap draws in the
  hc result's `samples` list-column (passed to
  [`ssdtools::ssd_hc()`](https://bcgov.github.io/ssdtools/reference/ssd_hc.html)).
  This is **output retention only** - it does not change the estimates
  or the per-task RNG, so it is not a grid or task axis (a single `TRUE`
  is a superset of `FALSE`). Changing it re-runs the hc step (the
  discarded draws must be re-bootstrapped) but yields byte-identical
  estimates; retained samples can be large (`nboot` draws per dist per
  task), so it is off by default.

- partition_by:

  An optional, possibly-partial named list keyed by step
  (`sample`/`fit`/`hc`) of character vectors naming the Hive **path**
  axes for that step (one shard per path cell; the inner complement
  rides as Parquet columns). Each entry must be unique, non-missing, and
  a subset of that step's axis vocabulary: `sample` = `dataset`, `sim`,
  `replace`; `fit` adds `nrow`, `rescale`, `computable`,
  `at_boundary_ok`, `min_pmix`, `range_shape1`, `range_shape2`; `hc`
  adds `nboot`, `ci_method`, `parametric` (`ci` and `est_method` are hc
  simulation settings, not axes; `dists` is the fit-level simulation
  setting). `"nrow"` is rejected only for `sample` (the shared draw
  carries no `nrow` axis; the `fit` step truncates it inline), and is a
  valid path axis for `fit`/`hc`. Steps partition **independently** -
  there is no cross-step constraint; a step may be finer or coarser than
  its neighbour on a shared axis (the m:n parent-shard relationship is
  resolved at the read layer). Steps left unnamed take their documented
  defaults (`sample = c("dataset", "sim", "replace")`,
  `fit = c("dataset", "sim", "nrow", "rescale")`,
  `hc = c("dataset", "sim")`; these supersede `TARGETS-DESIGN.md`
  section 5's pre-fold table). The split is orthogonal to the per-task
  RNG primer, so changing it shifts file paths only, never results.

- bundle:

  An optional, possibly-partial named list keyed by step, the per-step
  **complement** of `partition_by`: it names the **inner** axes to keep
  together within a shard, and the stored path axes become
  `setdiff(task_axes(step), bundle[[step]])`. `partition_by` and
  `bundle` are complementary per-step entry points - at most one may
  name a given step (a step in **both** is an error), but they may be
  mixed across steps and either may be partial. Use `partition_by` when
  you want few path axes, `bundle` when you want fine sharding and only
  a few inner axes. Both normalise into the single stored `partition_by`
  path list.

## Value

An S3 object of class `ssdsims_scenario`.

## Details

Input data arrives as an
[`ssd_scenario_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_data.md)
collection (already validated: a numeric `Conc` column is required) and
is retained on the scenario (as `$data`) so a local run
([`ssd_run_scenario_baseline()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario_baseline.md))
can sample it directly. The dataset *names* (`$datasets`) are what the
targets/cluster path hashes; the validated tibbles ride on the scenario
and are isolated by name via
[`scenario_dataset()`](https://poissonconsulting.github.io/ssdsims/reference/scenario_dataset.md),
so the hash need not carry the data frames.

## Dataset input

Dataset input is accepted **only** as an
[`ssd_scenario_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_data.md)
collection, which owns validation and naming. Assemble it first, then
pass it in:

    data <- ssd_scenario_data(boron = ccme_boron, cadmium = ccme_cadmium)
    scenario <- ssd_define_scenario(data, ...)

Generator inputs (a `fitdists`/`tmbfit` object, a generator function, or
a function-name string) are materialised - once, reproducibly - by
[`ssd_gen()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_gen.md)
and composed into the same collection; the constructor itself performs
**no** random-number generation.

## `ci`

`ci` is a scalar flag (not a cross-join axis): the point estimate `est`
is invariant to `ci` - it is computed analytically from the fit,
independent of the bootstrap and RNG - so a single `ci = TRUE` run is a
strict superset of `ci = FALSE` (same `est`, plus the `se`/`lcl`/`ucl`
columns). The choice is scenario-wide either/or: `ci = FALSE` for cheap,
bootstrap-free point estimates, or `ci = TRUE` for estimates plus
confidence intervals. When `ci = FALSE`, the bootstrap-only knobs
`nboot`, `ci_method`, and `parametric` are meaningless; passing any of
them in that case is an error, so set `ci = TRUE` to enable bootstrap,
or omit the knobs.

## `dists` and `est_method`

`dists` and `est_method` are **simulation settings**, not cross-join
axes - they are absent from `task_axes("fit")`/`task_axes("hc")`, so
they never multiply tasks or enter the per-task RNG primer. `dists` is
the *fit*-level setting: the whole character vector is handed to one
`ssd_fit_dists()` call per fit task (fanning out per distribution would
dissolve the model averaging that defines a fit). `est_method` is an
*hc*-level setting: every requested method is summarised from each hc
task's **single** bootstrap sample set rather than re-bootstrapping per
method (the CI is est_method-invariant and the point `est` is
analytical), so a vector `est_method` yields one row per method within a
task without fanning out into separate tasks.

## Examples

``` r
data <- ssd_scenario_data(ssddata::ccme_boron)
scenario <- ssd_define_scenario(data, nsim = 100L, seed = 42L, nrow = c(5L, 10L))
scenario
#> <ssdsims_scenario>
#>   seed:     42
#>   nsim:     100
#>   datasets: ccme_boron
#>   nrow:     5, 10
#>   replace:  FALSE
#>   fit grid:
#>     rescale: FALSE
#>     computable: FALSE
#>     at_boundary_ok: TRUE
#>     min_pmix: ssd_min_pmix
#>     range_shape1: {0.05, 20}
#>     range_shape2: {0.05, 20}
#>     dists: gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull (setting)
#>   hc grid:
#>     est_method: multi (setting)
#>     proportion: 0.05 (setting)
#>     ci: FALSE (setting)
#>     nboot: 1000
#>     ci_method: weighted_samples
#>     parametric: TRUE
#>     samples: FALSE (setting)
#>   partition_by:
#>     sample: dataset, sim, replace
#>     fit: dataset, sim, nrow, rescale
#>     hc: dataset, sim
#>   bundle:
#>     sample: 
#>     fit: replace, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2
#>     hc: replace, nrow, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2, nboot, ci_method, parametric
```
