# Define a Simulation Scenario

Constructs a purely declarative `ssdsims_scenario` object: the root of
the targets-based pipeline (see `TARGETS-DESIGN.md` §1). The object
stores only declarative fields - a scalar `seed`, the replicate count
`nsim`, the sample sizes `nrow`, the dataset *names*, and the `fit` and
`hc` argument grids. It performs **no** random-number generation, **no**
task expansion, and has **no** dependency on `targets`.

## Usage

``` r
ssd_define_scenario(
  data,
  nsim,
  seed,
  ...,
  name = NULL,
  nrow = 6L,
  dists = ssdtools::ssd_dists_bcanz(),
  rescale = FALSE,
  computable = FALSE,
  at_boundary_ok = TRUE,
  min_pmix = list(ssdtools::ssd_min_pmix),
  range_shape1 = list(c(0.05, 20)),
  range_shape2 = list(c(0.05, 20)),
  proportion = 0.05,
  ci = FALSE,
  nboot = 1000,
  est_method = "multi",
  ci_method = "weighted_samples",
  parametric = TRUE,
  partition_by = NULL,
  upload = NULL
)
```

## Arguments

- data:

  An
  [`ssd_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_data.md)
  collection (preferred), or - for convenience - a single data frame or
  a (named or unnamed) list of data frames. Bare inputs are validated
  via the same `Conc` contract as
  [`ssd_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_data.md).

- nsim:

  A count of the number of data sets to generate.

- seed:

  A scalar whole number; the scenario's RNG root. Required - changing it
  fully re-roots the scenario's random-number draws.

- ...:

  Unused; must be empty.

- name:

  An optional dataset name for the single-data-frame form, overriding
  the derived name. Must not be combined with a named list or an
  [`ssd_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_data.md)
  collection.

- nrow:

  A positive whole number of the minimum number of non-missing rows.

- dists:

  A character vector of the distribution names.

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
  gives `"ssd_min_pmix"`), mirroring dataset name derivation. Only the
  name is stored; the function is resolved later via the `min_pmix`
  registry (a future roadmap step).

- range_shape1:

  A list of numeric vectors of length two of the lower and upper bounds
  for the shape1 parameter.

- range_shape2:

  A list of numeric vectors of length two of the lower and upper bounds
  for the shape2 parameter.

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

- est_method:

  A string specifying whether to estimate directly from the
  model-averaged cumulative distribution function
  (`est_method = 'multi'`) or to take the arithmetic mean of the
  estimates from the individual cumulative distribution functions
  weighted by the AICc derived weights (`est_method = 'arithmetic'`) or
  or to use the geometric mean instead (`est_method = 'geometric'`).

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

- partition_by:

  An optional named list with `data`, `fit`, and `hc` character vectors
  naming the Hive partition axes per step. When `NULL` the documented
  per-step defaults are used.

- upload:

  An optional upload specification (a list), or `NULL` for no upload.

## Value

An S3 object of class `ssdsims_scenario`.

## Details

Input data is forwarded through
[`ssd_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_data.md)
for validation (a numeric `Conc` column is required) but the data frames
themselves are *not* stored; only their names are kept, so the scenario
serialises to a compact manifest.

## Dataset input

The preferred form is an
[`ssd_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_data.md)
collection, which owns validation and naming:
`ssd_define_scenario(ssd_data(boron = ccme_boron, cadmium = ccme_cadmium), ...)`.
For convenience, bare data frame input is also accepted in four forms
(routed through the same `Conc` validation):

1.  A single data frame, name derived from the argument expression:
    `ssd_define_scenario(ssddata::ccme_boron, ...)` gives
    `"ccme_boron"`.

2.  A single data frame with an explicit `name=`:
    `ssd_define_scenario(ssddata::ccme_boron, name = "boron", ...)`.

3.  A named list, names taken from the list:
    `ssd_define_scenario(list(boron = ccme_boron, cadmium = ccme_cadmium), ...)`.

4.  An unnamed list, names derived per element:
    `ssd_define_scenario(list(ccme_boron, ccme_cadmium), ...)`.

Supplying both a named list and `name=` is an error.

## `ci = FALSE`

When `ci = FALSE` is the only confidence-interval value, the
bootstrap-only knobs `nboot`, `ci_method`, and `parametric` are
meaningless. Passing any of them in that case is an error; set
`ci = c(FALSE, TRUE)` to enable bootstrap, or omit the knobs.

## Examples

``` r
ssd_define_scenario(ssddata::ccme_boron, nsim = 100L, nrow = c(5L, 10L), seed = 42L)
#> <ssdsims_scenario>
#>   seed:     42
#>   nsim:     100
#>   datasets: ccme_boron
#>   nrow:     5, 10
#>   fit grid:
#>     dists: gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull
#>     rescale: FALSE
#>     computable: FALSE
#>     at_boundary_ok: TRUE
#>     min_pmix: ssd_min_pmix
#>     range_shape1: {0.05, 20}
#>     range_shape2: {0.05, 20}
#>   hc grid:
#>     proportion: 0.05
#>     ci: FALSE
#>     nboot: 1000
#>     est_method: multi
#>     ci_method: weighted_samples
#>     parametric: TRUE
```
