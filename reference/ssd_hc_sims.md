# Estimate hazard concentrations for multiple simulations using bootstrapping

Estimate hazard concentrations for multiple simulations using
bootstrapping

## Usage

``` r
ssd_hc_sims(
  x,
  proportion = 0.05,
  ...,
  ci = FALSE,
  nboot = 1000,
  est_method = "multi",
  ci_method = "weighted_samples",
  parametric = TRUE,
  seed = NULL,
  save_to = NULL,
  .progress = FALSE
)
```

## Arguments

- x:

  A data frame with sim and stream integer columns and a list column of
  fitdists objects.

- proportion:

  A numeric vector of proportion values to estimate hazard
  concentrations for.

- ...:

  Additional arguments passed to
  [`ssdtools::ssd_hc()`](https://bcgov.github.io/ssdtools/reference/ssd_hc.html).

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
  completeness but is also not recommended. The values
  `ci_method = "MAW1"` and `ci_method = "MAW2"` use the two alternative
  equations of Burnham and Anderson (2002) to model average the weighted
  standard errors and then calculate the confidence limits using the
  Wald approach. The values `ci_method = "GMAW1"` and
  `ci_method = "GMAW2"` use the same equations as the previous two
  methods but model average the weighted standard errors on the
  geometric scale. Finally `ci_method = "arithmetic_samples"` and
  `ci_method = "geometric_samples"` take the weighted arithmetic or
  geometric mean of the values for each bootstrap iteration across all
  the distributions and then calculate the confidence limits (and SE)
  from the single set of samples.

- parametric:

  A flag specifying whether to perform parametric bootstrapping as
  opposed to non-parametrically resampling the original data with
  replacement.

- seed:

  An integer of the starting seed or NULL.

- save_to:

  NULL or a string specifying a directory to save where the bootstrap
  datasets and parameter estimates (when successfully converged) to.

- .progress:

  Whether to show a `purrr::progress bar`.

## Value

The x tibble with a list column hc of data frames produced by applying
ssd_hc() to fits.
