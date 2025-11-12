# Fit SSD Distributions to Simulated Data

Fit SSD Distributions to Simulated Data

## Usage

``` r
ssd_hc_sims(
  x,
  proportion = 0.05,
  ...,
  ci = FALSE,
  ci_method = "weighted_samples",
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

- ci_method:

  A string specifying which method to use for estimating the standard
  error and confidence limits from the bootstrap samples. Possible
  values include `ci_method = "multi_fixed"` and
  `ci_method = "multi_free"` which generate the bootstrap samples using
  the model-averaged cumulative distribution function but differ in
  whether the model weights are fixed at the values for the original
  dataset or re-estimated for each bootstrap sample dataset. The value
  `ci_method = "weighted_samples"` takes bootstrap samples from each
  distribution proportional to its AICc based weights and calculates the
  confidence limits (and SE) from this single set. The value
  `ci_method = "MACL"` (was `ci_method = "weighted_arithmetic"` but has
  been soft-deprecated) which is only included for historical reasons
  takes the weighted arithmetic mean of the confidence limits and
  `ci_method = MGCL` which was included for a research paper takes the
  weighted geometric mean of the confidence limits. The values
  `ci_method = "MAW1"` and `ci_method = "MAW2"` use the two alternative
  equations of Burnham and Anderson to model average the weighted
  standard errors and then calculate the confidence limits using the
  Wald approach. Finally `ci_method = "arithmetic"` and
  `ci_method = "geometric"` take the weighted arithmetic or geometric
  mean of the values for each bootstrap iteration across all the
  distributions and then calculate the confidence limits (and SE) from
  the single set of samples.

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
