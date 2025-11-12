# Run Scenario

Run Scenario

## Usage

``` r
ssd_run_scenario(x, ...)

# S3 method for class 'data.frame'
ssd_run_scenario(
  x,
  ...,
  nrow = 6L,
  replace = FALSE,
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
  seed = NULL,
  nsim = 100L,
  stream = getOption("ssdsims.stream", 1L),
  start_sim = 1L,
  .progress = FALSE
)

# S3 method for class 'fitdists'
ssd_run_scenario(
  x,
  ...,
  nrow = 6L,
  dist_sim = "top",
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
  seed = NULL,
  nsim = 100L,
  stream = getOption("ssdsims.stream", 1L),
  start_sim = 1L,
  .progress = FALSE
)

# S3 method for class 'tmbfit'
ssd_run_scenario(
  x,
  ...,
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
  seed = NULL,
  nsim = 100L,
  stream = getOption("ssdsims.stream", 1L),
  start_sim = 1L,
  .progress = FALSE
)

# S3 method for class 'character'
ssd_run_scenario(
  x,
  ...,
  nrow = 6L,
  args = list(),
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
  seed = NULL,
  nsim = 100L,
  stream = getOption("ssdsims.stream", 1L),
  start_sim = 1L,
  .progress = FALSE
)

# S3 method for class '`function`'
ssd_run_scenario(
  x,
  ...,
  nrow = 6L,
  args = list(),
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
  seed = NULL,
  nsim = 100L,
  stream = getOption("ssdsims.stream", 1L),
  start_sim = 1L,
  .progress = FALSE
)
```

## Arguments

- x:

  The object to use for the scenario.

- ...:

  Unused.

- nrow:

  A positive whole number of the minimum number of non-missing rows.

- replace:

  A logical vector specifying whether to sample with replacement.

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

  A number between 0 and 0.5 specifying the minimum proportion in
  mixture models.

- range_shape1:

  A numeric vector of length two of the lower and upper bounds for the
  shape1 parameter.

- range_shape2:

  shape2 parameter.

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

- parametric:

  A flag specifying whether to perform parametric bootstrapping as
  opposed to non-parametrically resampling the original data with
  replacement.

- seed:

  An integer of the starting seed or NULL.

- nsim:

  A count of the number of data sets to generate.

- stream:

  A count of the stream number.

- start_sim:

  A count of the number of the simulation to start from.

- .progress:

  Whether to show a `purrr::progress bar`.

- dist_sim:

  A character vector specifying the distributions in the fitdists object
  or
  ``` "all"`` for all the distributions and/or  ```"top"`to use the distribution with most weight and/or`"multi"\`
  to treat the distributions as a single distribution.

- args:

  A named list of the argument values.

## Value

A tibble of nested data sets.

## Methods (by class)

- `ssd_run_scenario(data.frame)`: Run scenario using data.frame to
  sample data

- `ssd_run_scenario(fitdists)`: Run scenario using fitdists object to
  generate data

- `ssd_run_scenario(tmbfit)`: Run scenario using tmbfit object to
  generate data

- `ssd_run_scenario(character)`: Run scenario using name of function to
  generate sequence of random numbers

- `` ssd_run_scenario(`function`) ``: Run scenario data using function
  to generate sequence of random numbers

## Examples

``` r
ssd_run_scenario(ssddata::ccme_boron, nsim = 2)
#> # A tibble: 2 × 17
#>     sim stream  nrow replace data     rescale computable at_boundary_ok min_pmix
#>   <int>  <int> <int> <lgl>   <list>   <lgl>   <lgl>      <lgl>          <list>  
#> 1     1      1     6 FALSE   <tibble> FALSE   FALSE      TRUE           <fn>    
#> 2     2      1     6 FALSE   <tibble> FALSE   FALSE      TRUE           <fn>    
#> # ℹ 8 more variables: range_shape1 <list>, range_shape2 <list>, fits <list>,
#> #   nboot <dbl>, est_method <chr>, ci_method <chr>, parametric <lgl>, hc <list>

fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron)
ssd_run_scenario(fit, dist_sim = c("lnorm", "top"), nsim = 3)
#> # A tibble: 6 × 17
#>     sim stream  nrow dist_sim data             rescale computable at_boundary_ok
#>   <int>  <int> <int> <chr>    <list>           <lgl>   <lgl>      <lgl>         
#> 1     1      1     6 lnorm    <tibble [6 × 1]> FALSE   FALSE      TRUE          
#> 2     1      1     6 top      <tibble [6 × 1]> FALSE   FALSE      TRUE          
#> 3     2      1     6 lnorm    <tibble [6 × 1]> FALSE   FALSE      TRUE          
#> 4     2      1     6 top      <tibble [6 × 1]> FALSE   FALSE      TRUE          
#> 5     3      1     6 lnorm    <tibble [6 × 1]> FALSE   FALSE      TRUE          
#> 6     3      1     6 top      <tibble [6 × 1]> FALSE   FALSE      TRUE          
#> # ℹ 9 more variables: min_pmix <list>, range_shape1 <list>,
#> #   range_shape2 <list>, fits <list>, nboot <dbl>, est_method <chr>,
#> #   ci_method <chr>, parametric <lgl>, hc <list>

fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron)
ssd_run_scenario(fit[[1]], nsim = 3)
#> # A tibble: 3 × 17
#>     sim stream  nrow args   data     rescale computable at_boundary_ok min_pmix
#>   <int>  <int> <int> <list> <list>   <lgl>   <lgl>      <lgl>          <list>  
#> 1     1      1     6 <list> <tibble> FALSE   FALSE      TRUE           <fn>    
#> 2     2      1     6 <list> <tibble> FALSE   FALSE      TRUE           <fn>    
#> 3     3      1     6 <list> <tibble> FALSE   FALSE      TRUE           <fn>    
#> # ℹ 8 more variables: range_shape1 <list>, range_shape2 <list>, fits <list>,
#> #   nboot <dbl>, est_method <chr>, ci_method <chr>, parametric <lgl>, hc <list>

ssd_run_scenario("rlnorm", nsim = 3)
#> # A tibble: 3 × 17
#>     sim stream  nrow args   data     rescale computable at_boundary_ok min_pmix
#>   <int>  <int> <int> <list> <list>   <lgl>   <lgl>      <lgl>          <list>  
#> 1     1      1     6 <list> <tibble> FALSE   FALSE      TRUE           <fn>    
#> 2     2      1     6 <list> <tibble> FALSE   FALSE      TRUE           <fn>    
#> 3     3      1     6 <list> <tibble> FALSE   FALSE      TRUE           <fn>    
#> # ℹ 8 more variables: range_shape1 <list>, range_shape2 <list>, fits <list>,
#> #   nboot <dbl>, est_method <chr>, ci_method <chr>, parametric <lgl>, hc <list>

ssd_run_scenario(ssdtools::ssd_rlnorm, nsim = 3)
#> # A tibble: 3 × 17
#>     sim stream  nrow args   data     rescale computable at_boundary_ok min_pmix
#>   <int>  <int> <int> <list> <list>   <lgl>   <lgl>      <lgl>          <list>  
#> 1     1      1     6 <list> <tibble> FALSE   FALSE      TRUE           <fn>    
#> 2     2      1     6 <list> <tibble> FALSE   FALSE      TRUE           <fn>    
#> 3     3      1     6 <list> <tibble> FALSE   FALSE      TRUE           <fn>    
#> # ℹ 8 more variables: range_shape1 <list>, range_shape2 <list>, fits <list>,
#> #   nboot <dbl>, est_method <chr>, ci_method <chr>, parametric <lgl>, hc <list>
```
