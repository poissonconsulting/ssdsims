# Fit SSD Distributions to Simulated Data

Fit SSD Distributions to Simulated Data

## Usage

``` r
ssd_fit_dists_sims(
  x,
  dists = ssdtools::ssd_dists_bcanz(),
  ...,
  rescale = FALSE,
  computable = FALSE,
  at_boundary_ok = TRUE,
  min_pmix = list(ssdtools::ssd_min_pmix),
  range_shape1 = list(c(0.05, 20)),
  range_shape2 = range_shape1,
  seed = NULL,
  silent = TRUE,
  .progress = FALSE
)
```

## Arguments

- x:

  A data frame with sim and stream integer columns and a list column of
  the data frames to fit distributions to.

- dists:

  A character vector of the distribution names.

- ...:

  Additional arguments passed to
  [`ssdtools::ssd_fit_dists()`](https://bcgov.github.io/ssdtools/reference/ssd_fit_dists.html).

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

  A list of one or more functions with a single argument that inputs the
  number of rows of data and returns a proportion between 0 and 0.5.

- range_shape1:

  A list of numeric vectors of length two of the lower and upper bounds
  for the shape1 parameter.

- range_shape2:

  A list of numeric vectors of length two of the lower and upper bounds
  for the shape2 parameter.

- seed:

  An integer of the starting seed or NULL.

- silent:

  A flag indicating whether fits should fail silently.

- .progress:

  Whether to show a `purrr::progress bar`.

## Value

The x tibble with a list column fits of fistdist objects.
