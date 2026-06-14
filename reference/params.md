# Parameter Descriptions for Package

Default parameter descriptions which may be overridden in individual
functions.

## Arguments

- ...:

  Unused.

- args:

  A named list of the argument values.

- ci:

  A scalar flag specifying whether to estimate confidence intervals (by
  bootstrapping). It is not a cross-join axis; the point estimate is
  identical whether `TRUE` or `FALSE`, so `ci = TRUE` is a superset of
  `ci = FALSE`.

- ci_method:

  A character vector specifying the method to use for estimating the
  confidence limits.
  [`ssdtools::ssd_ci_methods()`](https://bcgov.github.io/ssdtools/reference/ssd_ci_methods.html)
  returns the possible values.

- dist_sim:

  A character vector specifying the distributions in the fitdists object
  or
  ``` "all"`` for all the distributions and/or  ```"top"`to use the distribution with most weight and/or`"multi"\`
  to treat the distributions as a single distribution.

- nrow:

  A numeric vector of the number of rows in the generated data which
  must be between 5 and 1,000,

- nrow_max:

  A whole number (default `1000L`): the fixed size of the shared
  `sample` draw that every `nrow` value sub-truncates. A sample-level
  simulation setting, not a cross-join axis. The effective per-dataset
  draw is `min(nrow_max, nrow(data))` when `replace = FALSE` (the high
  default draws the full permutation) and `nrow_max` rows when
  `replace = TRUE`; each `nrow` must not exceed the effective draw size.

- nsim:

  A count of the number of data sets to generate.

- pars:

  A named list of the parameter values.

- replace:

  A logical vector specifying whether to sample with replacement.

- seed:

  An integer of the starting seed or NULL.

- start_sim:

  A count of the number of the simulation to start from.

- stream:

  A count of the stream number.

- .progress:

  Whether to show a `purrr::progress bar`.

## Details

A flag is a non-missing logical scalar.

A string is a non-missing character scalar.
