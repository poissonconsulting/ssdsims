# Parameter Descriptions for Package

Default parameter descriptions which may be overridden in individual
functions.

## Arguments

- ...:

  Unused.

- args:

  A named list of the argument values.

- ci:

  A flag specifying whether to estimate confidence intervals (by
  bootstrapping).

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
