# Generate Data for Simulations

A family of functions to generate a tibble of nested data sets.

## Usage

``` r
ssd_sim_data(x, ...)

# S3 method for class 'data.frame'
ssd_sim_data(
  x,
  ...,
  nrow = 6L,
  replace = FALSE,
  seed = NULL,
  nsim = 100L,
  stream = getOption("ssdsims.stream", 1L),
  start_sim = 1L,
  .progress = FALSE
)

# S3 method for class 'fitdists'
ssd_sim_data(
  x,
  ...,
  nrow = 6L,
  dist_sim = "top",
  seed = NULL,
  nsim = 100L,
  stream = getOption("ssdsims.stream", 1L),
  start_sim = 1L,
  .progress = FALSE
)

# S3 method for class 'tmbfit'
ssd_sim_data(
  x,
  ...,
  nrow = 6L,
  seed = NULL,
  nsim = 100L,
  stream = getOption("ssdsims.stream", 1L),
  start_sim = 1L,
  .progress = FALSE
)

# S3 method for class 'character'
ssd_sim_data(
  x,
  ...,
  nrow = 6L,
  args = list(),
  seed = NULL,
  nsim = 100L,
  stream = getOption("ssdsims.stream", 1L),
  start_sim = 1L,
  .progress = FALSE
)

# S3 method for class '`function`'
ssd_sim_data(
  x,
  ...,
  nrow = 6L,
  args = list(),
  seed = NULL,
  nsim = 100L,
  stream = getOption("ssdsims.stream", 1L),
  start_sim = 1L,
  .progress = FALSE
)
```

## Arguments

- x:

  The object to use for generating the data.

- ...:

  Unused.

- nrow:

  A numeric vector of the number of rows in the generated data which
  must be between 5 and 1,000,

- replace:

  A logical vector specifying whether to sample with replacement.

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

- `ssd_sim_data(data.frame)`: Generate data by sampling from data.frame

- `ssd_sim_data(fitdists)`: Generate data from fitdists object

- `ssd_sim_data(tmbfit)`: Generate data from tmbfit object

- `ssd_sim_data(character)`: Generate data using name of function

- `` ssd_sim_data(`function`) ``: Generate data using function to
  generate sequence of random numbers

## Examples

``` r
ssd_sim_data(ssddata::ccme_boron, nrow = 5, nsim = 3)
#> # A tibble: 3 × 5
#>     sim stream  nrow replace data            
#>   <int>  <int> <dbl> <lgl>   <list>          
#> 1     1      1     5 FALSE   <tibble [5 × 5]>
#> 2     2      1     5 FALSE   <tibble [5 × 5]>
#> 3     3      1     5 FALSE   <tibble [5 × 5]>

fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron)
ssd_sim_data(fit, nrow = 5, nsim = 3)
#> # A tibble: 3 × 5
#>     sim stream  nrow args             data            
#>   <int>  <int> <dbl> <list>           <list>          
#> 1     1      1     5 <named list [2]> <tibble [5 × 1]>
#> 2     2      1     5 <named list [2]> <tibble [5 × 1]>
#> 3     3      1     5 <named list [2]> <tibble [5 × 1]>

fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron)
ssd_sim_data(fit[[1]], nrow = 5, nsim = 3)
#> # A tibble: 3 × 5
#>     sim stream  nrow args             data            
#>   <int>  <int> <dbl> <list>           <list>          
#> 1     1      1     5 <named list [2]> <tibble [5 × 1]>
#> 2     2      1     5 <named list [2]> <tibble [5 × 1]>
#> 3     3      1     5 <named list [2]> <tibble [5 × 1]>

ssd_sim_data("rnorm", nrow = 5, nsim = 3)
#> # A tibble: 3 × 5
#>     sim stream  nrow args       data            
#>   <int>  <int> <dbl> <list>     <list>          
#> 1     1      1     5 <list [0]> <tibble [5 × 1]>
#> 2     2      1     5 <list [0]> <tibble [5 × 1]>
#> 3     3      1     5 <list [0]> <tibble [5 × 1]>

ssd_sim_data(ssdtools::ssd_rlnorm, nrow = 5, nsim = 3)
#> # A tibble: 3 × 5
#>     sim stream  nrow args       data            
#>   <int>  <int> <dbl> <list>     <list>          
#> 1     1      1     5 <list [0]> <tibble [5 × 1]>
#> 2     2      1     5 <list [0]> <tibble [5 × 1]>
#> 3     3      1     5 <list [0]> <tibble [5 × 1]>
```
