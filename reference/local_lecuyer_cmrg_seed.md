# Local L'Ecuyer-CMRG Seed

Seeds the L'Ecuyer-CMRG RNG with a scalar integer via
[`base::set.seed()`](https://rdrr.io/r/base/Random.html). For a
`.Random.seed`-style state vector (e.g. from
`get_lecuyer_cmrg_stream_state()` or
[`parallel::nextRNGStream()`](https://rdrr.io/r/parallel/RngStream.html))
use
[`local_lecuyer_cmrg_state()`](https://poissonconsulting.github.io/ssdsims/reference/local_lecuyer_cmrg_state.md).

## Usage

``` r
local_lecuyer_cmrg_seed(seed, .local_envir = parent.frame())
```

## Arguments

- seed:

  `[integer(1)]`  
  The random seed to use to evaluate the code.

- .local_envir:

  `[environment]`  
  The environment to use for scoping.

## See also

[`withr::local_seed()`](https://withr.r-lib.org/reference/with_seed.html),
[`local_lecuyer_cmrg_state()`](https://poissonconsulting.github.io/ssdsims/reference/local_lecuyer_cmrg_state.md),
[`parallel::nextRNGStream()`](https://rdrr.io/r/parallel/RngStream.html).

## Examples

``` r

local_lecuyer_cmrg_seed(42)
runif(3)
#> [1] 0.1738456 0.5547401 0.4833771
```
