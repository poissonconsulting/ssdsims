# With L'Ecuyer-CMRG Seed

Evaluates `code` with the L'Ecuyer-CMRG RNG seeded with a scalar integer
via [`base::set.seed()`](https://rdrr.io/r/base/Random.html), then
restores the previous state. For a `.Random.seed`-style state vector
(e.g. from `get_lecuyer_cmrg_stream_state()` or
[`parallel::nextRNGStream()`](https://rdrr.io/r/parallel/RngStream.html))
use
[`with_lecuyer_cmrg_state()`](https://poissonconsulting.github.io/ssdsims/reference/with_lecuyer_cmrg_state.md).

## Usage

``` r
with_lecuyer_cmrg_seed(seed, code)
```

## Arguments

- seed:

  `[integer(1)]`  
  The random seed to use to evaluate the code.

- code:

  `[any]`  
  Code to execute in the temporary environment

## See also

[`withr::with_seed()`](https://withr.r-lib.org/reference/with_seed.html),
[`with_lecuyer_cmrg_state()`](https://poissonconsulting.github.io/ssdsims/reference/with_lecuyer_cmrg_state.md),
[`parallel::nextRNGStream()`](https://rdrr.io/r/parallel/RngStream.html).

## Examples

``` r

with_lecuyer_cmrg_seed(42, {
  runif(3)
})
#> [1] 0.1738456 0.5547401 0.4833771
```
