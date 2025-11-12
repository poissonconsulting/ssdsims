# Local L'Euyer-CMRG Seed

Local L'Euyer-CMRG Seed

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

[`withr::local_seed()`](https://withr.r-lib.org/reference/with_seed.html)

## Examples

``` r
local_lecuyer_cmrg_seed(42)
runif(3)
#> [1] 0.1738456 0.5547401 0.4833771
```
