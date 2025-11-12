# With L'Euyer-CMRG Seed

With L'Euyer-CMRG Seed

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

[`withr::with_seed()`](https://withr.r-lib.org/reference/with_seed.html)

## Examples

``` r
with_lecuyer_cmrg_seed(42, {
runif(3)
})
#> [1] 0.1738456 0.5547401 0.4833771
```
