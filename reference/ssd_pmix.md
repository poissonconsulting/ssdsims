# Assemble and Validate `min_pmix` Functions for a Simulation Scenario

Collects one or more `min_pmix` functions into a validated, named
collection - the single entry point through which
[`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md)
takes `min_pmix` input. Each entry must be a single-argument
**function** (it inputs the number of rows of data and returns a
proportion between 0 and 0.5); a name-string is **not** accepted, so the
constructor performs no string-to-function resolution.

## Usage

``` r
ssd_pmix(...)
```

## Arguments

- ...:

  One or more single-argument functions, optionally named.

## Value

An `ssdsims_pmix` object: a named list of validated single-argument
functions.

## Details

Names are taken from the argument names where supplied, otherwise
derived from the argument expression by symbol capture (e.g.
[`ssdtools::ssd_min_pmix`](https://bcgov.github.io/ssdtools/reference/ssd_min_pmix.html)
becomes `"ssd_min_pmix"`), mirroring
[`ssd_scenario_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_data.md).
Names must be unique across the collection. The name - not the function
value - is what the task path hashes; the functions ride on the scenario
for execution, isolated by name via
[`scenario_min_pmix()`](https://poissonconsulting.github.io/ssdsims/reference/scenario_min_pmix.md).

## See also

[`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md),
[`scenario_min_pmix()`](https://poissonconsulting.github.io/ssdsims/reference/scenario_min_pmix.md).

## Examples

``` r
ssd_pmix(ssdtools::ssd_min_pmix)
#> <ssdsims_pmix>
#>   ssd_min_pmix: <fn>
ssd_pmix(ssd_min_pmix = ssdtools::ssd_min_pmix, strict = function(n) 0.1)
#> <ssdsims_pmix>
#>   ssd_min_pmix: <fn>
#>   strict: <fn>
```
