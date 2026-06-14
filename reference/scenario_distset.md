# Isolate a Distribution Set from a Scenario by Name

Returns the member character vector of the distribution set stored on
`scenario` under `name` (from `scenario$hc$distsets`).
[`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md)
validates each set at construction (via
[`ssd_distset()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_distset.md)),
so this accessor performs no registration, persistence, or
re-validation - it just isolates the members the hc runner subsets the
parent union fit by. Aborts with an informative error when `name` is not
one of the scenario's distribution-set names.

## Usage

``` r
scenario_distset(scenario, name)
```

## Arguments

- scenario:

  An `ssdsims_scenario` from
  [`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md).

- name:

  A scalar string naming one of the scenario's distribution sets.

## Value

The member character vector of the distribution set stored under `name`.

## Details

Names - not members - drive task hashing (`TARGETS-DESIGN.md` section
1.1): the hc-task path carries the `distset` *name*, and this accessor
resolves it back to the member vector at run time, so the members never
enter a task identity (the same by-name pattern as `min_pmix` and
datasets).

## See also

[`ssd_distset()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_distset.md),
[`scenario_min_pmix()`](https://poissonconsulting.github.io/ssdsims/reference/scenario_min_pmix.md).

## Examples

``` r
data <- ssd_scenario_data(ssddata::ccme_boron)
scenario <- ssd_define_scenario(
  data,
  nsim = 1L,
  seed = 42L,
  dists = ssd_distset(BCANZ = ssdtools::ssd_dists_bcanz())
)
scenario_distset(scenario, "BCANZ")
#> [1] "gamma"       "lgumbel"     "llogis"      "lnorm"       "lnorm_lnorm"
#> [6] "weibull"    
```
