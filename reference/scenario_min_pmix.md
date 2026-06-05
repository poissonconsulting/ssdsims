# Isolate a Materialised `min_pmix` Function from a Scenario by Name

Returns the single-argument `min_pmix` function materialised on
`scenario` under `name`.
[`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md)
resolves each `min_pmix` reference to a function once, at construction
(so a cluster worker needs no shared interactive environment), and
stores it keyed by name; this accessor isolates it. Aborts with an
informative error when `name` is not one of the scenario's `min_pmix`
names.

## Usage

``` r
scenario_min_pmix(scenario, name)
```

## Arguments

- scenario:

  An `ssdsims_scenario` from
  [`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md).

- name:

  A scalar string naming one of the scenario's `min_pmix` entries.

## Value

The single-argument `min_pmix` function stored under `name`.

## Details

Names - not function values - drive task hashing (`TARGETS-DESIGN.md`
section 1.1): the fit-task path carries the `min_pmix` *name*, and this
accessor resolves it back to the function at run time, so the function
value never enters a task identity (no byte-stability concern from
byte-compilation or captured environments).

## See also

[`scenario_dataset()`](https://poissonconsulting.github.io/ssdsims/reference/scenario_dataset.md)
for the dataset counterpart.

## Examples

``` r
scenario <- ssd_define_scenario(ssddata::ccme_boron, nsim = 1L, seed = 42L)
scenario_min_pmix(scenario, "ssd_min_pmix")
#> function(n) {
#>   chk_whole_number(n)
#>   chk_gt(n)
#>   max(min(3 / n, 0.5), 0.1)
#> }
#> <bytecode: 0x563a5d341a00>
#> <environment: namespace:ssdtools>
```
