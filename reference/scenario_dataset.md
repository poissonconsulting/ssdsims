# Isolate a Materialised Dataset from a Scenario by Name

Returns the validated, materialised dataset tibble stored on `scenario`
under `name`. The dataset was validated (a numeric `Conc` column) and
materialised at construction by
[`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md),
so this accessor performs no registration, persistence, or
re-validation - it just isolates the value a shard body fits. Aborts
with an informative error when `name` is not one of the scenario's
datasets.

## Usage

``` r
scenario_dataset(scenario, name)
```

## Arguments

- scenario:

  An `ssdsims_scenario` from
  [`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md).

- name:

  A scalar string naming one of the scenario's datasets.

## Value

The materialised dataset tibble stored under `name`.

## Details

Names - not values - drive task hashing (`TARGETS-DESIGN.md` section
1.1): the task path carries the dataset *name* and this accessor
resolves it back to the tibble at run time, so the tibble never enters a
task identity.

## See also

[`scenario_min_pmix()`](https://poissonconsulting.github.io/ssdsims/reference/scenario_min_pmix.md)
for the `min_pmix` counterpart.

## Examples

``` r
data <- ssd_scenario_data(ssddata::ccme_boron)
scenario <- ssd_define_scenario(data, nsim = 1L, seed = 42L)
scenario_dataset(scenario, "ccme_boron")
#> # A tibble: 28 × 6
#>    Chemical Species                  Conc Group        Units Medium    
#>    <chr>    <chr>                   <dbl> <fct>        <chr> <chr>     
#>  1 Boron    Oncorhynchus mykiss       2.1 Fish         mg/L  Freshwater
#>  2 Boron    Ictalurus punctatus       2.4 Fish         mg/L  Freshwater
#>  3 Boron    Micropterus salmoides     4.1 Fish         mg/L  Freshwater
#>  4 Boron    Brachydanio rerio        10   Fish         mg/L  Freshwater
#>  5 Boron    Carassius auratus        15.6 Fish         mg/L  Freshwater
#>  6 Boron    Pimephales promelas      18.3 Fish         mg/L  Freshwater
#>  7 Boron    Daphnia magna             6   Invertebrate mg/L  Freshwater
#>  8 Boron    Opercularia bimarginata  10   Invertebrate mg/L  Freshwater
#>  9 Boron    Ceriodaphnia dubia       13.4 Invertebrate mg/L  Freshwater
#> 10 Boron    Entosiphon sulcatum      15   Invertebrate mg/L  Freshwater
#> # ℹ 18 more rows
```
