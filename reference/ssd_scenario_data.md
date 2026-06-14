# Assemble and Validate Datasets for a Simulation Scenario

Collects one or more datasets into a validated, named collection - the
single entry point through which
[`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md)
takes dataset input. Each dataset must carry a numeric `Conc` column
(the species sensitivity distribution convention); additional columns
are preserved.

## Usage

``` r
ssd_scenario_data(...)
```

## Arguments

- ...:

  One or more data frames, optionally named, and/or
  [`ssd_gen()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_gen.md)
  collections (unnamed, or spliced with `!!!`). Each data frame is
  validated for a numeric `Conc` column.

## Value

An `ssdsims_data` object: a named list of validated tibbles.

## Details

Names are taken from the argument names where supplied, otherwise
derived from the argument expression by symbol capture (e.g.
[`ssddata::ccme_boron`](https://rdrr.io/pkg/ssddata/man/ccme_boron.html)
becomes `"ccme_boron"`). A literal with no derivable name (e.g. a bare
`data.frame(...)` call) must be given an explicit name. Names must be
unique across the collection.

Generator-style inputs (a `fitdists` or `tmbfit` object, a generator
function, or a function-name string) enter the collection through
[`ssd_gen()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_gen.md),
which materialises each, once, to a reproducible `Conc` tibble. Its
result composes with the data-frame inputs in two equivalent ways:
passed as an unnamed argument, the collection is flattened in (each
materialised tibble becomes a member under its own name); or spliced
with `!!!`
([`rlang::list2()`](https://rlang.r-lib.org/reference/list2.html)
splicing), with identical results:

    ssd_scenario_data(boron = ccme_boron, ssd_gen(synth = ssd_rlnorm, .n = 30, .seed = 1L))
    ssd_scenario_data(boron = ccme_boron, !!!ssd_gen(synth = ssd_rlnorm, .n = 30, .seed = 1L))

A materialised generator dataset is an ordinary tibble in the
collection, indistinguishable downstream from a data-frame dataset.

## See also

[`ssd_gen()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_gen.md),
[`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md).

## Examples

``` r
ssd_scenario_data(ssddata::ccme_boron)
#> $ccme_boron
#> # A tibble: 28 × 5
#>    Chemical Species                  Conc Group        Units
#>    <chr>    <chr>                   <dbl> <fct>        <chr>
#>  1 Boron    Oncorhynchus mykiss       2.1 Fish         mg/L 
#>  2 Boron    Ictalurus punctatus       2.4 Fish         mg/L 
#>  3 Boron    Micropterus salmoides     4.1 Fish         mg/L 
#>  4 Boron    Brachydanio rerio        10   Fish         mg/L 
#>  5 Boron    Carassius auratus        15.6 Fish         mg/L 
#>  6 Boron    Pimephales promelas      18.3 Fish         mg/L 
#>  7 Boron    Daphnia magna             6   Invertebrate mg/L 
#>  8 Boron    Opercularia bimarginata  10   Invertebrate mg/L 
#>  9 Boron    Ceriodaphnia dubia       13.4 Invertebrate mg/L 
#> 10 Boron    Entosiphon sulcatum      15   Invertebrate mg/L 
#> # ℹ 18 more rows
#> 
#> attr(,"class")
#> [1] "ssdsims_data"
ssd_scenario_data(boron = ssddata::ccme_boron, cadmium = ssddata::ccme_cadmium)
#> $boron
#> # A tibble: 28 × 5
#>    Chemical Species                  Conc Group        Units
#>    <chr>    <chr>                   <dbl> <fct>        <chr>
#>  1 Boron    Oncorhynchus mykiss       2.1 Fish         mg/L 
#>  2 Boron    Ictalurus punctatus       2.4 Fish         mg/L 
#>  3 Boron    Micropterus salmoides     4.1 Fish         mg/L 
#>  4 Boron    Brachydanio rerio        10   Fish         mg/L 
#>  5 Boron    Carassius auratus        15.6 Fish         mg/L 
#>  6 Boron    Pimephales promelas      18.3 Fish         mg/L 
#>  7 Boron    Daphnia magna             6   Invertebrate mg/L 
#>  8 Boron    Opercularia bimarginata  10   Invertebrate mg/L 
#>  9 Boron    Ceriodaphnia dubia       13.4 Invertebrate mg/L 
#> 10 Boron    Entosiphon sulcatum      15   Invertebrate mg/L 
#> # ℹ 18 more rows
#> 
#> $cadmium
#> # A tibble: 36 × 5
#>    Chemical Species                   Conc Group Units
#>    <chr>    <chr>                    <dbl> <fct> <chr>
#>  1 Cadmium  Oncorhynchus mykiss       0.23 Fish  ug/L 
#>  2 Cadmium  Salvelinus confluentus    0.83 Fish  ug/L 
#>  3 Cadmium  Cottus bairdi             0.96 Fish  ug/L 
#>  4 Cadmium  Salmo salar               0.99 Fish  ug/L 
#>  5 Cadmium  Acipenser transmontanus   1.14 Fish  ug/L 
#>  6 Cadmium  Prosopium williamsoni     1.25 Fish  ug/L 
#>  7 Cadmium  Salmo trutta              1.36 Fish  ug/L 
#>  8 Cadmium  Salvelinus fontinalis     2.23 Fish  ug/L 
#>  9 Cadmium  Oncorhynchus tshawytscha  2.29 Fish  ug/L 
#> 10 Cadmium  Pimephales promelas       2.36 Fish  ug/L 
#> # ℹ 26 more rows
#> 
#> attr(,"class")
#> [1] "ssdsims_data"
```
