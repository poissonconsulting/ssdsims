# Assemble One or More Distribution Sets

Constructs a validated, named `ssdsims_distset` collection - the single
entry point `ssd_define_scenario(dists = ...)` accepts. A distribution
**set** is the pool of distributions model-averaged together to form one
SSD (one `est`); a **collection** is a named list of such sets, each
supplied as a `...` argument whose name labels the set.

## Usage

``` r
ssd_distset(...)
```

## Arguments

- ...:

  One or more named character vectors, each a distribution set (its name
  labels the pool, its values are the distribution names averaged
  together).

## Value

An `ssdsims_distset` object: a validated, named list of
distribution-name character vectors.

## Details

The fit step fits the **union** of every set's members once (the single
model-averaged superset every pool is a subset of); the hc step then
subsets that one union fit down to each set's members and re-averages,
so several pools share one fit rather than re-fitting (`distset` is an
hc-level axis - see
[`ssd_scenario_hc_tasks()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_tasks.md)).
The set **name** is what hashes onto the hc task path (mirroring the
by-name treatment of `min_pmix` and datasets); the member vectors are
carried for execution only and never enter a task hash.

Set names are taken from the `...` argument names and must be unique,
non-missing, and filesystem-safe (each becomes a `distset=<name>` Hive
path segment). Each set must be a non-empty, unique, non-`NA` character
vector whose members are a subset of
[`ssdtools::ssd_dists_all()`](https://bcgov.github.io/ssdtools/reference/ssd_dists_all.html).

## See also

[`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md),
[`scenario_distset()`](https://poissonconsulting.github.io/ssdsims/reference/scenario_distset.md).

## Examples

``` r
ssd_distset(BCANZ = ssdtools::ssd_dists_bcanz())
#> <ssdsims_distset>
#>   BCANZ: gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull
ssd_distset(
  BCANZ = ssdtools::ssd_dists_bcanz(),
  Iwasaki = c("burrIII3", "gamma", "llogis", "lnorm", "weibull"),
  lnorm = "lnorm"
)
#> <ssdsims_distset>
#>   BCANZ: gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull
#>   Iwasaki: burrIII3, gamma, llogis, lnorm, weibull
#>   lnorm: lnorm
```
