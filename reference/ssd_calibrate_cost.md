# Calibrate the Per-task Cost Model on the Current Machine

Runs a small, fixed benchmark sweep on the *current* architecture - tiny
`nboot` values, all
[`ssdtools::ssd_ci_methods()`](https://bcgov.github.io/ssdtools/reference/ssd_ci_methods.html),
and a couple of `nrow` values - times each
[`ssdtools::ssd_hc()`](https://bcgov.github.io/ssdtools/reference/ssd_hc.html)
call, fits the per-`ci_method` cost model
`time = (base + slope * max(nboot, n0)) * nrow_factor(nrow)`, and
returns a versioned `ssdsims_cost_calibration` object carrying the
fitted coefficients and provenance. Producing an architecture-specific
estimator is nothing more than this single call plus passing the result
to
[`ssd_estimate_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_estimate_cost.md).

## Usage

``` r
ssd_calibrate_cost(
  nboot = c(20L, 50L, 100L, 200L),
  nrow = c(5L, 10L, 20L, 50L),
  data = NULL,
  seed = 42L
)
```

## Arguments

- nboot:

  An integer vector of tiny bootstrap sizes to sweep (the slope and
  floor are fit over these).

- nrow:

  An integer vector of at least two sample sizes to fit one model per
  (the `nrow_factor` is derived from these).

- data:

  A reference data frame with a numeric `Conc` column to resample
  (default
  [`ssddata::ccme_boron`](https://rdrr.io/pkg/ssddata/man/ccme_boron.html)).

- seed:

  A whole number seeding the resampling so the sweep is reproducible.

## Value

An `ssdsims_cost_calibration` object with per-`ci_method`
`base`/`slope`/`n0` coefficients, a bounded `nrow_factor`, a
`fixed_addend`, and provenance (cpu, R version, ssdtools version, date,
sweep grid).

## Details

The sweep is self-contained and dependency-light: it draws data by
resampling a reference dataset
([`ssddata::ccme_boron`](https://rdrr.io/pkg/ssddata/man/ccme_boron.html)
by default) and times with base
[`system.time()`](https://rdrr.io/r/base/system.time.html). It takes
minutes - not the hours a real scenario costs - because the `nboot`
values are tiny (the slope and floor are estimable from small
bootstraps). The one-time research that discovered the model's *form*
(which axes are free, the `max(nboot, n0)` shape, the non-monotonic
`nrow` factor) is preserved under the change's `exploration/` directory;
it is not rerun here.

## See also

[`ssd_cost_calibration()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_cost_calibration.md)
for the shipped default and
[`ssd_estimate_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_estimate_cost.md)
to apply the result.

## Examples

``` r
if (FALSE) { # \dontrun{
calibration <- ssd_calibrate_cost()
calibration
} # }
```
