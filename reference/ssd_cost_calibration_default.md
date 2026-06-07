# Default Cost Calibration Object

The `ssdsims_cost_calibration` object shipped with the package and
returned by
[`ssd_cost_calibration()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_cost_calibration.md).
It carries the per-`ci_method` cost-model coefficients (`base`, `slope`,
`n0`), the bounded `nrow_factor`, a `fixed_addend` (sample + fit
per-task overhead), and the provenance of the machine it was fitted on.
[`ssd_estimate_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_estimate_cost.md)
uses it when no `calibration` is supplied.

## Usage

``` r
ssd_cost_calibration_default
```

## Format

An `ssdsims_cost_calibration` object: a list with `coefficients` (a
tibble of `ci_method`, `base`, `slope`, `n0`), `nrow_factor` (a tibble
of `nrow`, `factor`), `fixed_addend` (a scalar), and `provenance` (cpu,
R version, ssdtools version, date, sweep grid).

## Source

Fitted by
[`ssd_calibrate_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_calibrate_cost.md)
during package development (Intel Xeon @ 2.10 GHz, R 4.5.3, ssdtools
2.6.0.9002).

## Details

Because the coefficients are architecture-specific, this default yields
a *ballpark* estimate sized for the machine in its provenance. Re-fit on
your own machine with
[`ssd_calibrate_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_calibrate_cost.md)
for a trustworthy estimate. It was produced by
`data-raw/cost_calibration.R` (which simply runs
[`ssd_calibrate_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_calibrate_cost.md)).

## See also

[`ssd_cost_calibration()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_cost_calibration.md),
[`ssd_calibrate_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_calibrate_cost.md),
[`ssd_estimate_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_estimate_cost.md).
