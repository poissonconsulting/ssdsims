# Default Cost Calibration

Returns the `ssdsims_cost_calibration` object shipped with the package -
the calibration fitted during development (see
[ssd_cost_calibration_default](https://poissonconsulting.github.io/ssdsims/reference/ssd_cost_calibration_default.md))
and used by
[`ssd_estimate_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_estimate_cost.md)
when no `calibration` is supplied. Because the coefficients are
architecture-specific, an estimate built on this default is a *ballpark*
sized for the machine in its provenance; rerun
[`ssd_calibrate_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_calibrate_cost.md)
on your own machine for a trustworthy estimate.

## Usage

``` r
ssd_cost_calibration()
```

## Value

The shipped `ssdsims_cost_calibration` object.

## See also

[`ssd_calibrate_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_calibrate_cost.md)
to re-fit on a target machine and
[`ssd_estimate_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_estimate_cost.md)
to apply a calibration to a scenario.

## Examples

``` r
ssd_cost_calibration()
#> <ssdsims_cost_calibration>
#>   per-ci_method cost model  time = (base + slope * max(nboot, n0)) * nrow_factor
#>     weighted_samples   base   0.58s     5.50 ms/boot  n0   0
#>     arithmetic_samples base   1.00s    18.01 ms/boot  n0  25
#>     MACL               base   0.92s    18.35 ms/boot  n0  25
#>     GMACL              base   0.91s    18.36 ms/boot  n0  25
#>     geometric_samples  base   0.95s    18.48 ms/boot  n0  25
#>     multi_free         base   0.18s    40.05 ms/boot  n0   0
#>     multi_fixed        base   0.15s    40.68 ms/boot  n0   0
#>   nrow_factor:   5:0.27  10:1.09  20:1.34  50:0.82
#>   fixed_addend:  0.05s (sample + fit per task)
#>   provenance:
#>     cpu:       Intel(R) Xeon(R) Processor @ 2.10GHz
#>     R:         R version 4.5.3 (2026-03-11)
#>     ssdtools:  2.6.0.9002
#>     date:      2026-06-07
#>     sweep:     nboot {20, 50, 100, 200} x nrow {5, 10, 20, 50} x 7 ci_methods
#>   Ballpark only: coefficients are architecture-specific - recalibrate with
#>   ssd_calibrate_cost() on the target machine for a trustworthy estimate.
#> 
```
