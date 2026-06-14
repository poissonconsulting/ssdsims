# Estimate a Scenario's Compute Cost and Longest Task

Predicts, *before* a scenario is launched, roughly how much compute it
costs and how long its single longest task runs. It expands the scenario
into its hc task table read-only (via
[`ssd_scenario_hc_tasks()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_tasks.md),
without running any fit or bootstrap), applies the calibrated per-task
cost model, and returns the ballpark serial **total** cost and the
**duration of the longest single task**, plus a per-axis breakdown of
which `ci_method`/`nboot` cells dominate.

## Usage

``` r
ssd_estimate_cost(scenario, calibration = ssd_cost_calibration())
```

## Arguments

- scenario:

  An `ssdsims_scenario` from
  [`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md).

- calibration:

  An `ssdsims_cost_calibration`; defaults to the shipped
  [`ssd_cost_calibration()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_cost_calibration.md).
  Pass the result of
  [`ssd_calibrate_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_calibrate_cost.md)
  for an architecture-specific estimate.

## Value

An `ssdsims_cost_estimate` object: a list with `total` and `longest`
(both `difftime` time quantities), a `breakdown` tibble grouped by
`ci_method` x `nboot`, and the calibration's `provenance`.

## Details

`proportion` and `est_method` are *free* axes: one bootstrap per
`nboot x ci_method x parametric` cell serves every
`proportion`/`est_method`, so adding values along those axes does not
change the estimate. The estimator does **not** execute the scenario,
draw random numbers, or alter any result.

The longest task is the irreducible wall-time floor under any amount of
parallelism; wall-time under `n` workers is roughly
`max(longest_task, total / n)`, computed by the caller.

## See also

[`ssd_calibrate_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_calibrate_cost.md)
and
[`ssd_cost_calibration()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_cost_calibration.md).

## Examples

``` r
data <- ssd_scenario_data(ssddata::ccme_boron)
scenario <- ssd_define_scenario(
  data,
  nsim = 10L,
  seed = 42L,
  ci = TRUE,
  nboot = c(1000L, 5000L, 10000L, 50000L),
  nrow = c(5L, 10L, 20L, 50L)
)
ssd_estimate_cost(scenario)
#> <ssdsims_cost_estimate>  (ballpark, serial)
#>   total compute:  3.6 hours
#>   longest task:   6.1 min
#>   breakdown (ci_method x nboot, by total cost):
#>     weighted_samples   nboot  50000    40 tasks  2.7 hours
#>     weighted_samples   nboot  10000    40 tasks  32.6 min
#>     weighted_samples   nboot   5000    40 tasks  16.5 min
#>     weighted_samples   nboot   1000    40 tasks  3.6 min
#>   calibration:    Intel(R) Xeon(R) Processor @ 2.10GHz | R 4.5.3 | ssdtools 2.6.0.9002 | 2026-06-07
#>   Ballpark only - recalibrate with ssd_calibrate_cost() on the target machine.
#> 
```
