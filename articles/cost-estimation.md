# Estimating a Scenario’s Compute Cost

``` r

library(ssdsims)
```

## Why estimate cost?

A scenario is *declarative*: a few scenario axes can fan out into a
multi-day run with no warning. A benchmark of the example scenario below
— 10 simulations × 4 sample sizes × 7 `ci_method`s × 4 `nboot` values
(up to 50 000), with `ci = TRUE` — measured roughly **400+ single-core
hours** (the better part of three weeks), dominated by the
hazard-concentration (hc) bootstrap, with a single `multi_*` +
`nboot = 50 000` task taking on the order of an hour on its own.

Before launching such a run you want two numbers:

- the **total** compute it costs (to size a budget), and
- the **single longest task** (the irreducible wall-time floor — no
  amount of parallelism finishes the job faster than its slowest task).

[`ssd_estimate_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_estimate_cost.md)
predicts both by *reading* the scenario’s task expansion and applying a
calibrated cost model. It never fits a distribution, runs a bootstrap,
draws a random number, or writes a file.

## The cost model

Session benchmarking established that for `ci = TRUE` the hc bootstrap
dominates: the fit and data-sampling steps are comparatively cheap.
Per-task time follows a simple model, fit separately for each
`ci_method`:

``` math
\text{time} = \bigl(\text{base} + \text{slope} \times \max(\text{nboot}, n_0)\bigr) \times \text{nrow\_factor}(\text{nrow})
```

Three properties make this tractable:

- **`proportion` and `est_method` are free.** One bootstrap per
  `nboot × ci_method × parametric` cell serves every `proportion` and
  `est_method`, so adding values along those axes does **not** multiply
  the cost. (`proportion` is vectorised into a single `ssd_hc()` call;
  `est_method` is a post-hoc aggregation of the same bootstrap.)
- **A bootstrap floor `n0`.** Below `n0` draws the per-call time is
  roughly constant (fixed overhead dominates); above it the time grows
  linearly in `nboot`. The model uses `max(nboot, n0)`.
- **A bounded, non-monotonic `nrow` factor.** Sample size has a weak,
  data-dependent effect — cheap at `nrow = 5` (where most `bcanz`
  distributions fail to fit), peaking around 10–20, easing again at 50.
  It is captured as a bounded lookup, **not** extrapolated as a linear
  term, and is the least precise part of the model.

The per-`ci_method` slopes span roughly 9× — `weighted_samples` is the
cheapest, the `multi_*` methods the most expensive — so the `ci_method`
grid is by far the dominant cost driver.

The one-time research that *discovered* this model’s form (which axes
are free, the `max(nboot, n0)` shape, the non-monotonic `nrow` factor)
is illustrative — you never rerun it. Recalibrating for a new machine is
a single function call, described below.

## The calibration object

The coefficients are **architecture-specific** — a slope measured on one
CPU will not match another. The package therefore ships both a *default*
calibration (fitted during development) and a *method* to re-measure it.
The default is returned by
[`ssd_cost_calibration()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_cost_calibration.md):

``` r

calibration <- ssd_cost_calibration()
calibration
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
```

The printed object carries the fitted per-`ci_method` coefficients, the
`nrow` factor, a fixed sample+fit per-task addend, and its
**provenance** — the CPU, R version, `ssdtools` version, date, and sweep
grid it was measured on. A stale default is therefore visible in any
estimate built from it, and the printed caveat is explicit: the estimate
is a *ballpark*.

## Estimating a scenario

[`ssd_estimate_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_estimate_cost.md)
takes a scenario and (optionally) a calibration. Here is the motivating
~430-hour scenario:

``` r

data <- ssd_scenario_data(ssddata::ccme_boron)
scenario <- ssd_define_scenario(
  data,
  nsim = 10L,
  seed = 42L,
  replace = TRUE,
  nrow = c(5L, 10L, 20L, 50L),
  ci = TRUE,
  nboot = c(1000L, 5000L, 10000L, 50000L),
  ci_method = ssdtools::ssd_ci_methods()
)

estimate <- ssd_estimate_cost(scenario)
estimate
#> <ssdsims_cost_estimate>  (ballpark, serial)
#>   total compute:  4.3 days
#>   longest task:   45.3 min
#>   breakdown (ci_method x nboot, by total cost):
#>     multi_fixed        nboot  50000    40 tasks  19.9 hours
#>     multi_free         nboot  50000    40 tasks  19.6 hours
#>     geometric_samples  nboot  50000    40 tasks  9.0 hours
#>     GMACL              nboot  50000    40 tasks  9.0 hours
#>     MACL               nboot  50000    40 tasks  9.0 hours
#>     arithmetic_samples nboot  50000    40 tasks  8.8 hours
#>     multi_fixed        nboot  10000    40 tasks  4.0 hours
#>     multi_free         nboot  10000    40 tasks  3.9 hours
#>     weighted_samples   nboot  50000    40 tasks  2.7 hours
#>     multi_fixed        nboot   5000    40 tasks  2.0 hours
#>     multi_free         nboot   5000    40 tasks  2.0 hours
#>     geometric_samples  nboot  10000    40 tasks  1.8 hours
#>     GMACL              nboot  10000    40 tasks  1.8 hours
#>     MACL               nboot  10000    40 tasks  1.8 hours
#>     arithmetic_samples nboot  10000    40 tasks  1.8 hours
#>     geometric_samples  nboot   5000    40 tasks  54.8 min
#>     GMACL              nboot   5000    40 tasks  54.4 min
#>     MACL               nboot   5000    40 tasks  54.4 min
#>     arithmetic_samples nboot   5000    40 tasks  53.4 min
#>     weighted_samples   nboot  10000    40 tasks  32.6 min
#>     multi_fixed        nboot   1000    40 tasks  24.0 min
#>     multi_free         nboot   1000    40 tasks  23.6 min
#>     weighted_samples   nboot   5000    40 tasks  16.5 min
#>     geometric_samples  nboot   1000    40 tasks  11.4 min
#>     GMACL              nboot   1000    40 tasks  11.3 min
#>     MACL               nboot   1000    40 tasks  11.3 min
#>     arithmetic_samples nboot   1000    40 tasks  11.2 min
#>     weighted_samples   nboot   1000    40 tasks  3.6 min
#>   calibration:    Intel(R) Xeon(R) Processor @ 2.10GHz | R 4.5.3 | ssdtools 2.6.0.9002 | 2026-06-07
#>   Ballpark only - recalibrate with ssd_calibrate_cost() on the target machine.
```

The estimate reports the serial **total** and the **longest single
task**, both as time quantities, plus a breakdown by `ci_method × nboot`
ordered by total cost — so you can see at a glance which cells dominate
(here, the `multi_*` methods at `nboot = 50000`). The exact figures
track the calibration’s machine; on a different CPU, recalibrate (below)
for trustworthy numbers.

`proportion` and `est_method` are free, so widening them leaves the
total unchanged:

``` r

data <- ssd_scenario_data(ssddata::ccme_boron)
wider <- ssd_define_scenario(
  data,
  nsim = 10L,
  seed = 42L,
  replace = TRUE,
  nrow = c(5L, 10L, 20L, 50L),
  est_method = ssdtools::ssd_est_methods(),
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = TRUE,
  nboot = c(1000L, 5000L, 10000L, 50000L),
  ci_method = ssdtools::ssd_ci_methods()
)

identical(
  ssd_estimate_cost(wider)$total,
  ssd_estimate_cost(scenario)$total
)
#> [1] TRUE
```

### From serial total to wall-time

The estimator deliberately reports *serial* numbers. Under `n` parallel
workers, wall-time is roughly

``` r

max(longest_task, total / n_workers)
```

The longest task is the floor: it cannot be split, so it sets the
minimum wall-time and the per-shard time budget regardless of how many
workers you throw at the job.

## Recalibrating for your machine

Producing an architecture-specific estimator is a **single call** —
[`ssd_calibrate_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_calibrate_cost.md).
It runs a small, fixed benchmark sweep on the current machine (tiny
`nboot` values over all `ci_method`s and a couple of `nrow` values),
times each `ssd_hc()` call, fits the model, and returns a fresh
`ssdsims_cost_calibration` carrying that machine’s coefficients and
provenance. There is no separate analysis script to run.

``` r

# Takes minutes (tiny nboot), not the hours a real scenario costs.
my_calibration <- ssd_calibrate_cost()

# Pass it to the estimator for trustworthy, machine-specific numbers.
ssd_estimate_cost(scenario, calibration = my_calibration)
```

The shipped default calibration is produced by exactly this call — the
function *is* the reproducibility mechanism, and this vignette only
documents and demonstrates it.

## Caveats

- Estimates are **ballpark** and **machine-specific**; treat them as
  order-of-magnitude / wall-time sizing, not stopwatch timing.
  Ship-default estimates carry the development machine’s provenance —
  recalibrate for your own.
- The `nrow` factor is the least precise part of the model (real, but
  weak and data-dependent).
- `ssdtools` performance can change across versions; the calibration’s
  provenance records the version it was measured against, and
  recalibration corrects drift.

## See also

- [`vignette("cost-analysis")`](https://poissonconsulting.github.io/ssdsims/articles/cost-analysis.md)
  — the complement: measure a finished run’s *observed* cost and
  recalibrate from it.
- [`vignette("defining-a-scenario")`](https://poissonconsulting.github.io/ssdsims/articles/defining-a-scenario.md)
  — the scenario axes whose fan-out this estimate reads.
- [`ssd_estimate_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_estimate_cost.md),
  [`ssd_calibrate_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_calibrate_cost.md),
  [`ssd_cost_calibration()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_cost_calibration.md).
