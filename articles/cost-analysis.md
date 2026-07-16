# Analysing a Run’s Observed Compute Cost

``` r

library(ssdsims)
```

## Prediction, then measurement

[`ssd_estimate_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_estimate_cost.md)
predicts a scenario’s compute cost *before* a run (see
[`vignette("cost-estimation")`](https://poissonconsulting.github.io/ssdsims/articles/cost-estimation.md)),
from a cost model calibrated by a synthetic micro-benchmark. Once a run
has happened it leaves **ground truth** behind: every `fit` and `hc`
task’s body is bracketed with a UTC `.start`/`.end` and tagged with the
`.host` CPU, and those land as columns of the shard Parquets themselves.
[`ssd_analyse_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_analyse_cost.md)
reads them back and attributes the *observed* compute to the same
`ci_method` × `nboot` axes the estimate uses — closing the loop from
predicted to measured.

Like the estimator, the analysis functions are strictly **read-only**:
they read result Parquets (and, optionally, the run’s `targets` store),
and never run the pipeline, fit a distribution, draw a random number, or
write a file.

## A small worked run

Any run leaves timings behind — the serial baseline, the single-core
[`ssd_run_scenario_shards()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario_shards.md),
or the `targets` pipeline. Here we materialise a small scenario
single-core so the vignette is self-contained:

``` r

data <- ssd_scenario_data(ssddata::ccme_boron)
scenario <- ssd_define_scenario(
  data,
  nsim = 2L,
  seed = 42L,
  nrow = c(6L, 10L),
  ci = TRUE,
  nboot = c(100L, 1000L),
  dists = ssd_distset(lnorm = "lnorm")
)

run <- ssd_run_scenario_shards(scenario)
#> duckdb is keeping downloaded extensions in a temporary directory:
#> ℹ /tmp/RtmpCssTpa/duckdb/extensions
#> This is removed when the R session ends, so extensions are re-downloaded each session.
#> ℹ To keep them, point `options(duckdb.extension_directory =)` or the `DUCKDB_EXTENSION_DIRECTORY` environment variable at a permanent path.
```

## Analysing observed cost

[`ssd_analyse_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_analyse_cost.md)
reads the run’s `fit`/`hc` shard timings and reports the observed
**total** compute and the observed **longest single task**, with a
breakdown by `ci_method` × `nboot` keyed exactly like the estimate’s:

``` r

analysis <- ssd_analyse_cost(scenario, root = run$dir)
analysis
#> <ssdsims_cost_analysis>  (observed, serial-equivalent; measured)
#>   total compute:  18.4 s
#>   longest task:   4.2 s
#>   breakdown (ci_method x nboot, by total cost):
#>     weighted_samples       nboot   1000     4 tasks  16.3 s
#>     weighted_samples       nboot    100     4 tasks  2.1 s
#>   host(s):        INTEL(R) XEON(R) PLATINUM 8573C
```

The observed **total is serial-equivalent compute** — the sum of the
per-task durations — *not* elapsed wall time. Under `n` parallel workers
the wall time is roughly `max(longest_task, total / n)`, so the longest
single task is the irreducible floor regardless of how many workers you
add.

### The shard envelope

Per-task durations measure the *work*; they do not include the per-shard
overhead of reading the parent shard, writing the Parquet, and `crew`
dispatch. Given the run’s `targets` store,
[`ssd_analyse_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_analyse_cost.md)
reads each shard target’s wall `seconds` from
[`tar_meta()`](https://docs.ropensci.org/targets/reference/tar_meta.html)
and reports the **envelope overhead** —
`target seconds − Σ task durations` — the number that informs
`partition_by` tuning:

``` r

# With a targets store (a pipeline run), add the per-shard envelope overhead.
ssd_analyse_cost(scenario, root = "results", store = "_targets")
```

## Comparing predicted against observed

[`ssd_compare_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_compare_cost.md)
places the prediction beside the observation and reports the
predicted/observed ratio for the total and the longest task — a
one-glance check of how well the (machine-specific) cost model tracked
reality:

``` r

ssd_compare_cost(scenario, root = run$dir)
#> <ssdsims_cost_comparison>  (predicted vs observed)
#>   total compute:  predicted 22.4 s | observed 18.4 s | obs/pred 0.82x
#>   longest task:   predicted 6.7 s | observed 4.2 s | obs/pred 0.63x
```

A ratio far from `1` means the shipped default calibration does not
match this machine — which is exactly what recalibration fixes.

## Recalibrating from the run

[`ssd_calibrate_cost_from_run()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_calibrate_cost_from_run.md)
re-fits the per-task cost model from the run’s **measured** hc task
durations (and a measured fit addend), returning the same
`ssdsims_cost_calibration` object
[`ssd_calibrate_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_calibrate_cost.md)
produces — so it drops straight back into
[`ssd_estimate_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_estimate_cost.md),
now grounded in real measurements from this scenario rather than a
synthetic sweep:

``` r

from_run <- ssd_calibrate_cost_from_run(scenario, root = run$dir)

# The run-derived calibration drives the estimator like any other.
ssd_estimate_cost(scenario, calibration = from_run)
#> <ssdsims_cost_estimate>  (ballpark, serial)
#>   total compute:  17.9 s
#>   longest task:   2.3 s
#>   breakdown (ci_method x nboot, by total cost):
#>     weighted_samples   nboot   1000     4 tasks  9.3 s
#>     weighted_samples   nboot    100     4 tasks  8.6 s
#>   calibration:    INTEL(R) XEON(R) PLATINUM 8573C | R 4.6.1 | ssdtools 2.6.0.9002 | 2026-07-16
#>   Ballpark only - recalibrate with ssd_calibrate_cost() on the target machine.
```

Because the calibration is **architecture-specific**,
[`ssd_calibrate_cost_from_run()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_calibrate_cost_from_run.md)
never silently pools timings from different CPUs: a run whose shards
span more than one `.host` (a cluster of mixed node types) requires an
explicit `host =`, or the function aborts listing the hosts it found.
The result’s provenance records that it was derived from a run.

## Across a design

A design
([`ssd_design()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_design.md);
see
[`vignette("scenario-to-design")`](https://poissonconsulting.github.io/ssdsims/articles/scenario-to-design.md))
runs several scenarios as one pipeline. The same three functions accept
a design and **roll the observed cost up across its members**:
`ssd_analyse_cost(design, root = ...)` returns a breakdown with a
leading `scenario` column and design totals, `ssd_compare_cost(design)`
places the design’s summed prediction beside it, and
`ssd_calibrate_cost_from_run(design)` pools every member’s measured
durations into one host-aware calibration.

``` r

design <- ssd_design(coarse = scenario, dense = other_scenario)

# Per-scenario observed breakdown + design totals (read from the run's results
# root; the combined <root>/summary.parquet is used as a one-read fast path when
# present, else each member is read from its seed group's shared shards).
ssd_analyse_cost(design, root = "results")

# Predicted (Σ members) vs observed, with ratios.
ssd_compare_cost(design, root = "results")

# One calibration pooled across all members (host-aware).
ssd_calibrate_cost_from_run(design, root = "results")
```

Because
[`ssd_design_targets()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_design_targets.md)
writes one **shared** `seed=/layout=` tree per seed group — a cell
shared by several members is computed once — the design total is
**per-member accounting**: a shared cell counts toward each member that
includes it (it answers “what does each scenario cost”), so the design
total can exceed the design’s once-built shared compute. As at the
scenario level, recalibration never silently pools timings across CPUs:
a design spanning more than one `.host` requires an explicit `host =`.

## The loop

Putting it together — predict, run, measure, compare, recalibrate,
predict again:

``` r

estimate <- ssd_estimate_cost(scenario)        # predict
run <- ssd_run_scenario_shards(scenario)        # run
ssd_compare_cost(scenario, root = run$dir)      # predicted vs observed
better <- ssd_calibrate_cost_from_run(scenario, root = run$dir) # recalibrate
ssd_estimate_cost(next_scenario, calibration = better)          # predict the next, from truth
```

Each larger scenario is then sized from the measured cost of the last,
rather than a synthetic micro-benchmark — the estimate improves every
time the loop turns.

## See also

- [`vignette("cost-estimation")`](https://poissonconsulting.github.io/ssdsims/articles/cost-estimation.md)
  — the complement: *predict* a scenario’s cost before running it.
- [`vignette("sharded-pipeline")`](https://poissonconsulting.github.io/ssdsims/articles/sharded-pipeline.md)
  — the shard runs whose per-task timings this reads back.
- [`ssd_analyse_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_analyse_cost.md),
  [`ssd_compare_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_compare_cost.md),
  [`ssd_calibrate_cost_from_run()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_calibrate_cost_from_run.md).
