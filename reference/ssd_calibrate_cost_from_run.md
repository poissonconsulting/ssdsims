# Recalibrate the Cost Model from an Observed Run

Re-fits the per-task cost model from a run's **measured** hc task
durations (the cost-analysis timings), returning an
`ssdsims_cost_calibration` of the same shape
[`ssd_calibrate_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_calibrate_cost.md)
produces - so it drops straight into
[`ssd_estimate_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_estimate_cost.md) -
but derived from real measurements rather than the synthetic
micro-benchmark. The fixed addend comes from the measured fit task
durations. Read-only: no pipeline, no RNG, no writes.

## Usage

``` r
ssd_calibrate_cost_from_run(
  scenario,
  root = scenario_results_dir(scenario),
  host = NULL
)
```

## Arguments

- scenario:

  An `ssdsims_scenario` from
  [`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md).

- root:

  The run's results root (the `fit`/`hc` shard trees live under it).
  Defaults to
  [`scenario_results_dir()`](https://poissonconsulting.github.io/ssdsims/reference/scenario_results_dir.md)
  for `scenario`.

- host:

  Optional CPU description (a `.host` value) to select when the run
  spans more than one host. `NULL` (the default) requires a single-host
  run.

## Value

An `ssdsims_cost_calibration` with run-derived provenance.

## Details

Because the calibration is architecture-specific, timings from different
`.host` values are never silently pooled: a run spanning more than one
host requires an explicit `host`, or the function aborts listing the
hosts found.

## See also

[`ssd_calibrate_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_calibrate_cost.md),
[`ssd_analyse_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_analyse_cost.md).
