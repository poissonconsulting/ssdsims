# Compare Predicted Against Observed Compute Cost

Places the
[`ssd_estimate_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_estimate_cost.md)
prediction beside the
[`ssd_analyse_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_analyse_cost.md)
observation for one scenario+run and reports the predicted and observed
total compute, the predicted and observed longest task, and the
predicted/observed ratio for each. Read-only.

## Usage

``` r
ssd_compare_cost(
  scenario,
  root = scenario_results_dir(scenario),
  store = NULL,
  calibration = ssd_cost_calibration()
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

- store:

  Optional path to the run's `targets` data store, to add the per-shard
  envelope overhead. `NULL` (the default) reads the shards only.

- calibration:

  An `ssdsims_cost_calibration` for the prediction; defaults to the
  shipped
  [`ssd_cost_calibration()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_cost_calibration.md).

## Value

An `ssdsims_cost_comparison`.

## See also

[`ssd_estimate_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_estimate_cost.md),
[`ssd_analyse_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_analyse_cost.md).
