# Analyse a Run's Observed Compute Cost

Reads the per-task `.start`/`.end`/`.host` timings a completed run left
in its `fit`/`hc` shard Parquets (the cost-analysis instrumentation),
and attributes the **observed** compute to the scenario's `ci_method` x
`nboot` axes - the measured counterpart to
[`ssd_estimate_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_estimate_cost.md)'s
prediction. It is strictly read-only: it reads result Parquets (and,
optionally, a `targets` meta store), and never runs the pipeline, fits a
distribution, draws random numbers, or writes a file. The observed
**total** is serial-equivalent compute (the sum of per-task durations),
distinct from elapsed wall time under parallel workers.

## Usage

``` r
ssd_analyse_cost(scenario, root = scenario_results_dir(scenario), store = NULL)
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

## Value

An `ssdsims_cost_analysis`: a list with `total` and `longest` (both
`difftime`), a `breakdown` tibble grouped by `ci_method` x `nboot`,
`fit_seconds` (the measured fit addend), the `hosts` seen, a `measured`
flag, the per-shard `envelope` (when a `store` is given), and
provenance.

## Details

Given a `targets` `store`, it additionally reads each shard target's
wall `seconds` from
[`targets::tar_meta()`](https://docs.ropensci.org/targets/reference/tar_meta.html)
and reports the per-shard **envelope overhead**
(`target seconds - sum(task durations)`: parent read, Parquet write, and
dispatch), the number that informs `partition_by` tuning. The combined
`summary` and `upload_<step>` targets are excluded; errored/unbuilt
(`NA`-seconds) targets are dropped from totals; unmatched stored targets
are reported, never silently dropped.

## See also

[`ssd_estimate_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_estimate_cost.md),
[`ssd_compare_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_compare_cost.md),
[`ssd_calibrate_cost_from_run()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_calibrate_cost_from_run.md).
