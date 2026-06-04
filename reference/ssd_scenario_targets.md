# Build the Targets Pipeline for a Scenario

A **target factory**: returns the list of `targets` objects that runs a
scenario as a static-branching Hive-sharded pipeline (TARGETS-DESIGN.md
section 6), so a whole `_targets.R` reduces to *build a scenario and
call this*:

## Usage

``` r
ssd_scenario_targets(scenario, root = scenario_results_dir(scenario))
```

## Arguments

- scenario:

  An `ssdsims_scenario` from
  [`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md).

- root:

  The results root the shards and summary are written under; defaults to
  the per-layout
  [`scenario_results_dir()`](https://poissonconsulting.github.io/ssdsims/reference/scenario_results_dir.md).

## Value

A list of `targets` target objects, for `_targets.R` to return.

## Details

    library(targets)
    library(tarchetypes)
    library(ssdsims)
    scenario <- ssd_define_scenario(ssddata::ccme_boron, nsim = 2L, seed = 42L)
    ssd_scenario_targets(scenario)

For each step it
[`tarchetypes::tar_map()`](https://docs.ropensci.org/tarchetypes/reference/tar_map.html)s
one named, `format = "file"`, `error = "null"` target per `partition_by`
path cell (the `names` are the step's path axes), wires
`sample -> fit -> hc -> summary` ordering with
[`tar_combine()`](https://docs.ropensci.org/tarchetypes/reference/tar_combine.html)
barriers (a step body reads its parents from disk by partition path, so
there is no automatic edge), and writes every shard and the summary
under the per-layout
[`scenario_results_dir()`](https://poissonconsulting.github.io/ssdsims/reference/scenario_results_dir.md)
root (so a changed `partition_by`/`bundle` never mixes shard
granularities). `scenario` is referenced as a global, so editing it
invalidates the dependent shards.

To parallelise the shards, set a controller (e.g. a mirai-backed
[`crew::crew_controller_local()`](https://wlandau.github.io/crew/reference/crew_controller_local.html))
with
[`targets::tar_option_set()`](https://docs.ropensci.org/targets/reference/tar_option_set.html)
in `_targets.R` before calling this - the target set is unchanged.

## See also

[`scenario_results_dir()`](https://poissonconsulting.github.io/ssdsims/reference/scenario_results_dir.md),
[`ssd_run_scenario_shards()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario_shards.md)
(the single-core, `targets`-free equivalent).

## Examples

``` r
if (FALSE) { # \dontrun{
# _targets.R
library(targets)
library(tarchetypes)
library(ssdsims)
scenario <- ssd_define_scenario(ssddata::ccme_boron, nsim = 2L, seed = 42L)
ssd_scenario_targets(scenario)
} # }
```
