# Running a Sharded Pipeline

``` r

library(ssdsims)
```

The [“Defining a
Scenario”](https://poissonconsulting.github.io/ssdsims/articles/defining-a-scenario.md)
vignette ends with the in-memory baseline runner, which threads each
step’s results forward in memory. This vignette covers the next layer:
materialising each step as **Hive-partitioned Parquet shards** and
linking the steps by reading parent shards back from disk — the storage
hand-off the cluster [targets](https://docs.ropensci.org/targets/)
pipeline is built on (`TARGETS-DESIGN.md` §5/§6).

The key idea is **two drivers over one execution core**. The per-task
work (draw, fit, hazard concentration) and its reproducible per-task RNG
are shared; only the *I/O and granularity* differ:

- [`ssd_run_scenario_baseline()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario_baseline.md)
  — single core, results threaded in memory.
- [`ssd_run_scenario_shards()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario_shards.md)
  — single core, results written as Parquet shards and read back by name
  (**this vignette**).
- a `targets` pipeline — the same shards, one named target each,
  runnable in parallel / on a cluster (**shown but not run below**).

Because each task installs its own `(seed, primer)`, all three produce
**byte-identical** per-task results: the storage layout is a free
re-layout.

## `partition_by` and `bundle`: how tasks group into shards

A **shard** is one Parquet file: one cell of the `partition_by` *path*
axes for a step. The remaining axes — the `bundle` (inner) axes — become
ordinary columns *within* a shard, so several tasks share one file. The
two are exact complements (`path ⊎ inner = task_axes(step)`), and you
can specify either, per step, with at most one of
`partition_by`/`bundle` naming a given step.

``` r

scenario <- ssd_define_scenario(
  ssddata::ccme_boron,
  nsim = 2L,
  seed = 42L,
  nrow = c(5L, 10L),
  rescale = c(FALSE, TRUE),
  dists = c("lnorm", "gamma")
)
scenario
#> <ssdsims_scenario>
#>   seed:     42
#>   nsim:     2
#>   datasets: ccme_boron
#>   nrow:     5, 10
#>   replace:  FALSE
#>   fit grid:
#>     rescale: FALSE, TRUE
#>     computable: FALSE
#>     at_boundary_ok: TRUE
#>     min_pmix: ssd_min_pmix
#>     range_shape1: {0.05, 20}
#>     range_shape2: {0.05, 20}
#>     dists: lnorm, gamma (setting)
#>   hc grid:
#>     est_method: multi (setting)
#>     proportion: 0.05 (setting)
#>     ci: FALSE (setting)
#>     nboot: 1000
#>     ci_method: weighted_samples
#>     parametric: TRUE
#>     samples: FALSE (setting)
#>   partition_by:
#>     sample: dataset, sim, replace
#>     fit: dataset, sim, nrow, rescale
#>     hc: dataset, sim
#>   bundle:
#>     sample: 
#>     fit: replace, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2
#>     hc: replace, nrow, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2, nboot, ci_method, parametric
```

The [`print()`](https://rdrr.io/r/base/print.html) shows, per step, the
`partition_by` (across-shards) path axes and the `bundle` (within-shard)
inner axes. The default coarsens downstream: `fit` shards on
`(dataset, sim, nrow, rescale)`, while `hc` shards only on
`(dataset, sim)`. Steps partition **independently** — there is no
cross-step constraint — so an `hc` shard reads *several* `fit` shards
and a `fit` shard can read *several* `sample` shards. That **m:n**
relationship is resolved when the parent shards are read back, not by
restricting `partition_by`.

The stored `partition_by` is the per-step path list (the `bundle`/inner
axes shown in [`print()`](https://rdrr.io/r/base/print.html) are its
complement):

``` r

scenario$partition_by$fit
#> [1] "dataset" "sim"     "nrow"    "rescale"
```

## Run it single core, over shards

[`ssd_run_scenario_shards()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario_shards.md)
runs the three steps and writes one Parquet per shard under a
Hive-partitioned tree, with **no** `targets` dependency. It returns the
shard paths per step.

``` r

run <- ssd_run_scenario_shards(scenario)
run
#> <ssdsims_shard_run>
#>   dir: /tmp/Rtmp22rH8n/ssdsims-shards-3daa58b2c89c
#>   sample shards: 2
#>   fit    shards: 8
#>   hc     shards: 2
```

The written tree is Hive-partitioned
(`<step>/<axis=value>/.../part.parquet`), so the path itself identifies
the shard:

``` r

fit_files <- list.files(
  file.path(run$dir, "fit"),
  pattern = "part.parquet",
  recursive = TRUE
)
fit_files
#> [1] "dataset=ccme_boron/sim=1/nrow=10/rescale=FALSE/part.parquet"
#> [2] "dataset=ccme_boron/sim=1/nrow=10/rescale=TRUE/part.parquet" 
#> [3] "dataset=ccme_boron/sim=1/nrow=5/rescale=FALSE/part.parquet" 
#> [4] "dataset=ccme_boron/sim=1/nrow=5/rescale=TRUE/part.parquet"  
#> [5] "dataset=ccme_boron/sim=2/nrow=10/rescale=FALSE/part.parquet"
#> [6] "dataset=ccme_boron/sim=2/nrow=10/rescale=TRUE/part.parquet" 
#> [7] "dataset=ccme_boron/sim=2/nrow=5/rescale=FALSE/part.parquet" 
#> [8] "dataset=ccme_boron/sim=2/nrow=5/rescale=TRUE/part.parquet"
```

One file per `partition_by` path cell — here
`2 (sim) x 2 (nrow) x 2 (rescale) = 8` fit shards — not one per task.
Each file carries the step’s inner axes and the per-task results as
columns.

### Results match the in-memory runner

`partition_by` is a free re-layout, so the sharded run’s per-task
results equal the in-memory baseline’s. The `hc` estimates, read back
from the shards, line up with
[`ssd_run_scenario_baseline()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario_baseline.md)
joined on the task identity:

``` r

base <- ssd_run_scenario_baseline(scenario)
base_est <- sort(do.call(rbind, base$hc$hc)$est)

summary_path <- ssd_summarize(
  file.path(run$dir, "sample"),
  file.path(run$dir, "fit"),
  file.path(run$dir, "hc"),
  file.path(run$dir, "summary.parquet")
)
summary_tbl <- dplyr::collect(duckplyr::read_parquet_duckdb(summary_path))
shard_est <- sort(summary_tbl$est)

all.equal(base_est, shard_est)
#> [1] TRUE
```

[`ssd_summarize()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_summarize.md)
fans the `hc` layer in across shards (via duckplyr) into a single
`summary.parquet` — the analysis-ready table — without recomputing
anything.

## Scaling up: the `targets` pipeline

The sharded runner is single core. To run shards **in parallel** (or on
a cluster), the same building blocks plug into a static-branching
`targets` pipeline:
[`tarchetypes::tar_map()`](https://docs.ropensci.org/tarchetypes/reference/tar_map.html)
mints one named target per shard, so `targets` caches, invalidates, and
reruns each shard independently. Three ready example projects ship with
the package — a minimal `small` one, a fuller `large` one (adapted from
`scripts/example.R`, sweeping `nrow`, `proportion`, and the estimation /
CI methods), and a `cluster` one for SLURM. The `large` project
parallelises its shards across local workers with a mirai-backed `crew`
controller
(`tar_option_set(controller = crew::crew_controller_local())`); the
`cluster` project is the same pipeline under a
[`crew.cluster::crew_controller_slurm()`](https://wlandau.github.io/crew.cluster/reference/crew_controller_slurm.html)
controller, with a connectivity + worker-prerequisite preflight — see
the [“Running on a SLURM
Cluster”](https://poissonconsulting.github.io/ssdsims/articles/cluster-pipeline.md)
vignette, a “zero to a running cluster job” guide that maps your site’s
own SLURM instructions onto the controller:

``` r

list.files(system.file("targets-templates", "small", package = "ssdsims"))
#> [1] "_targets.R"   "run-serial.R" "run.R"        "scenario.R"
```

The `small` and `large` directories each hold `scenario.R` (the study,
shared by both drivers), `_targets.R` (the pipeline), `run.R` (the
**targets** driver), and `run-serial.R` (the **single-core** driver);
the `cluster` directory is a minimal four-file variant (the scenario is
inline in `_targets.R`, and a separate `controller.R` holds the SLURM
controller). The whole `_targets.R` is just *build a scenario and call
the
[`ssd_scenario_targets()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_targets.md)
factory* —

``` r

library(targets)
library(tarchetypes)
library(ssdsims)
source("scenario.R") # builds `scenario`
ssd_scenario_targets(scenario) # the entire target list
```

— so editing the study means editing `scenario.R` only. Copy a
directory’s files to your project root, edit `scenario.R`, then run
either driver (the targets path needs the `targets`/`tarchetypes`
Suggests):

``` r

# install.packages(c("targets", "tarchetypes"))
dir <- system.file("targets-templates", "small", package = "ssdsims")
file.copy(list.files(dir, full.names = TRUE), ".")

source("run.R") # targets: tar_make() -> one target per shard -> results/<step>/...
# or, from a shell:  Rscript run.R

source("run-serial.R") # single core via ssd_run_scenario_shards() -> results-serial/
# ...and, if results/ exists, it asserts the two drivers' estimates are identical
```

[`ssd_scenario_targets()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_targets.md)
mints one `format = "file"` target per shard (targets passes the shard
*path*, not its value, between steps) with `error = "null"` (a failing
whole shard records its error and goes `NULL` without aborting the run —
the rest still build and the summary unions the survivors), and wires
step ordering with
[`tar_combine()`](https://docs.ropensci.org/tarchetypes/reference/tar_combine.html)
barriers (a directory read carries no automatic dependency edge).

Because a shard’s Hive-path depth depends on `partition_by`/`bundle`,
replaying with a *changed* split must not leave stale-granularity shards
beside the new ones (a `**` glob would union both). The two drivers
handle this differently: `run.R` (targets) writes each layout under its
own root, `scenario_results_dir(scenario)` = `results/layout=<hash>/`,
so a split change is a fresh tree; `run-serial.R` (single core) instead
**owns** its `dir` and clears each step subtree on every run.

Because both drivers `source("scenario.R")` — the same study — and
`partition_by` is a free re-layout, `run-serial.R` finishes by reading
back its own `summary.parquet` and the targets
`scenario_results_dir(scenario)` summary and asserting the per-task
estimates are byte-identical. Whatever the driver — baseline,
single-core shards, or `targets` — the results are the same; choose the
one that fits the run.

## See also

- [“Defining a
  Scenario”](https://poissonconsulting.github.io/ssdsims/articles/defining-a-scenario.md)
  — the scenario object and the baseline runner.
- `TARGETS-DESIGN.md` §5 (tasks into shards), §6 (inter-shard linking,
  the Hive layout, the `targets` sketch).
- [`?ssd_run_scenario_shards`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario_shards.md),
  [`?ssd_scenario_fit_shards`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_fit_shards.md),
  [`?ssd_run_fit_step`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_fit_step.md),
  [`?ssd_summarize`](https://poissonconsulting.github.io/ssdsims/reference/ssd_summarize.md).
