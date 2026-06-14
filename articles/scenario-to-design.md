# From a Single Scenario to a Design

``` r

library(ssdsims)
```

A single `ssd_scenario` is one **regular** grid — a rectangular
cross-join of the axes. A real study is rarely rectangular: you want a
coarse grid everywhere *and* a dense refinement in one region (more
`nrow` values, but only for one dataset, say). Forcing that into one
scenario computes a cross-product you never wanted; running it as
several separate pipelines recomputes the cells they share and gives you
no single summary.

A **design** is the answer: a named set of scenarios run as **one**
pipeline, unioned into one possibly-ragged task set. Overlapping cells
are computed **once**, and the result is a single combined summary with
a `scenario` column. This vignette shows how to grow a one-off run into
a design — a one-line change — and then refine it.

## Start with a single scenario

A typical one-off run defines a scenario and turns it into a `targets`
pipeline with
[`ssd_scenario_targets()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_targets.md):

``` r

data <- ssd_scenario_data(ssddata::ccme_boron)
coarse <- ssd_define_scenario(
  data,
  nsim = 10L,
  seed = 42L,
  nrow = c(5L, 10L, 20L),
  dists = ssd_distset(lnorm = "lnorm")
)
coarse
#> <ssdsims_scenario>
#>   seed:     42
#>   nsim:     10
#>   datasets: ccme_boron
#>   nrow:     5, 10, 20
#>   replace:  TRUE
#>   nrow_max: 1000 (setting)
#>   fit grid:
#>     rescale: FALSE
#>     computable: FALSE
#>     at_boundary_ok: TRUE
#>     min_pmix: ssd_min_pmix
#>     range_shape1: {0.05, 20}
#>     range_shape2: {0.05, 20}
#>     dists: lnorm (setting)
#>   hc grid:
#>     est_method: multi (setting)
#>     proportion: 0.05 (setting)
#>     ci: FALSE (setting)
#>     nboot: 1000
#>     ci_method: weighted_samples
#>     parametric: TRUE
#>     samples: FALSE (setting)
#>   distsets:
#>     lnorm: lnorm
#>   partition_by:
#>     sample: dataset, sim, replace
#>     fit: dataset, sim, nrow, rescale
#>     hc: dataset, sim
#>   bundle:
#>     sample: 
#>     fit: replace, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2
#>     hc: replace, nrow, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2, nboot, ci_method, parametric, distset
```

``` r

# _targets.R
library(targets)
library(tarchetypes)
library(ssdsims)

ssd_scenario_targets(coarse, root = "results")
```

## Migrate: wrap it in a design

To grow this into a study, wrap the scenario with
[`ssd_design()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_design.md)
and switch the factory to
[`ssd_design_targets()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_design_targets.md).
That is the entire change:

``` r

design <- ssd_design(coarse)
design
#> <ssdsims_design>
#>   scenarios: 1
#>     coarse (seed 42)
```

``` r

# _targets.R
design <- ssd_design(coarse)
ssd_design_targets(design, root = "results")
```

A design of **one** is valid and uniformly shaped — the recommended
starting point for a study that may grow. The per-task results are
**byte-identical** to the standalone run: combining changes only
*addressing* (target names and the results tree), never a task’s
reproducible `(seed, primer)`.

> **Safe but recomputing**
>
> The design tree gains a `seed=<value>` level the standalone `layout=`
> tree lacks (`results/seed=42/layout=.../...`), so the *first* design
> run recomputes into the new tree. Nothing is re-baselined — the
> numbers are identical — it is a one-time relocation. From then on,
> growing the design is incremental.

## Refine: add a denser member

Now add the refinement. Suppose you want a finer `nrow` sweep — but only
the extra sizes, not a whole new rectangular run. Define a second
scenario covering just the new region and add it to the design:

``` r

dense <- ssd_define_scenario(
  data,
  nsim = 10L,
  seed = 42L,
  nrow = c(12L, 14L, 16L, 18L),
  dists = ssd_distset(lnorm = "lnorm")
)
study <- ssd_design(coarse = coarse, dense = dense)
study
#> <ssdsims_design>
#>   scenarios: 2
#>     coarse (seed 42)
#>     dense (seed 42)
```

``` r

# _targets.R
ssd_design_targets(study, root = "results")
```

Both members share the same `seed`, dataset, and distributions,
differing only in their `nrow` coverage. So they **share** every
`sample` shard (the draw does not depend on `nrow`) and the `fit`/`hc`
shard *cells* — the union merges their per-`nrow` tasks into the shared
shards. Re-running
[`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html)
builds only the genuinely new work; the cells `coarse` already computed
stay cached.

The combined `results/summary.parquet` carries a `scenario` column
(`"coarse"` or `"dense"`), and each member’s rows are filtered to
exactly its own cells — ready to plot the coarse and refined sweeps
together.

## Notes

- **Consistency contract.**
  [`ssd_design()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_design.md)
  requires that a name means the same thing across members: the same
  `dataset` name must bind identical data, the same `min_pmix` name the
  same function, the same `distset` name the same members, and
  `partition_by` must match. This is what makes sharing a cell across
  members sound. Inconsistent bindings abort at construction.
- **Varying the seed.** Members *may* use different `seed`s
  (e.g. repeating the whole exploration under several master seeds);
  they land under separate `seed=` trees and share nothing. Members
  sharing a `seed` share their coincident cells (common random numbers).
- **Keep `nrow_max` uniform.** `nrow_max` is a sample draw-size guard,
  not a comparison axis; differing or changing it across members is
  undefined behaviour for shard sharing.

See [“Running a Sharded
Pipeline”](https://poissonconsulting.github.io/ssdsims/articles/sharded-pipeline.md)
for the shard/results layout a design builds on.
