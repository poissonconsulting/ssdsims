# Build the Targets Pipeline for a Design

A **target factory** (the multi-scenario sibling of
[`ssd_scenario_targets()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_targets.md)):
returns the list of `targets` objects that runs a
[`ssd_design()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_design.md) -
a named collection of scenarios - as one static-branching, Hive-sharded
pipeline, so a whole `_targets.R` reduces to *build a design and call
this*.

## Usage

``` r
ssd_design_targets(design, ..., root = "results", upload = NULL, cue = NULL)
```

## Arguments

- design:

  An `ssdsims_design` from
  [`ssd_design()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_design.md).

- ...:

  Unused; must be empty (forces `root`/`upload`/`cue` to be named).

- root:

  The results root the shards and summaries are written under.

- upload:

  An optional upload destination from
  [`ssd_upload_azure()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_azure.md)
  or
  [`ssd_upload_dryrun()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_azure.md),
  or `NULL` (default) for no upload targets.

- cue:

  An optional
  [`targets::tar_cue()`](https://docs.ropensci.org/targets/reference/tar_cue.html)
  applied to every shard target.

## Value

A list of `targets` target objects, for `_targets.R` to return.

## Details

A design is the **de-duplicated union** of its members' regular per-step
task sets - the irregular (ragged) grid. Members are grouped by `seed`;
within a group the union shard tables are computed (one target per cell,
a cell shared by several members built **once**) and written under a
legible `<root>/seed=<value>/layout=<hash>` tree, with the `seed` woven
into the target names so cells never collide across seed groups. Each
member then gets a `summary_<name>` target that filters the shared
shards to its own task identities, and the top-level `summary` target
unions those into `<root>/summary.parquet` with a `scenario` identity
column
([`ssd_summarise_design()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_summarise_design.md)).

## Migration from a single scenario

Growing a one-off
[`ssd_scenario_targets()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_targets.md)
run into a study is a one-line switch: wrap the scenario with
[`ssd_design()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_design.md)
and call `ssd_design_targets()`. It is **cache-preserving** - a design
of one addresses its shards identically to the standalone run (same
`seed=`/`layout=` root via
[`scenario_results_dir()`](https://poissonconsulting.github.io/ssdsims/reference/scenario_results_dir.md)
and the same `seed`-woven target names), so re-running into the same
root **reuses every existing shard** (no recompute); only the per-member
and combined `summary` targets are new. Later members are added by
extending the `ssd_design(...)` call; the cells they share (within a
seed) stay cached.

## Varying the seed

Members may use different `seed`s (e.g. repeating the exploration under
several master seeds); they land under separate `seed=` trees and share
nothing. Members sharing a `seed` share their coincident cells (common
random numbers).

## Per-overlap hc readout aggregation

Members of a seed group MAY differ in the four **non-axis** hc readout
settings (`proportion`, `est_method`, `ci`, `samples`) and in their fit
`dists` union; only the layout-shaping `nrow_max` and `partition_by`
stay uniform-required. Differing readouts are reconciled **per shared hc
cell, over only the members whose task set contains that cell** -
`proportion`/`est_method` are `union`-ed and `ci`/`samples` reduced with
[`any()`](https://rdrr.io/r/base/any.html) - so the cell computes the
maximal readout set in one shard and each member's summary filters its
slice. A cell one member reaches keeps that member's (smaller) demand,
so the expensive bootstrap runs only where a `ci = TRUE` member has
tasks. The draw-shaping hc axes
(`nboot`/`ci_method`/`parametric`/`distset`) are **not** aggregated -
they stay cell axes (in the per-task primer), so byte-identity holds: a
member's per-task results equal its standalone-run results.

Because a `ci = FALSE` task collapses `nboot`/`ci_method`/`parametric`
to `NA`, its cell never coincides with a `ci = TRUE` task's. The point
`est` is analytical and bootstrap-config-invariant, so a `ci = FALSE`
cell is **served by a coincident `ci = TRUE` shard** at the same
`(fit, distset)` when one exists (the computed hc shards are every
`ci = TRUE` cell plus the `ci = FALSE` cells with no overlapping
`ci = TRUE` shard); a `ci = TRUE` member's confidence interval still
uses its own cell's `(nboot, ci_method, parametric)` primer. Differing
fit `dists` unions are reconciled by fitting the **design-wide union**
once per fit cell, each member subsetting via its `distset` axis
(distset-subset-invariance), so members differing only in `distset`
coverage share every `sample`/`fit` shard.

## See also

[`ssd_design()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_design.md),
[`ssd_scenario_targets()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_targets.md),
[`ssd_summarise_design()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_summarise_design.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# _targets.R
library(targets)
library(tarchetypes)
library(ssdsims)
data <- ssd_scenario_data(ssddata::ccme_boron)
coarse <- ssd_define_scenario(data, nsim = 2L, seed = 42L, nrow = c(5L, 10L))
dense <- ssd_define_scenario(data, nsim = 2L, seed = 42L, nrow = c(6L, 7L, 8L))
design <- ssd_design(coarse, dense)
ssd_design_targets(design)
} # }
```
