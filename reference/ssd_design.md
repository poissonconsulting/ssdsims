# Assemble and Validate a Design of Scenarios

Collects one or more
[`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md)
scenarios into a validated, named collection - the **design**
(design-of-experiments sense: the set of conditions to run), the union
of regular per-scenario grids into one possibly-non-regular design. The
design is turned into a single `targets` pipeline by
[`ssd_design_targets()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_design_targets.md).

## Usage

``` r
ssd_design(...)
```

## Arguments

- ...:

  One or more `ssdsims_scenario` objects from
  [`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md),
  optionally named.

## Value

An `ssdsims_design` object: a named list of `ssdsims_scenario` objects.

## Details

Names are taken from the argument names where supplied, otherwise
derived from the argument expression by symbol capture (e.g. a variable
`base` becomes `"base"`), mirroring
[`ssd_scenario_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_data.md).
Each name is a **scenario name** within the design, used only as the
`scenario` identity column in the combined summary and the per-scenario
summary target-name suffix - never in a shard path, a shard target name,
the per-task primer, or any result value. Names must be unique,
non-empty, and safe to serve as a target-name suffix (start with a
letter; letters, digits, and underscore only). A design of **one**
scenario is valid and uniformly shaped - the recommended starting point
for a study that may grow (see the *migration* vignette).

## Consistency contract

Because
[`ssd_design_targets()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_design_targets.md)
addresses shards by cell and shares a cell across members (computing it
once), the same **name** must denote the same value across members, or
two members could disagree on a shared cell's bytes. `ssd_design()`
therefore validates across members that the same `dataset` name binds
identical data, the same `min_pmix` name binds an identical function,
the same `distset` name binds identical members, and that `partition_by`
is identical - aborting with an informative error otherwise. The `seed`
may vary across members (it becomes a `seed=` results level); members
sharing a `seed` share their coincident cells (common random numbers).

## See also

[`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md),
[`ssd_design_targets()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_design_targets.md).

## Examples

``` r
data <- ssd_scenario_data(ssddata::ccme_boron)
coarse <- ssd_define_scenario(data, nsim = 2L, seed = 42L, nrow = c(5L, 10L))
dense <- ssd_define_scenario(data, nsim = 2L, seed = 42L, nrow = c(6L, 7L, 8L))
ssd_design(coarse, dense)
#> <ssdsims_design>
#>   scenarios: 2
#>     coarse (seed 42)
#>     dense (seed 42)
```
