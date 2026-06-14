# Defining a Scenario

``` r

library(ssdsims)
```

## Why a declarative scenario?

ssdsims is moving from running a simulation study *immediately* (the
legacy
[`ssd_run_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario.md))
to a cluster-friendly [targets](https://docs.ropensci.org/targets/)
pipeline. The root of that pipeline is a **scenario**: a small, purely
declarative description of the study you want to run. It contains only
the knobs — a seed, the number of simulations, the sample sizes, the
dataset *names*, and the fit/hc argument grids. It draws **no** random
numbers, expands **no** tasks, writes **nothing**, and has **no**
dependency on `targets`.

Keeping the scenario declarative buys two things:

- It **serialises to a compact manifest** — no data frames, no RNG
  state, no function bodies — so it can be shipped to a cluster and
  stored alongside results.
- The set of work a pipeline expands to is a **pure function of the
  scenario**, so the same scenario always describes the same study.

This vignette walks the stages that work *today*, in order. It is
intended to grow: as roadmap features land (per-task seeding, shards,
the `targets` backend), new sections will document them here. See
`TARGETS-DESIGN.md` for the north-star design.

The four stages this vignette covers:

1.  **Assemble** the data with
    [`ssd_scenario_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_data.md)
    (and, for generated data,
    [`ssd_gen()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_gen.md)).
2.  **Declare** the scenario with
    [`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md).
3.  **Expand** it into per-step task tables with
    [`ssd_scenario_tasks()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_tasks.md).
4.  **Run** the baseline in-process loop with
    [`ssd_run_scenario_baseline()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario_baseline.md).

Stages 2–3 are side-effect-free; generation
([`ssd_gen()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_gen.md),
in stage 1) and stage 4 draw random numbers under scoped, seeded dqrng
state.

## Stage 1: assemble the data

[`ssd_scenario_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_data.md)
is the single entry point for dataset input. It validates each data
frame — every dataset must carry a numeric `Conc` column, the species
sensitivity distribution convention — and assembles them into a named
collection.

``` r

data <- ssd_scenario_data(
  boron = ssddata::ccme_boron,
  cadmium = ssddata::ccme_cadmium
)
data
#> $boron
#> # A tibble: 28 × 5
#>    Chemical Species                  Conc Group        Units
#>    <chr>    <chr>                   <dbl> <fct>        <chr>
#>  1 Boron    Oncorhynchus mykiss       2.1 Fish         mg/L 
#>  2 Boron    Ictalurus punctatus       2.4 Fish         mg/L 
#>  3 Boron    Micropterus salmoides     4.1 Fish         mg/L 
#>  4 Boron    Brachydanio rerio        10   Fish         mg/L 
#>  5 Boron    Carassius auratus        15.6 Fish         mg/L 
#>  6 Boron    Pimephales promelas      18.3 Fish         mg/L 
#>  7 Boron    Daphnia magna             6   Invertebrate mg/L 
#>  8 Boron    Opercularia bimarginata  10   Invertebrate mg/L 
#>  9 Boron    Ceriodaphnia dubia       13.4 Invertebrate mg/L 
#> 10 Boron    Entosiphon sulcatum      15   Invertebrate mg/L 
#> # ℹ 18 more rows
#> 
#> $cadmium
#> # A tibble: 36 × 5
#>    Chemical Species                   Conc Group Units
#>    <chr>    <chr>                    <dbl> <fct> <chr>
#>  1 Cadmium  Oncorhynchus mykiss       0.23 Fish  ug/L 
#>  2 Cadmium  Salvelinus confluentus    0.83 Fish  ug/L 
#>  3 Cadmium  Cottus bairdi             0.96 Fish  ug/L 
#>  4 Cadmium  Salmo salar               0.99 Fish  ug/L 
#>  5 Cadmium  Acipenser transmontanus   1.14 Fish  ug/L 
#>  6 Cadmium  Prosopium williamsoni     1.25 Fish  ug/L 
#>  7 Cadmium  Salmo trutta              1.36 Fish  ug/L 
#>  8 Cadmium  Salvelinus fontinalis     2.23 Fish  ug/L 
#>  9 Cadmium  Oncorhynchus tshawytscha  2.29 Fish  ug/L 
#> 10 Cadmium  Pimephales promelas       2.36 Fish  ug/L 
#> # ℹ 26 more rows
#> 
#> attr(,"class")
#> [1] "ssdsims_data"
```

Names come from the argument names where you supply them; otherwise they
are derived from the argument expression by symbol capture (so a bare
[`ssddata::ccme_boron`](https://rdrr.io/pkg/ssddata/man/ccme_boron.html)
becomes `"ccme_boron"`). Names must be unique, and a literal with no
derivable name (e.g. a bare `data.frame(...)`) must be given an explicit
name.

### Generated datasets with `ssd_gen()`

Each dataset can also be *generated* rather than observed.
[`ssd_gen()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_gen.md)
accepts the same generator inputs the legacy
[`ssd_run_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario.md)
dispatches over — a generator function, a function-name string, or a
`fitdists`/`tmbfit` object (drawn from the matching `ssd_r<dist>` with
the fit’s estimates) — and materialises each, **once**, to a `Conc`
tibble of `.n` rows. The result composes with the data frames in
[`ssd_scenario_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_data.md),
either as an unnamed argument or spliced with `!!!` (the two are
equivalent):

``` r

ssd_scenario_data(
  boron = ssddata::ccme_boron,
  ssd_gen(synth = ssdtools::ssd_rlnorm, .n = 30, .seed = 1L)
)
#> $boron
#> # A tibble: 28 × 5
#>    Chemical Species                  Conc Group        Units
#>    <chr>    <chr>                   <dbl> <fct>        <chr>
#>  1 Boron    Oncorhynchus mykiss       2.1 Fish         mg/L 
#>  2 Boron    Ictalurus punctatus       2.4 Fish         mg/L 
#>  3 Boron    Micropterus salmoides     4.1 Fish         mg/L 
#>  4 Boron    Brachydanio rerio        10   Fish         mg/L 
#>  5 Boron    Carassius auratus        15.6 Fish         mg/L 
#>  6 Boron    Pimephales promelas      18.3 Fish         mg/L 
#>  7 Boron    Daphnia magna             6   Invertebrate mg/L 
#>  8 Boron    Opercularia bimarginata  10   Invertebrate mg/L 
#>  9 Boron    Ceriodaphnia dubia       13.4 Invertebrate mg/L 
#> 10 Boron    Entosiphon sulcatum      15   Invertebrate mg/L 
#> # ℹ 18 more rows
#> 
#> $synth
#> # A tibble: 30 × 1
#>     Conc
#>    <dbl>
#>  1 1.05 
#>  2 0.814
#>  3 3.41 
#>  4 3.16 
#>  5 0.883
#>  6 2.71 
#>  7 0.741
#>  8 0.457
#>  9 5.49 
#> 10 0.310
#> # ℹ 20 more rows
#> 
#> attr(,"class")
#> [1] "ssdsims_data"
```

Both `.n` (the generated population size) and `.seed` are **required**,
so unsized or irreproducible generation is impossible. Each generator
draws under the dqrng `pcg64` backend on an independent stream keyed by
its dataset *name* (with `.seed` as the base seed), so one `.seed`
reproducibly fans out across all generators in a call; the global RNG
state is untouched.

A materialised generator dataset is a *fixture the scenario resamples*:
once inside the collection it is an ordinary tibble, indistinguishable
from a data-frame dataset downstream (the generator code is its
provenance).

## Stage 2: declare the scenario

[`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md)
is the constructor. Its required arguments are the dataset input, `nsim`
(the replicate count), and `seed` (the RNG root — it has no default
because changing it fully re-roots every draw). Everything else is a
knob with a sensible default.

``` r

scenario <- ssd_define_scenario(
  data,
  nsim = 3L,
  seed = 42L,
  nrow = c(5L, 10L),
  dists = ssd_distset(
    BCANZ = ssdtools::ssd_dists_bcanz(),
    lnorm = "lnorm"
  ),
  est_method = "multi",
  proportion = c(0.05, 0.2),
  ci = TRUE,
  nboot = c(10L, 50L),
  ci_method = "weighted_samples"
)
scenario
#> <ssdsims_scenario>
#>   seed:     42
#>   nsim:     3
#>   datasets: boron, cadmium
#>   nrow:     5, 10
#>   replace:  TRUE
#>   nrow_max: 1000 (setting)
#>   fit grid:
#>     rescale: FALSE
#>     computable: FALSE
#>     at_boundary_ok: TRUE
#>     min_pmix: ssd_min_pmix
#>     range_shape1: {0.05, 20}
#>     range_shape2: {0.05, 20}
#>     dists: gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull (setting)
#>   hc grid:
#>     est_method: multi (setting)
#>     proportion: 0.05, 0.2 (setting)
#>     ci: TRUE (setting)
#>     nboot: 10, 50
#>     ci_method: weighted_samples
#>     parametric: TRUE
#>     samples: FALSE (setting)
#>   distsets:
#>     BCANZ: gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull
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

The [`print()`](https://rdrr.io/r/base/print.html) method shows the
declarative fields: the scalar `seed` and `nsim`, the `nrow` sample
sizes, the dataset names, and the two argument grids (`fit` and `hc`).
Note what is *not* shown — the data frames themselves are retained for
the local runner but are not part of the declarative identity; the
cluster path carries only the names.

### Dataset input is the collection

Dataset input is accepted **only** as an
[`ssd_scenario_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_data.md)
collection — naming, validation, and (via
[`ssd_gen()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_gen.md))
generation all live there, so the constructor itself stays inert and
draws no random numbers:

``` r

data <- ssd_scenario_data(ssddata::ccme_boron)
ssd_define_scenario(data, nsim = 2L, seed = 1L)
#> <ssdsims_scenario>
#>   seed:     1
#>   nsim:     2
#>   datasets: ccme_boron
#>   nrow:     6
#>   replace:  TRUE
#>   nrow_max: 1000 (setting)
#>   fit grid:
#>     rescale: FALSE
#>     computable: FALSE
#>     at_boundary_ok: TRUE
#>     min_pmix: ssd_min_pmix
#>     range_shape1: {0.05, 20}
#>     range_shape2: {0.05, 20}
#>     dists: gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull (setting)
#>   hc grid:
#>     est_method: multi (setting)
#>     proportion: 0.05 (setting)
#>     ci: FALSE (setting)
#>     nboot: 1000
#>     ci_method: weighted_samples
#>     parametric: TRUE
#>     samples: FALSE (setting)
#>   distsets:
#>     BCANZ: gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull
#>   partition_by:
#>     sample: dataset, sim, replace
#>     fit: dataset, sim, nrow, rescale
#>     hc: dataset, sim
#>   bundle:
#>     sample: 
#>     fit: replace, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2
#>     hc: replace, nrow, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2, nboot, ci_method, parametric, distset
```

### `min_pmix` is referenced by name

The `min_pmix` knob is referenced **by name**: the name — not the
function body — is what enters the task identity and hashes, so the
scenario’s identity stays stable under a recompile or a cosmetic edit.
It is supplied as an
[`ssd_pmix()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_pmix.md)
collection (the only accepted form — a bare function, a plain list, or a
character vector of names is rejected, and no name-string is resolved to
a function). Names come from the
[`ssd_pmix()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_pmix.md)
argument names, or by symbol capture for a bare `pkg::name` (mirroring
[`ssd_scenario_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_data.md)).
The default `ssd_pmix(ssd_min_pmix = ssdtools::ssd_min_pmix)` is stored
as `"ssd_min_pmix"`:

``` r

scenario$fit$min_pmix
#> [1] "ssd_min_pmix"
```

The single-argument *function* from the collection is additionally
**materialised on the scenario** at construction. It rides along for
execution and is retrieved by name with the
[`scenario_min_pmix()`](https://poissonconsulting.github.io/ssdsims/reference/scenario_min_pmix.md)
accessor; datasets are reached the same way with
[`scenario_dataset()`](https://poissonconsulting.github.io/ssdsims/reference/scenario_dataset.md):

``` r

identical(scenario_min_pmix(scenario, "ssd_min_pmix"), ssdtools::ssd_min_pmix)
#> [1] TRUE
head(scenario_dataset(scenario, "boron"), 3)
#> # A tibble: 3 × 5
#>   Chemical Species                Conc Group Units
#>   <chr>    <chr>                 <dbl> <fct> <chr>
#> 1 Boron    Oncorhynchus mykiss     2.1 Fish  mg/L 
#> 2 Boron    Ictalurus punctatus     2.4 Fish  mg/L 
#> 3 Boron    Micropterus salmoides   4.1 Fish  mg/L
```

Because the *name* drives hashing while the *value* only rides along for
execution, two scenarios with the same `min_pmix` name but different
function bodies produce byte-identical task identities — the split that
lets a cluster worker resolve `min_pmix` off the transported scenario
with no shared interactive environment.

### `dists` is a collection of distribution sets

`dists` is supplied as an
[`ssd_distset()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_distset.md)
collection: one or more named **distribution sets**, each a pool of
distributions model-averaged together to form one SSD. The constructor
validates members (a subset of
[`ssdtools::ssd_dists_all()`](https://bcgov.github.io/ssdtools/reference/ssd_dists_all.html))
and names (unique, filesystem-safe) up front:

``` r

ds <- ssd_distset(
  BCANZ = ssdtools::ssd_dists_bcanz(),
  Iwasaki = c("burrIII3", "gamma", "llogis", "lnorm", "weibull"),
  lnorm = "lnorm"
)
ds
#> <ssdsims_distset>
#>   BCANZ: gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull
#>   Iwasaki: burrIII3, gamma, llogis, lnorm, weibull
#>   lnorm: lnorm
```

The fit step fits the **union** of every set’s members *once* — the
single model-averaged superset every pool is a subset of — and the hc
step then [`subset()`](https://rdrr.io/r/base/subset.html)s that one fit
down to each pool’s members and re-averages. So declaring several pools
costs **one** fit, not one per pool (the motivating `ssdaveragerr`
“iwasaki” pattern: fit a superset, then compare BCANZ, the Iwasaki set,
and single distributions carved from it):

``` r

scenario_iwasaki <- ssd_define_scenario(
  data,
  nsim = 3L,
  seed = 42L,
  dists = ds
)
# one union fit per fit task ...
scenario_iwasaki$fit$dists
#> [1] "burrIII3"    "gamma"       "lgumbel"     "llogis"      "lnorm"      
#> [6] "lnorm_lnorm" "weibull"
# ... and the named pools carried for the hc step to subset by name
names(scenario_iwasaki$hc$distsets)
#> [1] "BCANZ"   "Iwasaki" "lnorm"
```

The set **name** — not its members — is what enters the task identity
(the `distset=<name>` path segment and the per-task primer), mirroring
`min_pmix` and datasets; the members ride on the scenario and are
reached by name with the
[`scenario_distset()`](https://poissonconsulting.github.io/ssdsims/reference/scenario_distset.md)
accessor:

``` r

scenario_distset(scenario_iwasaki, "Iwasaki")
#> [1] "burrIII3" "gamma"    "llogis"   "lnorm"    "weibull"
```

`distset` is an **hc** cross-join axis, so the hc task table fans out
over the declared sets (the fit table is unchanged — it fits the union).
A single-set collection (`ssd_distset(BCANZ = ...)`) has one `distset`
value and so does not multiply the table. Individual distributions still
never fan out: an axis value is always a whole averaging pool. A bare
character vector or plain list is rejected, pointing you to
[`ssd_distset()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_distset.md).

### The `ci` flag

`ci` is a scalar flag, not a cross-join axis. The point estimate is
computed analytically from the fit and is identical whether `ci` is
`TRUE` or `FALSE`, so a `ci = TRUE` run is a strict superset of
`ci = FALSE` (the same estimate, plus the `se`/`lcl`/`ucl` columns). The
choice is a scenario-wide either/or: `ci = TRUE` for estimates plus
bootstrap intervals (as in the scenario above), or `ci = FALSE` for
cheap, bootstrap-free point estimates only.

When `ci = FALSE`, the bootstrap-only knobs (`nboot`, `ci_method`,
`parametric`) are meaningless, so passing any of them is an error — set
`ci = TRUE` to enable the bootstrap, or omit the knob:

``` r

data <- ssd_scenario_data(ssddata::ccme_boron)
ssd_define_scenario(data, nsim = 2L, seed = 1L, ci = FALSE, nboot = 1000)
#> Error in `ssd_define_scenario()`:
#> ! Bootstrap-only knob ('nboot') cannot be set when `ci = FALSE`. Set `ci = TRUE` to enable bootstrap, or omit the knob.
```

## Stage 3: expand into task tables

[`ssd_scenario_tasks()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_tasks.md)
expands the scenario into the three per-step task tables — `sample`,
`fit`, and `hc` — bundled into a task set. This is still RNG-free: no
draws happen here.

``` r

tasks <- ssd_scenario_tasks(scenario)
tasks
#> <ssdsims_task_set>
#>   sample tasks: 6
#>   fit    tasks: 12
#>   hc     tasks: 48
```

The counts show how each step fans out from its parent. The key design
choice is that the single expensive **random draw** is its own `sample`
task, keyed only by `(dataset, sim, replace)`. The draw is sized by the
scenario’s fixed `nrow_max` setting (`nrow_max` resampled rows under the
default `replace = TRUE`; capped at the dataset size — the full
permutation — when `replace = FALSE`); every `nrow` value is then a
cheap, RNG-free [`head()`](https://rdrr.io/r/utils/head.html)
*sub-truncation* of that one draw, done inline at the `fit` step. So
`nrow` is an ordinary cross-join axis of the `fit` step and never
multiplies the underlying draw — one draw is shared across all sample
sizes, and adding an `nrow` value (within the effective draw size) never
changes it.

Each row carries a path-style `<step>_id` primary key (the Hive
partition path) plus its parent step’s id as a foreign key, so
dependencies are explicit and joinable:

``` r

tasks$sample
#> <ssdsims_tasks: sample>
#>   axes:  dataset, sim, replace
#>   tasks: 6
#> # A tibble: 6 × 4
#>   dataset   sim replace sample_id                         
#>   <chr>   <int> <lgl>   <chr>                             
#> 1 boron       1 TRUE    dataset=boron/sim=1/replace=TRUE  
#> 2 boron       2 TRUE    dataset=boron/sim=2/replace=TRUE  
#> 3 boron       3 TRUE    dataset=boron/sim=3/replace=TRUE  
#> 4 cadmium     1 TRUE    dataset=cadmium/sim=1/replace=TRUE
#> 5 cadmium     2 TRUE    dataset=cadmium/sim=2/replace=TRUE
#> 6 cadmium     3 TRUE    dataset=cadmium/sim=3/replace=TRUE
```

The scalar `ci` is applied uniformly to every `hc` task (it is a setting
read from the scenario, not an axis or a table column), as is
`est_method`. With `ci = TRUE` the bootstrap knobs (`nboot`,
`ci_method`, `parametric`) fan out fully; a `ci = FALSE` scenario would
instead give exactly one `hc` row per fit task, carrying `NA` for those
bootstrap-only knobs (every requested `est_method` is summarised within
each task from a single bootstrap, so it never fans out into separate
tasks).

``` r

tasks$hc
#> <ssdsims_tasks: hc>
#>   axes:  dataset, sim, replace, nrow, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2, nboot, ci_method, parametric, distset
#>   tasks: 48
#> # A tibble: 48 × 16
#>    dataset   sim replace  nrow rescale computable at_boundary_ok min_pmix    
#>    <chr>   <int> <lgl>   <int> <lgl>   <lgl>      <lgl>          <chr>       
#>  1 boron       1 TRUE        5 FALSE   FALSE      TRUE           ssd_min_pmix
#>  2 boron       1 TRUE        5 FALSE   FALSE      TRUE           ssd_min_pmix
#>  3 boron       1 TRUE        5 FALSE   FALSE      TRUE           ssd_min_pmix
#>  4 boron       1 TRUE        5 FALSE   FALSE      TRUE           ssd_min_pmix
#>  5 boron       1 TRUE       10 FALSE   FALSE      TRUE           ssd_min_pmix
#>  6 boron       1 TRUE       10 FALSE   FALSE      TRUE           ssd_min_pmix
#>  7 boron       1 TRUE       10 FALSE   FALSE      TRUE           ssd_min_pmix
#>  8 boron       1 TRUE       10 FALSE   FALSE      TRUE           ssd_min_pmix
#>  9 boron       2 TRUE        5 FALSE   FALSE      TRUE           ssd_min_pmix
#> 10 boron       2 TRUE        5 FALSE   FALSE      TRUE           ssd_min_pmix
#> # ℹ 38 more rows
#> # ℹ 8 more variables: range_shape1 <list>, range_shape2 <list>, nboot <int>,
#> #   ci_method <chr>, parametric <lgl>, distset <chr>, hc_id <chr>, fit_id <chr>
```

The per-step derivations are also available individually
([`ssd_scenario_sample_tasks()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_tasks.md),
[`ssd_scenario_fit_tasks()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_tasks.md),
[`ssd_scenario_hc_tasks()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_tasks.md))
when you only need one table.

## Stage 4: run the baseline loop

[`ssd_run_scenario_baseline()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario_baseline.md)
is the no-frills runner. It executes the three task tables in dependency
order, threading each task’s result forward to its children by the
foreign-key id. It runs **in-process**, with no `targets`, no shard
grouping, and no Parquet I/O.

It **is reproducible without an external seed**: the runner opens one
[`local_dqrng_backend()`](https://poissonconsulting.github.io/ssdsims/reference/local_dqrng_backend.md)
scope and seeds each task exactly once from `scenario$seed` plus a
per-task primer
([`task_primer()`](https://poissonconsulting.github.io/ssdsims/reference/task_primer.md)
over the task’s identity), so two runs of a scenario with a fixed `seed`
yield identical results, regardless of the order in which tasks run:

``` r

out <- ssd_run_scenario_baseline(scenario)
names(out)
#> [1] "sample" "fit"    "hc"
```

Each element is the corresponding task table augmented with a list
column of per-task results — `sample` draws, `fits` objects, and `hc`
tibbles (the `fit` step truncates its sample inline before fitting):

``` r

out$hc
#> <ssdsims_tasks: hc>
#>   axes:  dataset, sim, replace, nrow, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2, nboot, ci_method, parametric, distset
#>   tasks: 48
#> # A tibble: 48 × 20
#>    dataset   sim replace  nrow rescale computable at_boundary_ok min_pmix    
#>    <chr>   <int> <lgl>   <int> <lgl>   <lgl>      <lgl>          <chr>       
#>  1 boron       1 TRUE        5 FALSE   FALSE      TRUE           ssd_min_pmix
#>  2 boron       1 TRUE        5 FALSE   FALSE      TRUE           ssd_min_pmix
#>  3 boron       1 TRUE        5 FALSE   FALSE      TRUE           ssd_min_pmix
#>  4 boron       1 TRUE        5 FALSE   FALSE      TRUE           ssd_min_pmix
#>  5 boron       1 TRUE       10 FALSE   FALSE      TRUE           ssd_min_pmix
#>  6 boron       1 TRUE       10 FALSE   FALSE      TRUE           ssd_min_pmix
#>  7 boron       1 TRUE       10 FALSE   FALSE      TRUE           ssd_min_pmix
#>  8 boron       1 TRUE       10 FALSE   FALSE      TRUE           ssd_min_pmix
#>  9 boron       2 TRUE        5 FALSE   FALSE      TRUE           ssd_min_pmix
#> 10 boron       2 TRUE        5 FALSE   FALSE      TRUE           ssd_min_pmix
#> # ℹ 38 more rows
#> # ℹ 12 more variables: range_shape1 <list>, range_shape2 <list>, nboot <int>,
#> #   ci_method <chr>, parametric <lgl>, distset <chr>, hc_id <chr>,
#> #   fit_id <chr>, hc <list>, .start <dttm>, .end <dttm>, .host <chr>
```

Unnest the hazard-concentration estimates back onto their task
identities to get a tidy results table. `proportion` and `est_method`
are not task axes — they are passed whole to each `hc` call (one row per
value, from a single bootstrap), so every result already carries its own
`proportion` and `est_method` columns:

``` r

hcs <- tidyr::unnest(
  out$hc[c("dataset", "sim", "nrow", "hc")],
  hc,
  names_sep = "_"
)
hcs[c("dataset", "sim", "nrow", "hc_est_method", "hc_proportion", "hc_est")]
#> # A tibble: 96 × 6
#>    dataset   sim  nrow hc_est_method hc_proportion hc_est
#>    <chr>   <int> <int> <chr>                 <dbl>  <dbl>
#>  1 boron       1     5 multi                  0.05  2.11 
#>  2 boron       1     5 multi                  0.2   3.29 
#>  3 boron       1     5 multi                  0.05  1.19 
#>  4 boron       1     5 multi                  0.2   3.26 
#>  5 boron       1     5 multi                  0.05  2.11 
#>  6 boron       1     5 multi                  0.2   3.29 
#>  7 boron       1     5 multi                  0.05  1.19 
#>  8 boron       1     5 multi                  0.2   3.26 
#>  9 boron       1    10 multi                  0.05  0.734
#> 10 boron       1    10 multi                  0.2   2.26 
#> # ℹ 86 more rows
```

## Next: shards and the targets pipeline

The baseline runner threads results in memory. The next layer
materialises each step as **Hive-partitioned Parquet shards** grouped by
the scenario’s `partition_by` axes, and links steps by reading parent
shards back — the storage hand-off the cluster `targets` pipeline uses.
The [“Running a sharded
pipeline”](https://poissonconsulting.github.io/ssdsims/articles/sharded-pipeline.md)
vignette covers that: the single-core
[`ssd_run_scenario_shards()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario_shards.md)
(byte-identical to the baseline runner) and the shipped `targets`
template.

Still ahead on the roadmap (`TARGETS-DESIGN.md` §12): a per-scenario
manifest, cloud upload, shard-completeness assertions, and the
crew/SLURM controller. As each lands, the vignettes grow to cover it.
