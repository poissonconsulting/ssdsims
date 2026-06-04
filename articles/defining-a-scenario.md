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
    [`ssd_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_data.md).
2.  **Declare** the scenario with
    [`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md).
3.  **Expand** it into per-step task tables with
    [`ssd_scenario_tasks()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_tasks.md).
4.  **Run** the baseline in-process loop with
    [`ssd_run_scenario_baseline()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario_baseline.md).

Stages 1–3 are side-effect-free. Only stage 4 touches the RNG.

## Stage 1: assemble the data

[`ssd_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_data.md)
is the single entry point for dataset input. It validates each data
frame — every dataset must carry a numeric `Conc` column, the species
sensitivity distribution convention — and assembles them into a named
collection.

``` r

datasets <- ssd_data(
  boron = ssddata::ccme_boron,
  cadmium = ssddata::ccme_cadmium
)
datasets
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

[`ssd_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_data.md)
is the extensible input point. A planned change (`scenario-input-types`)
will let each input also be a data *generator* — a `fitdists`/`tmbfit`
object, a generator function, or a function-name string — materialised
by a dataset registry. For now each input must be a data frame.

## Stage 2: declare the scenario

[`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md)
is the constructor. Its required arguments are the dataset input, `nsim`
(the replicate count), and `seed` (the RNG root — it has no default
because changing it fully re-roots every draw). Everything else is a
knob with a sensible default.

``` r

scenario <- ssd_define_scenario(
  datasets,
  nsim = 3L,
  nrow = c(5L, 10L, 20L),
  seed = 42L,
  dists = c("lnorm", "gamma"),
  proportion = c(0.05, 0.2),
  ci = c(FALSE, TRUE),
  nboot = c(10L, 100L),
  est_method = "multi",
  ci_method = "weighted_samples"
)
scenario
#> <ssdsims_scenario>
#>   seed:     42
#>   nsim:     3
#>   datasets: boron, cadmium
#>   nrow:     5, 10, 20
#>   replace:  FALSE
#>   fit grid:
#>     dists: lnorm, gamma
#>     rescale: FALSE
#>     computable: FALSE
#>     at_boundary_ok: TRUE
#>     min_pmix: ssd_min_pmix
#>     range_shape1: {0.05, 20}
#>     range_shape2: {0.05, 20}
#>   hc grid:
#>     proportion: 0.05, 0.2
#>     ci: FALSE, TRUE
#>     nboot: 10, 100
#>     est_method: multi
#>     ci_method: weighted_samples
#>     parametric: TRUE
#>     samples: FALSE
#>   partition_by:
#>     sample: dataset, sim, replace
#>     fit: dataset, sim, nrow, rescale
#>     hc: dataset, sim
#>   bundle:
#>     sample: 
#>     fit: replace, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2
#>     hc: replace, nrow, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2, ci, nboot, est_method, ci_method, parametric
```

The [`print()`](https://rdrr.io/r/base/print.html) method shows the
declarative fields: the scalar `seed` and `nsim`, the `nrow` sample
sizes, the dataset names, and the two argument grids (`fit` and `hc`).
Note what is *not* shown — the data frames themselves are retained for
the local runner but are not part of the declarative identity; the
cluster path carries only the names.

### Dataset input is flexible

The preferred form is an
[`ssd_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_data.md)
collection (above), which owns validation and naming. For convenience
the constructor also accepts bare data frame input, routed through the
same `Conc` validation, in several forms:

``` r

# 1. A single data frame; name derived from the expression ("ccme_boron").
ssd_define_scenario(ssddata::ccme_boron, nsim = 2L, seed = 1L)
#> <ssdsims_scenario>
#>   seed:     1
#>   nsim:     2
#>   datasets: ccme_boron
#>   nrow:     6
#>   replace:  FALSE
#>   fit grid:
#>     dists: gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull
#>     rescale: FALSE
#>     computable: FALSE
#>     at_boundary_ok: TRUE
#>     min_pmix: ssd_min_pmix
#>     range_shape1: {0.05, 20}
#>     range_shape2: {0.05, 20}
#>   hc grid:
#>     proportion: 0.05
#>     ci: FALSE
#>     nboot: 1000
#>     est_method: multi
#>     ci_method: weighted_samples
#>     parametric: TRUE
#>     samples: FALSE
#>   partition_by:
#>     sample: dataset, sim, replace
#>     fit: dataset, sim, nrow, rescale
#>     hc: dataset, sim
#>   bundle:
#>     sample: 
#>     fit: replace, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2
#>     hc: replace, nrow, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2, ci, nboot, est_method, ci_method, parametric

# 2. A single data frame with an explicit name.
ssd_define_scenario(ssddata::ccme_boron, name = "boron", nsim = 2L, seed = 1L)
#> <ssdsims_scenario>
#>   seed:     1
#>   nsim:     2
#>   datasets: boron
#>   nrow:     6
#>   replace:  FALSE
#>   fit grid:
#>     dists: gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull
#>     rescale: FALSE
#>     computable: FALSE
#>     at_boundary_ok: TRUE
#>     min_pmix: ssd_min_pmix
#>     range_shape1: {0.05, 20}
#>     range_shape2: {0.05, 20}
#>   hc grid:
#>     proportion: 0.05
#>     ci: FALSE
#>     nboot: 1000
#>     est_method: multi
#>     ci_method: weighted_samples
#>     parametric: TRUE
#>     samples: FALSE
#>   partition_by:
#>     sample: dataset, sim, replace
#>     fit: dataset, sim, nrow, rescale
#>     hc: dataset, sim
#>   bundle:
#>     sample: 
#>     fit: replace, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2
#>     hc: replace, nrow, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2, ci, nboot, est_method, ci_method, parametric
```

A named list (`list(boron = ..., cadmium = ...)`) takes names from the
list; an unnamed list derives them per element. Supplying both a named
list and `name=` is an error.

### `min_pmix` is referenced by name

The `min_pmix` knob is referenced **by name**: the name — not the
function body — is what enters the task identity and hashes, so the
scenario’s identity stays stable under a recompile or a cosmetic edit.
You can pass a character vector of names directly, or a function (or
list of functions) whose name is derived by symbol capture (mirroring
dataset naming). The default
[`ssdtools::ssd_min_pmix`](https://bcgov.github.io/ssdtools/reference/ssd_min_pmix.html)
is stored as `"ssd_min_pmix"`:

``` r

scenario$fit$min_pmix
#> [1] "ssd_min_pmix"
```

The resolved single-argument *function* is additionally **materialised
on the scenario** at construction (a name-string is resolved then, from
`ssdtools` or the caller’s environment, failing fast if it cannot be
resolved). It rides along for execution and is retrieved by name with
the
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

### The `ci = FALSE` rule

When `ci = FALSE` is the *only* confidence-interval value, the
bootstrap-only knobs (`nboot`, `ci_method`, `parametric`) are
meaningless, so passing any of them is an error:

``` r

ssd_define_scenario(
  ssddata::ccme_boron,
  nsim = 2L,
  seed = 1L,
  ci = FALSE,
  nboot = 1000
)
#> Error in `ssd_define_scenario()`:
#> ! Bootstrap-only knob ('nboot') cannot be set when `ci = FALSE`. Set `ci = c(FALSE, TRUE)` to enable bootstrap, or omit the knob.
```

To ask for *both* a point estimate and bootstrap intervals — the common
case — set `ci = c(FALSE, TRUE)` (as in the scenario above) rather than
a bare `ci = TRUE`. This keeps the study symmetric: the `ci = FALSE` row
is retained and collapses its bootstrap knobs to `NA` at expansion time,
while the `ci = TRUE` rows fan out across the full bootstrap grid.

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
#>   fit    tasks: 18
#>   hc     tasks: 54
```

The counts show how each step fans out from its parent. The key design
choice is that the single expensive **random draw** is its own `sample`
task, keyed only by `(dataset, sim, replace)`. The draw is of
`n_max = max(nrow)` rows; every `nrow` value is then a cheap, RNG-free
[`head()`](https://rdrr.io/r/utils/head.html) *sub-truncation* of that
one draw, done inline at the `fit` step. So `nrow` is an ordinary
cross-join axis of the `fit` step and never multiplies the underlying
draw — one draw is shared across all sample sizes.

Each row carries a path-style `<step>_id` primary key (the Hive
partition path) plus its parent step’s id as a foreign key, so
dependencies are explicit and joinable:

``` r

tasks$sample
#> <ssdsims_tasks: sample>
#>   axes:  dataset, sim, replace
#>   tasks: 6
#> # A tibble: 6 × 5
#>   dataset   sim replace n_max sample_id                          
#>   <chr>   <int> <lgl>   <int> <chr>                              
#> 1 boron       1 FALSE      20 dataset=boron/sim=1/replace=FALSE  
#> 2 boron       2 FALSE      20 dataset=boron/sim=2/replace=FALSE  
#> 3 boron       3 FALSE      20 dataset=boron/sim=3/replace=FALSE  
#> 4 cadmium     1 FALSE      20 dataset=cadmium/sim=1/replace=FALSE
#> 5 cadmium     2 FALSE      20 dataset=cadmium/sim=2/replace=FALSE
#> 6 cadmium     3 FALSE      20 dataset=cadmium/sim=3/replace=FALSE
```

The `ci = FALSE` collapse is visible in the `hc` table: the `ci = FALSE`
rows carry `NA` for the bootstrap-only knobs and are not multiplied
across them, while the `ci = TRUE` rows fan out fully.

``` r

tasks$hc
#> <ssdsims_tasks: hc>
#>   axes:  dataset, sim, replace, nrow, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2, ci, nboot, est_method, ci_method, parametric
#>   tasks: 54
#> # A tibble: 54 × 17
#>    dataset   sim replace  nrow rescale computable at_boundary_ok min_pmix    
#>    <chr>   <int> <lgl>   <int> <lgl>   <lgl>      <lgl>          <chr>       
#>  1 boron       1 FALSE       5 FALSE   FALSE      TRUE           ssd_min_pmix
#>  2 boron       1 FALSE       5 FALSE   FALSE      TRUE           ssd_min_pmix
#>  3 boron       1 FALSE       5 FALSE   FALSE      TRUE           ssd_min_pmix
#>  4 boron       1 FALSE      10 FALSE   FALSE      TRUE           ssd_min_pmix
#>  5 boron       1 FALSE      10 FALSE   FALSE      TRUE           ssd_min_pmix
#>  6 boron       1 FALSE      10 FALSE   FALSE      TRUE           ssd_min_pmix
#>  7 boron       1 FALSE      20 FALSE   FALSE      TRUE           ssd_min_pmix
#>  8 boron       1 FALSE      20 FALSE   FALSE      TRUE           ssd_min_pmix
#>  9 boron       1 FALSE      20 FALSE   FALSE      TRUE           ssd_min_pmix
#> 10 boron       2 FALSE       5 FALSE   FALSE      TRUE           ssd_min_pmix
#> # ℹ 44 more rows
#> # ℹ 9 more variables: range_shape1 <list>, range_shape2 <list>, ci <lgl>,
#> #   nboot <int>, est_method <chr>, ci_method <chr>, parametric <lgl>,
#> #   hc_id <chr>, fit_id <chr>
```

The per-step derivations are also available individually
([`ssd_scenario_sample_tasks()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_sample_tasks.md),
[`ssd_scenario_fit_tasks()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_fit_tasks.md),
[`ssd_scenario_hc_tasks()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_hc_tasks.md))
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
#>   axes:  dataset, sim, replace, nrow, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2, ci, nboot, est_method, ci_method, parametric
#>   tasks: 54
#> # A tibble: 54 × 18
#>    dataset   sim replace  nrow rescale computable at_boundary_ok min_pmix    
#>    <chr>   <int> <lgl>   <int> <lgl>   <lgl>      <lgl>          <chr>       
#>  1 boron       1 FALSE       5 FALSE   FALSE      TRUE           ssd_min_pmix
#>  2 boron       1 FALSE       5 FALSE   FALSE      TRUE           ssd_min_pmix
#>  3 boron       1 FALSE       5 FALSE   FALSE      TRUE           ssd_min_pmix
#>  4 boron       1 FALSE      10 FALSE   FALSE      TRUE           ssd_min_pmix
#>  5 boron       1 FALSE      10 FALSE   FALSE      TRUE           ssd_min_pmix
#>  6 boron       1 FALSE      10 FALSE   FALSE      TRUE           ssd_min_pmix
#>  7 boron       1 FALSE      20 FALSE   FALSE      TRUE           ssd_min_pmix
#>  8 boron       1 FALSE      20 FALSE   FALSE      TRUE           ssd_min_pmix
#>  9 boron       1 FALSE      20 FALSE   FALSE      TRUE           ssd_min_pmix
#> 10 boron       2 FALSE       5 FALSE   FALSE      TRUE           ssd_min_pmix
#> # ℹ 44 more rows
#> # ℹ 10 more variables: range_shape1 <list>, range_shape2 <list>, ci <lgl>,
#> #   nboot <int>, est_method <chr>, ci_method <chr>, parametric <lgl>,
#> #   hc_id <chr>, fit_id <chr>, hc <list>
```

Unnest the hazard-concentration estimates back onto their task
identities to get a tidy results table. `proportion` is not a task axis
— it is passed whole to each `hc` call, so every result already carries
its own `proportion` column:

``` r

hcs <- tidyr::unnest(
  out$hc[c("dataset", "sim", "nrow", "ci", "est_method", "hc")],
  hc,
  names_sep = "_"
)
hcs[c("dataset", "sim", "nrow", "ci", "hc_proportion", "hc_est")]
#> # A tibble: 108 × 6
#>    dataset   sim  nrow ci    hc_proportion hc_est
#>    <chr>   <int> <int> <lgl>         <dbl>  <dbl>
#>  1 boron       1     5 FALSE          0.05   2.65
#>  2 boron       1     5 FALSE          0.2    6.54
#>  3 boron       1     5 TRUE           0.05   2.65
#>  4 boron       1     5 TRUE           0.2    6.54
#>  5 boron       1     5 TRUE           0.05   2.65
#>  6 boron       1     5 TRUE           0.2    6.54
#>  7 boron       1    10 FALSE          0.05   1.63
#>  8 boron       1    10 FALSE          0.2    4.91
#>  9 boron       1    10 TRUE           0.05   1.63
#> 10 boron       1    10 TRUE           0.2    4.91
#> # ℹ 98 more rows
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
