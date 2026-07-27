# Generating and Assembling Datasets

``` r

library(ssdsims)
```

Before a scenario can be declared with
[`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md),
its datasets have to be *assembled*: named, validated, and (where a
dataset is synthetic) *generated*. That is the first stage of the
workflow the [“Defining a
Scenario”](https://poissonconsulting.github.io/ssdsims/articles/defining-a-scenario.md)
vignette walks through, and it is handled by exactly two functions:

- [`ssd_scenario_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_data.md)
  — the single entry point for dataset input. It validates each data
  frame and assembles them into a named collection.
- [`ssd_gen()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_gen.md)
  — the generation half. It materialises *generator*-style inputs (a
  distribution draw function, a fitted SSD, and so on) into the same
  kind of validated dataset, once and reproducibly.

This vignette documents both in detail: every argument, the accepted
input forms, the structure of what they return, and how they compose.
Neither function draws random numbers at scenario-definition time except
[`ssd_gen()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_gen.md),
whose generation is deliberately separated out precisely so that
[`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md)
itself stays RNG-free.

## `ssd_gen()`: materialise generated datasets

[`ssd_gen()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_gen.md)
takes one or more *generators* and materialises each — **once** — to a
validated dataset: a tibble with a numeric `Conc` column of `.n` rows.
“Materialise once” is the key idea: the output is a fixed tibble of
numbers, not a rule that keeps drawing. Downstream it is an ordinary
dataset that the scenario resamples, indistinguishable from an observed
one.

### The four generator kinds

Each argument in `...` is a generator, and there are four accepted
kinds:

- a **function** taking the number of rows as its first argument (e.g.
  [`ssdtools::ssd_rlnorm`](https://bcgov.github.io/ssdtools/reference/ssd_r.html));
- a **function-name string** (e.g. `"ssd_rlnorm"`), resolved as a bare
  name in the caller’s environment and then the `ssdtools` namespace
  (never by parsing code); the string doubles as the dataset name;
- a **`tmbfit`** object — one distribution of an
  [`ssdtools::ssd_fit_dists()`](https://bcgov.github.io/ssdtools/reference/ssd_fit_dists.html)
  fit — drawn from the matching `ssd_r<dist>` function with the fit’s
  estimates;
- a **`fitdists`** object — the top-weighted distribution is selected
  and then drawn as for a `tmbfit`.

A data frame is **not** a generator; pass observed data straight to
[`ssd_scenario_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_data.md).
Anything that is neither a function, a single function-name string, a
`tmbfit`, nor a `fitdists` is rejected.

### Arguments

`ssd_gen(..., .n, .seed)` has just two options beyond the generators
themselves, and both are **required** — by construction, generation can
be neither unsized nor unseeded:

| Argument | Meaning | Requirement |
|----|----|----|
| `...` | One or more generators (the four kinds above), optionally named. | At least one. |
| `.n` | The number of rows each generator materialises — the generated *population* size. | Required; a scalar whole number greater than 0. |
| `.seed` | The base seed for generation, independent of the scenario’s own `seed`. | Required; a scalar whole number. |

The two options are dot-prefixed (`.n`, `.seed`) so that a generator you
happen to name `n =` or `seed =` is never partial-matched onto them and
is never absorbed into `...`.

Note the division of labour: `.n` is the size of the *generated
population*, while the *scenario’s* `nrow`/`nrow_max` options control
the resample and truncation sizes drawn from it later. One `.n` applies
to the whole call; to generate populations of differing sizes, splice
several
[`ssd_gen()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_gen.md)
calls together.

### A worked example

Give each generator a name and
[`ssd_gen()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_gen.md)
returns a named collection of materialised datasets:

``` r

gen <- ssd_gen(synth = ssdtools::ssd_rlnorm, .n = 30, .seed = 1L)
gen
#> $synth
#> # A tibble: 30 × 1
#>     Conc
#>    <dbl>
#>  1 1.43 
#>  2 0.229
#>  3 1.77 
#>  4 0.812
#>  5 2.52 
#>  6 0.421
#>  7 2.89 
#>  8 3.85 
#>  9 0.643
#> 10 0.962
#> # ℹ 20 more rows
#> 
#> attr(,"class")
#> [1] "ssdsims_gen"
```

Each member is a validated `Conc` tibble of exactly `.n` rows:

``` r

gen$synth
#> # A tibble: 30 × 1
#>     Conc
#>    <dbl>
#>  1 1.43 
#>  2 0.229
#>  3 1.77 
#>  4 0.812
#>  5 2.52 
#>  6 0.421
#>  7 2.89 
#>  8 3.85 
#>  9 0.643
#> 10 0.962
#> # ℹ 20 more rows
nrow(gen$synth)
#> [1] 30
```

A single call can materialise several generators at once. Because each
is keyed by its own dataset **name** (see below), one `.seed` fans out
across them, giving independent draws rather than identical ones:

``` r

ssd_gen(
  a = ssdtools::ssd_rlnorm,
  b = ssdtools::ssd_rlnorm,
  .n = 5,
  .seed = 1L
)
#> $a
#> # A tibble: 5 × 1
#>    Conc
#>   <dbl>
#> 1 1.28 
#> 2 0.652
#> 3 3.77 
#> 4 1.06 
#> 5 1.61 
#> 
#> $b
#> # A tibble: 5 × 1
#>    Conc
#>   <dbl>
#> 1 1.25 
#> 2 0.819
#> 3 1.53 
#> 4 0.705
#> 5 0.458
#> 
#> attr(,"class")
#> [1] "ssdsims_gen"
```

### How generators are named

Names follow a fixed precedence, and must be unique across the call:

1.  the **argument name**, where you supply one (`synth = ...`);
2.  otherwise, for the function-name-string form, the **string itself**
    (`"ssd_rlnorm"` becomes the name `"ssd_rlnorm"`);
3.  otherwise, the name is **derived from the argument expression** by
    symbol capture (a bare
    [`ssdtools::ssd_rlnorm`](https://bcgov.github.io/ssdtools/reference/ssd_r.html)
    becomes `"ssd_rlnorm"`).

An input with no derivable name — an anonymous function literal, say —
must be given an explicit name, or
[`ssd_gen()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_gen.md)
aborts asking for one.

### Reproducibility and the name-keyed streams

Generation draws under a scoped dqrng `pcg64` backend
([`local_dqrng_backend()`](https://poissonconsulting.github.io/ssdsims/reference/local_dqrng_backend.md)).
Each generator is seeded through
[`local_dqrng_state()`](https://poissonconsulting.github.io/ssdsims/reference/local_dqrng_state.md)
with `.seed` as the base seed and its own dataset **name** as the stream
key
([`task_primer()`](https://poissonconsulting.github.io/ssdsims/reference/task_primer.md)
over `list(dataset = name)`). Two consequences follow:

- **One `.seed` reproducibly fans out** across every generator in the
  call, on independent, name-keyed streams — which is why the `a`/`b`
  example above drew two different samples from a single `.seed`.
- **The global RNG state is left untouched**:
  [`ssd_gen()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_gen.md)
  restores `.Random.seed` on return, so calling it never perturbs the
  surrounding session’s random stream.

Re-running the same call therefore reproduces the same data exactly:

``` r

identical(
  ssd_gen(synth = ssdtools::ssd_rlnorm, .n = 30, .seed = 1L),
  ssd_gen(synth = ssdtools::ssd_rlnorm, .n = 30, .seed = 1L)
)
#> [1] TRUE
```

As a guard on that reproducibility, each draw is bracketed by a
dqrng-integrity check: a generator that *escapes* the backend — for
instance by switching [`RNGkind()`](https://rdrr.io/r/base/Random.html)
and drawing from base R, which would not be reproducible under `.seed` —
aborts. A generator that consumes no randomness passes. A generator is
also required to return a numeric vector of length `.n`; anything else
is an error.

### Generating from a fitted distribution

The `tmbfit` and `fitdists` kinds let you generate data from a
distribution *fitted to real data* rather than one you parameterise by
hand. Fit an SSD, hand the fit to
[`ssd_gen()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_gen.md),
and it draws from the fit’s own estimates (for a `fitdists`, from its
top-weighted distribution):

``` r

fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron, dists = "lnorm")
ssd_gen(from_fit = fit, .n = 20, .seed = 7L)
#> $from_fit
#> # A tibble: 20 × 1
#>      Conc
#>     <dbl>
#>  1  1.39 
#>  2  3.42 
#>  3  3.84 
#>  4 15.9  
#>  5 21.2  
#>  6 40.2  
#>  7 44.5  
#>  8  5.04 
#>  9 22.0  
#> 10 52.4  
#> 11 62.2  
#> 12 29.4  
#> 13 11.6  
#> 14  0.996
#> 15 11.9  
#> 16 24.7  
#> 17 14.6  
#> 18  7.66 
#> 19  6.48 
#> 20 18.2  
#> 
#> attr(,"class")
#> [1] "ssdsims_gen"
```

## `ssd_scenario_data()`: assemble and validate the collection

[`ssd_scenario_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_data.md)
is the single entry point through which
[`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md)
takes dataset input. It collects one or more datasets into a validated,
named collection.

### Arguments and validation

`ssd_scenario_data(...)` takes a single `...`: one or more data frames
(optionally named) and/or
[`ssd_gen()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_gen.md)
collections. Every **data frame** is validated to carry a numeric `Conc`
column — the species sensitivity distribution convention — and is
coerced to a tibble; any additional columns are preserved. A data frame
that is not a data frame, or that lacks a numeric `Conc` column, aborts.

``` r

data <- ssd_scenario_data(
  boron = ssddata::ccme_boron,
  cadmium = ssddata::ccme_cadmium
)
data
#> $boron
#> # A tibble: 28 × 6
#>    Chemical Species                  Conc Group        Units Medium    
#>    <chr>    <chr>                   <dbl> <fct>        <chr> <chr>     
#>  1 Boron    Oncorhynchus mykiss       2.1 Fish         mg/L  Freshwater
#>  2 Boron    Ictalurus punctatus       2.4 Fish         mg/L  Freshwater
#>  3 Boron    Micropterus salmoides     4.1 Fish         mg/L  Freshwater
#>  4 Boron    Brachydanio rerio        10   Fish         mg/L  Freshwater
#>  5 Boron    Carassius auratus        15.6 Fish         mg/L  Freshwater
#>  6 Boron    Pimephales promelas      18.3 Fish         mg/L  Freshwater
#>  7 Boron    Daphnia magna             6   Invertebrate mg/L  Freshwater
#>  8 Boron    Opercularia bimarginata  10   Invertebrate mg/L  Freshwater
#>  9 Boron    Ceriodaphnia dubia       13.4 Invertebrate mg/L  Freshwater
#> 10 Boron    Entosiphon sulcatum      15   Invertebrate mg/L  Freshwater
#> # ℹ 18 more rows
#> 
#> $cadmium
#> # A tibble: 36 × 6
#>    Chemical Species                   Conc Group Units Medium    
#>    <chr>    <chr>                    <dbl> <fct> <chr> <chr>     
#>  1 Cadmium  Oncorhynchus mykiss       0.23 Fish  ug/L  Freshwater
#>  2 Cadmium  Salvelinus confluentus    0.83 Fish  ug/L  Freshwater
#>  3 Cadmium  Cottus bairdi             0.96 Fish  ug/L  Freshwater
#>  4 Cadmium  Salmo salar               0.99 Fish  ug/L  Freshwater
#>  5 Cadmium  Acipenser transmontanus   1.14 Fish  ug/L  Freshwater
#>  6 Cadmium  Prosopium williamsoni     1.25 Fish  ug/L  Freshwater
#>  7 Cadmium  Salmo trutta              1.36 Fish  ug/L  Freshwater
#>  8 Cadmium  Salvelinus fontinalis     2.23 Fish  ug/L  Freshwater
#>  9 Cadmium  Oncorhynchus tshawytscha  2.29 Fish  ug/L  Freshwater
#> 10 Cadmium  Pimephales promelas       2.36 Fish  ug/L  Freshwater
#> # ℹ 26 more rows
#> 
#> attr(,"class")
#> [1] "ssdsims_data"
```

Naming mirrors
[`ssd_gen()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_gen.md):
names come from the argument names where supplied, otherwise they are
derived from the argument expression by symbol capture (so a bare
[`ssddata::ccme_boron`](https://open-aims.github.io/ssddata/reference/ccme_boron.html)
becomes `"ccme_boron"`). Names must be unique, and a literal with no
derivable name (a bare `data.frame(...)` call, say) must be given an
explicit name.

``` r

ssd_scenario_data(ssddata::ccme_boron)
#> $ccme_boron
#> # A tibble: 28 × 6
#>    Chemical Species                  Conc Group        Units Medium    
#>    <chr>    <chr>                   <dbl> <fct>        <chr> <chr>     
#>  1 Boron    Oncorhynchus mykiss       2.1 Fish         mg/L  Freshwater
#>  2 Boron    Ictalurus punctatus       2.4 Fish         mg/L  Freshwater
#>  3 Boron    Micropterus salmoides     4.1 Fish         mg/L  Freshwater
#>  4 Boron    Brachydanio rerio        10   Fish         mg/L  Freshwater
#>  5 Boron    Carassius auratus        15.6 Fish         mg/L  Freshwater
#>  6 Boron    Pimephales promelas      18.3 Fish         mg/L  Freshwater
#>  7 Boron    Daphnia magna             6   Invertebrate mg/L  Freshwater
#>  8 Boron    Opercularia bimarginata  10   Invertebrate mg/L  Freshwater
#>  9 Boron    Ceriodaphnia dubia       13.4 Invertebrate mg/L  Freshwater
#> 10 Boron    Entosiphon sulcatum      15   Invertebrate mg/L  Freshwater
#> # ℹ 18 more rows
#> 
#> attr(,"class")
#> [1] "ssdsims_data"
```

### Relationship to `ssd_gen()`

[`ssd_gen()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_gen.md)
does not stand alone in the workflow: its output is designed to
**compose into**
[`ssd_scenario_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_data.md).
[`ssd_scenario_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_data.md)
does not call
[`ssd_gen()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_gen.md),
nor the reverse — generation and assembly stay separate, so the
assembler draws no random numbers. Instead, an
[`ssd_gen()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_gen.md)
collection passed to
[`ssd_scenario_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_data.md)
is *flattened in*: each already-validated member joins the collection
under its own name, alongside the observed data frames.

Names must be unique across the *entire* assembled collection, not
merely within any one input: a generated member — or a spliced-in list
member — whose name collides with another dataset, observed or
generated, aborts.

There are two equivalent ways to pass it, and an
[`ssd_gen()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_gen.md)
collection must always be supplied **unnamed** — the automatic flatten
is keyed on the collection’s `ssdsims_gen` class, and its members
already carry their own names:

``` r

# as an unnamed argument (flattened in) ...
ssd_scenario_data(
  boron = ssddata::ccme_boron,
  ssd_gen(synth = ssdtools::ssd_rlnorm, .n = 30, .seed = 1L)
)
#> $boron
#> # A tibble: 28 × 6
#>    Chemical Species                  Conc Group        Units Medium    
#>    <chr>    <chr>                   <dbl> <fct>        <chr> <chr>     
#>  1 Boron    Oncorhynchus mykiss       2.1 Fish         mg/L  Freshwater
#>  2 Boron    Ictalurus punctatus       2.4 Fish         mg/L  Freshwater
#>  3 Boron    Micropterus salmoides     4.1 Fish         mg/L  Freshwater
#>  4 Boron    Brachydanio rerio        10   Fish         mg/L  Freshwater
#>  5 Boron    Carassius auratus        15.6 Fish         mg/L  Freshwater
#>  6 Boron    Pimephales promelas      18.3 Fish         mg/L  Freshwater
#>  7 Boron    Daphnia magna             6   Invertebrate mg/L  Freshwater
#>  8 Boron    Opercularia bimarginata  10   Invertebrate mg/L  Freshwater
#>  9 Boron    Ceriodaphnia dubia       13.4 Invertebrate mg/L  Freshwater
#> 10 Boron    Entosiphon sulcatum      15   Invertebrate mg/L  Freshwater
#> # ℹ 18 more rows
#> 
#> $synth
#> # A tibble: 30 × 1
#>     Conc
#>    <dbl>
#>  1 1.43 
#>  2 0.229
#>  3 1.77 
#>  4 0.812
#>  5 2.52 
#>  6 0.421
#>  7 2.89 
#>  8 3.85 
#>  9 0.643
#> 10 0.962
#> # ℹ 20 more rows
#> 
#> attr(,"class")
#> [1] "ssdsims_data"
```

``` r

# ... or spliced with `!!!` --- exactly equivalent
ssd_scenario_data(
  boron = ssddata::ccme_boron,
  !!!ssd_gen(synth = ssdtools::ssd_rlnorm, .n = 30, .seed = 1L)
)
#> $boron
#> # A tibble: 28 × 6
#>    Chemical Species                  Conc Group        Units Medium    
#>    <chr>    <chr>                   <dbl> <fct>        <chr> <chr>     
#>  1 Boron    Oncorhynchus mykiss       2.1 Fish         mg/L  Freshwater
#>  2 Boron    Ictalurus punctatus       2.4 Fish         mg/L  Freshwater
#>  3 Boron    Micropterus salmoides     4.1 Fish         mg/L  Freshwater
#>  4 Boron    Brachydanio rerio        10   Fish         mg/L  Freshwater
#>  5 Boron    Carassius auratus        15.6 Fish         mg/L  Freshwater
#>  6 Boron    Pimephales promelas      18.3 Fish         mg/L  Freshwater
#>  7 Boron    Daphnia magna             6   Invertebrate mg/L  Freshwater
#>  8 Boron    Opercularia bimarginata  10   Invertebrate mg/L  Freshwater
#>  9 Boron    Ceriodaphnia dubia       13.4 Invertebrate mg/L  Freshwater
#> 10 Boron    Entosiphon sulcatum      15   Invertebrate mg/L  Freshwater
#> # ℹ 18 more rows
#> 
#> $synth
#> # A tibble: 30 × 1
#>     Conc
#>    <dbl>
#>  1 1.43 
#>  2 0.229
#>  3 1.77 
#>  4 0.812
#>  5 2.52 
#>  6 0.421
#>  7 2.89 
#>  8 3.85 
#>  9 0.643
#> 10 0.962
#> # ℹ 20 more rows
#> 
#> attr(,"class")
#> [1] "ssdsims_data"
```

### Passing an ordinary named list

The unnamed shortcut above works because an
[`ssd_gen()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_gen.md)
collection carries a class (`ssdsims_gen`) that marks it for flattening.
An *ordinary* named list of datasets — a hand-built list, or the
collection returned by
[`ssddata::ssd_data_sets()`](https://open-aims.github.io/ssddata/reference/ssd_data_sets.html)
— carries no such class, so it is not flattened when passed on its own.
Splice it in with `!!!`, which spreads its members into named arguments,
one per dataset:

``` r

sets <- list(boron = ssddata::ccme_boron, cadmium = ssddata::ccme_cadmium)
ssd_scenario_data(!!!sets)
#> $boron
#> # A tibble: 28 × 6
#>    Chemical Species                  Conc Group        Units Medium    
#>    <chr>    <chr>                   <dbl> <fct>        <chr> <chr>     
#>  1 Boron    Oncorhynchus mykiss       2.1 Fish         mg/L  Freshwater
#>  2 Boron    Ictalurus punctatus       2.4 Fish         mg/L  Freshwater
#>  3 Boron    Micropterus salmoides     4.1 Fish         mg/L  Freshwater
#>  4 Boron    Brachydanio rerio        10   Fish         mg/L  Freshwater
#>  5 Boron    Carassius auratus        15.6 Fish         mg/L  Freshwater
#>  6 Boron    Pimephales promelas      18.3 Fish         mg/L  Freshwater
#>  7 Boron    Daphnia magna             6   Invertebrate mg/L  Freshwater
#>  8 Boron    Opercularia bimarginata  10   Invertebrate mg/L  Freshwater
#>  9 Boron    Ceriodaphnia dubia       13.4 Invertebrate mg/L  Freshwater
#> 10 Boron    Entosiphon sulcatum      15   Invertebrate mg/L  Freshwater
#> # ℹ 18 more rows
#> 
#> $cadmium
#> # A tibble: 36 × 6
#>    Chemical Species                   Conc Group Units Medium    
#>    <chr>    <chr>                    <dbl> <fct> <chr> <chr>     
#>  1 Cadmium  Oncorhynchus mykiss       0.23 Fish  ug/L  Freshwater
#>  2 Cadmium  Salvelinus confluentus    0.83 Fish  ug/L  Freshwater
#>  3 Cadmium  Cottus bairdi             0.96 Fish  ug/L  Freshwater
#>  4 Cadmium  Salmo salar               0.99 Fish  ug/L  Freshwater
#>  5 Cadmium  Acipenser transmontanus   1.14 Fish  ug/L  Freshwater
#>  6 Cadmium  Prosopium williamsoni     1.25 Fish  ug/L  Freshwater
#>  7 Cadmium  Salmo trutta              1.36 Fish  ug/L  Freshwater
#>  8 Cadmium  Salvelinus fontinalis     2.23 Fish  ug/L  Freshwater
#>  9 Cadmium  Oncorhynchus tshawytscha  2.29 Fish  ug/L  Freshwater
#> 10 Cadmium  Pimephales promelas       2.36 Fish  ug/L  Freshwater
#> # ℹ 26 more rows
#> 
#> attr(,"class")
#> [1] "ssdsims_data"
```

The same idiom brings in a whole packaged collection —
`ssd_scenario_data(!!!ssddata::ssd_data_sets())` — each member entering
under its list name.

For now, `!!!` is the way to pass a plain list of datasets — an escape
hatch rather than a first-class list interface. Passing the list *bare*
(unspliced) currently errors: the automatic-flatten shortcut is keyed on
[`ssd_gen()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_gen.md)’s
`ssdsims_gen` class, not on being a list of data frames. A more direct
way to accept a list of data frames may be added later (see
[\#218](https://github.com/poissonconsulting/ssdsims/issues/218)); until
then, splice with `!!!`.

``` r

# `!!!` is the escape hatch: passing the list bare currently errors. This
# check is self-invalidating --- if a future release accepts a bare list of
# data frames (see #218), the assertion fails here and flags this section for
# updating.
err <- try(ssd_scenario_data(sets), silent = TRUE)
stopifnot(inherits(err, "try-error"))
cat(conditionMessage(attr(err, "condition")))
#> Dataset `sets` must be a data frame.
```

### The returned collection

[`ssd_scenario_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_data.md)
returns an `ssdsims_data` object: a named list of validated tibbles.
Once a generated dataset is inside the collection it is an ordinary
tibble, indistinguishable downstream from an observed one — the
generator code is simply its provenance. This collection is what
[`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md)
accepts as its dataset input:

``` r

data <- ssd_scenario_data(
  boron = ssddata::ccme_boron,
  ssd_gen(synth = ssdtools::ssd_rlnorm, .n = 30, .seed = 1L)
)
ssd_define_scenario(data, nsim = 2L, seed = 42L)
#> <ssdsims_scenario>
#>   seed:     42
#>   nsim:     2
#>   datasets: boron, synth
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

## A note on seeds

Two independent seeds sit either side of this stage, and it is worth
keeping them distinct:

- [`ssd_gen()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_gen.md)’s
  `.seed` controls **generation** — the one-off draw that materialises a
  synthetic population. It is fixed at assembly time and baked into the
  resulting tibble.
- [`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md)’s
  `seed` controls the **scenario’s** downstream draws — the resampling
  and bootstrapping the pipeline performs on whatever datasets it is
  given, generated or observed.

Because a generated dataset is materialised once and then treated as
ordinary data, the scenario resamples it under `seed` exactly as it
would an observed dataset; `.seed` no longer plays any role past
generation.

## See also

- [`vignette("defining-a-scenario")`](https://poissonconsulting.github.io/ssdsims/articles/defining-a-scenario.md)
  — the full workflow this data-assembly stage feeds into.
- [`ssd_scenario_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_data.md),
  [`ssd_gen()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_gen.md),
  [`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md).
