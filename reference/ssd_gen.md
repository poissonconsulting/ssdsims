# Materialise Generator Datasets for a Simulation Scenario

Accepts generator-style inputs and materialises each, once, to a
validated tibble with a numeric `Conc` column of `.n` rows. The four
generator kinds are:

## Usage

``` r
ssd_gen(..., .n, .seed)
```

## Arguments

- ...:

  One or more generator inputs (a function, a function-name string, a
  `fitdists` object, or a `tmbfit` object), optionally named.

- .n:

  A scalar whole number: the number of rows each generator materialises.
  Required. (This is the generated population size; the scenario's
  `nrow` controls the resample/truncation sizes.)

- .seed:

  A scalar whole number: the base seed for generation. Required, and
  independent of the scenario's `seed`.

## Value

An `ssdsims_gen` object: a named list of validated `Conc` tibbles of
`.n` rows, for use within (or splicing into)
[`ssd_scenario_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_data.md).

## Details

- a **function** taking the number of rows as its first argument (e.g.
  [`ssdtools::ssd_rlnorm`](https://bcgov.github.io/ssdtools/reference/ssd_r.html));

- a **function-name string** (e.g. `"ssd_rlnorm"`), resolved as a bare
  name in the caller's environment and then the `ssdtools` namespace
  (never via parsing code); the string is also the dataset name;

- a **`tmbfit`** object (one distribution of a
  [`ssdtools::ssd_fit_dists()`](https://bcgov.github.io/ssdtools/reference/ssd_fit_dists.html)
  fit): drawn from the matching `ssd_r<dist>` function with the fit's
  estimates;

- a **`fitdists`** object: the top-weighted distribution is selected and
  drawn as for a `tmbfit`.

A data frame is **not** a generator - pass it directly to
[`ssd_scenario_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_data.md).

The result is an `ssdsims_gen` collection designed to compose with
[`ssd_scenario_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_data.md)
in two equivalent ways: as an unnamed argument (flattened in) or spliced
with `!!!`:

    ssd_scenario_data(boron = ccme_boron, ssd_gen(synth = ssd_rlnorm, .n = 30, .seed = 1L))
    ssd_scenario_data(boron = ccme_boron, !!!ssd_gen(synth = ssd_rlnorm, .n = 30, .seed = 1L))

## Reproducibility: `.seed` and the name-keyed streams

`.n` and `.seed` are **required**: irreproducible or unsized generation
is impossible by construction. They are dot-prefixed formals, so they
are never absorbed into `...` and a generator named `seed =` or `n =` is
never partial-matched onto them. Each generator draws under a scoped
dqrng state seeded by `.seed` with its dataset **name** as the dqrng
stream
([`task_primer()`](https://poissonconsulting.github.io/ssdsims/reference/task_primer.md)
over `list(dataset = name)`), so a single `.seed` fans out across all
generators in the call on independent, name-keyed streams. One `.n`
applies per call; for differing sizes, splice several `ssd_gen()` calls
together. The global `.Random.seed` is unchanged on return.

Names are taken from the argument names where supplied, otherwise the
function-name string itself, otherwise derived from the argument
expression by symbol capture (e.g.
[`ssdtools::ssd_rlnorm`](https://bcgov.github.io/ssdtools/reference/ssd_r.html)
becomes `"ssd_rlnorm"`); an input with no derivable name (e.g. an
anonymous function literal) must be given an explicit name. Names must
be unique across the call.

## dqrng-backed generation

Generation draws under the dqrng `pcg64` backend
([`local_dqrng_backend()`](https://poissonconsulting.github.io/ssdsims/reference/local_dqrng_backend.md)),
with each generator seeded through
[`local_dqrng_state()`](https://poissonconsulting.github.io/ssdsims/reference/local_dqrng_state.md),
which brackets the draw with the per-task dqrng-integrity witness: it
aborts if a generator escapes the backend (e.g. switches
[`RNGkind()`](https://rdrr.io/r/base/Random.html) and draws from base
R), since such draws are not reproducible under `.seed`. A generator
that consumes no randomness passes.

## See also

[`ssd_scenario_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_data.md),
[`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md),
[`local_dqrng_backend()`](https://poissonconsulting.github.io/ssdsims/reference/local_dqrng_backend.md),
[`task_primer()`](https://poissonconsulting.github.io/ssdsims/reference/task_primer.md).

## Examples

``` r
ssd_gen(synth = ssdtools::ssd_rlnorm, .n = 30, .seed = 42)
#> $synth
#> # A tibble: 30 × 1
#>     Conc
#>    <dbl>
#>  1 0.532
#>  2 0.782
#>  3 0.718
#>  4 0.137
#>  5 3.18 
#>  6 1.70 
#>  7 1.98 
#>  8 0.627
#>  9 0.788
#> 10 2.40 
#> # ℹ 20 more rows
#> 
#> attr(,"class")
#> [1] "ssdsims_gen"

# one .seed fans out across generators on independent name-keyed streams
ssd_gen(a = ssdtools::ssd_rlnorm, b = ssdtools::ssd_rlnorm, .n = 30, .seed = 42)
#> $a
#> # A tibble: 30 × 1
#>     Conc
#>    <dbl>
#>  1 2.41 
#>  2 0.914
#>  3 1.48 
#>  4 1.28 
#>  5 1.33 
#>  6 2.08 
#>  7 1.51 
#>  8 1.51 
#>  9 4.19 
#> 10 0.308
#> # ℹ 20 more rows
#> 
#> $b
#> # A tibble: 30 × 1
#>     Conc
#>    <dbl>
#>  1 5.05 
#>  2 2.96 
#>  3 0.266
#>  4 2.32 
#>  5 2.29 
#>  6 2.29 
#>  7 4.16 
#>  8 2.74 
#>  9 0.242
#> 10 1.33 
#> # ℹ 20 more rows
#> 
#> attr(,"class")
#> [1] "ssdsims_gen"

# composes with data frames in ssd_scenario_data()
ssd_scenario_data(
  boron = ssddata::ccme_boron,
  ssd_gen(synth = ssdtools::ssd_rlnorm, .n = 30, .seed = 42)
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
#>  1 0.532
#>  2 0.782
#>  3 0.718
#>  4 0.137
#>  5 3.18 
#>  6 1.70 
#>  7 1.98 
#>  8 0.627
#>  9 0.788
#> 10 2.40 
#> # ℹ 20 more rows
#> 
#> attr(,"class")
#> [1] "ssdsims_data"
```
