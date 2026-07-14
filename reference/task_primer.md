# Derive a Per-task Primer from its Parameters

Derives the per-task **primer** – a length-2 integer vector – from
`rlang::hash(params)`, suitable for the `stream` argument of
[`dqrng::dqset.seed()`](https://daqana.github.io/dqrng/reference/dqrng-functions.html).
Together with the scenario `seed`, the primer fully specifies a task's
RNG starting point:
`dqrng::dqset.seed(seed, stream = task_primer(params))`. It pairs with
[`local_dqrng_state()`](https://poissonconsulting.github.io/ssdsims/reference/local_dqrng_state.md),
which installs the `(seed, primer)` pair under an active
[`local_dqrng_backend()`](https://poissonconsulting.github.io/ssdsims/reference/local_dqrng_backend.md)
scope.

## Usage

``` r
task_primer(params)
```

## Arguments

- params:

  A plain named list of task parameters, or a single-row data frame (one
  task-table row).

## Value

An integer vector of length 2 – the per-task primer – to pass as the
`stream` argument of
[`dqrng::dqset.seed()`](https://daqana.github.io/dqrng/reference/dqrng-functions.html)
(via
[`local_dqrng_state()`](https://poissonconsulting.github.io/ssdsims/reference/local_dqrng_state.md)).

## Details

The primer packs 64 bits of the
[`rlang::hash()`](https://rlang.r-lib.org/reference/hash.html) digest
(xxhash128) as `c(hi32, lo32)`. Each 32-bit half is encoded as a signed
int32, with the reserved bit pattern `0x80000000` (INT_MIN, which R
cannot represent as a non-`NA` integer) mapped to `NA_integer_`; dqrng
accepts `NA_integer_` in `stream` and treats it as INT_MIN, so the
encoding recovers the full 64 bits of stream entropy.

`params` may be a plain named list or a single-row data frame (one row
of a `{sample,fit,hc}_tasks` table). A data-frame row is normalised to a
canonical plain list – the inverse of
[`tibble::tibble_row()`](https://tibble.tidyverse.org/reference/tibble.html)
– by dropping all attributes, unwrapping length-1 list-style columns to
their element, and leaving df-style (nested data-frame) columns as data
frames, before hashing. The primer is therefore identical whether
derived from the row or from the equivalent plain list. Note that
[`rlang::hash()`](https://rlang.r-lib.org/reference/hash.html) is
order-sensitive, so the plain list must use the **same name order** as
the task-table columns to reproduce the row's primer (assembling
`params` in a canonical column order is part of the `task-tables` caller
contract below).

`task_primer()` normalises **structure, not meaning**: it hashes
whatever `params` it is given. The canonical, name-keyed representation
is a caller contract assembled where `params` is built (`task-tables`,
over the `task-lists` tables). Per the three-step model the
RNG-consuming steps each take a primer over their task identity:

- **sample** – keyed `(dataset, sim, replace)` only. `nrow` is
  deliberately absent: every `nrow` shares one draw (sized by the
  scenario's `nrow_max` setting) that the `fit` step truncates inline
  (`head(sample, nrow)`, RNG-free, no separate primer), so excluding
  `nrow` is load-bearing for the sub-truncation property
  (`TARGETS-DESIGN.md` §5).

- **fit** – the parent `sample` identity plus `nrow` and the fit-grid
  row (`rescale`, `computable`, `at_boundary_ok`, `min_pmix` name,
  `range_shape1`, `range_shape2`). `nrow` IS part of the fit primer: a
  fit on a different truncation is a genuinely different computation.

- **hc** – the parent `fit` identity plus the hc-grid row (`nboot`,
  `est_method`, `ci_method`, `parametric`). `ci` is a scalar hc setting
  applied uniformly and read from the scenario, not part of the task
  identity, so it is never a primer field (nor a task-row column).

Function-valued parameters (e.g. `min_pmix`) MUST be referenced **by
name**, not by function value, so a recompile or JIT does not move a
task's primer.

The primer is derived from
[`rlang::hash()`](https://rlang.r-lib.org/reference/hash.html), a fast
within-session identity hash rather than a portable checksum, so its
digest is not *guaranteed* stable across `rlang` versions. Such changes
are rare and not anticipated, but have happened, and would shift primers
– and therefore simulation seeds. Reproducibility is therefore anchored
by pinning the execution environment (including the `rlang` version) for
a given simulation run.

## See also

[`local_dqrng_state()`](https://poissonconsulting.github.io/ssdsims/reference/local_dqrng_state.md),
[`local_dqrng_backend()`](https://poissonconsulting.github.io/ssdsims/reference/local_dqrng_backend.md).

## Examples

``` r
task_primer(list(dataset = "boron", sim = 1L, replace = FALSE))
#> [1] -563099096 -147061236
```
