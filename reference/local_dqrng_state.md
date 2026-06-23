# Local/With dqrng State

`local_dqrng_state()` installs a per-task `(seed, primer)` starting
point as the running dqrng RNG state via
[`dqrng::dqset.seed()`](https://daqana.github.io/dqrng/reference/dqrng-functions.html),
restoring the previous state when `.local_envir` exits.
`with_dqrng_state()` evaluates `code` with that state installed, then
restores the previous state. The `primer` argument is the per-task
primer (the value handed to dqrng's `stream` argument, per
`TARGETS-DESIGN.md` §2 and the GLOSSARY); the `_state` suffix marks that
the wrapper installs that primer as the running RNG state.

## Usage

``` r
local_dqrng_state(seed, primer, .local_envir = parent.frame())

with_dqrng_state(seed, primer, code)
```

## Arguments

- seed:

  `[whole number]`  
  A scalar seed passed to
  [`dqrng::dqset.seed()`](https://daqana.github.io/dqrng/reference/dqrng-functions.html).

- primer:

  `[integer(2)]`  
  A length-2 integer primer passed as the `stream` argument of
  [`dqrng::dqset.seed()`](https://daqana.github.io/dqrng/reference/dqrng-functions.html).
  `NA_integer_` is permitted (the reserved INT_MIN encoding of
  `TARGETS-DESIGN.md` §2).

- .local_envir:

  `[environment]`  
  The environment to use for scoping.

- code:

  `[any]`  
  Code to execute in the temporary environment

## Value

`local_dqrng_state()` invisibly returns `primer`; `with_dqrng_state()`
returns the value of `code`.

## Details

They snapshot the RNG state on entry (via
[`dqrng::dqrng_get_state()`](https://daqana.github.io/dqrng/reference/dqrng-functions.html))
and [`withr::defer()`](https://withr.r-lib.org/reference/defer.html) a
restore (via
[`dqrng::dqrng_set_state()`](https://daqana.github.io/dqrng/reference/dqrng-functions.html)),
so a call leaves the surrounding RNG stream undisturbed, including on
error.

Both require an active dqrng backend: they abort unless a
[`local_dqrng_backend()`](https://poissonconsulting.github.io/ssdsims/reference/local_dqrng_backend.md)
scope is open. This fails fast rather than silently seeding base R's
Mersenne-Twister.

## See also

[`withr::local_seed()`](https://withr.r-lib.org/reference/with_seed.html),
[`local_dqrng_backend()`](https://poissonconsulting.github.io/ssdsims/reference/local_dqrng_backend.md).

## Examples

``` r

local_dqrng_backend()
local_dqrng_state(42, c(1L, 2L))
#> Error in local_dqrng_state(42, c(1L, 2L)): The dqrng backend is not intact: it was reset mid-task. Base R's RNG is now `Mersenne-Twister`, not dqrng's pcg64, so the task's draws did not come from dqrng.
runif(3)
#> [1] 0.8011529 0.1608404 0.8040301

with_dqrng_state(42, c(1L, 2L), runif(3))
#> Error in local_dqrng_state(seed, primer): The dqrng backend is not intact: it was reset mid-task. Base R's RNG is now `Mersenne-Twister`, not dqrng's pcg64, so the task's draws did not come from dqrng.
```
