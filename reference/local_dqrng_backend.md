# Local dqrng pcg64 Backend

Activates the dqrng `pcg64` RNG backend for the duration of the calling
frame, then resets it when `.local_envir` exits. While active, base R's
[`runif()`](https://rdrr.io/r/stats/Uniform.html),
[`rnorm()`](https://rdrr.io/r/stats/Normal.html),
[`rbinom()`](https://rdrr.io/r/stats/Binomial.html),
[`rexp()`](https://rdrr.io/r/stats/Exponential.html),
[`rgamma()`](https://rdrr.io/r/stats/GammaDist.html),
[`rpois()`](https://rdrr.io/r/stats/Poisson.html),
[`sample.int()`](https://rdrr.io/r/base/sample.html), and
[`sample()`](https://rdrr.io/r/base/sample.html) (and therefore
[`dplyr::slice_sample()`](https://dplyr.tidyverse.org/reference/slice.html)
and `ssdtools::ssd_r*()`) draw from dqrng's `pcg64`, seeded via
[`dqrng::dqset.seed()`](https://daqana.github.io/dqrng/reference/dqrng-functions.html).
`pcg64` is forced explicitly because it accepts the length-2 `stream`
argument the per-task primer design relies on; dqrng's own default
(`Xoroshiro128++`) does not.

## Usage

``` r
local_dqrng_backend(.local_envir = parent.frame())
```

## Arguments

- .local_envir:

  `[environment]`  
  The environment to use for scoping.

## Value

Invisibly returns `TRUE` if this call activated the backend (the
outermost scope) or `FALSE` if the backend was already active and the
call was a no-op.

## Details

Registering the backend is a process-global side effect that also
advances base R's `.Random.seed`. `local_dqrng_backend()` follows the
withr convention (compare
[`withr::local_seed()`](https://withr.r-lib.org/reference/with_seed.html)):
it pairs activation with deferred reset so the backend is always
restored, including on error.

The helper is reentrant.
[`dqrng::register_methods()`](https://daqana.github.io/dqrng/reference/user-supplied-rng.html)
/
[`dqrng::restore_methods()`](https://daqana.github.io/dqrng/reference/user-supplied-rng.html)
keep a single global save-slot, so a nested reset would tear the backend
down for the still-open outer scope. To avoid this, a
`local_dqrng_backend()` call made while the backend is already active is
a no-op: it does not re-activate the backend and schedules no further
reset. Only the outermost call activates the backend on entry and resets
it on exit, so the RNG stream is identical whether or not a nested call
occurs.

## See also

[`withr::local_seed()`](https://withr.r-lib.org/reference/with_seed.html),
[`dqrng::dqset.seed()`](https://daqana.github.io/dqrng/reference/dqrng-functions.html).

## Examples

``` r

local_dqrng_backend()
dqrng::dqset.seed(42, stream = c(1L, 2L))
runif(3)
#> [1] 0.4411014 0.1730826 0.1833968
```
