# Local L'Ecuyer-CMRG State

Sets the L'Ecuyer-CMRG RNG state to a `.Random.seed`-style integer
vector (length 7) by assigning to `.Random.seed` directly, restoring the
previous state when `.local_envir` exits. A *state* is the full internal
RNG state (as returned by
[`parallel::nextRNGStream()`](https://rdrr.io/r/parallel/RngStream.html)
or `get_lecuyer_cmrg_stream_state()`); contrast with
[`base::set.seed()`](https://rdrr.io/r/base/Random.html) which takes a
scalar *seed* (see
[`local_lecuyer_cmrg_seed()`](https://poissonconsulting.github.io/ssdsims/reference/local_lecuyer_cmrg_seed.md)).

## Usage

``` r
local_lecuyer_cmrg_state(state, .local_envir = parent.frame())
```

## Arguments

- state:

  `[integer(7)]`  
  A L'Ecuyer-CMRG `.Random.seed` vector.

- .local_envir:

  `[environment]`  
  The environment to use for scoping.

## Value

Invisibly returns `state`.

## See also

[`parallel::nextRNGStream()`](https://rdrr.io/r/parallel/RngStream.html),
[`local_lecuyer_cmrg_seed()`](https://poissonconsulting.github.io/ssdsims/reference/local_lecuyer_cmrg_seed.md).

## Examples

``` r

state <- with_lecuyer_cmrg_seed(42, parallel::nextRNGStream(.Random.seed))
local_lecuyer_cmrg_state(state)
runif(3)
#> [1] 0.1738456 0.5547401 0.4833771
```
