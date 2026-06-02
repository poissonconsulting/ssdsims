# Local/With L'Ecuyer-CMRG State

`local_lecuyer_cmrg_state()` sets the L'Ecuyer-CMRG RNG state to a
`.Random.seed`-style integer vector (length 7) by assigning to
`.Random.seed` directly, restoring the previous state when
`.local_envir` exits. `with_lecuyer_cmrg_state()` evaluates `code` with
that state in effect, then restores the previous state. A *state* is the
full internal RNG state (as returned by
[`parallel::nextRNGStream()`](https://rdrr.io/r/parallel/RngStream.html)
or `get_lecuyer_cmrg_stream_state()`); contrast with
[`base::set.seed()`](https://rdrr.io/r/base/Random.html) which takes a
scalar *seed* (see
[`local_lecuyer_cmrg_seed()`](https://poissonconsulting.github.io/ssdsims/reference/local_lecuyer_cmrg_seed.md)
/
[`with_lecuyer_cmrg_seed()`](https://poissonconsulting.github.io/ssdsims/reference/local_lecuyer_cmrg_seed.md)).

## Usage

``` r
local_lecuyer_cmrg_state(state, .local_envir = parent.frame())

with_lecuyer_cmrg_state(state, code)
```

## Arguments

- state:

  `[integer(7)]`  
  A L'Ecuyer-CMRG `.Random.seed` vector.

- .local_envir:

  `[environment]`  
  The environment to use for scoping.

- code:

  `[any]`  
  Code to execute in the temporary environment

## Value

`local_lecuyer_cmrg_state()` invisibly returns `state`;
`with_lecuyer_cmrg_state()` returns the value of `code`.

## See also

[`parallel::nextRNGStream()`](https://rdrr.io/r/parallel/RngStream.html),
[`local_lecuyer_cmrg_seed()`](https://poissonconsulting.github.io/ssdsims/reference/local_lecuyer_cmrg_seed.md).

## Examples

``` r

state <- with_lecuyer_cmrg_seed(42, parallel::nextRNGStream(.Random.seed))
local_lecuyer_cmrg_state(state)
runif(3)
#> [1] 0.1738456 0.5547401 0.4833771

with_lecuyer_cmrg_state(state, runif(3))
#> [1] 0.8685000 0.1017511 0.4696414
```
