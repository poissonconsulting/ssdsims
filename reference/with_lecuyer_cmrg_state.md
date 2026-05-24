# With L'Ecuyer-CMRG State

Evaluates `code` with the L'Ecuyer-CMRG RNG state temporarily set to
`state` (a `.Random.seed`-style integer vector of length 7), then
restores the previous state. A *state* is the full internal RNG state
(as returned by
[`parallel::nextRNGStream()`](https://rdrr.io/r/parallel/RngStream.html)
or `get_lecuyer_cmrg_stream_state()`); contrast with
[`base::set.seed()`](https://rdrr.io/r/base/Random.html) which takes a
scalar *seed* (see
[`with_lecuyer_cmrg_seed()`](https://poissonconsulting.github.io/ssdsims/reference/with_lecuyer_cmrg_seed.md)).

## Usage

``` r
with_lecuyer_cmrg_state(state, code)
```

## Arguments

- state:

  `[integer(7)]`  
  A L'Ecuyer-CMRG `.Random.seed` vector.

- code:

  `[any]`  
  Code to execute in the temporary environment

## Value

The value of `code`.

## See also

[`parallel::nextRNGStream()`](https://rdrr.io/r/parallel/RngStream.html),
[`with_lecuyer_cmrg_seed()`](https://poissonconsulting.github.io/ssdsims/reference/with_lecuyer_cmrg_seed.md).

## Examples

``` r

state <- with_lecuyer_cmrg_seed(42, parallel::nextRNGStream(.Random.seed))
with_lecuyer_cmrg_state(state, runif(3))
#> [1] 0.8685000 0.1017511 0.4696414
```
