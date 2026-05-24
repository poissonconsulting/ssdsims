#' Generate Random Integers
#'
#' Generates `n` random integers drawn uniformly from
#' `[-.Machine$integer.max, .Machine$integer.max]`.
#'
#' The values are produced with [stats::runif()] using `-.Machine$integer.max`
#' (i.e. `-(2^31 - 1) = -2147483647`) and `.Machine$integer.max` (`2^31 - 1 =
#' 2147483647`) as bounds and then coerced to integer. This is used internally
#' to seed L'Ecuyer-CMRG streams when the caller has not supplied an explicit
#' seed.
#'
#' The symmetric range deliberately excludes `-2^31 = -2147483648`: R reserves
#' that bit pattern for `NA_integer_`, so any attempt to coerce it from a
#' double via [as.integer()] returns `NA` with a warning rather than a valid
#' integer. Using `-.Machine$integer.max` as the lower bound therefore keeps
#' every draw inside R's representable integer range. This costs at most one
#' value of entropy out of `~2^32`, which is irrelevant given the function's
#' sole use - producing a single scalar to pass to [set.seed()] when
#' bootstrapping a fresh L'Ecuyer-CMRG state. [set.seed()] would in turn
#' expand any such scalar into the six-integer L'Ecuyer-CMRG seed vector, so
#' the missing value has no observable effect on the diversity of generated
#' streams.
#'
#' @param n A count of the number of integers to generate.
#' @return An integer vector of length `n`.
#' @noRd
rinteger <- function(n = 1L) {
  if (n == 0) {
    integer(0)
  }
  mx <- 2147483647L
  as.integer(stats::runif(n, -mx, mx))
}

# internal function from withr
has_seed <- function() {
  exists(".Random.seed", globalenv(), mode = "integer", inherits = FALSE)
}

#' Capture the Current RNG State
#'
#' Returns the current random number generator state in a form that can later
#' be restored with `set_seed()`.
#'
#' Re-implementation of the (unexported) `withr:::get_seed()` helper. The
#' returned list contains the integer `.Random.seed` vector together with the
#' result of [RNGkind()] so the RNG kind, normal kind and sample kind can be
#' restored. Returns `NULL` if no random numbers have yet been generated in
#' the session.
#'
#' @return A list with components `random_seed` and `rng_kind`, or `NULL`.
#' @noRd
get_seed <- function() {
  if (!has_seed()) {
    return(NULL)
  }
  list(
    random_seed = get(
      ".Random.seed",
      globalenv(),
      mode = "integer",
      inherits = FALSE
    ),
    rng_kind = RNGkind()
  )
}

#' Restore the RNG Kind
#'
#' Restores the RNG kind, normal kind and sample kind from a value previously
#' produced by [RNGkind()].
#'
#' Re-implementation of the (unexported) `withr:::restore_rng_kind()` helper.
#' The `"Rounding"` sample kind is restored under [suppressWarnings()] because
#' R emits a non-actionable warning when it is selected.
#'
#' @param kind A character vector of length 3 as returned by [RNGkind()].
#' @return `NULL`, invisibly.
#' @noRd
restore_rng_kind <- function(kind) {
  RNGkind <- get("RNGkind")
  RNGkind(kind[[1]], normal.kind = kind[[2]])
  sample_kind <- kind[[3]]
  if (identical(sample_kind, "Rounding")) {
    suppressWarnings(RNGkind(sample.kind = sample_kind))
  } else {
    RNGkind(sample.kind = sample_kind)
  }
  NULL
}

#' Restore a Captured RNG State
#'
#' Restores the random number generator state captured by `get_seed()`.
#'
#' First restores the RNG kind via `restore_rng_kind()`, then either assigns
#' `.Random.seed` directly in the global environment or, if a scalar `seed`
#' was supplied, calls [set.seed()] with it. Returns the new RNG state
#' invisibly so callers can chain capture-and-restore operations.
#'
#' @param seed A list as produced by `get_seed()`, optionally with an extra
#'   `seed` component to be passed to [set.seed()].
#' @return The new RNG state, invisibly.
#' @noRd
set_seed <- function(seed) {
  restore_rng_kind(seed$rng_kind)
  if (is.null(seed$seed)) {
    assign(".Random.seed", seed$random_seed, globalenv())
  } else {
    set.seed(seed$seed)
  }
  invisible(get_seed())
}

#' Local L'Euyer-CMRG Seed
#'
#' Sets the random number generator state in the calling environment to use
#' the L'Ecuyer-CMRG algorithm with the supplied seed.
#'
#' This is a thin wrapper around [withr::local_seed()] that pins the RNG kind
#' to `"L'Ecuyer-CMRG"`, the normal kind to `"Inversion"` and the sample kind
#' to `"Rejection"`. The previous RNG state is restored when the calling
#' frame exits, making this convenient for tests and reproducible workflows
#' that need a parallel-safe RNG without polluting the global state.
#'
#' @inheritParams withr::local_seed
#' @seealso [`withr::local_seed()`]
#' @export
#' @examples
#'
#' local_lecuyer_cmrg_seed(42)
#' runif(3)
local_lecuyer_cmrg_seed <- function(seed, .local_envir = parent.frame()) {
  withr::local_seed(
    seed,
    .local_envir = .local_envir,
    .rng_kind = "L'Ecuyer-CMRG",
    .rng_normal_kind = "Inversion",
    .rng_sample_kind = "Rejection"
  )
}

#' With L'Euyer-CMRG Seed
#'
#' Evaluates an R expression with the random number generator temporarily set
#' to the L'Ecuyer-CMRG algorithm and the supplied seed.
#'
#' This is a thin wrapper around [withr::with_seed()] that pins the RNG kind
#' to `"L'Ecuyer-CMRG"`, the normal kind to `"Inversion"` and the sample kind
#' to `"Rejection"` while `code` is being evaluated. The previous RNG state
#' is restored on exit, which makes it suitable for generating reproducible
#' parallel-safe random numbers inside a larger pipeline.
#'
#' @inheritParams withr::with_seed
#' @seealso [`withr::with_seed()`]
#' @export
#' @examples
#'
#' with_lecuyer_cmrg_seed(42, {
#' runif(3)
#' })
with_lecuyer_cmrg_seed <- function(seed, code) {
  force(seed)
  withr::with_seed(
    seed,
    code,
    .rng_kind = "L'Ecuyer-CMRG",
    .rng_normal_kind = "Inversion",
    .rng_sample_kind = "Rejection"
  )
}

#' Get a Fresh L'Ecuyer-CMRG Seed
#'
#' Switches the RNG to L'Ecuyer-CMRG, seeds it with a random integer and
#' returns the resulting `.Random.seed` vector.
#'
#' This is used internally as the starting point for generating independent
#' parallel streams via [parallel::nextRNGStream()] and
#' [parallel::nextRNGSubStream()]. Note that this function mutates the global
#' RNG state; callers that need to preserve the existing state should
#' surround the call with `get_seed()` / `set_seed()`.
#'
#' @return An integer vector representing a L'Ecuyer-CMRG `.Random.seed`.
#' @noRd
get_lecuyer_cmrg_seed <- function() {
  RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
  set.seed(rinteger(1))
  globalenv()$.Random.seed
}

#' Generate a Sequence of L'Ecuyer-CMRG Seeds for a Stream
#'
#' Builds a list of `nsim` L'Ecuyer-CMRG seeds that are statistically
#' independent across both `stream` and simulation index.
#'
#' Starting from a base L'Ecuyer-CMRG seed, the helper advances the stream by
#' `stream - 1` full RNG streams using [parallel::nextRNGStream()] and then
#' the simulation index by `start_sim - 1` sub-streams using
#' [parallel::nextRNGSubStream()]. The subsequent `nsim - 1` seeds are then
#' obtained by repeated sub-stream advancement. The existing global RNG state
#' is captured before the call and restored on exit so the function has no
#' visible side effects.
#'
#' Inspired by `furrr:::generate_seed_streams`.
#'
#' @inheritParams params
#' @return A list of `nsim` integer vectors, each a valid L'Ecuyer-CMRG seed.
#' @noRd
# inspired by furrr:::generate_seed_streams
get_lecuyer_cmrg_seeds_stream <- function(seed, nsim, stream, start_sim) {
  if (nsim == 0) {
    return(list())
  }

  oseed <- get_seed()
  on.exit(set_seed(oseed))

  if (!is.null(seed)) {
    set.seed(seed)
  }

  seeds <- vector("list", length = nsim)
  seeds[[1]] <- parallel::nextRNGStream(get_lecuyer_cmrg_seed())
  for (i in seq_len(stream - 1)) {
    seeds[[1]] <- parallel::nextRNGStream(seeds[[1]])
  }
  for (i in seq_len(start_sim - 1)) {
    seeds[[1]] <- parallel::nextRNGSubStream(seeds[[1]])
  }
  for (i in seq_len(nsim - 1)) {
    seeds[[i + 1]] <- parallel::nextRNGSubStream(seeds[[i]])
  }
  seeds
}

#' Get a Single L'Ecuyer-CMRG Seed for a Stream
#'
#' Convenience wrapper around `get_lecuyer_cmrg_seeds_stream()` that returns
#' the seed for a single `(stream, start_sim)` coordinate.
#'
#' @inheritParams params
#' @return An integer vector that is a valid L'Ecuyer-CMRG seed.
#' @noRd
get_lecuyer_cmrg_seed_stream <- function(seed = NULL, stream, start_sim) {
  get_lecuyer_cmrg_seeds_stream(
    seed = seed,
    nsim = 1L,
    stream = stream,
    start_sim = start_sim
  )[[1]]
}
