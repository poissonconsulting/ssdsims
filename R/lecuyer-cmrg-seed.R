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

# internal function from withr
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

# internal function from withr
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

set_seed <- function(seed) {
  restore_rng_kind(seed$rng_kind)
  if (is.null(seed$seed)) {
    assign(".Random.seed", seed$random_seed, globalenv())
  } else {
    set.seed(seed$seed)
  }
  invisible(get_seed())
}

#' Local L'Ecuyer-CMRG Seed
#'
#' Seeds the L'Ecuyer-CMRG RNG with a scalar integer via [base::set.seed()].
#' For a `.Random.seed`-style state vector (e.g. from
#' `get_lecuyer_cmrg_seed_stream()` or [`parallel::nextRNGStream()`]) use
#' [`local_lecuyer_cmrg_state()`]. See [`ssdsims-glossary`] for the
#' distinction between a *seed* and a *state*.
#' @inheritParams withr::local_seed
#' @seealso [`withr::local_seed()`], [`local_lecuyer_cmrg_state()`],
#'   [`parallel::nextRNGStream()`], [`ssdsims-glossary`].
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

#' With L'Ecuyer-CMRG Seed
#'
#' Evaluates `code` with the L'Ecuyer-CMRG RNG seeded with a scalar integer
#' via [base::set.seed()], then restores the previous state. For a
#' `.Random.seed`-style state vector (e.g. from
#' `get_lecuyer_cmrg_seed_stream()` or [`parallel::nextRNGStream()`]) use
#' [`with_lecuyer_cmrg_state()`]. See [`ssdsims-glossary`] for the
#' distinction between a *seed* and a *state*.
#' @inheritParams withr::with_seed
#' @seealso [`withr::with_seed()`], [`with_lecuyer_cmrg_state()`],
#'   [`parallel::nextRNGStream()`], [`ssdsims-glossary`].
#' @export
#' @examples
#'
#' with_lecuyer_cmrg_seed(42, {
#'   runif(3)
#' })
with_lecuyer_cmrg_seed <- function(seed, code) {
  local_lecuyer_cmrg_seed(seed)
  code
}

#' Local L'Ecuyer-CMRG State
#'
#' Sets the L'Ecuyer-CMRG RNG state to a `.Random.seed`-style integer
#' vector (length 7) by assigning to `.Random.seed` directly, restoring the
#' previous state when `.local_envir` exits. A *state* is the full internal
#' RNG state (as returned by [`parallel::nextRNGStream()`] or
#' `get_lecuyer_cmrg_seed_stream()`); contrast with [base::set.seed()]
#' which takes a scalar *seed* (see [`local_lecuyer_cmrg_seed()`]). See
#' [`ssdsims-glossary`] for the distinction.
#' @param state `[integer(7)]`\cr A L'Ecuyer-CMRG `.Random.seed` vector.
#' @inheritParams withr::local_seed
#' @return Invisibly returns `state`.
#' @seealso [`parallel::nextRNGStream()`], [`local_lecuyer_cmrg_seed()`],
#'   [`ssdsims-glossary`].
#' @export
#' @examples
#'
#' state <- with_lecuyer_cmrg_seed(
#'   42,
#'   get_lecuyer_cmrg_seed_stream(stream = 1L, start_sim = 1L)
#' )
#' local_lecuyer_cmrg_state(state)
#' runif(3)
local_lecuyer_cmrg_state <- function(state, .local_envir = parent.frame()) {
  old <- get_seed()
  withr::defer(
    if (!is.null(old)) set_seed(old),
    envir = .local_envir
  )
  set_seed(list(
    random_seed = state,
    rng_kind = c("L'Ecuyer-CMRG", "Inversion", "Rejection")
  ))
  invisible(state)
}

#' With L'Ecuyer-CMRG State
#'
#' Evaluates `code` with the L'Ecuyer-CMRG RNG state temporarily set to
#' `state` (a `.Random.seed`-style integer vector of length 7), then
#' restores the previous state. A *state* is the full internal RNG state
#' (as returned by [`parallel::nextRNGStream()`] or
#' `get_lecuyer_cmrg_seed_stream()`); contrast with [base::set.seed()]
#' which takes a scalar *seed* (see [`with_lecuyer_cmrg_seed()`]). See
#' [`ssdsims-glossary`] for the distinction.
#' @param state `[integer(7)]`\cr A L'Ecuyer-CMRG `.Random.seed` vector.
#' @inheritParams withr::with_seed
#' @return The value of `code`.
#' @seealso [`parallel::nextRNGStream()`], [`with_lecuyer_cmrg_seed()`],
#'   [`ssdsims-glossary`].
#' @export
#' @examples
#'
#' state <- with_lecuyer_cmrg_seed(
#'   42,
#'   get_lecuyer_cmrg_seed_stream(stream = 1L, start_sim = 1L)
#' )
#' with_lecuyer_cmrg_state(state, runif(3))
with_lecuyer_cmrg_state <- function(state, code) {
  force(state)
  local_lecuyer_cmrg_state(state)
  code
}

get_lecuyer_cmrg_seed <- function() {
  RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
  set.seed(rinteger(1))
  globalenv()$.Random.seed
}

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

get_lecuyer_cmrg_seed_stream <- function(seed = NULL, stream, start_sim) {
  get_lecuyer_cmrg_seeds_stream(
    seed = seed,
    nsim = 1L,
    stream = stream,
    start_sim = start_sim
  )[[1]]
}
