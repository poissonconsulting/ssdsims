rinteger <- function(n = 1L) {
  if (n == 0) {
    integer(0)
  }
  mx <- 2147483647L
  as.integer(stats::runif(n, -mx, mx))
}

# inspired by withr::has_seed()
has_state <- function() {
  exists(".Random.seed", globalenv(), mode = "integer", inherits = FALSE)
}

# inspired by withr::get_seed()
get_state <- function() {
  if (!has_state()) {
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

# inspired by withr:::restore_rng_kind()
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

# `state` is either NULL (no-op, used to restore from a fresh R session
# where `.Random.seed` did not yet exist) or a list from `get_state()`.
set_state <- function(state) {
  if (is.null(state)) {
    return(invisible(NULL))
  }
  restore_rng_kind(state$rng_kind)
  assign(".Random.seed", state$random_seed, globalenv())
  invisible(get_state())
}

#' Local L'Ecuyer-CMRG Seed
#'
#' Seeds the L'Ecuyer-CMRG RNG with a scalar integer via [base::set.seed()].
#' For a `.Random.seed`-style state vector (e.g. from
#' `get_lecuyer_cmrg_stream_state()` or [`parallel::nextRNGStream()`]) use
#' [`local_lecuyer_cmrg_state()`].
#' @inheritParams withr::local_seed
#' @seealso [`withr::local_seed()`], [`local_lecuyer_cmrg_state()`],
#'   [`parallel::nextRNGStream()`].
#' @export
#' @examples
#'
#' local_lecuyer_cmrg_seed(42)
#' runif(3)
local_lecuyer_cmrg_seed <- function(seed, .local_envir = parent.frame()) {
  chk::chk_whole_number(seed)
  chk::chk_environment(.local_envir)
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
#' `get_lecuyer_cmrg_stream_state()` or [`parallel::nextRNGStream()`]) use
#' [`with_lecuyer_cmrg_state()`].
#' @inheritParams withr::with_seed
#' @seealso [`withr::with_seed()`], [`with_lecuyer_cmrg_state()`],
#'   [`parallel::nextRNGStream()`].
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
#' `get_lecuyer_cmrg_stream_state()`); contrast with [base::set.seed()]
#' which takes a scalar *seed* (see [`local_lecuyer_cmrg_seed()`]).
#' @param state `[integer(7)]`\cr A L'Ecuyer-CMRG `.Random.seed` vector.
#' @inheritParams withr::local_seed
#' @return Invisibly returns `state`.
#' @seealso [`parallel::nextRNGStream()`], [`local_lecuyer_cmrg_seed()`].
#' @export
#' @examples
#'
#' state <- with_lecuyer_cmrg_seed(42, parallel::nextRNGStream(.Random.seed))
#' local_lecuyer_cmrg_state(state)
#' runif(3)
local_lecuyer_cmrg_state <- function(state, .local_envir = parent.frame()) {
  chk::chk_integer(state)
  chk::chk_not_any_na(state)
  chk::chk_length(state, length = 7L)
  chk::chk_environment(.local_envir)
  old <- get_state()
  withr::defer(set_state(old), envir = .local_envir)
  set_state(list(
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
#' `get_lecuyer_cmrg_stream_state()`); contrast with [base::set.seed()]
#' which takes a scalar *seed* (see [`with_lecuyer_cmrg_seed()`]).
#' @param state `[integer(7)]`\cr A L'Ecuyer-CMRG `.Random.seed` vector.
#' @inheritParams withr::with_seed
#' @return The value of `code`.
#' @seealso [`parallel::nextRNGStream()`], [`with_lecuyer_cmrg_seed()`].
#' @export
#' @examples
#'
#' state <- with_lecuyer_cmrg_seed(42, parallel::nextRNGStream(.Random.seed))
#' with_lecuyer_cmrg_state(state, runif(3))
with_lecuyer_cmrg_state <- function(state, code) {
  force(state)
  local_lecuyer_cmrg_state(state)
  code
}

get_lecuyer_cmrg_state <- function() {
  RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
  set.seed(rinteger(1))
  globalenv()$.Random.seed
}

# inspired by furrr:::generate_seed_streams
get_lecuyer_cmrg_stream_states <- function(seed, nsim, stream, start_sim) {
  if (nsim == 0) {
    return(list())
  }

  ostate <- get_state()
  on.exit(set_state(ostate))

  if (!is.null(seed)) {
    set.seed(seed)
  }

  states <- vector("list", length = nsim)
  states[[1]] <- parallel::nextRNGStream(get_lecuyer_cmrg_state())
  for (i in seq_len(stream - 1)) {
    states[[1]] <- parallel::nextRNGStream(states[[1]])
  }
  for (i in seq_len(start_sim - 1)) {
    states[[1]] <- parallel::nextRNGSubStream(states[[1]])
  }
  for (i in seq_len(nsim - 1)) {
    states[[i + 1]] <- parallel::nextRNGSubStream(states[[i]])
  }
  states
}

get_lecuyer_cmrg_stream_state <- function(seed = NULL, stream, start_sim) {
  get_lecuyer_cmrg_stream_states(
    seed = seed,
    nsim = 1L,
    stream = stream,
    start_sim = start_sim
  )[[1]]
}
