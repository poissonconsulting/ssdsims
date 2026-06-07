# dqrng-path scoped state installation.
#
# The dqrng analogue of `local_lecuyer_cmrg_state()` (see
# `R/lecuyer-cmrg-seed.R`): a `withr`-style wrapper that installs a per-task
# `(seed, primer)` starting point as the running dqrng RNG state and restores
# the prior state on frame exit. See
# `openspec/changes/local-dqrng-state/design.md`.

# Capture the active dqrng generator state. dqrng (>= 0.4.1, the version pinned
# in DESCRIPTION) exposes `dqrng_get_state()` (the generator kind plus its state
# words, as a character vector) and `dqrng_set_state()`; the pair round-trips a
# draw sequence byte-for-byte (verified against 0.4.1). Because
# `register_methods()` routes base R RNG through dqrng, this is the state that
# governs base R draws too.
get_dqrng_state <- function() {
  dqrng::dqrng_get_state()
}

# Reinstate a state captured by `get_dqrng_state()`.
set_dqrng_state <- function(state) {
  dqrng::dqrng_set_state(state)
  invisible(NULL)
}

#' Local/With dqrng State
#'
#' `local_dqrng_state()` installs a per-task `(seed, primer)` starting point as
#' the running dqrng RNG state via [dqrng::dqset.seed()], restoring the previous
#' state when `.local_envir` exits. `with_dqrng_state()` evaluates `code` with
#' that state installed, then restores the previous state. The `primer` argument
#' is the per-task primer (the value handed to dqrng's `stream` argument, per
#' `TARGETS-DESIGN.md` §2 and the GLOSSARY); the `_state` suffix marks that the
#' wrapper installs that primer as the running RNG state.
#'
#' These are the dqrng-path analogues of [local_lecuyer_cmrg_state()] /
#' [with_lecuyer_cmrg_state()]. Like those helpers they snapshot the RNG state
#' on entry (via `dqrng::dqrng_get_state()`) and `withr::defer()` a restore (via
#' `dqrng::dqrng_set_state()`), so a call leaves the surrounding RNG stream
#' undisturbed, including on error.
#'
#' Both require an active dqrng backend: they abort unless a
#' [local_dqrng_backend()] scope is open. This fails fast rather than silently
#' seeding base R's Mersenne-Twister.
#'
#' @param seed `[whole number]`\cr A scalar seed passed to
#'   [dqrng::dqset.seed()].
#' @param primer `[integer(2)]`\cr A length-2 integer primer passed as the
#'   `stream` argument of [dqrng::dqset.seed()]. `NA_integer_` is permitted (the
#'   reserved INT_MIN encoding of `TARGETS-DESIGN.md` §2).
#' @inheritParams withr::local_seed
#' @inheritParams withr::with_seed
#' @return `local_dqrng_state()` invisibly returns `primer`; `with_dqrng_state()`
#'   returns the value of `code`.
#' @seealso [withr::local_seed()], [local_dqrng_backend()],
#'   [local_lecuyer_cmrg_state()].
#' @export
#' @examples
#'
#' local_dqrng_backend()
#' local_dqrng_state(42, c(1L, 2L))
#' runif(3)
#'
#' with_dqrng_state(42, c(1L, 2L), runif(3))
local_dqrng_state <- function(seed, primer, .local_envir = parent.frame()) {
  chk::chk_whole_number(seed)
  chk::chk_integer(primer)
  chk::chk_length(primer, length = 2L)
  chk::chk_environment(.local_envir)
  # Entry precondition: refuse to start the task on a hijacked or torn-down
  # backend. Stronger than the old `chk_dqrng_backend_active()` probe -- it
  # verifies dqrng *specifically* holds base R's RNG slot. Non-destructive, and
  # runs before `dqset.seed()` (which overwrites the state), so it leaves the
  # seed untouched.
  chk_dqrng_backend_intact()
  old <- get_dqrng_state()
  withr::defer(set_dqrng_state(old), envir = .local_envir)
  # Exit postcondition: re-assert integrity when the task frame unwinds, so a
  # mid-task teardown or foreign-RNG hijack aborts the task loudly instead of
  # returning non-dqrng draws. The witness rides this `withr::defer()` (the same
  # mechanism that restores the state above), so the seed-and-run wrappers and
  # `with_dqrng_state()` need no integrity code of their own. It is gated to the
  # success path via `returnValue()` and a per-call unique sentinel: on an error
  # / non-local exit `returnValue()` yields the sentinel and the witness is
  # skipped, so a failing task body's own error is never masked by a witness
  # abort. See `exploration/user-rng-conflict/case8-task-brackets.R`.
  #
  # The exit witness fires on a normal *function* (closure) return -- which is
  # every real task path: the `*_data_task_primer()` wrappers and
  # `with_dqrng_state()` are closures. `returnValue()` does not report a normal
  # return for an `eval()`/`local()` frame, so the exit witness is skipped there
  # (the entry witness still runs, and the state-restore defer above still
  # fires); this only affects ad-hoc `local({...})` use, not task execution.
  sentinel <- new.env()
  withr::defer(
    if (!identical(returnValue(sentinel), sentinel)) {
      chk_dqrng_backend_intact()
    },
    envir = .local_envir
  )
  dqrng::dqset.seed(seed, stream = primer)
  invisible(primer)
}

#' @rdname local_dqrng_state
#' @export
with_dqrng_state <- function(seed, primer, code) {
  local_dqrng_state(seed, primer)
  code
}
