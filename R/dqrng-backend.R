# dqrng backend wiring.
#
# Package load is intentionally inert: ssdsims does NOT register the dqrng
# RNG backend on `.onLoad()`. Instead, the backend is activated for the
# duration of a scenario run and reset on exit (see `run_scenario()`), so
# the caller's session RNG behaviour is left untouched outside of scenario
# execution. See `openspec/changes/dqrng-init/design.md`.

# Activate the dqrng pcg64 backend for the current process.
#
# Sets `dqRNGkind("pcg64")` -- overriding dqrng's own default generator
# (`Xoroshiro128++`), which cannot accept the length-2 `stream` argument the
# per-task primer design relies on -- and registers dqrng's methods so that
# base R's `runif()`, `rnorm()`, `rbinom()`, `rexp()`, `rgamma()`, `rpois()`,
# `sample.int()`, and `sample()` (and therefore `dplyr::slice_sample()` and
# `ssdtools::ssd_r*()`) draw from dqrng's pcg64.
#
# This is a process-global side effect that also advances base R's
# `.Random.seed`. Prefer `local_dqrng_backend()`, which guarantees the backend
# is reset on scope exit; call `set_dqrng_backend()` directly only when paired
# with `on.exit(reset_dqrng_backend())`.
set_dqrng_backend <- function() {
  dqrng::dqRNGkind("pcg64")
  dqrng::register_methods()
  invisible(NULL)
}

# Reset the base R RNG backend, routing base R RNG functions back to their
# defaults (the inverse of `set_dqrng_backend()`'s `register_methods()`).
reset_dqrng_backend <- function() {
  dqrng::restore_methods()
  invisible(NULL)
}

# Is base R's RNG currently routed through a user-supplied backend (dqrng)?
#
# `dqrng::register_methods()` installs base R's RNG functions as
# "user-supplied" (it calls `RNGkind("user", "user")`), so
# `RNGkind()[1] == "user-supplied"` is the observable signal that the backend
# is in effect. dqrng (0.4.1) exposes no getter for the active generator
# (`dqRNGkind()` is a setter only), so this stateless probe is what
# `local_dqrng_backend()` uses to detect an already-active backend.
#
# Why a probe and not ssdsims-private state: re-asserting `RNGkind("user",
# "user")` *reseeds* the dqrng generator (verified against dqrng 0.4.1), so the
# invariant we must protect is "do not re-register while base R RNG is already
# served by a user-supplied generator" -- which is exactly what this probe
# detects, whether the active backend was installed by ssdsims, by an enclosing
# scope, or externally by the caller. Tracking only ssdsims' own activation
# would miss the externally-active case and reseed (corrupt) a live stream.
#
# Caveat (accepted): a *foreign* non-dqrng user-supplied RNG would also satisfy
# this probe, so `local_dqrng_backend()` would conservatively no-op rather than
# activate dqrng. In practice dqrng is the only user-supplied backend in this
# stack, and no-op (preserve the active stream) is the safer failure mode than
# re-registering (reseed) it.
dqrng_backend_active <- function() {
  identical(RNGkind()[1L], "user-supplied")
}

# Assert the dqrng backend is active, aborting otherwise. A `chk`-style guard
# (returns invisibly on success) for dqrng-path helpers such as
# `local_dqrng_state()` that are only meaningful while a `local_dqrng_backend()`
# scope is open. `call` defaults to the calling frame so the error is reported
# in the context of the user-facing function (CLAUDE.md error-call-origin rule).
chk_dqrng_backend_active <- function(call = rlang::caller_call()) {
  if (!dqrng_backend_active()) {
    chk::abort_chk(
      "The dqrng backend is not active. ",
      "Open a `local_dqrng_backend()` scope first.",
      call = call
    )
  }
  invisible(NULL)
}

#' Local dqrng pcg64 Backend
#'
#' Activates the dqrng `pcg64` RNG backend for the duration of the calling
#' frame, then resets it when `.local_envir` exits. While active, base R's
#' `runif()`, `rnorm()`, `rbinom()`, `rexp()`, `rgamma()`, `rpois()`,
#' `sample.int()`, and `sample()` (and therefore [dplyr::slice_sample()] and
#' `ssdtools::ssd_r*()`) draw from dqrng's `pcg64`, seeded via
#' [dqrng::dqset.seed()]. `pcg64` is forced explicitly because it accepts the
#' length-2 `stream` argument the per-task primer design relies on; dqrng's own
#' default (`Xoroshiro128++`) does not.
#'
#' Registering the backend is a process-global side effect that also advances
#' base R's `.Random.seed`. `local_dqrng_backend()` follows the withr
#' convention (compare [withr::local_seed()]): it pairs activation with
#' deferred reset so the backend is always restored, including on error.
#'
#' The helper is reentrant. `dqrng::register_methods()` /
#' `dqrng::restore_methods()` keep a single global save-slot, so a nested
#' reset would tear the backend down for the still-open outer scope. To avoid
#' this, a `local_dqrng_backend()` call made while the backend is already
#' active is a no-op: it does not re-activate the backend and schedules no
#' further reset. Only the outermost call activates the backend on entry and
#' resets it on exit, so the RNG stream is identical whether or not a nested
#' call occurs.
#' @inheritParams withr::local_seed
#' @return Invisibly returns `TRUE` if this call activated the backend (the
#'   outermost scope) or `FALSE` if the backend was already active and the call
#'   was a no-op.
#' @seealso [withr::local_seed()], [dqrng::dqset.seed()].
#' @export
#' @examples
#'
#' local_dqrng_backend()
#' dqrng::dqset.seed(42, stream = c(1L, 2L))
#' runif(3)
local_dqrng_backend <- function(.local_envir = parent.frame()) {
  chk::chk_environment(.local_envir)
  # Reentrant: if a backend scope is already open, do nothing -- neither
  # re-activate nor schedule a reset -- so nesting leaves the RNG stream
  # untouched and only the outermost scope owns the backend lifetime.
  if (dqrng_backend_active()) {
    return(invisible(FALSE))
  }
  set_dqrng_backend()
  withr::defer(reset_dqrng_backend(), envir = .local_envir)
  invisible(TRUE)
}
