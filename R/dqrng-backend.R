# dqrng backend wiring.
#
# Package load is intentionally inert: ssdsims does NOT register the dqrng
# RNG backend on `.onLoad()`. Instead, the backend is activated for the
# duration of a scenario run and reset on exit (see `run_scenario()`), so
# the caller's session RNG behaviour is left untouched outside of scenario
# execution. See `openspec/changes/dqrng-init/design.md`.
#
# dqrng is a *Suggested*, conditionally-used dependency: it is not in
# `Imports`, so loading ssdsims does not load dqrng. Loading a package that
# registers a user-supplied RNG is a process-global, potentially destructive
# act (it places a `user_unif_rand` provider into the session, which can
# collide with another such package -- see
# `openspec/changes/task-rng-postcheck/exploration/user-rng-conflict/`), so
# ssdsims uses dqrng only when the caller has already loaded it.

# Is dqrng already loaded at a usable version (>= 0.4.1)?
#
# This is the single gate every `dqrng::` touch in package code sits behind.
# It deliberately tests *already-loaded* -- `isNamespaceLoaded("dqrng")` -- and
# NOT `requireNamespace("dqrng")` (nor a bare `dqrng::` call), because those
# would themselves *load* dqrng, which is the act we must not trigger uninvited.
# `getNamespaceVersion()` only reads the loaded namespace's metadata (no load),
# and `numeric_version()` makes the comparison version-correct rather than
# lexicographic.
dqrng_usable <- function() {
  isNamespaceLoaded("dqrng") &&
    numeric_version(getNamespaceVersion("dqrng")) >= "0.4.1"
}

# Assert dqrng is usable (already loaded at >= 0.4.1), aborting otherwise with
# actionable guidance. ssdsims never loads dqrng itself and never falls back to
# base R's RNG (which would silently drop the per-task reproducibility the
# dqrng path exists to provide -- see the design's "abort, not fall back"
# decision). `call` defaults to the calling frame so the error is reported in
# the user-facing function (AGENTS.md error-call-origin rule).
chk_dqrng_usable <- function(call = rlang::caller_call()) {
  if (!dqrng_usable()) {
    chk::abort_chk(
      "The dqrng RNG backend requires the dqrng package (>= 0.4.1) to be ",
      "loaded. Run `library(dqrng)` first. ",
      "ssdsims does not load dqrng itself, to avoid imposing a ",
      "user-supplied RNG on your session.",
      call = call
    )
  }
  invisible(NULL)
}

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
#
# Aborts (via `chk_dqrng_usable()`) when dqrng is not already loaded at the
# required version: this is the one place the `dqrng::` activation calls run, so
# gating here keeps every backend touch behind the "already-loaded" guard.
set_dqrng_backend <- function(call = rlang::caller_call()) {
  chk_dqrng_usable(call = call)
  dqrng::dqRNGkind("pcg64")
  dqrng::register_methods()
  invisible(NULL)
}

# Reset the base R RNG backend, routing base R RNG functions back to their
# defaults (the inverse of `set_dqrng_backend()`'s `register_methods()`). Only
# ever deferred after a successful `set_dqrng_backend()`, so dqrng is loaded
# (usable) by the time this runs -- the `dqrng::` touch needs no further gate.
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
# in the context of the user-facing function (AGENTS.md error-call-origin rule).
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

# Assert dqrng *specifically* holds base R's user-supplied RNG slot, aborting
# otherwise. The dqrng-specific integrity witness (peer to
# `chk_dqrng_backend_active()`); see
# `openspec/changes/task-rng-postcheck/exploration/user-rng-conflict/`.
#
# Distinction from `dqrng_backend_active()`: base R has a single process-global
# user-supplied RNG slot, keyed on the C symbol `user_unif_rand` and resolved
# across *all* loaded DLLs to the most-recently-loaded match. So the cheap
# `RNGkind()` probe in `dqrng_backend_active()` answers only "is *a*
# user-supplied RNG active" -- a foreign user-RNG package (e.g. randtoolbox)
# loaded after dqrng hijacks the slot while `RNGkind()[1]` still reads
# "user-supplied" (case6). The probe stays the cheap, side-effect-free
# reentrancy gate for `local_dqrng_backend()` (re-asserting `register_methods()`
# reseeds, so that gate must stay cheap); this witness answers the stronger
# question "is **dqrng** the active one", using dqrng's own state as evidence
# (case5): a base-R draw routes through the slot and MUST advance dqrng's
# internal state iff dqrng is the bound generator. The recorded state is
# restored immediately via `dqrng_set_state()`, so the check is non-destructive
# (consumes no net randomness). All `dqrng::` touches here are safe: the witness
# only runs inside an already-activated backend scope, so dqrng is loaded.
chk_dqrng_backend_intact <- function(call = rlang::caller_call()) {
  s0 <- dqrng::dqrng_get_state()
  invisible(stats::runif(1L)) # routes through base R's user_unif_rand slot
  s1 <- dqrng::dqrng_get_state()
  dqrng::dqrng_set_state(s0) # roll the witness draw back: non-destructive
  if (!identical(s0, s1)) {
    # dqrng's state advanced -> dqrng is the bound generator -> intact.
    return(invisible(NULL))
  }

  # Not intact: the draw advanced something other than dqrng. Branch the
  # diagnosis on `RNGkind()[1]` -- the owner is only meaningful while a
  # user-supplied RNG is still active.
  if (!identical(RNGkind()[1L], "user-supplied")) {
    # Torn-down: the backend was reset mid-task. The `user_unif_rand` symbol
    # still resolves to some loaded DLL that is NOT serving RNG, so naming an
    # owner would mislead -- report the current RNGkind() instead.
    chk::abort_chk(
      "The dqrng RNG backend is no longer intact: it was reset mid-task. ",
      sprintf(
        "base R's RNG is now `%s`, not dqrng's pcg64, ",
        RNGkind()[1L]
      ),
      "so the draws this task produced did not come from dqrng.",
      call = call
    )
  }
  # Foreign hijack: a non-dqrng user-supplied RNG holds the slot while
  # `RNGkind()[1]` still reads "user-supplied". Name the culprit.
  owner <- rng_slot_owner()
  providers <- user_rng_providers()
  chk::abort_chk(
    "The dqrng RNG backend is no longer intact: a foreign user-supplied RNG ",
    sprintf("has taken base R's RNG slot (owned by `%s`). ", owner),
    sprintf(
      "Loaded user-supplied RNG providers: %s. ",
      paste(providers, collapse = ", ")
    ),
    "The draws this task produced did not come from dqrng.",
    call = call
  )
}

# Diagnostic helpers for `chk_dqrng_backend_intact()`'s abort message (lifted
# from `exploration/.../case7-who-owns-rng.R`). `getNativeSymbolInfo()` and
# `getLoadedDLLs()` are exported, documented base R functions (not the C-level
# non-API entry points the "checking compiled code" NOTE concerns).

# The package whose `user_unif_rand` symbol R currently resolves (the same
# all-DLL `R_FindSymbol` search base R's `RNG_Init` uses). GOTCHA: a `DLLInfo`
# overloads `$` to look up a *native symbol* by name, so read the DLL name with
# `[["name"]]`, never `$name` (which would try to resolve a symbol called
# `name` and error).
rng_slot_owner <- function() {
  si <- tryCatch(
    getNativeSymbolInfo("user_unif_rand"),
    error = function(e) NULL
  )
  if (is.null(si)) NA_character_ else si$dll[["name"]]
}

# Every loaded DLL that exports `user_unif_rand` -- i.e. every loaded package
# that could take the single global slot (e.g. dqrng, randtoolbox), which
# points at the offending co-load even when more than one is present.
user_rng_providers <- function() {
  providers <- character(0L)
  for (dll in getLoadedDLLs()) {
    ok <- tryCatch(
      {
        getNativeSymbolInfo("user_unif_rand", PACKAGE = dll)
        TRUE
      },
      error = function(e) FALSE
    )
    if (isTRUE(ok)) {
      providers <- c(providers, dll[["name"]])
    }
  }
  providers
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
#'
#' dqrng is a Suggested dependency that ssdsims never loads implicitly:
#' activation requires the caller to have already run `library(dqrng)` (`>=
#' 0.4.1`). When dqrng is not loaded, the call aborts with actionable guidance
#' rather than loading dqrng or falling back to base R's RNG.
#' @inheritParams withr::local_seed
#' @return Invisibly returns `TRUE` if this call activated the backend (the
#'   outermost scope) or `FALSE` if the backend was already active and the call
#'   was a no-op.
#' @seealso [withr::local_seed()], [dqrng::dqset.seed()].
#' @export
#' @examples
#'
#' library(dqrng)
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
  # Aborts (reported at this `local_dqrng_backend()` frame, via
  # `set_dqrng_backend()`'s default `call = rlang::caller_call()`) if dqrng is
  # not already loaded.
  set_dqrng_backend()
  withr::defer(reset_dqrng_backend(), envir = .local_envir)
  invisible(TRUE)
}
