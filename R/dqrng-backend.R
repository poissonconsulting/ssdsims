# dqrng backend wiring.
#
# Package load is intentionally inert: ssdsims does NOT register the dqrng
# RNG backend on `.onLoad()`. Instead, the backend is activated for the
# duration of a scenario run and reset on exit (see `run_scenario()`), so
# the caller's session RNG behaviour is left untouched outside of scenario
# execution. See `openspec/changes/dqrng-init/design.md`.

# Is dqrng usable without loading it? `TRUE` iff the caller has already loaded
# dqrng at the version `DESCRIPTION` suggests (>= 0.4.1).
#
# Deliberately tests *already-loaded*: the usual Suggests idiom --
# `requireNamespace("dqrng")` or a bare `dqrng::` touch -- would itself LOAD
# dqrng, which is the act this gate exists to avoid. Loading a package that
# registers a user-supplied RNG is a process-global, potentially destructive
# act: base R keeps a single `user_unif_rand` slot resolved across all loaded
# DLLs, so a second user-RNG package in the session can silently hijack
# `runif()` (see `openspec/changes/task-rng-postcheck/exploration/
# user-rng-conflict/`). ssdsims therefore uses dqrng only when the user has
# opted in by loading it; every `dqrng::` reference in package code is reached
# only behind this gate (directly, or transitively inside an already-activated
# backend scope).
dqrng_usable <- function() {
  isNamespaceLoaded("dqrng") && getNamespaceVersion("dqrng") >= "0.4.1"
}

# Assert dqrng is usable (already loaded at >= 0.4.1), aborting with actionable
# guidance otherwise. ssdsims never loads dqrng itself, and never silently
# falls back to base R's ambient RNG (that would quietly drop the per-task
# reproducibility the dqrng path exists for). `call` defaults to the calling
# frame so the error is reported in the context of the user-facing function
# (AGENTS.md error-call-origin rule).
chk_dqrng_usable <- function(call = rlang::caller_call()) {
  if (!dqrng_usable()) {
    chk::abort_chk(
      "The dqrng RNG backend requires the `dqrng` package (>= 0.4.1) to be ",
      "loaded. ssdsims never loads it implicitly (registering a user-supplied ",
      "RNG provider is a process-global side effect); run `library(dqrng)` ",
      "first.",
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
set_dqrng_backend <- function() {
  chk_dqrng_usable()
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

# Diagnostic: name the package whose `user_unif_rand` C symbol base R currently
# resolves -- the owner of the single process-global user-supplied RNG slot
# (the same all-DLL `R_FindSymbol` search `RNG_Init` uses). Gotcha: a `DLLInfo`
# overloads `$` to look up a *native symbol* by name, so the DLL's name must be
# read with `[["name"]]`, never `$name`. Validated in
# `openspec/changes/task-rng-postcheck/exploration/user-rng-conflict/
# case7-who-owns-rng.R`.
rng_slot_owner <- function() {
  si <- tryCatch(
    getNativeSymbolInfo("user_unif_rand"),
    error = function(e) NULL
  )
  if (is.null(si)) {
    return(NA_character_)
  }
  si$dll[["name"]]
}

# Diagnostic: every loaded DLL that exports `user_unif_rand` -- i.e. every
# loaded package that could take the single global user-supplied RNG slot
# (e.g. dqrng, randtoolbox). Listing them points at the offending co-load even
# when more than one is present.
user_rng_providers <- function() {
  out <- character(0)
  for (dll in getLoadedDLLs()) {
    ok <- tryCatch(
      {
        getNativeSymbolInfo("user_unif_rand", PACKAGE = dll)
        TRUE
      },
      error = function(e) FALSE
    )
    if (ok) {
      out <- c(out, dll[["name"]])
    }
  }
  out
}

# Assert dqrng is still the generator actually serving base R's draws, aborting
# otherwise. The per-task integrity witness (peer to `chk_dqrng_backend_active()`),
# run as the exit bookend of each `*_data_task_primer()` wrapper.
#
# Distinction from `dqrng_backend_active()`: the `RNGkind()` probe answers "is
# *a* user-supplied RNG active" (and stays the cheap, side-effect-free
# reentrancy gate for `local_dqrng_backend()`), but it is fooled by a *foreign*
# user-RNG package holding the `user_unif_rand` slot (see
# `openspec/changes/task-rng-postcheck/exploration/user-rng-conflict/`). This
# witness answers "is **dqrng** the active one", using dqrng's own state: a
# base-R `runif(1)` draw must advance dqrng's internal state iff dqrng is the
# bound generator. The recorded state is restored afterwards, so the check
# consumes no net randomness (the next draw is byte-identical with or without
# it).
chk_dqrng_backend_intact <- function(call = rlang::caller_call()) {
  s0 <- dqrng::dqrng_get_state()
  # A base-R draw routes through the `user_unif_rand` slot, advancing dqrng's
  # own state iff dqrng holds it (the witness).
  invisible(stats::runif(1L))
  s1 <- dqrng::dqrng_get_state()
  dqrng::dqrng_set_state(s0)
  if (!identical(s0, s1)) {
    return(invisible(NULL))
  }
  rng_kind <- RNGkind()[1L]
  if (!identical(rng_kind, "user-supplied")) {
    # Torn down: the backend was reset mid-task and base R is serving its own
    # generator. The symbol owner is not meaningful here (the symbol still
    # resolves to some loaded DLL that is not serving RNG), so do not name one.
    chk::abort_chk(
      "The dqrng backend is no longer intact: it was reset during the task ",
      "(base R RNG is now ",
      encodeString(rng_kind, quote = "\""),
      "), so the task's draws did not all come from dqrng and are not ",
      "reproducible.",
      call = call
    )
  }
  # Foreign hijack: a user-supplied RNG is active but the draw did not advance
  # dqrng, so another user-RNG package holds the `user_unif_rand` slot.
  chk::abort_chk(
    "The dqrng backend is no longer intact: a user-supplied RNG other than ",
    "dqrng is serving base R's draws (the `user_unif_rand` slot is owned by ",
    encodeString(rng_slot_owner(), quote = "\""),
    "; loaded user-RNG providers: ",
    chk::cc(user_rng_providers(), conj = " and "),
    "), so the task's draws are not reproducible. Do not load a second ",
    "user-RNG package while the dqrng backend is active.",
    call = call
  )
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
#' dqrng is a Suggested dependency that ssdsims never loads itself (loading a
#' user-supplied RNG provider is a process-global side effect): activating the
#' backend requires dqrng (>= 0.4.1) to be **already loaded** -- run
#' `library(dqrng)` first -- and aborts with that guidance otherwise rather
#' than silently falling back to base R's ambient RNG.
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
  call <- environment()
  # Reentrant: if a backend scope is already open, do nothing -- neither
  # re-activate nor schedule a reset -- so nesting leaves the RNG stream
  # untouched and only the outermost scope owns the backend lifetime.
  if (dqrng_backend_active()) {
    return(invisible(FALSE))
  }
  chk_dqrng_usable(call = call)
  set_dqrng_backend()
  withr::defer(reset_dqrng_backend(), envir = .local_envir)
  invisible(TRUE)
}
