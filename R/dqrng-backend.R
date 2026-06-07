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

# Diagnostic: name the package that currently owns base R's `user_unif_rand`
# slot. `getNativeSymbolInfo()` runs the same all-DLL `R_FindSymbol` search R's
# `RNG_Init` uses, so the resolved DLL is the owner R would route `runif()`
# through. Read the DLL name with `[["name"]]` -- a `DLLInfo`'s `$` is
# overloaded to look up a *native symbol* of that name, so `$name` would try to
# resolve a symbol called `name` and error. Returns `NA_character_` if the
# symbol resolves nowhere (no user-supplied RNG provider is loaded). Lifted from
# `openspec/changes/task-rng-postcheck/exploration/user-rng-conflict/case7-who-owns-rng.R`.
rng_slot_owner <- function() {
  info <- tryCatch(
    getNativeSymbolInfo("user_unif_rand"),
    error = function(e) NULL
  )
  if (is.null(info)) NA_character_ else info$dll[["name"]]
}

# Diagnostic: every loaded package that exports `user_unif_rand`, i.e. that
# could take base R's single global user-supplied RNG slot (e.g. `dqrng`,
# `randtoolbox`). Lists the offending co-load even when more than one is present.
user_rng_providers <- function() {
  providers <- character(0)
  for (dll in getLoadedDLLs()) {
    has_symbol <- tryCatch(
      {
        getNativeSymbolInfo("user_unif_rand", PACKAGE = dll)
        TRUE
      },
      error = function(e) FALSE
    )
    if (isTRUE(has_symbol)) {
      providers <- c(providers, dll[["name"]])
    }
  }
  providers
}

# Assert that *dqrng specifically* holds base R's user-supplied RNG slot,
# aborting otherwise. The companion to `chk_dqrng_backend_active()`, but
# stronger: the cheap `RNGkind()` probe answers "is *a* user-supplied RNG
# active" and so is fooled by a foreign user-supplied RNG that has taken the
# single process-global `user_unif_rand` slot; this answers "is *dqrng* the
# active one". `dqrng_backend_active()` / `chk_dqrng_backend_active()` stay the
# cheap reentrancy gate for `local_dqrng_backend()`; this is the per-task
# integrity assertion.
#
# The witness is dqrng's own state: a base-R `runif(1)` draw routes through the
# `user_unif_rand` slot, so it advances dqrng's internal state iff dqrng is the
# bound generator. The draw is rolled back via `dqrng_set_state()`, so the check
# is non-destructive (consumes no net randomness). On failure it branches on
# `RNGkind()[1]`: a torn-down backend reports the current `RNGkind()` and names
# no owner (the symbol still resolves to a loaded DLL that is not serving RNG);
# a foreign hijack names `rng_slot_owner()` and lists `user_rng_providers()`.
#
# See `openspec/changes/task-rng-postcheck/exploration/user-rng-conflict/`
# (case5: the witness; case6: it catches a hijack the cheap probe misses; case7:
# naming the owner; case8: the per-task entry/exit brackets). `call` defaults to
# the calling frame so the error is reported in the context of the user-facing
# function (AGENTS.md error-call-origin rule).
chk_dqrng_backend_intact <- function(call = rlang::caller_call()) {
  state_before <- dqrng::dqrng_get_state()
  invisible(stats::runif(1L)) # base RNG -> whatever holds the user_unif_rand slot
  state_after <- dqrng::dqrng_get_state()
  dqrng::dqrng_set_state(state_before) # roll the witness draw back: non-destructive
  if (!identical(state_before, state_after)) {
    return(invisible(NULL)) # dqrng advanced -> dqrng is the bound generator
  }
  if (!identical(RNGkind()[1L], "user-supplied")) {
    chk::abort_chk(
      "The dqrng backend is not intact: it was reset mid-task. ",
      "Base R's RNG is now `",
      RNGkind()[1L],
      "`, not dqrng's pcg64, so the task's draws did not come from dqrng.",
      call = call
    )
  }
  providers <- user_rng_providers()
  providers_msg <- if (length(providers) > 0L) {
    paste0(
      " Loaded user-RNG providers: ",
      paste0("`", providers, "`", collapse = ", "),
      "."
    )
  } else {
    ""
  }
  chk::abort_chk(
    "The dqrng backend is not intact: a foreign user-supplied RNG holds base ",
    "R's RNG slot, so the task's draws did not come from dqrng. The ",
    "`user_unif_rand` slot is owned by `",
    rng_slot_owner(),
    "`.",
    providers_msg,
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
