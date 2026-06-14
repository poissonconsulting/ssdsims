## Case 8 -- Entry + exit integrity brackets in ONE wrapper, success-path-gated.
##
## Run with: Rscript case8-task-brackets.R
##
## Cases 5-7 established the witness (use dqrng's own state to prove dqrng --
## not merely *some* user-supplied RNG -- holds base R's user_unif_rand slot).
## This case shows how the parent change WIRES that witness: both the entry
## precondition and the exit postcondition live inside a single withr-style
## wrapper (`local_dqrng_state()`, R/dqrng-state.R), so the three
## `*_data_task_primer()` wrappers need no integrity code of their own.
##
## The exit check rides the `withr::defer()` that the wrapper already registers
## to restore RNG state -- but a deferred handler fires on EVERY frame exit,
## including error unwinding, which would let a witness abort MASK a failing
## task's own error. The fix is `base::returnValue()`: inside an exit handler it
## returns the frame's return value on a normal exit, or a sentinel `default` on
## an error / non-local exit. With a private `new.env()` sentinel (compared by
## reference, so no real return value can collide) the exit witness runs ONLY on
## the happy path.
##
## This reprex is self-contained: it models `local_dqrng_state()` with base
## `stop()` standing in for `chk::abort_chk()`, and tears the backend down via
## base `RNGkind("Mersenne-Twister")` (no second user-RNG package needed -- the
## foreign-hijack path is case6/case7). Demonstrated below:
##
##   A. healthy           -> entry + exit both pass, task returns its value
##   B. torn down, returns -> exit witness aborts the task (loud, not silent)
##   C. torn down, on entry-> entry witness aborts; the task refuses to start
##   D. torn down, errors  -> the task's OWN error propagates; witness skipped
##   E. reproducibility    -> draws identical with and without the brackets

suppressMessages(library(dqrng))
library(withr)

## The aborting witness -- the reprex stand-in for chk_dqrng_backend_intact().
## Non-destructive: it records dqrng's state, draws once through base runif()
## (which routes via the user_unif_rand slot), checks the state advanced, then
## rolls the witness draw back.
chk_dqrng_backend_intact <- function() {
  s0 <- dqrng::dqrng_get_state()
  invisible(runif(1))
  s1 <- dqrng::dqrng_get_state()
  dqrng::dqrng_set_state(s0) # roll back -> consumes no net randomness
  if (identical(s0, s1)) {
    stop(
      "dqrng backend not intact (current RNGkind: ",
      RNGkind()[1L],
      ")",
      call. = FALSE
    )
  }
  invisible(NULL)
}

## Model of local_dqrng_state(): both brackets in one function.
local_dqrng_state <- function(
  seed,
  primer = c(1L, 2L),
  .local_envir = parent.frame()
) {
  chk_dqrng_backend_intact() # (1) ENTRY precondition: refuse to start if corrupt
  old <- dqrng::dqrng_get_state()
  withr::defer(dqrng::dqrng_set_state(old), envir = .local_envir) # restore state
  sentinel <- new.env() # unique per call; a return value cannot equal it
  withr::defer(
    # (2) EXIT postcondition, success-path only: returnValue() == sentinel iff
    # the frame is unwinding from an error, in which case we skip the witness.
    if (!identical(returnValue(sentinel), sentinel)) chk_dqrng_backend_intact(),
    envir = .local_envir
  )
  dqrng::dqset.seed(seed, stream = primer)
  invisible(primer)
}

## A task is any body run under the per-task scope. The brackets attach to
## run_task()'s frame, so the exit witness fires as run_task() returns.
run_task <- function(body, seed = 42L) {
  local_dqrng_state(seed)
  body()
}

## Re-establish a healthy dqrng pcg64 backend before each scenario. We reset to
## base R's Mersenne-Twister first (via base RNGkind(), bypassing dqrng's single
## save-slot) so each scenario starts from a known state. Calling
## register_methods() twice without an intervening teardown corrupts that slot
## (a re-register saves "user-supplied" as the kind restore_methods() would then
## reinstate, so a later restore_methods() no longer tears the backend down) --
## exactly the reentrancy hazard the real `dqrng_backend_active()` gate guards
## against in R/dqrng-backend.R. Tearing down via base RNGkind() side-steps it.
reset_base <- function() suppressWarnings(RNGkind("Mersenne-Twister"))
activate <- function() {
  reset_base()
  dqrng::dqRNGkind("pcg64")
  dqrng::register_methods()
  invisible(NULL)
}
tear_down <- function() reset_base() # -> base Mersenne-Twister; dqrng unbound

report <- function(label, expr) {
  out <- tryCatch(
    {
      val <- expr
      paste0("returned ", format(val))
    },
    error = function(e) paste0("ABORT: ", conditionMessage(e))
  )
  cat(sprintf("%-34s %s\n", label, out))
}

## --- A. Healthy: entry + exit both pass, the task returns normally ---
activate()
report(
  "[A] healthy backend",
  run_task(function() {
    x <- runif(3) # genuine dqrng draws
    "task-result"
  })
)

## --- B. Torn down before a normal return: the EXIT witness aborts ---
activate()
report(
  "[B] torn down, returns normally",
  run_task(function() {
    tear_down() # backend gone, but the body still returns a value
    "task-result"
  })
)

## --- C. Torn down before the task starts: the ENTRY witness aborts ---
activate()
tear_down() # corrupt the backend up front
report("[C] torn down, at entry", run_task(function() "task-result"))

## --- D. Torn down AND the body errors: the task's OWN error wins ---
activate()
report(
  "[D] torn down, body errors",
  run_task(function() {
    tear_down()
    stop("boom: the real task failure", call. = FALSE)
  })
)
cat("    ^ reports the body's error, NOT the witness -- returnValue() gate\n")

## --- E. Reproducibility: the brackets consume no net randomness ---
## local_dqrng_state(42) seeds dqset.seed(42, c(1,2)) itself, so the bracketed
## task's draws must match a bare seed-then-draw with no brackets at all.
activate()
dqrng::dqset.seed(42L, stream = c(1L, 2L))
reference <- runif(3)

activate()
bracketed <- run_task(function() runif(3)) # entry witness + seed + body + exit witness

cat(sprintf(
  "[E] draws identical with/without brackets: %s\n",
  identical(reference, bracketed)
))
