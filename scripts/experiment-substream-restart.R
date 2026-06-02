## scripts/experiment-substream-restart.R
##
## Experiment: does L'Ecuyer-CMRG sub-stream enumeration restart
## correctly from a persisted state?
##
## Design assumption under test (TARGETS-DESIGN.md §5):
##   A child scenario stores its parent's last sub-stream state and
##   advances from there with parallel::nextRNGSubStream() to get the
##   first sub-stream of the child. The states it derives must equal
##   what a single enumeration from the master would have produced.
##
## Two facts to confirm:
##   (a) Round-trip:    state -> saveRDS -> readRDS -> nextSubStream
##                      reproduces the next state byte-for-byte.
##   (b) Concatenation: states 1..N from one call equal
##                      states 1..k from call 1 concatenated with
##                      states k+1..N derived by chaining nextSubStream
##                      from the persisted state[k].
##   (c) Use-doesn't-mutate: invoking the RNG inside
##                          with_lecuyer_cmrg_state(state, {...})
##                          leaves the persisted `state` integer
##                          vector untouched, so the same state can
##                          be advanced after use.
##
## Run from package root:
##   Rscript scripts/experiment-substream-restart.R

stopifnot(getRversion() >= "4.0.0")

# Use a clean baseline. We avoid the package helpers on purpose so the
# experiment depends only on `parallel`.
old_kind <- RNGkind()
on.exit(RNGkind(old_kind[1], old_kind[2], old_kind[3]), add = TRUE)

RNGkind("L'Ecuyer-CMRG")
set.seed(42L)
master <- .Random.seed
stopifnot(length(master) == 7L, is.integer(master))

# ---- Enumerate states 1..N in one shot -------------------------------
N <- 10L
states_oneshot <- vector("list", N)
states_oneshot[[1]] <- parallel::nextRNGSubStream(master)
for (i in seq.int(2, N)) {
  states_oneshot[[i]] <- parallel::nextRNGSubStream(states_oneshot[[i - 1]])
}

# ---- Persist state[k] and restart from it ----------------------------
k <- 5L
tmp <- tempfile(fileext = ".rds")
saveRDS(states_oneshot[[k]], tmp)
restored <- readRDS(tmp)

stopifnot(identical(restored, states_oneshot[[k]])) # (a) round-trip

states_restart <- vector("list", N - k)
states_restart[[1]] <- parallel::nextRNGSubStream(restored)
for (i in seq.int(2, N - k)) {
  states_restart[[i]] <- parallel::nextRNGSubStream(states_restart[[i - 1]])
}

# (b) concatenation: states_restart[[j]] must equal states_oneshot[[k + j]]
ok_concat <- all(vapply(
  seq.int(1, N - k),
  \(j) identical(states_restart[[j]], states_oneshot[[k + j]]),
  logical(1)
))

# ---- Use-doesn't-mutate ----------------------------------------------
# Save a copy, enter the state, draw some uniforms, exit, then advance
# from the *saved* state. It must match nextSubStream applied to the
# pristine state.
saved <- states_oneshot[[k]]
saved_copy <- saved # integer vector, copy-on-write

old_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) {
  get(".Random.seed", envir = .GlobalEnv)
} else {
  NULL
}
assign(".Random.seed", saved, envir = .GlobalEnv)
draws_in_state <- runif(100L)
# pop back to whatever was there
if (is.null(old_seed)) {
  rm(".Random.seed", envir = .GlobalEnv)
} else {
  assign(".Random.seed", old_seed, envir = .GlobalEnv)
}

ok_no_mutate <- identical(saved, saved_copy) # (c) part 1
ok_advance <- identical(
  # (c) part 2
  parallel::nextRNGSubStream(saved),
  states_oneshot[[k + 1L]]
)

# ---- Reproducibility across an independent draw sequence -------------
# Draw uniforms from states_oneshot[[k+1]] and from the restarted
# states_restart[[1]]; they must agree element-wise.
draw_from <- function(state, n) {
  old <- if (exists(".Random.seed", envir = .GlobalEnv)) {
    get(".Random.seed", envir = .GlobalEnv)
  } else {
    NULL
  }
  assign(".Random.seed", state, envir = .GlobalEnv)
  on.exit(
    {
      if (is.null(old)) {
        rm(".Random.seed", envir = .GlobalEnv)
      } else {
        assign(".Random.seed", old, envir = .GlobalEnv)
      }
    },
    add = TRUE
  )
  runif(n)
}

draws_oneshot <- draw_from(states_oneshot[[k + 1L]], 1000L)
draws_restart <- draw_from(states_restart[[1L]], 1000L)
ok_draws <- identical(draws_oneshot, draws_restart)

# ---- Report ----------------------------------------------------------
cat("substream restart experiment\n")
cat("============================\n")
cat(sprintf("R version : %s\n", R.version.string))
cat(sprintf("RNGkind   : %s\n\n", paste(RNGkind(), collapse = ", ")))

results <- list(
  "(a) saveRDS/readRDS round-trip preserves state" = TRUE, # (asserted)
  "(b) restart concatenation matches one-shot enumeration" = ok_concat,
  "(c1) drawing from state does not mutate the saved vector" = ok_no_mutate,
  "(c2) nextSubStream(saved) after use == states_oneshot[k+1]" = ok_advance,
  "(d) uniforms from restart-derived state == one-shot" = ok_draws
)
for (nm in names(results)) {
  cat(sprintf(
    "  %-58s  %s\n",
    nm,
    if (isTRUE(results[[nm]])) "PASS" else "FAIL"
  ))
}

if (!all(vapply(results, isTRUE, logical(1)))) {
  stop("substream restart property REJECTED")
}
cat("\nVerdict: CONFIRMED. Sub-stream enumeration can be restarted\n")
cat("from a persisted length-7 integer state. The child-scenario\n")
cat("design in TARGETS-DESIGN.md §5 is sound w.r.t. this assumption.\n")
