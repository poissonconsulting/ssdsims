## Case 5 -- The state-witness check (basis for the task-rng-postcheck design).
##
## Run with: Rscript case5-state-witness.R
##
## Cases 2-4 showed that base R's RNGkind()[1] == "user-supplied" probe is
## FOOLED by a foreign user-supplied RNG: it reads "healthy" while randtoolbox
## is actually the bound generator. This case shows a check that verifies
## *dqrng specifically* is still bound, using dqrng's OWN state as the witness:
##
##   1. s0 <- dqrng::dqrng_get_state()
##   2. draw once through base runif()         # routes via user_unif_rand
##   3. s1 <- dqrng::dqrng_get_state()
##   4. dqrng is bound  <=>  s0 != s1          # the draw advanced dqrng's state
##
## If a foreign RNG holds the user_unif_rand slot, the draw advances ITS state,
## dqrng's state stays frozen (s0 == s1), and the check fails -> abort.
##
## The witness draw can be rolled back with dqrng_set_state(s0), so the probe
## is non-destructive (consumes no net randomness).

suppressMessages(library(dqrng))

witness_dqrng_bound <- function() {
  s0 <- dqrng::dqrng_get_state()
  invisible(runif(1)) # base runif() -> whatever holds the user_unif_rand slot
  s1 <- dqrng::dqrng_get_state()
  dqrng::dqrng_set_state(s0) # roll the witness draw back: non-destructive
  !identical(s0, s1) # TRUE iff the draw advanced dqrng -> dqrng is bound
}

## --- Healthy: dqrng is the bound backend ---
dqrng::register_methods()
dqrng::dqset.seed(42)
cat(
  "[healthy dqrng]     RNGkind:",
  RNGkind()[1],
  " witness(dqrng bound)=",
  witness_dqrng_bound(),
  "\n"
)

## --- Non-destructive: the witness leaves the stream untouched ---
dqrng::dqset.seed(42)
before <- runif(1)
dqrng::dqset.seed(42)
invisible(witness_dqrng_bound()) # runs a witness draw, then rolls it back
after <- runif(1)
cat(
  "[non-destructive]   next draw before=",
  format(before, digits = 6),
  " after witness=",
  format(after, digits = 6),
  " identical=",
  identical(before, after),
  "\n"
)

## --- Foreign hijack (see case6): RNGkind() lies, the witness does not ---
cat(
  "\nFor the hijack case (randtoolbox bound, RNGkind() still 'user-supplied'\n"
)
cat("but witness=FALSE -> abort), run:  Rscript case6-witness-vs-hijack.R\n")
