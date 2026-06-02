## scripts/experiment-subset-property.R
##
## Proof of work for the `nrow` subset claim in TARGETS-DESIGN §5:
##
##   slice_sample(data, n = small,   replace = R, state = S)
##     ==  head(slice_sample(data, n = large, replace = R, state = S), small)
##
## for any large >= small, and BOTH R = FALSE and R = TRUE.
##
## Implication for the design:
##   `nrow` is **never** an independent axis of the data step. The
##   data state is keyed by `(dataset, sim, replace)` only, and the
##   slice for any `nrow` value is the prefix of a single fixed
##   sample of size `max(scenario$nrow)`.
##
## Mechanism:
##   * replace = FALSE: sample.int(N, n_max, replace = FALSE) is a
##     Fisher-Yates prefix; the first `n` indices are a permutation
##     of a size-n sample by construction.
##   * replace = TRUE: sample.int(N, n_max, replace = TRUE) is
##     `n_max` independent uniform draws; the first `n` are a
##     size-n sample drawn from the same RNG sequence.
##
## In both cases, byte equivalence comes from `sample.int()`
## consuming RNG in the same order; replace doesn't matter.

library(dqrng)
library(dplyr)

dqRNGkind("pcg64")
dqrng::register_methods()
on.exit(dqrng::restore_methods(), add = TRUE)

# Helpers
seed_val <- 42L
state_a <- c(1234L, 5678L) # arbitrary length-2 state

reset <- function() dqset.seed(seed_val, stream = state_a)

# --- (1) sample.int prefix equivalence ---------------------------------

for (rep in c(FALSE, TRUE)) {
  reset()
  big <- sample.int(100L, 50L, replace = rep)
  reset()
  for (n in c(1L, 5L, 20L, 50L)) {
    reset()
    small <- sample.int(100L, n, replace = rep)
    stopifnot(identical(big[seq_len(n)], small))
  }
  cat(sprintf(
    "(1) sample.int(N, n, replace=%-5s): prefix property holds for n in {1,5,20,50}\n",
    rep
  ))
}

# --- (2) dplyr::slice_sample prefix equivalence ------------------------

df <- tibble(id = seq_len(100L), x = seq_len(100L) * 1.0)

for (rep in c(FALSE, TRUE)) {
  reset()
  big <- slice_sample(df, n = 50L, replace = rep)
  for (n in c(1L, 5L, 20L, 50L)) {
    reset()
    small <- slice_sample(df, n = n, replace = rep)
    stopifnot(identical(big[seq_len(n), , drop = FALSE], small))
  }
  cat(sprintf(
    "(2) slice_sample(df, n, replace=%-5s): prefix property holds for n in {1,5,20,50}\n",
    rep
  ))
}

# --- (3) Sanity: different replace values give DIFFERENT sequences -----

reset()
v_false <- sample.int(100L, 50L, replace = FALSE)
reset()
v_true <- sample.int(100L, 50L, replace = TRUE)
stopifnot(!identical(v_false, v_true))
cat(
  "(3) replace=FALSE vs replace=TRUE: different sequences under same state (correctly)\n"
)

# --- (4) Implication: a single slice per (dataset, sim, replace) -------

# Toy generator function that mimics the proposed slice_sample_state.
slice_sample_state <- function(data, n_max, n, seed, state, replace) {
  dqset.seed(seed, stream = state)
  idx <- sample.int(nrow(data), size = n_max, replace = replace)
  data[idx[seq_len(n)], , drop = FALSE]
}

# Given a single (seed, state, replace), every nrow value is a prefix
# of the same n_max-row sample.
n_max <- 50L
for (rep in c(FALSE, TRUE)) {
  full <- slice_sample_state(df, n_max, n_max, seed_val, state_a, rep)
  for (n in c(5L, 10L, 25L)) {
    sub <- slice_sample_state(df, n_max, n, seed_val, state_a, rep)
    stopifnot(identical(full[seq_len(n), , drop = FALSE], sub))
  }
  cat(sprintf(
    "(4) slice_sample_state(n_max=50, n=%-5s, replace=%-5s): prefix property holds\n",
    "5/10/25",
    rep
  ))
}

cat("\nVerdict: nrow is NEVER an independent axis; the data state\n")
cat("is keyed by (dataset, sim, replace) only. The slice for any\n")
cat("nrow value is head(., n) of a single n_max-sized sample.\n")
