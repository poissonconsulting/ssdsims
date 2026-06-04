## Case 6 -- The witness catches a hijack the cheap probe misses.
##
## Run with: Rscript case6-witness-vs-hijack.R
##
## dqrng loaded first, randtoolbox loaded last (wins the user_unif_rand slot),
## then randtoolbox configured + initialised (so no segfault -- contrast
## case3). RNGkind()[1] still reports "user-supplied", so the cheap probe is
## fooled into thinking the dqrng backend is intact. The dqrng-state witness
## (case5) is not fooled: the draw advances randtoolbox, not dqrng.

suppressMessages(library(dqrng)) # loaded first
suppressMessages(library(randtoolbox)) # loaded last -> wins the slot

dqrng::dqset.seed(42) # give dqrng a known state
set.generator("WELL", order = 512, version = "a", seed = 42) # randtoolbox now bound

cat(
  "cheap probe  RNGkind()[1] == 'user-supplied':",
  RNGkind()[1] == "user-supplied",
  "  <- fooled, looks healthy\n"
)

s0 <- dqrng::dqrng_get_state()
draw <- runif(1) # draws from randtoolbox (WELL), NOT dqrng
s1 <- dqrng::dqrng_get_state()
dqrng::dqrng_set_state(s0)

cat(
  "witness      dqrng-state-advanced:",
  !identical(s0, s1),
  "  <- FALSE: dqrng is NOT the bound generator -> ABORT\n"
)
cat("(draw =", format(draw, digits = 6), "-- a WELL512a value, not dqrng's)\n")
