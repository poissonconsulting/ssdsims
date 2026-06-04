## Case 3 — SEGFAULT (hard crash).
##
## Run with: Rscript case3-segfault.R   (this script is EXPECTED to crash)
##
## dqrng is loaded FIRST, randtoolbox SECOND. randtoolbox is now the
## most-recently-loaded DLL, so base R binds *its* `user_unif_rand`.
## But randtoolbox's internal generator state was never initialised
## (no set.generator()/setSeed() call), so the first draw dereferences
## a NULL pointer:
##
##     *** caught segfault ***
##     address (nil), cause 'memory not mapped'
##     1: RNGkind("user", "user")   # inside dqrng::register_methods()
##
## This is the same single-symbol collision as Case 2, but here the
## resolved-to package is in an uninitialised state, so instead of
## silently-wrong numbers you get a process crash.

suppressMessages(library(dqrng)) # DLL loaded first
suppressMessages(library(randtoolbox)) # DLL loaded second  <- "wins" the symbol

cat("About to call dqrng::register_methods() -> RNGkind('user','user')\n")
cat("R will bind randtoolbox's (uninitialised) user_unif_rand and crash.\n")
flush(stdout())

dqrng::register_methods()
dqset.seed(42)
cat("If you see this line, no crash happened: runif =", runif(1), "\n")
