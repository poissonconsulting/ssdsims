## Case 2 — SILENT CORRUPTION (no error, wrong random numbers).
##
## Run with: Rscript case2-silent-wrong-numbers.R
##
## randtoolbox is loaded FIRST, dqrng SECOND. The user configures ONLY
## randtoolbox (WELL512a, seed 42) and never touches dqrng. They expect
## WELL512a numbers. They get dqrng numbers instead -- because base R's
## single `user_unif_rand` symbol resolves to the most-recently-loaded
## DLL (dqrng), not to the package the user configured.

suppressMessages(library(randtoolbox)) # DLL loaded first
suppressMessages(library(dqrng)) # DLL loaded second  <- "wins" the symbol

set.generator("WELL", order = 512, version = "a", seed = 42)

cat("RNGkind:", RNGkind()[1], "\n")
got <- format(runif(3), digits = 6)
cat("runif(3) actual:        ", got, "\n")
cat(
  "runif(3) user expected: ",
  "0.825669 0.341247 0.848482  (WELL512a seed 42)\n"
)
cat("\nNo error is raised. The sequence is dqrng's, not randtoolbox's:\n")
cat("reproducibility is silently broken.\n")
