## Case 1 — Baselines: each package works fine ALONE.
##
## Run with: Rscript case1-baselines.R
##
## dqrng and randtoolbox each install themselves as base R's single
## "user-supplied" uniform RNG. In isolation this is unambiguous.

cat("--- dqrng alone ---\n")
local({
  suppressMessages(library(dqrng))
  dqrng::register_methods() # routes base R runif() -> dqrng (default generator)
  dqset.seed(42)
  cat("RNGkind:", RNGkind()[1], "\n")
  cat(
    "runif(3):",
    format(runif(3), digits = 6),
    "  (expect 0.908370 0.330612 0.950935)\n"
  )
  dqrng::restore_methods()
})

cat("\n--- randtoolbox alone (fresh session needed; shown for reference) ---\n")
cat("set.generator('WELL', order=512, version='a', seed=42); runif(3)\n")
cat("=> 0.825669 0.341247 0.848482\n")
