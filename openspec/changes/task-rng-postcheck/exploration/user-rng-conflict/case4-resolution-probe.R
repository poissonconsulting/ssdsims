## Case 4 — The resolution rule, decoupled from any package config.
##
## Run with: Rscript case4-resolution-probe.R LAST
##   where LAST is "dqrng" or "randtoolbox" (which DLL to load last).
##
## We configure NEITHER package -- we just call base R's
## RNGkind("user-supplied") directly and see which DLL's symbol binds.
## This proves the rule is purely "most-recently-loaded DLL wins",
## independent of which package's setup function you call.
##
##   Rscript case4-resolution-probe.R dqrng        -> dqrng binds, works
##   Rscript case4-resolution-probe.R randtoolbox  -> randtoolbox binds,
##                                                    SEGFAULT (uninitialised)

last <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(last)) {
  last <- "dqrng"
}

if (identical(last, "dqrng")) {
  suppressMessages(library(randtoolbox))
  suppressMessages(library(dqrng)) # loaded last
} else {
  suppressMessages(library(dqrng))
  suppressMessages(library(randtoolbox)) # loaded last
}

cat("last-loaded DLL:", last, "\n")
RNGkind("user-supplied") # base call, no package config
cat("runif(2):", format(runif(2), digits = 6), "\n")
