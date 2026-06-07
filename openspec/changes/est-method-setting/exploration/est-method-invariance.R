# Exploration: est_method is a post-hoc aggregation of a shared bootstrap draw
#
# Decisive evidence for the `est-method-setting` change. Run interactively:
#   Rscript --no-environ openspec/changes/est-method-setting/exploration/est-method-invariance.R
#
# IMPORTANT — the SAME-SEED caveat
# --------------------------------
# Every claim below holds *at a fixed seed*. We `set.seed(7)` before each
# `ssd_hc()` call so all three est_methods bootstrap from the identical draw.
#
# This is exactly why the byte-identity is only verifiable here, not post-hoc
# against the pipeline: the per-task RNG primer hashes the hc-grid row, which
# today includes `est_method` (R/task-primer.R:91-94). Removing `est_method`
# from `task_axes("hc")` changes the hash -> re-seeds every hc task, so the
# collapsed pipeline's bootstrap CIs are NOT byte-comparable to the old,
# per-est_method-seeded output. The analytical point estimate `est` is
# seed-independent and therefore unchanged; the CIs change numerically (and
# become consistent across est_methods within a task). This script justifies
# the design at the unit level; it does not assert old-vs-new pipeline equality.

suppressMessages(library(ssdtools))
suppressMessages(library(ssddata))

fit <- ssd_fit_dists(
  data.frame(Conc = sample(ssddata::ccme_boron$Conc, 15, replace = TRUE)),
  dists = ssd_dists_bcanz(),
  nrow = 5L
)

ests <- ssd_est_methods() # arithmetic, geometric, multi

# Same seed, sweep est_method against EVERY ci_method.
run_m <- function(m, cim) {
  set.seed(7)
  ssd_hc(
    fit,
    ci = TRUE, nboot = 40,
    est_method = m, ci_method = cim, parametric = TRUE,
    samples = TRUE, min_pboot = 0
  )
}

cat(sprintf(
  "%-18s  %-22s  %-12s  %s\n",
  "ci_method", "CI(lcl/ucl) invariant", "est differs", "samples identical"
))
for (cim in ssd_ci_methods()) {
  hs <- lapply(ests, run_m, cim = cim)
  ci_same <- isTRUE(all.equal(hs[[1]][, c("lcl", "ucl")], hs[[3]][, c("lcl", "ucl")]))
  est_diff <- !isTRUE(all.equal(hs[[1]]$est, hs[[3]]$est))
  smp_same <- isTRUE(all.equal(hs[[1]]$samples, hs[[3]]$samples))
  cat(sprintf("%-18s  %-22s  %-12s  %s\n", cim, ci_same, est_diff, smp_same))
}

# Observed in-session (Intel Xeon @ 2.10 GHz, R 4.5.3, ssdtools 2.6.0.9002):
# for ALL seven ci_methods -> CI invariant: TRUE | est differs: TRUE |
# samples identical: TRUE. i.e. one bootstrap draw serves every est_method;
# the CI is est_method-invariant; only the analytical `est` changes.
#
# Conclusion: `est_method` need not be a bootstrap (task) axis. One bootstrap
# per (nboot, ci_method, parametric) cell yields the est_method-invariant CI,
# and each method's `est` is the cheap analytical (ci = FALSE) value.
