# Exploration 1/2: which hc axes actually cost anything (ci = TRUE)?
#
#   Rscript --no-environ openspec/changes/cost-estimation/exploration/01-axis-probe.R
#
# Establishes the free-vs-costly split that the cost model relies on, at a
# fixed nboot = 1000 on the bcanz set. Findings drive the estimator: the hc
# bootstrap dominates, ci_method is the cost driver (~15x spread), and
# proportion / est_method / samples are free (one bootstrap serves all).

suppressMessages(library(ssdtools))
suppressMessages(library(ssddata))

set.seed(1)
fit <- ssd_fit_dists(ssddata::ccme_boron, dists = ssd_dists_bcanz())
tt <- function(expr) as.numeric(system.time(expr)["elapsed"])
nb <- 1000

# proportion: 1 vs 4 values, same bootstrap?
t_p1 <- tt(ssd_hc(fit, proportion = 0.05, ci = TRUE, nboot = nb, min_pboot = 0))
t_p4 <- tt(ssd_hc(
  fit,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = TRUE,
  nboot = nb,
  min_pboot = 0
))
cat(sprintf(
  "proportion x1 = %.2fs  x4 = %.2fs  (ratio %.2f -> FREE)\n",
  t_p1,
  t_p4,
  t_p4 / t_p1
))

# ci_method: per-method cost (the dominant axis)
t_ci <- vapply(
  ssd_ci_methods(),
  \(m) tt(ssd_hc(fit, ci = TRUE, nboot = nb, ci_method = m, min_pboot = 0)),
  numeric(1)
)
cat("\nper-ci_method time at nboot=1000 (the cost driver):\n")
print(round(sort(t_ci), 2))

# est_method: free (post-hoc aggregation, see est-method-setting change)
t_est <- vapply(
  ssd_est_methods(),
  \(m) tt(ssd_hc(fit, ci = TRUE, nboot = nb, est_method = m, min_pboot = 0)),
  numeric(1)
)
cat("\nper-est_method time (all ~equal -> FREE):\n")
print(round(t_est, 2))

# samples retention: free
t_sampF <- tt(ssd_hc(
  fit,
  ci = TRUE,
  nboot = nb,
  samples = FALSE,
  min_pboot = 0
))
t_sampT <- tt(ssd_hc(fit, ci = TRUE, nboot = nb, samples = TRUE, min_pboot = 0))
cat(sprintf(
  "\nsamples=FALSE %.2fs  samples=TRUE %.2fs  (-> FREE)\n",
  t_sampF,
  t_sampT
))

# In-session result (Intel Xeon @ 2.10 GHz, R 4.5.3, ssdtools 2.6.0.9002):
#   proportion x4/x1 = 0.93 (free); est_method 6.04/5.87/5.77 (free);
#   samples free; ci_method spread weighted_samples 6.0s ... multi_fixed 89s.
