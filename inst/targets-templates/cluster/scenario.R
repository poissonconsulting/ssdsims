# INGREDIENT C — the scenario for the cluster targets example (TARGETS-DESIGN.md
# section 4). The same shape as `large/scenario.R`: a wider study sweeping sample
# sizes, hazard proportions, estimation and CI methods, with bootstrap CIs.
# Scheduler-independent — shared by `_targets.R` (the targets pipeline) and
# `run-serial.R` (the single-core shard runner) so both run the same study and
# their results can be compared. Edit to taste.
#
# MINIMAL FIRST JOB: for your first cluster run, follow the README's step 3 and
# shrink this to the built-in `small` scenario (a cheap, fast job) to confirm
# the controller + probe before launching the full sweep. Then (step 4) edit
# this file to your own study, leaving `_targets.R` and the controller untouched.
library(ssdsims)

scenario <- ssd_define_scenario(
  ssddata::ccme_boron,
  nsim = 2L,
  nrow = c(5L, 10L), # c(5L, 6L, 10L, 20L, 50L),
  proportion = c(0.01, 0.05, 0.1, 0.2),
  est_method = c("arithmetic", "geometric", "multi"),
  ci = TRUE,
  ci_method = c(
    "arithmetic_samples",
    "geometric_samples",
    "GMACL",
    "MACL",
    "multi_fixed",
    "multi_free",
    "weighted_samples"
  ),
  parametric = TRUE,
  nboot = c(5, 50), # c(1, 5, 10, 50, 100, 500), # * 100,
  samples = TRUE,
  seed = 42L,
  bundle = list(
    hc = c("ci_method", "est_method")
  )
)
