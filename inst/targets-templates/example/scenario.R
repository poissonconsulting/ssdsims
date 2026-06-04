# The scenario for the fuller targets example, adapted from `scripts/example.R`:
# a wider study sweeping sample sizes, hazard proportions, estimation and CI
# methods, and CI on/off. Shared by `_targets.R` (the targets pipeline) and
# `run-serial.R` (the single-core shard runner) so both run the same study and
# their results can be compared. Edit to taste.
library(ssdsims)

scenario <- ssd_define_scenario(
  ssddata::ccme_boron,
  nsim = 2L,
  nrow = c(5L, 6L, 10L, 20L, 50L),
  proportion = c(0.01, 0.05, 0.1, 0.2),
  est_method = c("arithmetic", "geometric", "multi"),
  ci = c(FALSE, TRUE),
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
  # scripts/example.R sweeps nboot = c(1, 5, 10, 50, 100, 500); kept to two
  # values here so the example runs in a reasonable time — widen for a real
  # study (run time scales with the bootstrap grid).
  nboot = c(10L, 100L),
  seed = 42L
)
