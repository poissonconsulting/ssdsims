# Generate the shipped default cost calibration, `ssd_cost_calibration_default`.
#
#   Rscript --no-environ data-raw/cost_calibration.R
#
# The default is produced by the package's own calibration harness,
# `ssd_calibrate_cost()` - there is no separate analysis script to run.
# Recalibrating for a new architecture is exactly this one call; the object
# carries its own provenance so a stale default is visible in any estimate built
# from it (see `vignettes/cost-estimation.qmd`).
#
# This session (Intel Xeon @ 2.10 GHz, R 4.5.3, ssdtools 2.6.0.9002) measured the
# per-ci_method slopes, in ascending ms/boot:
#
#   weighted_samples  5.68 | GMACL 18.90 | MACL 19.64 | arithmetic_samples 21.24
#   | geometric_samples 21.60 | multi_fixed 52.63 | multi_free 55.00  (ms/boot),
#   with the bootstrap floor n0 ~ 25-30 and a weak, non-monotonic nrow factor
#   (cheap at 5 where most bcanz dists fail to fit, peaking ~10-20, easing at 50).
#
# The model-*form* derivation (which axes are free, the max(nboot, n0) shape, the
# non-monotonic nrow factor) is preserved one-time under
# openspec/changes/cost-estimation/exploration/; it is not rerun here.

devtools::load_all(".")

set.seed(42)
ssd_cost_calibration_default <- ssd_calibrate_cost(
  nboot = c(20L, 50L, 100L, 200L),
  nrow = c(5L, 10L, 20L, 50L),
  data = ssddata::ccme_boron,
  seed = 42L
)

print(ssd_cost_calibration_default)

usethis::use_data(ssd_cost_calibration_default, overwrite = TRUE)
