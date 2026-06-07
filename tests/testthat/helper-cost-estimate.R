# A deterministic `ssdsims_cost_calibration` for the cost-estimation tests, so
# assertions and snapshots do not depend on the shipped (machine-measured)
# default. `multi_free` is given the steepest slope so it is the costliest cell;
# the `*_samples`/ACL group carries an `n0 = 25` bootstrap floor.
test_cost_calibration <- function() {
  # Explicit name -> coefficient mapping (not positional against
  # `ssd_ci_methods()`), so the fixture does not depend on that vector's order.
  new_ssdsims_cost_calibration(
    coefficients = tibble::tribble(
      ~ci_method           , ~base , ~slope , ~n0 ,
      "arithmetic_samples" , 1.0   , 0.0210 ,  30 ,
      "geometric_samples"  , 1.0   , 0.0216 ,  25 ,
      "GMACL"              , 1.0   , 0.0189 ,  25 ,
      "MACL"               , 1.0   , 0.0196 ,  25 ,
      "multi_fixed"        , 0.1   , 0.0526 ,   0 ,
      "multi_free"         , 1.0   , 0.0550 ,  25 ,
      "weighted_samples"   , 0.6   , 0.0057 ,   0
    ),
    nrow_factor = tibble::tibble(
      nrow = c(5L, 10L, 20L, 50L),
      factor = c(0.3, 1.0, 1.0, 0.8)
    ),
    fixed_addend = 0.05,
    provenance = list(
      cpu = "Test CPU @ 1.00GHz",
      r_version = "R version 4.5.3 (2026-03-11)",
      ssdtools_version = "2.6.0.9002",
      date = as.Date("2026-06-07"),
      sweep_grid = list(
        nboot = c(20L, 50L, 100L, 200L),
        nrow = c(5L, 10L, 20L, 50L),
        ci_method = ssdtools::ssd_ci_methods()
      )
    )
  )
}
