# Structural equivalence between the legacy ssd_run_scenario() and the new
# ssd_run_scenario2(ssd_sim_data2(...)). These tests are the only place
# that exercises the old code paths.
#
# Byte-for-byte equality is not asserted: the new runner derives data, fit
# and hc states from independent L'Ecuyer-CMRG substreams of the master
# seed, whereas the old runner draws fit/hc seeds from the global RNG state
# at call time. List columns that contain S3 objects with opaque pointers
# (fits) or columns the new pair drops (cross-joined fit/hc metadata
# layout) are also out of scope. We compare the metadata grid and the
# scalar contents of the unnested data and hc tibbles.

strip_lists <- function(x) {
  x[, !vapply(x, is.list, logical(1)), drop = FALSE]
}

sort_rows <- function(x) {
  dplyr::arrange(x, dplyr::across(dplyr::everything()))
}

expect_run_equivalent <- function(build_old, build_new) {
  suppressWarnings({
    old <- with_lecuyer_cmrg_seed(10, build_old())
  })
  suppressWarnings({
    new <- with_lecuyer_cmrg_seed(10, build_new())
  })

  # Same set of task-grid combinations after unnesting the data list-column.
  # Actual sampled values diverge because the new runner uses independent
  # L'Ecuyer-CMRG substreams per (sim, stream) for fit/hc.
  old_data <- strip_lists(tidyr::unnest(old, cols = "data"))
  new_data <- strip_lists(tidyr::unnest(new, cols = "data"))
  grid_cols <- intersect(
    setdiff(names(old_data), c("Conc", "Chemical", "Species", "Group", "Units")),
    names(new_data)
  )
  testthat::expect_equal(
    sort_rows(dplyr::distinct(old_data[, grid_cols, drop = FALSE])),
    sort_rows(dplyr::distinct(new_data[, grid_cols, drop = FALSE]))
  )
  testthat::expect_equal(nrow(old_data), nrow(new_data))

  # Same set of hc-grid combinations after unnesting; estimates diverge.
  old_hc <- strip_lists(tidyr::unnest(old, cols = "hc"))
  new_hc <- strip_lists(tidyr::unnest(new, cols = "hc"))
  hc_grid_cols <- intersect(
    setdiff(names(old_hc), c("est", "se", "lcl", "ucl", "wt", "pboot")),
    names(new_hc)
  )
  testthat::expect_equal(
    sort_rows(dplyr::distinct(old_hc[, hc_grid_cols, drop = FALSE])),
    sort_rows(dplyr::distinct(new_hc[, hc_grid_cols, drop = FALSE]))
  )
  testthat::expect_equal(nrow(old_hc), nrow(new_hc))
}

test_that("new code is structurally equivalent to old for data.frame source", {
  expect_run_equivalent(
    function() {
      ssd_run_scenario(
        ssddata::ccme_boron,
        replace = c(TRUE, FALSE),
        nsim = 2
      )
    },
    function() {
      ssd_run_scenario2(ssd_sim_data2(
        ssddata::ccme_boron,
        replace = c(TRUE, FALSE),
        nsim = 2
      ))
    }
  )
})

test_that("new code is structurally equivalent to old for function source", {
  expect_run_equivalent(
    function() ssd_run_scenario(rlnorm, nsim = 2),
    function() ssd_run_scenario2(ssd_sim_data2(rlnorm, nsim = 2))
  )
})

test_that("new code is structurally equivalent to old for character source", {
  expect_run_equivalent(
    function() ssd_run_scenario("rlnorm", nsim = 2),
    function() ssd_run_scenario2(ssd_sim_data2("rlnorm", nsim = 2))
  )
})

test_that("new code is structurally equivalent to old for tmbfit source", {
  expect_run_equivalent(
    function() {
      fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron)
      ssd_run_scenario(fit[[1]], nsim = 2)
    },
    function() {
      fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron)
      ssd_run_scenario2(ssd_sim_data2(fit[[1]], nsim = 2))
    }
  )
})

test_that("new code is structurally equivalent to old for fitdists source", {
  expect_run_equivalent(
    function() {
      fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron)
      ssd_run_scenario(fit, dist_sim = c("lnorm", "top"), nsim = 2)
    },
    function() {
      fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron)
      ssd_run_scenario2(ssd_sim_data2(
        fit,
        dist_sim = c("lnorm", "top"),
        nsim = 2
      ))
    }
  )
})
