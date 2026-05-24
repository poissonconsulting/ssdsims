# Equivalence tests verifying ssd_run_scenario2(ssd_sim_data2(...)) produces
# byte-identical output to the legacy ssd_run_scenario(...). These tests are
# the only place that exercises the old code paths.

strip_lists <- function(x) {
  x[, !vapply(x, is.list, logical(1)), drop = FALSE]
}

expect_run_equivalent <- function(build_old, build_new) {
  suppressWarnings({
    old <- with_lecuyer_cmrg_seed(10, build_old())
  })
  suppressWarnings({
    new <- with_lecuyer_cmrg_seed(10, build_new())
  })
  old_data <- strip_lists(tidyr::unnest(old, cols = "data"))
  new_data <- strip_lists(tidyr::unnest(new, cols = "data"))
  testthat::expect_equal(new_data, old_data)
  old_hc <- strip_lists(tidyr::unnest(old, cols = "hc"))
  new_hc <- strip_lists(tidyr::unnest(new, cols = "hc"))
  testthat::expect_equal(new_hc, old_hc)
}

test_that("new code is equivalent to old for data.frame source", {
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

test_that("new code is equivalent to old for function source", {
  expect_run_equivalent(
    function() ssd_run_scenario(rlnorm, nsim = 2),
    function() ssd_run_scenario2(ssd_sim_data2(rlnorm, nsim = 2))
  )
})

test_that("new code is equivalent to old for character source", {
  expect_run_equivalent(
    function() ssd_run_scenario("rlnorm", nsim = 2),
    function() ssd_run_scenario2(ssd_sim_data2("rlnorm", nsim = 2))
  )
})

test_that("new code is equivalent to old for tmbfit source", {
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

test_that("new code is equivalent to old for fitdists source", {
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
