# ---- calibration object and accessor ---------------------------------------

test_that("cost-estimation: shipped default is a provenance-carrying object", {
  cal <- ssd_cost_calibration()
  expect_s3_class(cal, "ssdsims_cost_calibration")
  expect_setequal(
    names(cal),
    c("coefficients", "nrow_factor", "fixed_addend", "provenance")
  )
  expect_setequal(
    names(cal$coefficients),
    c("ci_method", "base", "slope", "n0")
  )
  expect_setequal(cal$coefficients$ci_method, ssdtools::ssd_ci_methods())
  expect_setequal(
    names(cal$provenance),
    c("cpu", "r_version", "ssdtools_version", "date", "sweep_grid")
  )
})

test_that("cost-estimation: calibration print renders coefficients and caveat", {
  expect_snapshot(test_cost_calibration())
})

# ---- estimator: no side effects --------------------------------------------

test_that("cost-estimation: estimation runs no RNG and alters no seed", {
  scenario <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 2L,
    seed = 42L,
    ci = TRUE,
    nboot = c(100L, 1000L),
    nrow = c(5L, 10L),
    ci_method = ssdtools::ssd_ci_methods()
  )
  withr::local_seed(1)
  before <- .Random.seed
  est <- ssd_estimate_cost(scenario, test_cost_calibration())
  expect_identical(.Random.seed, before)
  expect_s3_class(est, "ssdsims_cost_estimate")
  expect_s3_class(est$total, "difftime")
  expect_s3_class(est$longest, "difftime")
})

# ---- estimator: free axes do not multiply ----------------------------------

test_that("cost-estimation: proportion and est_method do not change the total", {
  lean <- ssd_define_scenario(
    ssd_data(boron = ssddata::ccme_boron),
    nsim = 2L,
    seed = 42L,
    ci = TRUE,
    nboot = c(100L, 1000L),
    nrow = c(5L, 10L),
    est_method = "multi",
    proportion = 0.05
  )
  fat <- ssd_define_scenario(
    ssd_data(boron = ssddata::ccme_boron),
    nsim = 2L,
    seed = 42L,
    ci = TRUE,
    nboot = c(100L, 1000L),
    nrow = c(5L, 10L),
    est_method = ssdtools::ssd_est_methods(),
    proportion = c(0.01, 0.05, 0.1, 0.2)
  )
  cal <- test_cost_calibration()
  expect_equal(
    ssd_estimate_cost(fat, cal)$total,
    ssd_estimate_cost(lean, cal)$total
  )
})

# ---- estimator: longest task tracks the costliest cell ----------------------

test_that("cost-estimation: longest task is the costliest multi cell at max nboot", {
  scenario <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 3L,
    seed = 42L,
    ci = TRUE,
    nboot = c(100L, 1000L),
    nrow = 10L,
    ci_method = ssdtools::ssd_ci_methods()
  )
  est <- ssd_estimate_cost(scenario, test_cost_calibration())
  # multi_free has the steepest slope, so at nboot = 1000 (nrow factor 1) it is
  # the costliest single task, not merely the most numerous.
  expected <- (1.0 + 0.055 * 1000) * 1.0 + 0.05
  expect_equal(as.numeric(est$longest, units = "secs"), expected)
  top <- est$breakdown[1, ]
  expect_identical(top$ci_method, "multi_free")
  expect_identical(top$nboot, 1000L)
})

# ---- cost model: max(nboot, n0) floor --------------------------------------

test_that("cost-estimation: per-task cost obeys the max(nboot, n0) floor", {
  cal <- test_cost_calibration()
  # GMACL carries an n0 = 25 floor; nrow 10 has factor 1.
  tasks <- tibble::tibble(
    ci_method = "GMACL",
    nboot = c(10L, 20L, 50L, 100L),
    nrow = 10L,
    ci = TRUE
  )
  secs <- cost_task_seconds(tasks, cal)
  # Both below the floor -> equal; above the floor -> linear in nboot.
  expect_equal(secs[[1]], secs[[2]])
  expect_gt(secs[[3]], secs[[2]])
  expect_equal(secs[[4]] - secs[[3]], 0.0189 * 50)
})

test_that("cost-estimation: ci = FALSE tasks cost only the fixed addend", {
  cal <- test_cost_calibration()
  tasks <- tibble::tibble(
    ci_method = NA_character_,
    nboot = NA_integer_,
    nrow = 10L,
    ci = FALSE
  )
  expect_equal(cost_task_seconds(tasks, cal), cal$fixed_addend)
})

# ---- estimate print --------------------------------------------------------

test_that("cost-estimation: estimate print is stable", {
  scenario <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 2L,
    seed = 42L,
    ci = TRUE,
    nboot = c(1000L, 10000L),
    nrow = c(10L, 20L),
    ci_method = c("weighted_samples", "multi_free")
  )
  expect_snapshot(ssd_estimate_cost(scenario, test_cost_calibration()))
})

# ---- calibration harness ----------------------------------------------------

test_that("cost-estimation: input validation errors", {
  expect_snapshot(error = TRUE, {
    ssd_calibrate_cost(nrow = 10L)
  })
  expect_snapshot(error = TRUE, {
    ssd_calibrate_cost(nboot = -1L)
  })
})

test_that("cost-estimation: a single call calibrates a usable object", {
  skip_on_cran()
  skip_if_not_installed("ssddata")
  # The smallest grid the harness accepts (two `nboot`, two `nrow`); this is a
  # wiring smoke test, so keep the bootstrap counts low rather than realistic.
  cal <- ssd_calibrate_cost(nboot = c(10L, 20L), nrow = c(5L, 10L), seed = 1L)
  expect_s3_class(cal, "ssdsims_cost_calibration")
  expect_setequal(cal$coefficients$ci_method, ssdtools::ssd_ci_methods())
  expect_identical(nrow(cal$nrow_factor), 2L)
  expect_identical(cal$provenance$sweep_grid$nboot, c(10L, 20L))
  # The freshly fitted calibration drives the estimator just like the default.
  scenario <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 1L,
    seed = 42L,
    ci = TRUE,
    nboot = 1000L,
    nrow = 10L
  )
  expect_s3_class(ssd_estimate_cost(scenario, cal), "ssdsims_cost_estimate")
})
