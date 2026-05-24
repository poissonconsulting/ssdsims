test_that("ssd_sim_data2 returns ssdsims_scenario for data.frame", {
  scenario <- ssd_sim_data2(ssddata::ccme_boron, nsim = 2)
  expect_s3_class(scenario, "ssdsims_scenario")
  expect_equal(scenario$generator$kind, "data.frame")
  expect_equal(scenario$sim$nsim, 2)
})

test_that("ssd_sim_data2 normalizes character to function", {
  scenario <- ssd_sim_data2("rlnorm", nsim = 2)
  expect_equal(scenario$generator$kind, "function")
  expect_true(is.function(scenario$generator$fn))
})

test_that("ssd_sim_data2 normalizes tmbfit to function", {
  withr::local_seed(42)
  fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron, dists = "lnorm")
  scenario <- ssd_sim_data2(fit[[1]], nsim = 2)
  expect_equal(scenario$generator$kind, "function")
  expect_named(scenario$generator$args, c("meanlog", "sdlog"))
})

test_that("ssd_sim_data2 keeps fitdists kind", {
  withr::local_seed(42)
  fit <- ssdtools::ssd_fit_dists(
    ssddata::ccme_boron,
    dists = c("lnorm", "gamma")
  )
  scenario <- ssd_sim_data2(fit, dist_sim = c("lnorm", "top"), nsim = 2)
  expect_equal(scenario$generator$kind, "fitdists")
  expect_equal(scenario$generator$dist_sim, c("lnorm", "top"))
})

test_that("ssd_sim_data2 errors unrecognised argument", {
  chk::expect_chk_error(
    ssd_sim_data2(ssddata::ccme_boron, nsim = 2, thingy = 1),
    "thingy"
  )
})

test_that("print.ssdsims_scenario data.frame", {
  scenario <- ssd_sim_data2(ssddata::ccme_boron, nsim = 2, seed = 1)
  expect_snapshot(scenario)
})

test_that("print.ssdsims_scenario data.frame vectorized", {
  scenario <- ssd_sim_data2(
    ssddata::ccme_boron,
    nrow = c(5, 10),
    replace = c(TRUE, FALSE),
    nsim = 5,
    seed = 42
  )
  expect_snapshot(scenario)
})

test_that("print.ssdsims_scenario function", {
  scenario <- ssd_sim_data2(ssdtools::ssd_rlnorm, nsim = 2, seed = 1)
  expect_snapshot(scenario)
})

test_that("print.ssdsims_scenario character", {
  scenario <- ssd_sim_data2("rlnorm", nsim = 2, seed = 1)
  expect_snapshot(scenario)
})

test_that("print.ssdsims_scenario tmbfit", {
  withr::local_seed(42)
  fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron, dists = "lnorm")
  scenario <- ssd_sim_data2(fit[[1]], nsim = 2, seed = 1)
  expect_snapshot(scenario)
})

test_that("print.ssdsims_scenario fitdists", {
  withr::local_seed(42)
  fit <- ssdtools::ssd_fit_dists(
    ssddata::ccme_boron,
    dists = c("lnorm", "gamma")
  )
  scenario <- ssd_sim_data2(
    fit,
    dist_sim = c("lnorm", "top"),
    nsim = 2,
    seed = 1
  )
  expect_snapshot(scenario)
})

test_that("print.ssdsims_scenario with custom fit and hc args", {
  scenario <- ssd_sim_data2(
    ssddata::ccme_boron,
    nsim = 2,
    seed = 1,
    rescale = c(TRUE, FALSE),
    ci = TRUE,
    nboot = c(100, 200),
    proportion = c(0.05, 0.1)
  )
  expect_snapshot(scenario)
})

test_that("print.ssdsims_scenario with extras", {
  scenario <- ssd_sim_data2(
    ssddata::ccme_boron,
    nsim = 2,
    seed = 1,
    left = "Conc",
    samples = TRUE
  )
  expect_snapshot(scenario)
})
