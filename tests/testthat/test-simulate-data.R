test_that("ssd_simulate_data.function works", {
  withr::local_seed(42)
  data <- ssd_simulate_data(ssdtools::ssd_rlnorm, nrow = 5, nsim = 10)
  expect_snapshot_data(data, "function")
})

test_that("ssd_simulate_data.character works", {
  withr::local_seed(42)
  data <- ssd_simulate_data("lnorm", nrow = 5, nsim = 10)
  expect_snapshot_data(data, "character")
})

test_that("ssd_simulate_data.fitdists works top", {
  data <- ssddata::ccme_boron
  fit <- ssdtools::ssd_fit_dists(data, dists = c("lnorm", "gamma"))
  withr::local_seed(42)
  data <- ssd_simulate_data(fit, dist = "lnorm", nrow = 5, nsim = 10)
  expect_snapshot_data(data, "fitdists_top")
})

test_that("ssd_simulate_data.fitdists works name", {
  data <- ssddata::ccme_boron
  fit <- ssdtools::ssd_fit_dists(data, dists = c("lnorm", "gamma"))
  withr::local_seed(42)
  data <- ssd_simulate_data(fit, dist = "gamma", nrow = 5, nsim = 10)
  expect_snapshot_data(data, "fitdists_name")
})

test_that("ssd_simulate_data.tmbfit works", {
  data <- ssddata::ccme_boron
  tmbfit <- ssdtools::ssd_fit_dists(data, dists = "lnorm")[[1]]
  withr::local_seed(42)
  data <- ssd_simulate_data(tmbfit, nrow = 5, nsim = 10)
  expect_snapshot_data(data, "tmbfit")
})

test_that("ssd_simulate_data.data.frame works", {
  withr::local_seed(42)
  data <- ssd_simulate_data(ssddata::ccme_boron, nrow = 5, nsim = 10)
  expect_snapshot_data(data, "data_frame")
})
