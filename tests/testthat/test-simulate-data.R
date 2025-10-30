test_that("ssd_simulate_data.function works", {
  withr::local_seed(42)
  data <- ssd_simulate_data(ssdtools::ssd_rlnorm, nrow = 5, nsim = 10)
  expect_snapshot_data(data, "function")
  data <- tidyr::unnest(data, cols = c(data))
  expect_snapshot_data(data, "function_unnest")
})

test_that("ssd_simulate_data.character works", {
  withr::local_seed(42)
  data <- ssd_simulate_data("rlnorm", nrow = 5, nsim = 10)
  expect_snapshot_data(data, "character")
  data <- tidyr::unnest(data, cols = c(data))
  expect_snapshot_data(data, "character_unnest")
})

test_that("ssd_simulate_data.fitdists works top", {
  data <- ssddata::ccme_boron
  fit <- ssdtools::ssd_fit_dists(data, dists = c("lnorm", "gamma"))
  withr::local_seed(42)
  data <- ssd_simulate_data(fit, dist_sim = "lnorm", nrow = 5, nsim = 10)
  expect_snapshot_data(data, "fitdists_top")
  data <- tidyr::unnest(data, cols = c(data))
  expect_snapshot_data(data, "fitdists_top_unnest")
})

test_that("ssd_simulate_data.fitdists works name", {
  data <- ssddata::ccme_boron
  fit <- ssdtools::ssd_fit_dists(data, dists = c("lnorm", "gamma"))
  withr::local_seed(42)
  data <- ssd_simulate_data(fit, dist_sim = "gamma", nrow = 5, nsim = 10)
  expect_snapshot_data(data, "fitdists_name")
  data <- tidyr::unnest(data, cols = c(data))
  expect_snapshot_data(data, "fitdists_name_unnest")
})

test_that("ssd_simulate_data.tmbfit works", {
  data <- ssddata::ccme_boron
  tmbfit <- ssdtools::ssd_fit_dists(data, dists = "lnorm")[[1]]
  withr::local_seed(42)
  data <- ssd_simulate_data(tmbfit, nrow = 5, nsim = 10)
  expect_snapshot_data(data, "tmbfit")
  data <- tidyr::unnest(data, cols = c(data))
  expect_snapshot_data(data, "tmbfit_unnest")
})

test_that("ssd_simulate_data.data.frame works", {
  withr::local_seed(42)
  data <- ssd_simulate_data(ssddata::ccme_boron, nrow = 5, nsim = 10)
  expect_snapshot_data(data, "data_frame")
  data <- tidyr::unnest(data, cols = c(data))
  expect_snapshot_data(data, "data_frame_unnest")
})
