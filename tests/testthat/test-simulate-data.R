test_that("ssd_simulate_data.function works", {
  withr::local_seed(42)
  data <- ssd_simulate_data(ssdtools::ssd_rlnorm, nrow = 5, nsim = 10)
  expect_snapshot(data)
  data <- tidyr::unnest(data, cols = c(data))
  expect_snapshot_data(data, "function_unnest")
})

test_that("ssd_simulate_data.function vectorized", {
  withr::local_seed(42)
  data <- ssd_simulate_data(ssdtools::ssd_rlnorm, nrow = c(5, 10), nsim = 2)
  expect_snapshot(data)
  data <- tidyr::unnest(data, cols = c(data))
  expect_snapshot_data(data, "function_vectorize")
})

test_that("ssd_simulate_data.character works", {
  withr::local_seed(42)
  data <- ssd_simulate_data("rlnorm", nrow = 5, nsim = 10)
  expect_snapshot(data)
  data <- tidyr::unnest(data, cols = c(data))
  expect_snapshot_data(data, "character_unnest")
})

test_that("ssd_simulate_data.character vectorized", {
  withr::local_seed(42)
  data <- ssd_simulate_data("rlnorm", nrow = c(5, 10), nsim = 2)
  expect_snapshot(data)
  data <- tidyr::unnest(data, cols = c(data))
  expect_snapshot_data(data, "character_vectorize")
})

test_that("ssd_simulate_data.fitdists works top", {
  data <- ssddata::ccme_boron
  fit <- ssdtools::ssd_fit_dists(data, dists = c("lnorm", "gamma"))
  withr::local_seed(42)
  data <- ssd_simulate_data(fit, dist_sim = "lnorm", nrow = 5, nsim = 10)
  expect_snapshot(data)
  data <- tidyr::unnest(data, cols = c(data))
  expect_snapshot_data(data, "fitdists_top_unnest")
})

test_that("ssd_simulate_data.fitdists works multi", {
  data <- ssddata::ccme_boron
  fit <- ssdtools::ssd_fit_dists(data, dists = c("lnorm", "gamma"))
  withr::local_seed(42)
  data <- ssd_simulate_data(fit, dist_sim = "multi", nrow = 5, nsim = 10)
  expect_snapshot(data)
  data <- tidyr::unnest(data, cols = c(data))
  expect_snapshot_data(data, "fitdists_multi_unnest")
})

test_that("ssd_simulate_data.fitdists works name", {
  data <- ssddata::ccme_boron
  fit <- ssdtools::ssd_fit_dists(data, dists = c("lnorm", "gamma"))
  withr::local_seed(42)
  data <- ssd_simulate_data(fit, dist_sim = "gamma", nrow = 5, nsim = 10)
  expect_snapshot(data)
  data <- tidyr::unnest(data, cols = c(data))
  expect_snapshot_data(data, "fitdists_name_unnest")
})

test_that("ssd_simulate_data.tmbfit works", {
  data <- ssddata::ccme_boron
  tmbfit <- ssdtools::ssd_fit_dists(data, dists = "lnorm")[[1]]
  withr::local_seed(42)
  data <- ssd_simulate_data(tmbfit, nrow = 5, nsim = 10)
  expect_snapshot(data)
  data <- tidyr::unnest(data, cols = c(data))
  expect_snapshot_data(data, "tmbfit_unnest")
})

test_that("ssd_simulate_data.data.frame works", {
  withr::local_seed(42)
  data <- ssd_simulate_data(ssddata::ccme_boron, nrow = 5, nsim = 10)
  expect_snapshot(data)
  data <- tidyr::unnest(data, cols = c(data))
  expect_snapshot_data(data, "data_frame_unnest")
})

test_that("ssd_simulate_data.data.frame vectorized", {
  withr::local_seed(42)
  data <- ssd_simulate_data(ssddata::ccme_boron, nrow = c(5,10), nsim = 2)
  expect_snapshot(data)
  data <- tidyr::unnest(data, cols = c(data))
  expect_snapshot_data(data, "data_frame_vectorize")
})