test_that("ssd_generate_data.function works", {
  withr::local_seed(42)
  data <- ssd_generate_data(ssdtools::ssd_rlnorm, nrow = 5, nsim = 10)
  expect_snapshot_data(data, "function")
})

test_that("ssd_generate_data.character works", {
  withr::local_seed(42)
  data <- ssd_generate_data("lnorm", nrow = 5, nsim = 10)
  expect_snapshot_data(data, "character")
})

test_that("ssd_generate_data.fitdists works top", {
  data <- ssddata::ccme_boron
  fit <- ssdtools::ssd_fit_dists(data, dists = c("lnorm", "gamma"))
  withr::local_seed(42)
  data <- ssd_generate_data(fit, dist = "lnorm", nrow = 5, nsim = 10)
  expect_snapshot_data(data, "fitdists_top")
})

test_that("ssd_generate_data.fitdists works name", {
  data <- ssddata::ccme_boron
  fit <- ssdtools::ssd_fit_dists(data, dists = c("lnorm", "gamma"))
  withr::local_seed(42)
  data <- ssd_generate_data(fit, dist = "gamma", nrow = 5, nsim = 10)
  expect_snapshot_data(data, "fitdists_name")
})

test_that("ssd_generate_data.tmbfit works", {
  data <- ssddata::ccme_boron
  tmbfit <- ssdtools::ssd_fit_dists(data, dists = "lnorm")[[1]]
  withr::local_seed(42)
  data <- ssd_generate_data(tmbfit, nrow = 5, nsim = 10)
  expect_snapshot_data(data, "tmbfit")
})
