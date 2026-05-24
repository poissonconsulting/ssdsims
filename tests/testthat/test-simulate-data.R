test_that("ssd_sim_data2.function works", {
  withr::local_seed(42)
  scenario <- ssd_sim_data2(ssdtools::ssd_rlnorm, nrow = 5, nsim = 10)
  data <- materialize_jobs(scenario)
  expect_snapshot(data)
  data <- tidyr::unnest(data, cols = c(data))
  expect_snapshot_data(data, "function_unnest")
})

test_that("ssd_sim_data2.function vectorized", {
  withr::local_seed(42)
  scenario <- ssd_sim_data2(ssdtools::ssd_rlnorm, nrow = c(5, 10), nsim = 2)
  data <- materialize_jobs(scenario)
  expect_snapshot(data)
  data <- tidyr::unnest(data, cols = c(data))
  expect_snapshot_data(data, "function_vectorize")
})

test_that("ssd_sim_data2.character works", {
  withr::local_seed(42)
  scenario <- ssd_sim_data2("rlnorm", nrow = 5, nsim = 10)
  data <- materialize_jobs(scenario)
  expect_snapshot(data)
  data <- tidyr::unnest(data, cols = c(data))
  expect_snapshot_data(data, "character_unnest")
})

test_that("ssd_sim_data2.character vectorized", {
  withr::local_seed(42)
  scenario <- ssd_sim_data2("rlnorm", nrow = c(5, 10), nsim = 2)
  data <- materialize_jobs(scenario)
  expect_snapshot(data)
  data <- tidyr::unnest(data, cols = c(data))
  expect_snapshot_data(data, "character_vectorize")
})

test_that("ssd_sim_data2.fitdists works top", {
  data <- ssddata::ccme_boron
  fit <- ssdtools::ssd_fit_dists(data, dists = c("lnorm", "gamma"))
  withr::local_seed(42)
  scenario <- ssd_sim_data2(fit, dist_sim = "lnorm", nrow = 5, nsim = 10)
  data <- materialize_jobs(scenario)
  expect_snapshot(data)
  data <- tidyr::unnest(data, cols = c(data))
  expect_snapshot_data(data, "fitdists_top_unnest")
})

test_that("ssd_sim_data2.fitdists works multi", {
  data <- ssddata::ccme_boron
  fit <- ssdtools::ssd_fit_dists(data, dists = c("lnorm", "gamma"))
  withr::local_seed(42)
  scenario <- ssd_sim_data2(fit, dist_sim = "multi", nrow = 5, nsim = 10)
  data <- materialize_jobs(scenario)
  expect_snapshot(data)
  data <- tidyr::unnest(data, cols = c(data))
  expect_snapshot_data(data, "fitdists_multi_unnest")
})

test_that("ssd_sim_data2.fitdists works name", {
  data <- ssddata::ccme_boron
  fit <- ssdtools::ssd_fit_dists(data, dists = c("lnorm", "gamma"))
  withr::local_seed(42)
  scenario <- ssd_sim_data2(fit, dist_sim = "gamma", nrow = 5, nsim = 10)
  data <- materialize_jobs(scenario)
  expect_snapshot(data)
  data <- tidyr::unnest(data, cols = c(data))
  expect_snapshot_data(data, "fitdists_name_unnest")
})

test_that("ssd_sim_data2.fitdists works vectorized", {
  data <- ssddata::ccme_boron
  fit <- ssdtools::ssd_fit_dists(data, dists = c("lnorm", "gamma"))
  withr::local_seed(42)
  scenario <- ssd_sim_data2(
    fit,
    dist_sim = c("top", "multi", "lnorm"),
    nrow = c(5, 10),
    nsim = 2
  )
  data <- materialize_jobs(scenario)
  expect_snapshot(data)
  data <- tidyr::unnest(data, cols = c(data))
  expect_snapshot_data(data, "fitdists_name_vectorize")
})

test_that("ssd_sim_data2.fitdists works all vectorized", {
  data <- ssddata::ccme_boron
  fit <- ssdtools::ssd_fit_dists(data, dists = c("lnorm", "gamma"))
  withr::local_seed(42)
  scenario <- ssd_sim_data2(
    fit,
    dist_sim = c("multi", "lnorm", "all"),
    nrow = c(5, 10),
    nsim = 2
  )
  data <- materialize_jobs(scenario)
  expect_snapshot(data)
  data <- tidyr::unnest(data, cols = c(data))
  expect_snapshot_data(data, "fitdists_name_all_vectorize")
})

test_that("ssd_sim_data2.tmbfit works", {
  data <- ssddata::ccme_boron
  tmbfit <- ssdtools::ssd_fit_dists(data, dists = "lnorm")[[1]]
  withr::local_seed(42)
  scenario <- ssd_sim_data2(tmbfit, nrow = 5, nsim = 10)
  data <- materialize_jobs(scenario)
  expect_snapshot(data)
  data <- tidyr::unnest(data, cols = c(data))
  expect_snapshot_data(data, "tmbfit_unnest")
})

test_that("ssd_sim_data2.data.frame works", {
  withr::local_seed(42)
  scenario <- ssd_sim_data2(ssddata::ccme_boron, nrow = 5, nsim = 10)
  data <- materialize_jobs(scenario)
  expect_snapshot(data)
  data <- tidyr::unnest(data, cols = c(data))
  expect_snapshot_data(data, "data_frame_unnest")
})

test_that("ssd_sim_data2.data.frame vectorized", {
  withr::local_seed(42)
  scenario <- ssd_sim_data2(ssddata::ccme_boron, nrow = c(5, 10), nsim = 2)
  data <- materialize_jobs(scenario)
  expect_snapshot(data)
  data <- tidyr::unnest(data, cols = c(data))
  expect_snapshot_data(data, "data_frame_vectorize")
})
