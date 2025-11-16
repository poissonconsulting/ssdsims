test_that("fit_dists_sims edge cases", {
  with_lecuyer_cmrg_seed(10, {
    data <- ssd_simulate_data("rlnorm", nsim = 1)
    expect_snapshot(ssd_fit_dists_sims(data))
  })
})

test_that("fit_dists_sims no seed", {
  data <- ssd_simulate_data("rlnorm", nsim = 1)
  expect_snapshot(ssd_fit_dists_sims(data))
})

test_that("fit_dists_sims 1 sim", {
  with_lecuyer_cmrg_seed(10, {
    data <- ssd_simulate_data("rlnorm", nsim = 1L)
    fits <- ssd_fit_dists_sims(data, seed = 10)
  })
  expect_snapshot(fits)
  expect_snapshot_data(ssdtools::ssd_gof(fits$fits[[1]], wt = TRUE), "fits1")
})
