test_that("hc_sims 1 sim", {
  withr::with_seed(42, {
  sims <- ssd_simulate_data("lnorm", seed = 10, nsim = 1L)
  sims <- ssd_fit_dists_sims(sims, seed = 10)
  sims <- ssd_hc_sims(sims)
  })
  sims <- tidyr::unnest(sims, cols = c(hc))
  expect_snapshot_data(sims, "hc_sims1")
})
