test_that("fit_dists_sims edge cases", {
  expect_identical(ssd_fit_dists_sims(list()), list())
})

test_that("fit_dists_sims edge cases", {
  datas <- ssd_simulate_data.character("lnorm", nsim = 2)
#  expect_snapshot_data(ssd_fit_dists_sims(datas), "simsedge")
})
