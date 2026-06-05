test_that("hc_sims 1 sim", {
  withr::with_seed(42, {
    sims <- ssd_sim_data("rlnorm", seed = 10, nsim = 1L)
    sims <- ssd_fit_dists_sims(sims, seed = 10)
    sims <- ssd_hc_sims(sims)
  })
  sims <- tidyr::unnest(sims, cols = c(hc))
  expect_snapshot_data(sims, "hc_sims1")
})

test_that("hc_sims 1 sim ci", {
  withr::with_seed(42, {
    sims <- ssd_sim_data("rlnorm", seed = 10, nsim = 1L)
    sims <- ssd_fit_dists_sims(sims, seed = 10)
    sims <- ssd_hc_sims(sims, ci = TRUE, nboot = 2L)
  })
  sims <- tidyr::unnest(sims, cols = c(hc))
  expect_snapshot_data(sims, "hc_sims1ci")
})

test_that("hazard-concentrations: a vector ci is rejected", {
  withr::with_seed(42, {
    sims <- ssd_sim_data("rlnorm", seed = 10, nsim = 1L)
    sims <- ssd_fit_dists_sims(sims, seed = 10)
  })
  expect_snapshot(error = TRUE, {
    ssd_hc_sims(sims, ci = c(FALSE, TRUE))
  })
})

test_that("hazard-concentrations: est is identical for ci = FALSE and ci = TRUE", {
  withr::with_seed(42, {
    sims <- ssd_sim_data("rlnorm", seed = 10, nsim = 1L)
    sims <- ssd_fit_dists_sims(sims, seed = 10)
    no_ci <- ssd_hc_sims(sims, ci = FALSE)
    with_ci <- ssd_hc_sims(sims, ci = TRUE, nboot = 2L)
  })
  expect_identical(
    tidyr::unnest(no_ci, cols = c(hc))$est,
    tidyr::unnest(with_ci, cols = c(hc))$est
  )
})
