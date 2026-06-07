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
  # se/lcl/ucl come from the unseeded bootstrap, which draws off the ambient
  # RNG state. That state differs between CRAN and dev withr (withr #286/#287:
  # local_seed() leaks vs restores .Random.seed), so the interval is snapshotted
  # separately in the test below and excluded here.
  sims <- dplyr::select(sims, !c("se", "lcl", "ucl"))
  expect_snapshot_data(sims, "hc_sims1ci")
})

test_that("hc_sims 1 sim ci bootstrap interval (dev withr only)", {
  skip_if_withr_seed_leak()
  withr::with_seed(42, {
    sims <- ssd_sim_data("rlnorm", seed = 10, nsim = 1L)
    sims <- ssd_fit_dists_sims(sims, seed = 10)
    sims <- ssd_hc_sims(sims, ci = TRUE, nboot = 2L)
  })
  sims <- tidyr::unnest(sims, cols = c(hc))
  ci <- signif(unlist(sims[1, c("se", "lcl", "ucl")]), 6)
  # An md snapshot (not a separate file) so a CRAN-withr run, where this test
  # skips, preserves it instead of treating it as an orphaned file snapshot.
  expect_snapshot(ci)
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
