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

test_that("hazard-concentrations: multiple est_methods do not multiply rows", {
  withr::with_seed(42, {
    sims <- ssd_sim_data("rlnorm", seed = 10, nsim = 1L)
    sims <- ssd_fit_dists_sims(sims, seed = 10)
    n_in <- nrow(sims)
    out <- ssd_hc_sims(
      sims,
      ci = TRUE,
      nboot = 2L,
      est_method = c("arithmetic", "multi")
    )
  })
  # `est_method` is summarised within each cell, so the outer tibble keeps its
  # N rows; each row's `hc` tibble covers both methods from one bootstrap.
  expect_identical(nrow(out), n_in)
  hc <- out$hc[[1L]]
  expect_setequal(unique(hc$est_method), c("arithmetic", "multi"))
  for (g in split(hc, hc$proportion)) {
    expect_identical(length(unique(g$lcl)), 1L)
    expect_identical(length(unique(g$ucl)), 1L)
  }
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
