test_that("fit_dists_sims edge cases", {
  rlang::local_options(
    pillar.bold = NULL,
    pillar.max_footer_lines = NULL,
    pillar.max_title_chars = NULL,
    pillar.min_title_chars = NULL
  )

  with_lecuyer_cmrg_seed(10, {
    data <- ssd_sim_data("rlnorm", nsim = 1)
    expect_snapshot(ssd_fit_dists_sims(data))
  })
})

test_that("fit_dists_sims no seed", {
  rlang::local_options(
    pillar.bold = NULL,
    pillar.max_footer_lines = NULL,
    pillar.max_title_chars = NULL,
    pillar.min_title_chars = NULL
  )

  data <- ssd_sim_data("rlnorm", nsim = 1)
  expect_snapshot(ssd_fit_dists_sims(data))
})

test_that("fit_dists_sims 1 sim", {
  rlang::local_options(
    pillar.bold = NULL,
    pillar.max_footer_lines = NULL,
    pillar.max_title_chars = NULL,
    pillar.min_title_chars = NULL
  )

  with_lecuyer_cmrg_seed(10, {
    data <- ssd_sim_data("rlnorm", nsim = 1L)
    fits <- ssd_fit_dists_sims(data, seed = 10)
  })
  expect_snapshot(fits)
  expect_snapshot_data(ssdtools::ssd_gof(fits$fits[[1]], wt = TRUE), "fits1")
})
