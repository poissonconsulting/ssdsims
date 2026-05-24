test_that("ssd_run_scenario2 data.frame works", {
  with_lecuyer_cmrg_seed(10, {
    scenario <- ssd_run_scenario2(ssd_sim_data2(
      ssddata::ccme_boron,
      replace = c(TRUE, FALSE),
      nsim = 2
    ))
  })

  scenariod <- tidyr::unnest(scenario, cols = data)
  expect_snapshot_data(scenariod, "scenario2TF2_data")

  scenario <- tidyr::unnest(scenario, cols = hc)
  expect_snapshot_data(scenario, "scenario2TF2")
})

test_that("ssd_run_scenario2 errors unknown argument at construction", {
  chk::expect_chk_error(
    ssd_sim_data2(
      ssddata::ccme_boron,
      replace = c(TRUE, FALSE),
      nsim = 2,
      thingy = 1
    ),
    "thingy"
  )
})

test_that("ssd_run_scenario2 data.frame vectorized rescale", {
  scenario <- ssd_run_scenario2(ssd_sim_data2(
    ssddata::ccme_boron,
    nrow = 5,
    nsim = 1,
    rescale = c(TRUE, FALSE),
    seed = 1
  ))
  coef <- coef(scenario$fits[[1]])
  expect_snapshot_data(coef, "scenario2_coef_rescale1")
  coef <- coef(scenario$fits[[2]])
  expect_snapshot_data(coef, "scenario2_coef_rescale2")
})

test_that("ssd_run_scenario2 function works", {
  with_lecuyer_cmrg_seed(10, {
    scenario <- ssd_run_scenario2(ssd_sim_data2(rlnorm, nsim = 2))
  })

  scenariod <- tidyr::unnest(scenario, cols = data)
  expect_snapshot_data(scenariod, "scenario2Fun2_data")

  scenario <- tidyr::unnest(scenario, cols = hc)
  expect_snapshot_data(scenario, "scenario2Fun2")
})

test_that("ssd_run_scenario2 character works", {
  with_lecuyer_cmrg_seed(10, {
    scenario <- ssd_run_scenario2(ssd_sim_data2("rlnorm", nsim = 2))
  })

  scenariod <- tidyr::unnest(scenario, cols = data)
  expect_snapshot_data(scenariod, "scenario2Str2_data")

  scenario <- tidyr::unnest(scenario, cols = hc)
  expect_snapshot_data(scenario, "scenario2Str2")
})

test_that("ssd_run_scenario2 tmbfit works", {
  with_lecuyer_cmrg_seed(10, {
    fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron)
    scenario <- ssd_run_scenario2(ssd_sim_data2(fit[[1]], nsim = 2))
  })

  scenariod <- tidyr::unnest(scenario, cols = data)
  expect_snapshot_data(scenariod, "scenario2TMB2_data")

  scenario <- tidyr::unnest(scenario, cols = hc)
  expect_snapshot_data(scenario, "scenario2TMB2")
})

test_that("ssd_run_scenario2 fitdists works", {
  with_lecuyer_cmrg_seed(10, {
    fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron)
    scenario <- ssd_run_scenario2(
      ssd_sim_data2(fit, dist_sim = c("lnorm", "top"), nsim = 2)
    )
  })

  scenario <- tidyr::unnest(scenario, cols = data)
  expect_snapshot_data(scenario, "scenario2Fit2_data")

  scenario <- tidyr::unnest(scenario, cols = hc)
  expect_snapshot_data(scenario, "scenario2Fit2")
})

test_that("ssd_run_scenario2 reproducible with seed", {
  scenario <- ssd_sim_data2(ssddata::ccme_boron, nsim = 2, seed = 42)
  a <- ssd_run_scenario2(scenario)
  b <- ssd_run_scenario2(scenario)
  # TMB carries opaque pointers in $fits; compare stable artefacts instead.
  expect_equal(a$data, b$data)
  expect_equal(a$hc, b$hc)
  expect_equal(lapply(a$fits, coef), lapply(b$fits, coef))
})

test_that("ssd_run_scenario2 input must be ssdsims_scenario", {
  chk::expect_chk_error(ssd_run_scenario2(ssddata::ccme_boron))
})

test_that("ssd_run_scenario2 ... must be empty", {
  scenario <- ssd_sim_data2(ssddata::ccme_boron, nsim = 2)
  chk::expect_chk_error(ssd_run_scenario2(scenario, thingy = 1))
})
