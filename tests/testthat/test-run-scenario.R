test_that("ssd_run_scenario.data.frame works", {
  skip_if_not_installed("dqrng")
  with_lecuyer_cmrg_seed(10, {
    scenario <- ssd_run_scenario(
      ssddata::ccme_boron,
      replace = c(TRUE, FALSE),
      nsim = 2
    )
  })

  scenariod <- tidyr::unnest(scenario, cols = data)
  expect_snapshot_data(scenariod, "scenarioTF2_data")

  scenario <- tidyr::unnest(scenario, cols = hc)
  expect_snapshot_data(scenario, "scenarioTF2")
})

test_that("ssd_run_scenario.data.frame errors unknown argument", {
  skip_if_not_installed("dqrng")
  chk::expect_chk_error(
    ssd_run_scenario(
      ssddata::ccme_boron,
      replace = c(TRUE, FALSE),
      nsim = 2,
      thingy = 1
    ),
    "thingy"
  )
})

test_that("ssd_run_scenario.data.frame passes vectorized arguments fits", {
  skip_if_not_installed("dqrng")
  # Pin the seed so the sampled data (and therefore the fitted coefficients)
  # are deterministic on their own, rather than depending on the ambient RNG
  # state left by earlier tests in the session.
  with_lecuyer_cmrg_seed(10, {
    scenario <- ssd_run_scenario(
      ssddata::ccme_boron,
      nrow = 5,
      nsim = 1,
      rescale = c(TRUE, FALSE)
    )
  })
  coef <- coef(scenario$fits[[1]])
  expect_snapshot_data(coef, "coef_rescale1")
  coef <- coef(scenario$fits[[2]])
  expect_snapshot_data(coef, "coef_rescale2")
})

test_that("ssd_run_scenario.data.frame errors min_pboot", {
  skip_if_not_installed("dqrng")
  expect_error(
    ssd_run_scenario(
      ssddata::ccme_boron,
      nrow = 5,
      nsim = 1,
      ci = TRUE,
      nboot = 2L,
      min_pboot = 0
    ),
    "`min_pboot` is fixed at 0 in ssdsims and cannot be set by the user\\."
  )
})

test_that("ssd_run_scenario.function works", {
  skip_if_not_installed("dqrng")
  with_lecuyer_cmrg_seed(10, {
    scenario <- ssd_run_scenario(rlnorm, nsim = 2)
  })

  scenariod <- tidyr::unnest(scenario, cols = data)
  expect_snapshot_data(scenariod, "scenarioFun2_data")

  scenario <- tidyr::unnest(scenario, cols = hc)
  expect_snapshot_data(scenario, "scenarioFun2")
})

test_that("ssd_run_scenario.character works", {
  skip_if_not_installed("dqrng")
  with_lecuyer_cmrg_seed(10, {
    scenario <- ssd_run_scenario("rlnorm", nsim = 2)
  })

  scenariod <- tidyr::unnest(scenario, cols = data)
  expect_snapshot_data(scenariod, "scenarioStr2_data")

  scenario <- tidyr::unnest(scenario, cols = hc)
  expect_snapshot_data(scenario, "scenarioStr2")
})

test_that("ssd_run_scenario.tmbfit works", {
  skip_if_not_installed("dqrng")
  with_lecuyer_cmrg_seed(10, {
    fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron)
    scenario <- ssd_run_scenario(fit[[1]], nsim = 2)
  })

  scenariod <- tidyr::unnest(scenario, cols = data)
  expect_snapshot_data(scenariod, "scenarioTMB2_data")

  scenario <- tidyr::unnest(scenario, cols = hc)
  expect_snapshot_data(scenario, "scenarioTMB2")
})

test_that("ssd_run_scenario.fitdist works", {
  skip_if_not_installed("dqrng")
  with_lecuyer_cmrg_seed(10, {
    fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron)
    scenario <- ssd_run_scenario(fit, dist_sim = c("lnorm", "top"), nsim = 2)
  })

  scenario <- tidyr::unnest(scenario, cols = data)
  expect_snapshot_data(scenario, "scenarioFit2_data")

  scenario <- tidyr::unnest(scenario, cols = hc)
  expect_snapshot_data(scenario, "scenarioFit2")
})

test_that("ssd_run_scenario.tmbfit uses the tmbfit's fitted estimates", {
  skip_if_not_installed("dqrng")
  # Regression test: ssd_run_scenario.tmbfit computed the estimates from the
  # tmbfit but dropped them on the recursive call, so the rng function ran
  # with default parameters instead of the fitted ones. ssd_sim_data.tmbfit
  # threads them through correctly, so the two paths should agree.
  fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron, dists = "lnorm")
  tmbfit <- fit[[1]]

  via_run <- with_lecuyer_cmrg_seed(10, ssd_run_scenario(tmbfit, nsim = 2))
  via_sim <- with_lecuyer_cmrg_seed(10, ssd_sim_data(tmbfit, nsim = 2))

  expect_equal(via_run$data, via_sim$data)
})
