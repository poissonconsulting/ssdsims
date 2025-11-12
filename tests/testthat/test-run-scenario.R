test_that("ssd_run_scenario.data.frame works", {
  with_lecuyer_cmrg_seed(10, {
    scenario <- ssd_run_scenario(ssddata::ccme_boron, replace = c(TRUE, FALSE), nsim = 2)
  })
  
  scenariod <- tidyr::unnest(scenario, cols = data)
   expect_snapshot_data(scenariod, "scenarioTF2_data")

  scenario <- tidyr::unnest(scenario, cols = hc)
   expect_snapshot_data(scenario, "scenarioTF2")

})

test_that("ssd_run_scenario.data.frame errors unknown argument", {
  chk::expect_chk_error(ssd_run_scenario(ssddata::ccme_boron, replace = c(TRUE, FALSE), nsim = 2, thingy = 1), "thingy")
})

test_that("ssd_run_scenario.data.frame passes vectorized arguments fits", {
  scenario <- ssd_run_scenario(ssddata::ccme_boron, nrow = 5, nsim = 1, rescale = c(TRUE, FALSE))
  coef <- coef(scenario$fits[[1]])
  expect_snapshot_data(coef, "coef_rescale1")
  coef <- coef(scenario$fits[[2]])
  expect_snapshot_data(coef, "coef_rescale2")
})

test_that("ssd_run_scenario.data.frame errors min_pboot", {
  expect_error(ssd_run_scenario(ssddata::ccme_boron, nrow = 5, nsim = 1, ci = TRUE, nboot = 3, min_pboot = 0), "`min_pboot` is fixed at 0 in ssdsims and cannot be set by the user\\.")
})

test_that("ssd_run_scenario.function works", {
  with_lecuyer_cmrg_seed(10, {
    scenario <- ssd_run_scenario(rlnorm, nsim = 2)
  })

  scenariod <- tidyr::unnest(scenario, cols = data)
   expect_snapshot_data(scenariod, "scenarioFun2_data")

  scenario <- tidyr::unnest(scenario, cols = hc)
   expect_snapshot_data(scenario, "scenarioFun2")
})

test_that("ssd_run_scenario.character works", {
  with_lecuyer_cmrg_seed(10, {
    scenario <- ssd_run_scenario("rlnorm", nsim = 2)
  })

  scenariod <- tidyr::unnest(scenario, cols = data)
   expect_snapshot_data(scenariod, "scenarioStr2_data")
  
  scenario <- tidyr::unnest(scenario, cols = hc)
   expect_snapshot_data(scenario, "scenarioStr2")
})

test_that("ssd_run_scenario.tmbfit works", {
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
  with_lecuyer_cmrg_seed(10, {
    fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron) 
    scenario <- ssd_run_scenario(fit, dist_sim = c("lnorm", "top"), nsim = 2)
  })

  scenario <- tidyr::unnest(scenario, cols = data)
   expect_snapshot_data(scenario, "scenarioFit2_data")

  scenario <- tidyr::unnest(scenario, cols = hc)
   expect_snapshot_data(scenario, "scenarioFit2")
})
