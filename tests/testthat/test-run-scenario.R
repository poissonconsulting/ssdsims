test_that("ssd_run_scenario.data.frame works", {
  with_lecuyer_cmrg_seed(10, {
    scenario <- ssd_run_scenario(ssddata::ccme_boron, replace = c(TRUE, FALSE), nsim = 2)
  })
  scenario$fits <- NULL
  scenario <- tidyr::unnest(scenario, cols = hc)
   expect_snapshot_data(scenario, "scenarioTF2")
  
  scenario <- tidyr::unnest(scenario, cols = data)
   expect_snapshot_data(scenario, "scenarioTF2_data")
})

test_that("ssd_run_scenario.function works", {
  with_lecuyer_cmrg_seed(10, {
    scenario <- ssd_run_scenario(rlnorm, nsim = 2)
  })
  scenario$fits <- NULL
  scenario <- tidyr::unnest(scenario, cols = hc)
   expect_snapshot_data(scenario, "scenarioFun2")
  
  scenario <- tidyr::unnest(scenario, cols = data)
   expect_snapshot_data(scenario, "scenarioFun2_data")
})

test_that("ssd_run_scenario.character works", {
  with_lecuyer_cmrg_seed(10, {
    scenario <- ssd_run_scenario("rlnorm", nsim = 2)
  })
  scenario$fits <- NULL
  scenario <- tidyr::unnest(scenario, cols = hc)
   expect_snapshot_data(scenario, "scenarioStr2")
  
  scenario <- tidyr::unnest(scenario, cols = data)
   expect_snapshot_data(scenario, "scenarioStr2_data")
})

test_that("ssd_run_scenario.tmbfit works", {
  with_lecuyer_cmrg_seed(10, {
    fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron) 
    scenario <- ssd_run_scenario(fit[[1]], nsim = 2)
  })
  scenario$fits <- NULL
  scenario <- tidyr::unnest(scenario, cols = hc)
   expect_snapshot_data(scenario, "scenarioTMB2")
  
  scenario <- tidyr::unnest(scenario, cols = data)
   expect_snapshot_data(scenario, "scenarioTMB2_data")
})