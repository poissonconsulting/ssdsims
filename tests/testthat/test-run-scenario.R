test_that("ssd_run_scenario works", {
  with_lecuyer_cmrg_seed(10, {
    scenario <- ssd_run_scenario(ssddata::ccme_boron, replace = c(TRUE, FALSE), nsim = 2)
  })
  scenario <- tidyr::unnest(scenario, cols = hc)
   expect_snapshot_data(scenario, "scenarioTF2")
  
  scenario <- tidyr::unnest(scenario, cols = data)
   expect_snapshot_data(scenario, "scenarioTF2_data")
})
