test_that("print works for data.frame source", {
  scenario <- ssd_sim_data2(
    ssddata::ccme_boron,
    replace = c(TRUE, FALSE),
    nsim = 2
  )
  expect_snapshot(print(scenario))
})

test_that("print works for function source", {
  scenario <- ssd_sim_data2(rlnorm, nsim = 2)
  expect_snapshot(print(scenario))
})

test_that("print works for character source", {
  scenario <- ssd_sim_data2("rlnorm", nsim = 2)
  expect_snapshot(print(scenario))
})

test_that("print works for tmbfit source", {
  fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron)
  scenario <- ssd_sim_data2(fit[[1]], nsim = 2)
  expect_snapshot(print(scenario))
})

test_that("print works for fitdists source", {
  fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron)
  scenario <- ssd_sim_data2(fit, dist_sim = c("lnorm", "top"), nsim = 2)
  expect_snapshot(print(scenario))
})

test_that("print reflects custom fit and hc params", {
  scenario <- ssd_sim_data2(
    rlnorm,
    nsim = 3,
    nrow = c(5, 10),
    nboot = 500,
    proportion = c(0.05, 0.1),
    ci = TRUE,
    seed = 42L,
    stream = 2L,
    start_sim = 7L
  )
  expect_snapshot(print(scenario))
})
