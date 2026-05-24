test_that("ssd_scenario_tasks returns a non-empty grid for a basic scenario", {
  s <- ssd_sim_data2(ssddata::ccme_boron, nsim = 2)
  tasks <- ssd_scenario_tasks(s)
  expect_s3_class(tasks, "tbl_df")
  expect_true(nrow(tasks) > 0)
  expect_true(all(c("sim", "stream", "nrow", "replace") %in% names(tasks)))
})

test_that("ssd_run_job on a single task matches ssd_run_scenario2 row", {
  s <- ssd_sim_data2(
    ssddata::ccme_boron,
    nrow = c(5L, 10L),
    nsim = 2L,
    nboot = 50L,
    seed = 42
  )
  tasks <- ssd_scenario_tasks(s)
  job <- ssd_run_job(tasks[2, , drop = FALSE], s)
  full <- ssd_run_scenario2(s)
  expect_equal(nrow(job), 1L)
  expect_equal(job$sim, full$sim[2])
  expect_equal(job$nrow, full$nrow[2])
  expect_equal(job$hc, full$hc[2])
})

test_that("parquet round-trip preserves task metadata and fits class", {
  skip_if_not_installed("duckplyr")
  skip_if_not_installed("qs2")

  s <- ssd_sim_data2(
    ssddata::ccme_boron,
    nrow = 6L,
    nsim = 1L,
    nboot = 50L,
    seed = 42
  )
  job <- ssd_run_job(ssd_scenario_tasks(s), s)

  path <- tempfile(fileext = ".parquet")
  ssd_write_job_parquet(job, path)
  back <- ssd_read_job_parquet(path)

  expect_equal(nrow(back), nrow(job))
  expect_setequal(intersect(names(job), names(back)), names(job))
  expect_s3_class(back$fits[[1]], "fitdists")
  expect_true(is.function(back$min_pmix[[1]]))
})

test_that("ssd_run_job on empty tasks returns empty result with right cols", {
  s <- ssd_sim_data2(ssddata::ccme_boron, nsim = 2)
  tasks <- ssd_scenario_tasks(s)
  empty <- ssd_run_job(tasks[0, , drop = FALSE], s)
  expect_equal(nrow(empty), 0L)
  expect_true(all(c("data", "fits", "hc") %in% names(empty)))
})
