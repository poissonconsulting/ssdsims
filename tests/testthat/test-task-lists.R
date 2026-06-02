# ---- data task table -------------------------------------------------------

test_that("task-lists: data table has D * nsim * R rows with axes populated", {
  scenario <- ssd_define_scenario(
    ssd_data(boron = ssddata::ccme_boron, cadmium = ssddata::ccme_cadmium),
    nsim = 3L,
    nrow = c(5L, 10L),
    seed = 42L
  )
  tasks <- ssd_scenario_data_tasks(scenario)
  # D = 2 datasets, nsim = 3, R = 1 (replace defaults to FALSE)
  expect_identical(nrow(tasks), 6L)
  expect_setequal(tasks$dataset, c("boron", "cadmium"))
  expect_setequal(tasks$sim, 1:3)
  expect_identical(unique(tasks$replace), FALSE)
})

test_that("task-lists: nrow is carried but does not multiply the row count", {
  scenario <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 4L,
    nrow = c(5L, 10L, 20L),
    seed = 42L
  )
  tasks <- ssd_scenario_data_tasks(scenario)
  # 1 dataset * 4 sims * 1 replace = 4, not multiplied by the 3 nrow values
  expect_identical(nrow(tasks), 4L)
  expect_true("nrow" %in% names(tasks))
  expect_identical(tasks$nrow[[1L]], c(5L, 10L, 20L))
})

test_that("task-lists: data derivation is RNG-free with no seeding columns", {
  scenario <- ssd_define_scenario(ssddata::ccme_boron, nsim = 5L, seed = 42L)
  withr::local_seed(1L)
  before <- .Random.seed
  tasks <- ssd_scenario_data_tasks(scenario)
  expect_identical(.Random.seed, before)
  expect_false(any(c("seed", "primer", "stream") %in% names(tasks)))
})

# ---- fit task table --------------------------------------------------------

test_that("task-lists: fit table has M * F rows with parent identity + fit grid", {
  scenario <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 3L,
    seed = 42L,
    rescale = c(FALSE, TRUE)
  )
  data_tasks <- ssd_scenario_data_tasks(scenario)
  fit_tasks <- ssd_scenario_fit_tasks(scenario)
  # M = 3 data tasks, F = 2 (rescale) * 1 (everything else)
  expect_identical(nrow(fit_tasks), nrow(data_tasks) * 2L)
  expect_true(all(
    c("dataset", "sim", "replace") %in% names(fit_tasks)
  ))
  expect_true(all(
    c(
      "rescale",
      "computable",
      "at_boundary_ok",
      "min_pmix",
      "range_shape1",
      "range_shape2"
    ) %in%
      names(fit_tasks)
  ))
})

test_that("task-lists: min_pmix is stored by name, not function value", {
  scenario <- ssd_define_scenario(ssddata::ccme_boron, nsim = 1L, seed = 42L)
  fit_tasks <- ssd_scenario_fit_tasks(scenario)
  expect_type(fit_tasks$min_pmix, "character")
  expect_identical(unique(fit_tasks$min_pmix), "ssd_min_pmix")
})

# ---- hc task table ---------------------------------------------------------

test_that("task-lists: hc table has K * H rows", {
  scenario <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 2L,
    seed = 42L,
    ci = TRUE,
    nboot = c(10L, 100L),
    est_method = c("arithmetic", "multi")
  )
  fit_tasks <- ssd_scenario_fit_tasks(scenario)
  hc_tasks <- ssd_scenario_hc_tasks(scenario)
  # H = 2 nboot * 2 est_method * 1 ci_method * 1 parametric = 4
  expect_identical(nrow(hc_tasks), nrow(fit_tasks) * 4L)
})

test_that("task-lists: ci = c(FALSE, TRUE) collapses the bootstrap-only knobs", {
  scenario <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 2L,
    seed = 42L,
    ci = c(FALSE, TRUE),
    nboot = c(10L, 100L)
  )
  fit_tasks <- ssd_scenario_fit_tasks(scenario)
  hc_tasks <- ssd_scenario_hc_tasks(scenario)
  # Per fit task: 1 collapsed ci = FALSE row + 2 ci = TRUE rows (nboot) = 3,
  # not the naive 2 ci * 2 nboot = 4 full cross-join.
  expect_identical(nrow(hc_tasks), nrow(fit_tasks) * 3L)
  false_rows <- hc_tasks[hc_tasks$ci == FALSE, ]
  expect_identical(nrow(false_rows), nrow(fit_tasks))
  expect_true(all(is.na(false_rows$nboot)))
  expect_true(all(is.na(false_rows$ci_method)))
  expect_true(all(is.na(false_rows$parametric)))
})

# ---- ssdsims_tasks class ---------------------------------------------------

test_that("task-lists: derived tables carry the ssdsims_tasks class and step", {
  scenario <- ssd_define_scenario(ssddata::ccme_boron, nsim = 2L, seed = 42L)
  data_tasks <- ssd_scenario_data_tasks(scenario)
  fit_tasks <- ssd_scenario_fit_tasks(scenario)
  hc_tasks <- ssd_scenario_hc_tasks(scenario)
  expect_s3_class(data_tasks, "ssdsims_tasks")
  expect_s3_class(fit_tasks, "ssdsims_tasks")
  expect_s3_class(hc_tasks, "ssdsims_tasks")
  expect_s3_class(data_tasks, "tbl_df")
  expect_identical(attr(data_tasks, "step"), "data")
  expect_identical(attr(fit_tasks, "step"), "fit")
  expect_identical(attr(hc_tasks, "step"), "hc")
})

test_that("task-lists: task tables survive dplyr/tidyr verbs", {
  scenario <- ssd_define_scenario(ssddata::ccme_boron, nsim = 3L, seed = 42L)
  tasks <- ssd_scenario_data_tasks(scenario)
  expect_identical(nrow(dplyr::filter(tasks, sim == 2L)), 1L)
  expect_true("extra" %in% names(dplyr::mutate(tasks, extra = 1L)))
  expect_identical(nrow(dplyr::arrange(tasks, dplyr::desc(sim))), 3L)
})

test_that("task-lists: printing a task table is informative", {
  expect_snapshot(
    ssd_scenario_data_tasks(
      ssd_define_scenario(ssddata::ccme_boron, nsim = 2L, seed = 42L)
    )
  )
  expect_snapshot(
    ssd_scenario_fit_tasks(
      ssd_define_scenario(
        ssddata::ccme_boron,
        nsim = 1L,
        seed = 42L,
        rescale = c(FALSE, TRUE)
      )
    )
  )
  expect_snapshot(
    ssd_scenario_hc_tasks(
      ssd_define_scenario(
        ssddata::ccme_boron,
        nsim = 1L,
        seed = 42L,
        ci = c(FALSE, TRUE),
        nboot = c(10L, 100L)
      )
    )
  )
})

# ---- baseline runner -------------------------------------------------------

test_that("task-lists: baseline runner threads data -> fit -> hc and collects", {
  scenario <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 1L,
    nrow = 6L,
    seed = 42L,
    dists = "lnorm"
  )
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  out <- withr::with_seed(
    42L,
    ssd_run_scenario_baseline(scenario, ssddata::ccme_boron)
  )
  expect_named(out, c("data", "fit", "hc"))
  expect_s3_class(out$data$data[[1L]], "data.frame")
  expect_s3_class(out$fit$fits[[1L]], "fitdists")
  expect_s3_class(out$hc$hc[[1L]], "data.frame")
  # No targets machinery and no Parquet I/O at this step.
  expect_false("targets" %in% loadedNamespaces())
  expect_length(list.files(tmp, pattern = "\\.parquet$", recursive = TRUE), 0L)
})

test_that("task-lists: runner errors on missing dataset", {
  scenario <- ssd_define_scenario(
    ssd_data(boron = ssddata::ccme_boron, cadmium = ssddata::ccme_cadmium),
    nsim = 1L,
    seed = 42L,
    dists = "lnorm"
  )
  expect_snapshot(error = TRUE, {
    ssd_run_scenario_baseline(scenario, ssd_data(boron = ssddata::ccme_boron))
  })
})

# ---- column contract -------------------------------------------------------

test_that("task-lists: task-table column contracts are pinned", {
  scenario <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 1L,
    seed = 42L,
    ci = c(FALSE, TRUE)
  )
  expect_snapshot({
    names(ssd_scenario_data_tasks(scenario))
    names(ssd_scenario_fit_tasks(scenario))
    names(ssd_scenario_hc_tasks(scenario))
  })
})
