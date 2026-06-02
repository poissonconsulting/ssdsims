# ---- sample task table -----------------------------------------------------

test_that("task-lists: sample table has D * nsim * R rows with axes populated", {
  scenario <- ssd_define_scenario(
    ssd_data(boron = ssddata::ccme_boron, cadmium = ssddata::ccme_cadmium),
    nsim = 3L,
    nrow = c(5L, 10L),
    seed = 42L
  )
  tasks <- ssd_scenario_sample_tasks(scenario)
  # D = 2 datasets, nsim = 3, R = 1 (replace defaults to FALSE)
  expect_identical(nrow(tasks), 6L)
  expect_setequal(tasks$dataset, c("boron", "cadmium"))
  expect_setequal(tasks$sim, 1:3)
  expect_identical(unique(tasks$replace), FALSE)
})

test_that("task-lists: nrow does not multiply the sample draw; n_max is carried", {
  scenario <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 4L,
    nrow = c(5L, 10L, 20L),
    seed = 42L
  )
  tasks <- ssd_scenario_sample_tasks(scenario)
  # 1 dataset * 4 sims * 1 replace = 4, not multiplied by the 3 nrow values
  expect_identical(nrow(tasks), 4L)
  expect_false("nrow" %in% names(tasks))
  expect_identical(unique(tasks$n_max), 20L)
})

test_that("task-lists: sample derivation is RNG-free with no seeding columns", {
  scenario <- ssd_define_scenario(ssddata::ccme_boron, nsim = 5L, seed = 42L)
  withr::local_seed(1L)
  before <- .Random.seed
  tasks <- ssd_scenario_sample_tasks(scenario)
  expect_identical(.Random.seed, before)
  expect_false(any(c("seed", "primer", "stream") %in% names(tasks)))
})

# ---- data task table -------------------------------------------------------

test_that("task-lists: data table crosses sample identity with nrow", {
  scenario <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 4L,
    nrow = c(5L, 10L, 20L),
    seed = 42L
  )
  sample_tasks <- ssd_scenario_sample_tasks(scenario)
  data_tasks <- ssd_scenario_data_tasks(scenario)
  # nrow is a genuine axis of the (RNG-free) truncation step
  expect_identical(nrow(data_tasks), nrow(sample_tasks) * 3L)
  expect_setequal(data_tasks$nrow, c(5L, 10L, 20L))
})

# ---- fit task table --------------------------------------------------------

test_that("task-lists: fit table has M * F rows with parent identity + fit grid", {
  scenario <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 3L,
    nrow = c(5L, 10L),
    seed = 42L,
    rescale = c(FALSE, TRUE)
  )
  data_tasks <- ssd_scenario_data_tasks(scenario)
  fit_tasks <- ssd_scenario_fit_tasks(scenario)
  # M = 6 data tasks (3 sim * 2 nrow), F = 2 (rescale)
  expect_identical(nrow(fit_tasks), nrow(data_tasks) * 2L)
  expect_true(all(c("dataset", "sim", "replace", "nrow") %in% names(fit_tasks)))
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

# ---- task ids / foreign keys -----------------------------------------------

test_that("task-lists: each table carries a path-style id and parent foreign key", {
  scenario <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 2L,
    nrow = c(5L, 10L),
    seed = 42L
  )
  tasks <- ssd_scenario_tasks(scenario)
  # path-style primary keys
  expect_true(all(grepl("^dataset=", tasks$sample$sample_id)))
  expect_setequal(
    tasks$sample$sample_id,
    c(
      "dataset=ccme_boron/sim=1/replace=FALSE",
      "dataset=ccme_boron/sim=2/replace=FALSE"
    )
  )
  # data id extends its sample foreign key with the nrow axis
  expect_true(all(
    startsWith(tasks$data$data_id, tasks$data$sample_id)
  ))
  expect_true(all(grepl("/nrow=", tasks$data$data_id)))
  # every foreign key resolves to a parent primary key
  expect_true(all(tasks$data$sample_id %in% tasks$sample$sample_id))
  expect_true(all(tasks$fit$data_id %in% tasks$data$data_id))
  expect_true(all(tasks$hc$fit_id %in% tasks$fit$fit_id))
  # primary keys are unique
  expect_false(anyDuplicated(tasks$hc$hc_id) > 0L)
})

# ---- ssdsims_tasks class ---------------------------------------------------

test_that("task-lists: derived tables carry the ssdsims_tasks class and step", {
  scenario <- ssd_define_scenario(ssddata::ccme_boron, nsim = 2L, seed = 42L)
  expect_identical(
    attr(ssd_scenario_sample_tasks(scenario), "step"),
    "sample"
  )
  expect_identical(attr(ssd_scenario_data_tasks(scenario), "step"), "data")
  expect_identical(attr(ssd_scenario_fit_tasks(scenario), "step"), "fit")
  expect_identical(attr(ssd_scenario_hc_tasks(scenario), "step"), "hc")
  expect_s3_class(ssd_scenario_data_tasks(scenario), "ssdsims_tasks")
  expect_s3_class(ssd_scenario_data_tasks(scenario), "tbl_df")
})

test_that("task-lists: task tables survive dplyr/tidyr verbs", {
  scenario <- ssd_define_scenario(ssddata::ccme_boron, nsim = 3L, seed = 42L)
  tasks <- ssd_scenario_sample_tasks(scenario)
  expect_identical(nrow(dplyr::filter(tasks, sim == 2L)), 1L)
  expect_true("extra" %in% names(dplyr::mutate(tasks, extra = 1L)))
  expect_identical(nrow(dplyr::arrange(tasks, dplyr::desc(sim))), 3L)
})

test_that("task-lists: printing a task table is informative", {
  expect_snapshot(
    ssd_scenario_sample_tasks(
      ssd_define_scenario(ssddata::ccme_boron, nsim = 2L, seed = 42L)
    )
  )
  expect_snapshot(
    ssd_scenario_data_tasks(
      ssd_define_scenario(
        ssddata::ccme_boron,
        nsim = 1L,
        nrow = c(5L, 10L),
        seed = 42L
      )
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

# ---- compound expansion ----------------------------------------------------

test_that("task-lists: ssd_scenario_tasks bundles the four task tables", {
  scenario <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 2L,
    nrow = c(5L, 10L),
    seed = 42L,
    rescale = c(FALSE, TRUE),
    ci = c(FALSE, TRUE)
  )
  tasks <- ssd_scenario_tasks(scenario)
  expect_s3_class(tasks, "ssdsims_task_set")
  expect_named(tasks, c("sample", "data", "fit", "hc"))
  expect_identical(tasks$sample, ssd_scenario_sample_tasks(scenario))
  expect_identical(tasks$data, ssd_scenario_data_tasks(scenario))
  expect_identical(tasks$fit, ssd_scenario_fit_tasks(scenario))
  expect_identical(tasks$hc, ssd_scenario_hc_tasks(scenario))
})

test_that("task-lists: printing a task set reports per-step counts", {
  expect_snapshot(
    ssd_scenario_tasks(
      ssd_define_scenario(
        ssddata::ccme_boron,
        nsim = 2L,
        nrow = c(5L, 10L),
        seed = 42L,
        rescale = c(FALSE, TRUE),
        ci = c(FALSE, TRUE),
        nboot = c(10L, 100L)
      )
    )
  )
})

# ---- baseline runner -------------------------------------------------------

test_that("task-lists: baseline runner threads sample -> data -> fit -> hc", {
  scenario <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 1L,
    nrow = c(5L, 6L),
    seed = 42L,
    dists = "lnorm"
  )
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  out <- withr::with_seed(
    42L,
    ssd_run_scenario_baseline(scenario, ssddata::ccme_boron)
  )
  expect_named(out, c("sample", "data", "fit", "hc"))
  expect_s3_class(out$sample$sample[[1L]], "data.frame")
  expect_s3_class(out$fit$fits[[1L]], "fitdists")
  expect_s3_class(out$hc$hc[[1L]], "data.frame")
  # the data truncation is head(sample, nrow) of the shared draw
  expect_identical(nrow(out$sample$sample[[1L]]), 6L)
  expect_identical(
    vapply(out$data$data, nrow, integer(1)),
    c(5L, 6L)
  )
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
    nrow = c(5L, 10L),
    seed = 42L,
    ci = c(FALSE, TRUE)
  )
  expect_snapshot({
    names(ssd_scenario_sample_tasks(scenario))
    names(ssd_scenario_data_tasks(scenario))
    names(ssd_scenario_fit_tasks(scenario))
    names(ssd_scenario_hc_tasks(scenario))
  })
})
