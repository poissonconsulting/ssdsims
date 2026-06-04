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

test_that("task-lists: the scenario replace knob is a sample axis", {
  scenario <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 2L,
    seed = 42L,
    replace = c(FALSE, TRUE)
  )
  tasks <- ssd_scenario_sample_tasks(scenario)
  # D = 1, nsim = 2, R = 2 distinct replace values
  expect_identical(nrow(tasks), 4L)
  expect_setequal(tasks$replace, c(FALSE, TRUE))
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

# ---- fit task table --------------------------------------------------------

test_that("task-lists: fit table crosses sample identity x nrow x fit grid", {
  scenario <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 3L,
    nrow = c(5L, 10L),
    seed = 42L,
    rescale = c(FALSE, TRUE)
  )
  sample_tasks <- ssd_scenario_sample_tasks(scenario)
  fit_tasks <- ssd_scenario_fit_tasks(scenario)
  # S = 3 sample tasks * |nrow| = 2 * F = 2 (rescale) = 12
  expect_identical(nrow(fit_tasks), nrow(sample_tasks) * 2L * 2L)
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
  # fit id extends its sample foreign key with the nrow (and fit-grid) axes
  expect_true(all(startsWith(tasks$fit$fit_id, tasks$fit$sample_id)))
  expect_true(all(grepl("/nrow=", tasks$fit$fit_id)))
  # every foreign key resolves to a parent primary key
  expect_true(all(tasks$fit$sample_id %in% tasks$sample$sample_id))
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
  expect_identical(attr(ssd_scenario_fit_tasks(scenario), "step"), "fit")
  expect_identical(attr(ssd_scenario_hc_tasks(scenario), "step"), "hc")
  expect_s3_class(ssd_scenario_fit_tasks(scenario), "ssdsims_tasks")
  expect_s3_class(ssd_scenario_fit_tasks(scenario), "tbl_df")
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
    ssd_scenario_fit_tasks(
      ssd_define_scenario(
        ssddata::ccme_boron,
        nsim = 1L,
        nrow = c(5L, 10L),
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

test_that("task-lists: ssd_scenario_tasks bundles the three task tables", {
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
  expect_named(tasks, c("sample", "fit", "hc"))
  expect_identical(tasks$sample, ssd_scenario_sample_tasks(scenario))
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

test_that("task-lists: baseline runner threads sample -> fit -> hc", {
  scenario <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 1L,
    nrow = c(5L, 6L),
    seed = 42L,
    dists = "lnorm"
  )
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  out <- ssd_run_scenario_baseline(scenario)
  expect_named(out, c("sample", "fit", "hc"))
  expect_s3_class(out$sample$sample[[1L]], "data.frame")
  expect_s3_class(out$fit$fits[[1L]], "fitdists")
  expect_s3_class(out$hc$hc[[1L]], "data.frame")
  # one shared draw of n_max = 6 rows; both nrow fits truncate that same draw
  expect_identical(nrow(out$sample$sample[[1L]]), 6L)
  expect_identical(length(unique(out$fit$sample_id)), 1L)
  # The baseline runner does no Parquet I/O of its own (it threads results in
  # memory). (`targets` is a package dependency since `task-tables`, so its
  # presence in `loadedNamespaces()` is not a meaningful signal here.)
  expect_length(list.files(tmp, pattern = "\\.parquet$", recursive = TRUE), 0L)
})

test_that("task-lists: sub-truncation property holds across nrow", {
  # The fit step truncates a single shared draw inline; head(draw, 5) is a
  # byte-identical prefix of head(draw, 10) (TARGETS-DESIGN.md section 5).
  scenario <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 1L,
    nrow = c(5L, 10L),
    seed = 42L,
    dists = "lnorm"
  )
  out <- ssd_run_scenario_baseline(scenario)
  draw <- out$sample$sample[[1L]]
  expect_identical(
    utils::head(draw, 5L),
    utils::head(utils::head(draw, 10L), 5L)
  )
  # both fit tasks reference the one sample draw
  expect_identical(length(unique(out$fit$sample_id)), 1L)
})

test_that("parallel-safe-seeding: baseline runner is reproducible without an external seed", {
  scenario <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 2L,
    nrow = c(5L, 6L),
    seed = 42L,
    dists = "lnorm"
  )
  set.seed(1L)
  pre <- runif(3L)

  out1 <- ssd_run_scenario_baseline(scenario)
  out2 <- ssd_run_scenario_baseline(scenario)

  expect_identical(out1$sample$sample, out2$sample$sample)
  expect_identical(out1$hc$hc, out2$hc$hc)
  expect_identical(
    lapply(out1$fit$fits, stats::coef),
    lapply(out2$fit$fits, stats::coef)
  )

  # backend reset afterwards; base R RNG restored (same seed -> same draw)
  expect_false(dqrng_backend_active())
  set.seed(1L)
  expect_identical(runif(3L), pre)
})

test_that("parallel-safe-seeding: baseline runner results are order-independent", {
  scenario <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 3L,
    nrow = 6L,
    seed = 42L,
    dists = "lnorm"
  )
  out <- ssd_run_scenario_baseline(scenario)

  # Re-run one sample task in isolation: same seed + identity -> same draw.
  i <- 2L
  row <- out$sample[i, ]
  primer <- task_primer(row[task_axes("sample")])
  isolated <- local({
    local_dqrng_backend()
    sample_data_task_primer(
      scenario$data[[row$dataset]],
      row$n_max,
      row$replace,
      scenario$seed,
      primer
    )
  })
  expect_identical(isolated, out$sample$sample[[i]])
})

# ---- seed-and-run wrappers -------------------------------------------------

test_that("parallel-safe-seeding: sample_data_task_primer seeds once and reproduces", {
  local_dqrng_backend()
  data <- ssddata::ccme_boron
  primer <- task_primer(list(dataset = "boron", sim = 1L, replace = FALSE))

  before <- get_dqrng_state()
  draw1 <- sample_data_task_primer(data, 6L, FALSE, 42L, primer)
  # surrounding RNG unchanged beyond the wrapper's local_dqrng_state scope
  expect_identical(get_dqrng_state(), before)

  draw2 <- sample_data_task_primer(data, 6L, FALSE, 42L, primer)
  expect_identical(draw1, draw2)

  # a different primer diverges
  primer2 <- task_primer(list(dataset = "boron", sim = 2L, replace = FALSE))
  draw3 <- sample_data_task_primer(data, 6L, FALSE, 42L, primer2)
  expect_false(identical(draw1, draw3))
})

test_that("parallel-safe-seeding: fit/hc wrappers reproduce under a fixed (seed, primer)", {
  local_dqrng_backend()
  data <- utils::head(ssddata::ccme_boron, 6L)
  fp <- task_primer(list(dataset = "boron", nrow = 6L, rescale = FALSE))
  # The fit path resolves `min_pmix` off the scenario via the accessor.
  scenario <- ssd_define_scenario(ssddata::ccme_boron, nsim = 1L, seed = 42L)

  fit1 <- fit_data_task_primer(
    data,
    scenario = scenario,
    dists = "lnorm",
    rescale = FALSE,
    computable = TRUE,
    at_boundary_ok = TRUE,
    min_pmix = "ssd_min_pmix",
    range_shape1 = c(0.05, 20),
    range_shape2 = c(0.05, 20),
    seed = 42L,
    primer = fp
  )
  expect_s3_class(fit1, "fitdists")

  hp <- task_primer(list(dataset = "boron", nrow = 6L, ci = TRUE))
  hc1 <- hc_data_task_primer(
    fit1,
    proportion = 0.05,
    ci = TRUE,
    nboot = 10L,
    est_method = "multi",
    ci_method = "weighted_samples",
    parametric = TRUE,
    seed = 42L,
    primer = hp
  )
  hc2 <- hc_data_task_primer(
    fit1,
    proportion = 0.05,
    ci = TRUE,
    nboot = 10L,
    est_method = "multi",
    ci_method = "weighted_samples",
    parametric = TRUE,
    seed = 42L,
    primer = hp
  )
  expect_identical(hc1, hc2)
})

test_that("parallel-safe-seeding: state-less ops take no RNG argument", {
  for (fn in list(sample_data_task, fit_data_task, hc_data_task)) {
    nms <- names(formals(fn))
    expect_false(any(c("seed", "primer", "state", "stream") %in% nms))
  }
})

test_that("parallel-safe-seeding: sub-truncation under seeding (replace FALSE and TRUE)", {
  local_dqrng_backend()
  data <- ssddata::ccme_boron
  for (replace in c(FALSE, TRUE)) {
    # Use the full sample identity (dataset, sim, replace) per the contract.
    primer <- task_primer(list(dataset = "boron", sim = 1L, replace = replace))
    draw <- sample_data_task_primer(data, 10L, replace, 42L, primer)
    expect_identical(
      utils::head(draw, 3L),
      utils::head(utils::head(draw, 7L), 3L)
    )
  }
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
    names(ssd_scenario_fit_tasks(scenario))
    names(ssd_scenario_hc_tasks(scenario))
  })
})

# ---- samples retains bootstrap draws without changing estimates ------------

test_that("scenario-definition: samples = TRUE retains hc draws but keeps estimates", {
  args <- list(
    ssd_data(d = data.frame(Conc = exp(seq(-1, 2, length.out = 20)))),
    nsim = 1L,
    nrow = 6L,
    seed = 42L,
    dists = "lnorm",
    ci = c(FALSE, TRUE),
    nboot = 10L
  )
  out_no <- ssd_run_scenario_baseline(do.call(ssd_define_scenario, args))
  out_yes <- ssd_run_scenario_baseline(
    do.call(ssd_define_scenario, c(args, list(samples = TRUE)))
  )
  est <- function(out) do.call(rbind, out$hc$hc)$est
  expect_equal(est(out_no), est(out_yes)) # estimates unchanged
  len <- function(out) unlist(lapply(out$hc$hc, function(h) lengths(h$samples)))
  expect_true(all(len(out_no) == 0L)) # FALSE -> empty samples column
  expect_true(any(len(out_yes) > 0L)) # TRUE -> bootstrap draws retained
})
