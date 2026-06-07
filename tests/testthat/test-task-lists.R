# ---- sample task table -----------------------------------------------------

test_that("task-lists: sample table has D * nsim * R rows with axes populated", {
  scenario <- ssd_define_scenario(
    ssd_data(boron = ssddata::ccme_boron, cadmium = ssddata::ccme_cadmium),
    nsim = 3L,
    seed = 42L,
    nrow = c(5L, 10L)
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
    seed = 42L,
    nrow = c(5L, 10L, 20L)
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
    seed = 42L,
    nrow = c(5L, 10L),
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

test_that("task-lists: hc table has K * H rows, not multiplied by est_method", {
  scenario <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 2L,
    seed = 42L,
    est_method = c("arithmetic", "multi"),
    ci = TRUE,
    nboot = c(10L, 100L)
  )
  fit_tasks <- ssd_scenario_fit_tasks(scenario)
  hc_tasks <- ssd_scenario_hc_tasks(scenario)
  # H = 2 nboot * 1 ci_method * 1 parametric = 2 (est_method is a setting, not
  # an axis, so the 2 est_method values do NOT multiply the task count).
  expect_identical(nrow(hc_tasks), nrow(fit_tasks) * 2L)
  expect_false("est_method" %in% names(hc_tasks))
  expect_false("est_method" %in% attr(hc_tasks, "axes"))
})

test_that("task-lists: ci is not an hc axis", {
  expect_false("ci" %in% task_axes("hc"))
})

test_that("task-lists: ci = FALSE leaves bootstrap-only knobs NA", {
  scenario <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 2L,
    seed = 42L,
    est_method = c("arithmetic", "multi"),
    ci = FALSE
  )
  fit_tasks <- ssd_scenario_fit_tasks(scenario)
  hc_tasks <- ssd_scenario_hc_tasks(scenario)
  # Exactly one hc row per fit task: with `est_method` a setting (not an axis),
  # `ci = FALSE` leaves no fan-out axis, so the two est_method values are
  # summarised within the single task rather than multiplying it.
  expect_identical(nrow(hc_tasks), nrow(fit_tasks))
  expect_false("est_method" %in% names(hc_tasks))
  expect_true(all(hc_tasks$ci == FALSE))
  expect_true(all(is.na(hc_tasks$nboot)))
  expect_true(all(is.na(hc_tasks$ci_method)))
  expect_true(all(is.na(hc_tasks$parametric)))
})

test_that("task-lists: ci = TRUE fans out over the bootstrap knobs", {
  scenario <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 2L,
    seed = 42L,
    ci = TRUE,
    nboot = c(10L, 100L)
  )
  fit_tasks <- ssd_scenario_fit_tasks(scenario)
  hc_tasks <- ssd_scenario_hc_tasks(scenario)
  # 2 nboot * 1 ci_method * 1 parametric = 2 per fit task (est_method is a
  # within-task setting, absent from the fan-out).
  expect_identical(nrow(hc_tasks), nrow(fit_tasks) * 2L)
  expect_true(all(hc_tasks$ci == TRUE))
  expect_false("est_method" %in% attr(hc_tasks, "axes"))
})

test_that("scenario-definition: dists is a setting, not a fit axis or identity", {
  expect_false("dists" %in% task_axes("fit"))
  s1 <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 2L,
    seed = 1L,
    dists = c("lnorm", "gamma")
  )
  s2 <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 2L,
    seed = 1L,
    dists = "lnorm"
  )
  # `dists` is stored on the fit step but is not part of fit/hc identity, so
  # scenarios differing only in `dists` produce identical ids and primers.
  expect_identical(s1$fit$dists, c("lnorm", "gamma"))
  expect_identical(
    ssd_scenario_fit_tasks(s1)$fit_id,
    ssd_scenario_fit_tasks(s2)$fit_id
  )
  expect_identical(
    ssd_scenario_hc_tasks(s1)$hc_id,
    ssd_scenario_hc_tasks(s2)$hc_id
  )
  expect_identical(
    task_primers(ssd_scenario_fit_tasks(s1), "fit"),
    task_primers(ssd_scenario_fit_tasks(s2), "fit")
  )
})

test_that("hazard-concentrations: collapsed est_methods match per-method ssd_hc at the same seed", {
  fit <- ssdtools::ssd_fit_dists(
    ssddata::ccme_boron,
    dists = c("lnorm", "gamma"),
    silent = TRUE
  )
  methods <- c("arithmetic", "geometric", "multi")
  primer <- c(3L, 4L)
  for (ci in c(FALSE, TRUE)) {
    local_dqrng_backend()
    local_dqrng_state(42L, primer = primer)
    collapsed <- hc_collapse_est_methods(
      fit,
      proportion = c(0.05, 0.1),
      ci = ci,
      nboot = if (ci) 50L else NA_integer_,
      est_method = methods,
      ci_method = if (ci) "weighted_samples" else NA_character_,
      parametric = if (ci) TRUE else NA,
      samples = ci
    )
    per_method <- purrr::list_rbind(purrr::map(methods, function(m) {
      local_dqrng_backend()
      local_dqrng_state(42L, primer = primer)
      if (ci) {
        ssdtools::ssd_hc(
          fit,
          proportion = c(0.05, 0.1),
          ci = TRUE,
          nboot = 50L,
          est_method = m,
          ci_method = "weighted_samples",
          parametric = TRUE,
          samples = TRUE,
          min_pboot = 0
        )
      } else {
        ssdtools::ssd_hc(
          fit,
          proportion = c(0.05, 0.1),
          ci = FALSE,
          est_method = m,
          samples = FALSE,
          min_pboot = 0
        )
      }
    }))
    expect_identical(collapsed, per_method)
  }
})

test_that("hazard-concentrations: a vector est_method is summarised within one hc task", {
  scenario <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 1L,
    seed = 42L,
    dists = "lnorm",
    est_method = c("arithmetic", "geometric", "multi"),
    ci = TRUE,
    nboot = 10L
  )
  out <- ssd_run_scenario_baseline(scenario)
  # est_method does not fan out: exactly one hc task.
  expect_identical(nrow(out$hc), 1L)
  hc <- out$hc$hc[[1L]]
  expect_setequal(unique(hc$est_method), c("arithmetic", "geometric", "multi"))
  # One row per est_method at each proportion, all sharing the bootstrap CI
  # (est_method-invariant): lcl/ucl are constant across methods at a proportion.
  for (g in split(hc, hc$proportion)) {
    expect_identical(nrow(g), 3L)
    expect_identical(length(unique(g$lcl)), 1L)
    expect_identical(length(unique(g$ucl)), 1L)
  }
})

# ---- task ids / foreign keys -----------------------------------------------

test_that("task-lists: each table carries a path-style id and parent foreign key", {
  scenario <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 2L,
    seed = 42L,
    nrow = c(5L, 10L)
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
        seed = 42L,
        nrow = c(5L, 10L),
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
        ci = TRUE,
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
    seed = 42L,
    nrow = c(5L, 10L),
    rescale = c(FALSE, TRUE),
    ci = TRUE
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
        seed = 42L,
        nrow = c(5L, 10L),
        rescale = c(FALSE, TRUE),
        ci = TRUE,
        nboot = c(10L, 100L)
      )
    )
  )
})

# ---- baseline runner -------------------------------------------------------

test_that("task-lists: baseline runner threads sample -> fit -> hc", {
  skip_if_not_installed("dqrng")
  scenario <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 1L,
    seed = 42L,
    nrow = c(5L, 6L),
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
  skip_if_not_installed("dqrng")
  # The fit step truncates a single shared draw inline; head(draw, 5) is a
  # byte-identical prefix of head(draw, 10) (TARGETS-DESIGN.md section 5).
  scenario <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 1L,
    seed = 42L,
    nrow = c(5L, 10L),
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
  skip_if_not_installed("dqrng")
  scenario <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 2L,
    seed = 42L,
    nrow = c(5L, 6L),
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
  skip_if_not_installed("dqrng")
  scenario <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 3L,
    seed = 42L,
    nrow = 6L,
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
  skip_if_not_installed("dqrng")
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
  skip_if_not_installed("dqrng")
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
    nboot = 2L,
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
    nboot = 2L,
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
  skip_if_not_installed("dqrng")
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
    seed = 42L,
    nrow = c(5L, 10L),
    ci = TRUE
  )
  expect_snapshot({
    names(ssd_scenario_sample_tasks(scenario))
    names(ssd_scenario_fit_tasks(scenario))
    names(ssd_scenario_hc_tasks(scenario))
  })
})

# ---- samples retains bootstrap draws without changing estimates ------------

test_that("scenario-definition: samples = TRUE retains hc draws but keeps estimates", {
  skip_if_not_installed("dqrng")
  args <- list(
    ssd_data(d = data.frame(Conc = exp(seq(-1, 2, length.out = 20)))),
    nsim = 1L,
    nrow = 6L,
    seed = 42L,
    dists = "lnorm",
    ci = TRUE,
    nboot = 2L
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

test_that("scenario-definition: samples = TRUE works with multiple dists and ci = FALSE", {
  skip_if_not_installed("dqrng")
  # Regression: `samples = TRUE` retains the *bootstrap* draws, which only exist
  # for `ci = TRUE`. With multiple dists (model averaging), asking ssdtools to
  # keep a non-existent `samples` column on the `ci = FALSE` path errored
  # ("Can't select columns that don't exist"). The no-CI path must therefore
  # never request samples, whatever the scenario flag.
  scenario <- ssd_define_scenario(
    ssd_data(d = data.frame(Conc = exp(seq(-1, 2, length.out = 20)))),
    nsim = 1L,
    seed = 42L,
    nrow = 6L,
    dists = c("lnorm", "gamma"),
    ci = FALSE,
    samples = TRUE
  )
  expect_no_error(out <- ssd_run_scenario_baseline(scenario))
  # The no-CI path never retains samples, so every samples column is empty.
  lens <- unlist(lapply(out$hc$hc, function(h) lengths(h$samples)))
  expect_true(all(lens == 0L))
})
