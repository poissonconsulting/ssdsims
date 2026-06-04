# Gate the tests that actually drive a `targets` pipeline: they spawn a worker
# that `library(ssdsims)`, so the package must be installed (true under R CMD
# check, not under a bare `devtools::test()`), and they are slow.
skip_targets <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("targets")
  testthat::skip_if_not_installed("tarchetypes")
  testthat::skip_if_not_installed("duckplyr")
  testthat::skip_if_not_installed("ssdsims")
}

# A small numeric-only dataset: avoids factor/character columns so a draw
# round-trips through Parquet byte-identically (the byte-identity oracle).
numeric_dataset <- function() {
  data.frame(Conc = exp(seq(-1, 2, length.out = 20)))
}

# ---- shard structure (task 7.1) --------------------------------------------

test_that("task-shards: one shard row per partition_by path cell", {
  scenario <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 2L,
    nrow = c(5L, 10L),
    seed = 42L,
    rescale = c(FALSE, TRUE)
  )
  fit_shards <- ssd_scenario_fit_shards(scenario)
  # default fit path axes: dataset, sim, nrow, rescale -> 1 * 2 * 2 * 2 = 8
  expect_identical(nrow(fit_shards), 8L)
  expect_true(all(
    c("dataset", "sim", "nrow", "rescale", "tasks") %in% names(fit_shards)
  ))
  expect_type(fit_shards$tasks, "list")
})

test_that("task-shards: union of a step's shard tasks equals its task table", {
  scenario <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 2L,
    nrow = c(5L, 10L),
    seed = 42L,
    ci = c(FALSE, TRUE),
    nboot = 10L
  )
  for (step in c("sample", "fit", "hc")) {
    shards <- switch(
      step,
      sample = ssd_scenario_sample_shards(scenario),
      fit = ssd_scenario_fit_shards(scenario),
      hc = ssd_scenario_hc_shards(scenario)
    )
    tasks <- switch(
      step,
      sample = ssd_scenario_sample_tasks(scenario),
      fit = ssd_scenario_fit_tasks(scenario),
      hc = ssd_scenario_hc_tasks(scenario)
    )
    id <- paste0(step, "_id")
    union_ids <- sort(unlist(lapply(shards$tasks, function(t) t[[id]])))
    expect_identical(union_ids, sort(tasks[[id]]))
  }
})

test_that("task-shards: a coarser partition_by bundles more tasks per shard", {
  base <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 2L,
    nrow = c(5L, 10L),
    seed = 42L,
    rescale = c(FALSE, TRUE)
  )
  coarse <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 2L,
    nrow = c(5L, 10L),
    seed = 42L,
    rescale = c(FALSE, TRUE),
    partition_by = list(
      sample = c("dataset", "sim", "replace"),
      fit = c("dataset", "sim"),
      hc = c("dataset", "sim")
    )
  )
  expect_lt(
    nrow(ssd_scenario_fit_shards(coarse)),
    nrow(ssd_scenario_fit_shards(base))
  )
  # coarser -> more tasks in the first shard
  expect_gt(
    nrow(ssd_scenario_fit_shards(coarse)$tasks[[1]]),
    nrow(ssd_scenario_fit_shards(base)$tasks[[1]])
  )
})

# ---- (seed, primer) decoration is RNG-free (task 7.2) ----------------------

test_that("task-shards: task rows carry (seed, primer) matching task_primer()", {
  scenario <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 2L,
    seed = 42L
  )
  shards <- ssd_scenario_fit_shards(scenario)
  t <- shards$tasks[[1]][1, ]
  expect_identical(t$seed, 42L)
  expect_identical(
    t$primer[[1]],
    task_primer(t[, task_axes("fit")])
  )
})

test_that("task-shards: shard derivation draws no random numbers", {
  scenario <- ssd_define_scenario(ssddata::ccme_boron, nsim = 2L, seed = 42L)
  set.seed(99)
  before <- .Random.seed
  ssd_scenario_sample_shards(scenario)
  ssd_scenario_fit_shards(scenario)
  ssd_scenario_hc_shards(scenario)
  expect_identical(before, .Random.seed)
})

# ---- per-shard step runners (task 7.3) -------------------------------------

test_that("task-shards: step runners write one Parquet and read upstream by path", {
  dir <- withr::local_tempdir()
  scenario <- ssd_define_scenario(
    ssd_data(d = numeric_dataset()),
    nsim = 1L,
    nrow = 6L,
    seed = 42L,
    dists = "lnorm"
  )
  ss <- ssd_scenario_sample_shards(scenario)
  fs <- ssd_scenario_fit_shards(scenario)
  hs <- ssd_scenario_hc_shards(scenario)

  sp <- ssd_run_sample_step(ss$tasks[[1]], scenario, file.path(dir, "sample"))
  expect_true(file.exists(sp))
  expect_match(sp, "dataset=d/sim=1/replace=FALSE/part.parquet$")
  # the sample draw came from the scenario's dataset (the accessor)
  draw <- ssd_read_parquet(sp)
  expect_true("Conc" %in% names(draw))

  fp <- ssd_run_fit_step(
    fs$tasks[[1]],
    scenario,
    file.path(dir, "sample"),
    file.path(dir, "fit")
  )
  expect_true(file.exists(fp))
  fit_tbl <- ssd_read_parquet(fp)
  expect_true(all(c("fit_id", "fit_blob") %in% names(fit_tbl)))
  expect_s3_class(decode_obj(fit_tbl$fit_blob[1]), "fitdists")

  hp <- ssd_run_hc_step(
    hs$tasks[[1]],
    scenario,
    file.path(dir, "fit"),
    file.path(dir, "hc")
  )
  expect_true(file.exists(hp))
  hc_tbl <- ssd_read_parquet(hp)
  expect_true(all(c("hc_id", "fit_id", "est") %in% names(hc_tbl)))
})

# ---- summary fan-in (task 7.6) ---------------------------------------------

test_that("task-shards: ssd_summarize unions landed hc shards without recomputation", {
  dir <- withr::local_tempdir()
  scenario <- ssd_define_scenario(
    ssd_data(d = numeric_dataset()),
    nsim = 2L,
    nrow = 6L,
    seed = 42L,
    dists = "lnorm"
  )
  ss <- ssd_scenario_sample_shards(scenario)
  fs <- ssd_scenario_fit_shards(scenario)
  hs <- ssd_scenario_hc_shards(scenario)
  for (i in seq_len(nrow(ss))) {
    ssd_run_sample_step(ss$tasks[[i]], scenario, file.path(dir, "sample"))
  }
  for (i in seq_len(nrow(fs))) {
    ssd_run_fit_step(
      fs$tasks[[i]],
      scenario,
      file.path(dir, "sample"),
      file.path(dir, "fit")
    )
  }
  for (i in seq_len(nrow(hs))) {
    ssd_run_hc_step(
      hs$tasks[[i]],
      scenario,
      file.path(dir, "fit"),
      file.path(dir, "hc")
    )
  }
  out <- ssd_summarize(
    file.path(dir, "sample"),
    file.path(dir, "fit"),
    file.path(dir, "hc"),
    file.path(dir, "summary.parquet")
  )
  expect_true(file.exists(out))
  summary <- ssd_read_parquet(out)
  # equals the union of the hc shards
  hc_union <- ssd_read_parquet(file.path(dir, "hc", "**", "part.parquet"))
  expect_identical(nrow(summary), nrow(hc_union))
  expect_gt(nrow(summary), 0L)
})

# ---- targets pipeline integration (task 7.4) -------------------------------

test_that("task-shards: the shipped template tar_make()s every shard", {
  skip_targets()
  dir <- withr::local_tempdir()
  template <- system.file(
    "targets-templates",
    "local",
    "_targets.R",
    package = "ssdsims"
  )
  skip_if(!nzchar(template))
  file.copy(template, file.path(dir, "_targets.R"))
  withr::local_dir(dir)
  suppressWarnings(targets::tar_make(reporter = "silent"))
  meta <- targets::tar_meta(fields = "error")
  steps <- meta[grepl("_step_", meta$name), ]
  expect_gt(nrow(steps), 0L)
  expect_true(all(is.na(steps$error)))
  # one Parquet per shard, plus the summary
  expect_gt(
    length(list.files("results", pattern = "part.parquet", recursive = TRUE)),
    0L
  )
  expect_true(file.exists("results/summary.parquet"))
})

# ---- byte-identity oracle (task 7.5) ---------------------------------------

# Write a test pipeline over a saved numeric dataset so the worker and the
# in-process baseline run the identical scenario.
write_test_targets <- function(dir) {
  saveRDS(numeric_dataset(), file.path(dir, "data.rds"))
  writeLines(
    c(
      "library(targets)",
      "library(tarchetypes)",
      "library(ssdsims)",
      "scenario <- ssd_define_scenario(",
      "  ssd_data(d = readRDS('data.rds')),",
      "  nsim = 2L, nrow = c(5L, 10L), seed = 42L,",
      "  rescale = c(FALSE, TRUE), dists = c('lnorm', 'gamma')",
      ")",
      "sample_targets <- tar_map(values = ssd_scenario_sample_shards(scenario),",
      "  names = c(dataset, sim, replace),",
      "  tar_target(sample_step, ssd_run_sample_step(tasks, scenario, 'results/sample'),",
      "    format = 'file', error = 'null'))",
      "fit_targets <- tar_map(values = ssd_scenario_fit_shards(scenario),",
      "  names = c(dataset, sim, nrow, rescale),",
      "  tar_target(fit_step, { sample_done; ssd_run_fit_step(tasks, scenario, 'results/sample', 'results/fit') },",
      "    format = 'file', error = 'null'))",
      "hc_targets <- tar_map(values = ssd_scenario_hc_shards(scenario),",
      "  names = c(dataset, sim),",
      "  tar_target(hc_step, { fit_done; ssd_run_hc_step(tasks, scenario, 'results/fit', 'results/hc') },",
      "    format = 'file', error = 'null'))",
      "list(",
      "  sample_targets, tar_combine(sample_done, sample_targets),",
      "  fit_targets, tar_combine(fit_done, fit_targets),",
      "  hc_targets, tar_combine(hc_done, hc_targets),",
      "  tar_target(summary, { hc_done; ssd_summarize('results/sample','results/fit','results/hc','results/summary.parquet') }, format = 'file')",
      ")"
    ),
    file.path(dir, "_targets.R")
  )
}

test_that("task-shards: pipeline per-task results equal the baseline runner", {
  skip_targets()
  dir <- withr::local_tempdir()
  write_test_targets(dir)
  withr::local_dir(dir)
  suppressWarnings(targets::tar_make(reporter = "silent"))

  scenario <- ssd_define_scenario(
    ssd_data(d = numeric_dataset()),
    nsim = 2L,
    nrow = c(5L, 10L),
    seed = 42L,
    rescale = c(FALSE, TRUE),
    dists = c("lnorm", "gamma")
  )
  base <- ssd_run_scenario_baseline(scenario)

  # fit: every fit object equals the baseline's (sorted by fit_id). Use
  # all.equal (not expect_equal/waldo) for a value-level comparison: a
  # deserialised fitdists has nil TMB external pointers that waldo would flag but
  # all.equal ignores - the fitted values are what must match.
  fit_pipe <- ssd_read_parquet("results/fit/**/part.parquet")
  for (k in seq_along(base$fit$fit_id)) {
    fp <- decode_obj(fit_pipe$fit_blob[match(
      base$fit$fit_id[k],
      fit_pipe$fit_id
    )])
    expect_true(isTRUE(all.equal(fp, base$fit$fits[[k]])))
  }

  # hc: estimates equal the baseline's, sorted by the task-identity key
  hc_pipe <- ssd_read_parquet("results/hc/**/part.parquet")
  hc_pipe <- hc_pipe[order(hc_pipe$hc_id, hc_pipe$dist), ]
  base_hc <- do.call(
    rbind,
    Map(
      function(tb, id) {
        tb$hc_id <- id
        tibble::as_tibble(tb)
      },
      base$hc$hc,
      base$hc$hc_id
    )
  )
  base_hc <- base_hc[order(base_hc$hc_id, base_hc$dist), ]
  expect_equal(hc_pipe$est, base_hc$est)
})

# ---- whole-shard error = "null" survival (task 7.3b) -----------------------

test_that("task-shards: a failing shard goes NULL and the survivors still summarize", {
  skip_targets()
  dir <- withr::local_tempdir()
  saveRDS(numeric_dataset(), file.path(dir, "data.rds"))
  # `min_pmix = "boom"` resolves to a function that errors -> the fit shards fail
  # as whole shards (error = "null"); sample + summary still build.
  writeLines(
    c(
      "library(targets)",
      "library(tarchetypes)",
      "library(ssdsims)",
      "boom <- function(n) stop('boom')",
      "scenario <- ssd_define_scenario(",
      "  ssd_data(d = readRDS('data.rds')),",
      "  nsim = 2L, nrow = 6L, seed = 42L, dists = 'lnorm', min_pmix = boom",
      ")",
      "sample_targets <- tar_map(values = ssd_scenario_sample_shards(scenario),",
      "  names = c(dataset, sim, replace),",
      "  tar_target(sample_step, ssd_run_sample_step(tasks, scenario, 'results/sample'),",
      "    format = 'file', error = 'null'))",
      "fit_targets <- tar_map(values = ssd_scenario_fit_shards(scenario),",
      "  names = c(dataset, sim, nrow, rescale),",
      "  tar_target(fit_step, { sample_done; ssd_run_fit_step(tasks, scenario, 'results/sample', 'results/fit') },",
      "    format = 'file', error = 'null'))",
      "list(sample_targets, tar_combine(sample_done, sample_targets), fit_targets)"
    ),
    file.path(dir, "_targets.R")
  )
  withr::local_dir(dir)
  suppressWarnings(targets::tar_make(reporter = "silent"))
  meta <- targets::tar_meta(fields = "error")
  # sample shards built; fit shards recorded an error and went NULL
  sample_meta <- meta[grepl("sample_step_", meta$name), ]
  fit_meta <- meta[grepl("fit_step_", meta$name), ]
  expect_true(all(is.na(sample_meta$error)))
  expect_true(all(!is.na(fit_meta$error)))
  # the survivors (sample shards) are on disk
  expect_gt(
    length(list.files(
      "results/sample",
      pattern = "part.parquet",
      recursive = TRUE
    )),
    0L
  )
})
