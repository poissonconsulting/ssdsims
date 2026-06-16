# `skip_targets()` and `numeric_dataset()` live in `helper.R` (shared with
# test-path-axis-growth.R).

# ---- shard structure (task 7.1) --------------------------------------------

test_that("task-shards: one shard row per partition_by path cell", {
  scenario <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 2L,
    seed = 42L,
    nrow = c(5L, 10L),
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
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 2L,
    seed = 42L,
    nrow = c(5L, 10L),
    ci = TRUE,
    nboot = 10L
  )
  for (step in c("sample", "fit", "hc")) {
    shards <- switch(
      step,
      sample = ssd_scenario_sample_shards(scenario),
      fit = ssd_scenario_fit_shards(scenario),
      hc = ssd_scenario_hc_shards(scenario)
    )
    tasks <- ssd_scenario_tasks(scenario, step)
    id <- paste0(step, "_id")
    union_ids <- sort(unlist(lapply(shards$tasks, function(t) t[[id]])))
    expect_identical(union_ids, sort(tasks[[id]]))
  }
})

test_that("task-shards: ci is not a column, path axis, or inner axis on the hc shards", {
  scenario <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 1L,
    seed = 42L,
    ci = TRUE,
    nboot = 10L
  )
  tasks <- ssd_scenario_hc_shards(scenario)$tasks[[1]]
  # `ci` is an hc setting the runner reads from the scenario slice: it is not
  # emitted as a task-row column and is absent from the hc vocabulary.
  expect_false("ci" %in% names(tasks))
  expect_false("ci" %in% task_axes("hc"))
  expect_false("ci" %in% scenario_partition_axes(scenario, "hc")$path)
  expect_false("ci" %in% scenario_partition_axes(scenario, "hc")$inner)
})

test_that("task-shards: a coarser partition_by bundles more tasks per shard", {
  base <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 2L,
    seed = 42L,
    nrow = c(5L, 10L),
    rescale = c(FALSE, TRUE)
  )
  coarse <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 2L,
    seed = 42L,
    nrow = c(5L, 10L),
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
    ssd_scenario_data(ssddata::ccme_boron),
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
  scenario <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 2L,
    seed = 42L
  )
  # Check both RNG paths: base R's `.Random.seed` and the dqrng stream state
  # (the package's actual draw path). The derivation only hashes (task_primer
  # via rlang::hash()) and groups, so neither should advance.
  local_dqrng_backend()
  set.seed(99)
  before_base <- .Random.seed
  before_dqrng <- get_dqrng_state()
  ssd_scenario_sample_shards(scenario)
  ssd_scenario_fit_shards(scenario)
  ssd_scenario_hc_shards(scenario)
  expect_identical(.Random.seed, before_base)
  expect_identical(get_dqrng_state(), before_dqrng)
})

# ---- per-shard step runners (task 7.3) -------------------------------------

test_that("task-shards: step runners write one Parquet and read upstream by path", {
  dir <- withr::local_tempdir()
  scenario <- ssd_define_scenario(
    ssd_scenario_data(d = numeric_dataset()),
    nsim = 1L,
    seed = 42L,
    nrow = 6L,
    dists = ssd_distset(lnorm = "lnorm")
  )
  ss <- ssd_scenario_sample_shards(scenario)
  fs <- ssd_scenario_fit_shards(scenario)
  hs <- ssd_scenario_hc_shards(scenario)

  sp <- ssd_run_sample_step(ss$tasks[[1]], scenario, file.path(dir, "sample"))
  expect_true(file.exists(sp))
  expect_match(sp, "dataset=d/sim=1/replace=TRUE/part.parquet$")
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

test_that("task-shards: ssd_summarise unions landed hc shards without recomputation", {
  dir <- withr::local_tempdir()
  scenario <- ssd_define_scenario(
    ssd_scenario_data(d = numeric_dataset()),
    nsim = 2L,
    seed = 42L,
    nrow = 6L,
    dists = ssd_distset(lnorm = "lnorm")
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
  out <- ssd_summarise(
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

# ---- dual summary outputs (dual-summary-outputs change) --------------------

# Materialise every sample/fit/hc shard for `scenario` under `dir`.
materialise_shards <- function(scenario, dir) {
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
}

test_that("dual-summary-outputs: path_with_samples writes a full summary retaining samples", {
  dir <- withr::local_tempdir()
  scenario <- ssd_define_scenario(
    ssd_scenario_data(d = numeric_dataset()),
    nsim = 1L,
    seed = 42L,
    nrow = 6L,
    dists = ssd_distset(lnorm = "lnorm"),
    ci = TRUE,
    nboot = 10L,
    samples = TRUE
  )
  materialise_shards(scenario, dir)
  compact_path <- file.path(dir, "summary.parquet")
  full_path <- file.path(dir, "summary-samples.parquet")
  out <- ssd_summarise(
    file.path(dir, "sample"),
    file.path(dir, "fit"),
    file.path(dir, "hc"),
    compact_path,
    path_with_samples = full_path
  )
  # both paths returned and written
  expect_identical(out, c(compact_path, full_path))
  expect_true(all(file.exists(out)))

  compact <- ssd_read_parquet(compact_path)
  full <- ssd_read_parquet(full_path)
  # compact projects the heavy list-columns out; full retains them
  expect_false(any(c("dists", "samples") %in% names(compact)))
  expect_true(all(c("dists", "samples") %in% names(full)))
  # the full summary's draws are populated (samples = TRUE)
  expect_true(any(lengths(full$samples) > 0L))
  # same rows and identical estimates across the two files (the union read is
  # unordered, so compare sorted)
  expect_identical(nrow(compact), nrow(full))
  expect_equal(sort(compact$est), sort(full$est))
  expect_equal(sort(compact$lcl), sort(full$lcl))
  expect_equal(sort(compact$ucl), sort(full$ucl))
})

test_that("dual-summary-outputs: path_with_samples = NULL writes only the compact summary", {
  dir <- withr::local_tempdir()
  scenario <- ssd_define_scenario(
    ssd_scenario_data(d = numeric_dataset()),
    nsim = 1L,
    seed = 42L,
    nrow = 6L,
    dists = ssd_distset(lnorm = "lnorm")
  )
  materialise_shards(scenario, dir)
  out <- ssd_summarise(
    file.path(dir, "sample"),
    file.path(dir, "fit"),
    file.path(dir, "hc"),
    file.path(dir, "summary.parquet")
  )
  expect_identical(out, file.path(dir, "summary.parquet"))
  expect_true(file.exists(out))
  expect_false(file.exists(file.path(dir, "summary-samples.parquet")))
})

test_that("dual-summary-outputs: the pipeline gates the full summary on samples", {
  skip_targets()
  root <- withr::local_tempdir()
  with_samples <- ssd_define_scenario(
    ssd_scenario_data(d = numeric_dataset()),
    nsim = 1L,
    seed = 42L,
    nrow = 6L,
    dists = ssd_distset(lnorm = "lnorm"),
    ci = TRUE,
    nboot = 10L,
    samples = TRUE
  )
  without_samples <- ssd_define_scenario(
    ssd_scenario_data(d = numeric_dataset()),
    nsim = 1L,
    seed = 42L,
    nrow = 6L,
    dists = ssd_distset(lnorm = "lnorm")
  )
  # the summary target is the last element of the factory's list
  summary_cmd <- function(scenario) {
    tgts <- ssd_scenario_targets(scenario, root = root)
    tgts[[length(tgts)]]$command$string
  }
  expect_match(
    summary_cmd(with_samples),
    "summary-samples.parquet",
    fixed = TRUE
  )
  expect_match(summary_cmd(with_samples), "path_with_samples = ", fixed = TRUE)
  # samples = FALSE injects `path_with_samples = NULL` and no second file path
  expect_no_match(
    summary_cmd(without_samples),
    "summary-samples.parquet",
    fixed = TRUE
  )
  expect_match(
    summary_cmd(without_samples),
    "path_with_samples = NULL",
    fixed = TRUE
  )
})

# ---- targets pipeline integration (task 7.4) -------------------------------

test_that("task-shards: the shipped template tar_make()s every shard", {
  skip_targets()
  dir <- withr::local_tempdir()
  template_dir <- system.file("targets-templates", "small", package = "ssdsims")
  skip_if(!nzchar(template_dir))
  # `_targets.R` sources `scenario.R`, so copy both.
  file.copy(
    file.path(template_dir, c("_targets.R", "scenario.R")),
    dir
  )
  withr::local_dir(dir)
  tar_make_local()
  meta <- targets::tar_meta(fields = "error")
  steps <- meta[grepl("_step_", meta$name), ]
  expect_gt(nrow(steps), 0L)
  expect_true(all(is.na(steps$error)))
  # one Parquet per shard, plus the summary, under the per-layout root
  expect_gt(
    length(list.files("results", pattern = "part.parquet", recursive = TRUE)),
    0L
  )
  expect_length(
    list.files("results", pattern = "summary.parquet", recursive = TRUE),
    1L
  )
})

# ---- byte-identity oracle (task 7.5) ---------------------------------------

# Copy a plain-text pipeline fixture (`fixtures/<name>.R`) into `dir` as its
# `_targets.R`, alongside the saved numeric dataset the fixture reads.
setup_targets_fixture <- function(dir, fixture) {
  saveRDS(numeric_dataset(), file.path(dir, "data.rds"))
  file.copy(test_path("fixtures", fixture), file.path(dir, "_targets.R"))
}

test_that("task-shards: pipeline per-task results equal the baseline runner", {
  skip_targets()
  dir <- withr::local_tempdir()
  setup_targets_fixture(dir, "byte-identity-targets.R")
  withr::local_dir(dir)
  tar_make_local()

  scenario <- ssd_define_scenario(
    ssd_scenario_data(d = numeric_dataset()),
    nsim = 2L,
    seed = 42L,
    nrow = c(5L, 10L),
    rescale = c(FALSE, TRUE),
    dists = ssd_distset(set = c("lnorm", "gamma"))
  )
  base <- ssd_run_scenario_baseline(scenario)

  # fit: every fit object equals the baseline's (sorted by fit_id). Use
  # all.equal (not expect_equal/waldo) for a value-level comparison: a
  # deserialised fitdists has nil TMB external pointers that waldo would flag but
  # all.equal ignores - the fitted values are what must match.
  fit_pipe <- ssd_read_parquet(file.path(
    grep("/fit$", list.dirs("results", recursive = TRUE), value = TRUE),
    "**",
    "part.parquet"
  ))
  for (k in seq_along(base$fit$fit_id)) {
    fp <- decode_obj(fit_pipe$fit_blob[match(
      base$fit$fit_id[k],
      fit_pipe$fit_id
    )])
    expect_true(isTRUE(all.equal(fp, base$fit$fits[[k]])))
  }

  # hc: estimates equal the baseline's, sorted by the task-identity key
  hc_pipe <- ssd_read_parquet(file.path(
    grep("/hc$", list.dirs("results", recursive = TRUE), value = TRUE),
    "**",
    "part.parquet"
  ))
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
  # The fixture's `min_pmix` resolves to a function that errors -> the fit shards
  # fail as whole shards (error = "null"); the sample shards still build.
  setup_targets_fixture(dir, "error-null-targets.R")
  withr::local_dir(dir)
  tar_make_local()
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

# ---- per-layout results root (Option D) ------------------------------------

test_that("task-shards: scenario_results_dir keys the root on partition_by", {
  a <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 1L,
    seed = 1L
  )
  b <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 1L,
    seed = 1L,
    partition_by = list(fit = c("dataset", "sim"))
  )
  expect_match(basename(scenario_results_dir(a)), "^layout=")
  # the root is `<root>/seed=<seed>/layout=<hash>`
  expect_identical(dirname(scenario_results_dir(a, root = "out")), "out/seed=1")
  # same scenario -> same root (idempotent); different partition_by -> different root
  expect_identical(scenario_results_dir(a), scenario_results_dir(a))
  expect_false(scenario_results_dir(a) == scenario_results_dir(b))
  # the `seed` is now part of the root (the seed= level), so it changes the root -
  # this is what lets a single-scenario run and a design-of-one share addressing
  a2 <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 1L,
    seed = 99L
  )
  expect_false(scenario_results_dir(a) == scenario_results_dir(a2))
  expect_match(scenario_results_dir(a2), "seed=99/layout=")
})

# ---- target factory (ssd_scenario_targets) ---------------------------------

test_that("task-shards: ssd_scenario_targets returns a target list", {
  skip_targets()
  scenario <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 1L,
    seed = 42L
  )
  tg <- ssd_scenario_targets(scenario)
  expect_type(tg, "list")
  expect_gt(length(tg), 0L)
})

# ---- invalidation model: per-child Option-3 edges (hive-partitioning) ------

# The target names a command references that match a vector of candidate names -
# the upstream shard edges `targets` records for that command.
shard_target_names <- function(mapped, step) {
  vapply(
    mapped[[paste0(step, "_step")]],
    function(t) t$settings$name,
    character(1)
  )
}
declared_edges <- function(target, candidates) {
  intersect(all.names(target$command$expr), candidates)
}

test_that("hive: each child shard names exactly the parent shards it reads (m:n)", {
  # `ssd_scenario_targets()` builds with both `tarchetypes::tar_map()` and
  # `targets::tar_target_raw()`, so both Suggests must be installed.
  skip_if_not_installed("targets")
  skip_if_not_installed("tarchetypes")
  # sample/fit per-sim; hc coarse (dataset only) so its one shard reads BOTH fits.
  scenario <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 2L,
    seed = 42L,
    nrow = 6L,
    dists = ssd_distset(lnorm = "lnorm"),
    partition_by = list(
      sample = c("dataset", "sim"),
      fit = c("dataset", "sim"),
      hc = "dataset"
    )
  )
  tg <- ssd_scenario_targets(scenario)
  sample_names <- shard_target_names(tg[[1L]], "sample")
  fit_inner <- tg[[2L]][["fit_step"]]
  fit_names <- shard_target_names(tg[[2L]], "fit")
  hc_inner <- tg[[3L]][["hc_step"]]

  cell_names <- shard_cell_names(
    tg[[1L]],
    ssd_scenario_sample_shards(scenario),
    scenario,
    "sample"
  )
  fsh <- ssd_scenario_fit_shards(scenario)
  # Each fit shard declares an edge to exactly the sample target(s) it reads -
  # the same projection read_parent_shards() uses (graph == read, task 4.3).
  for (i in seq_along(fit_inner)) {
    read_cells <- unique(path_key(
      fsh$tasks[[i]],
      scenario$partition_by[["sample"]]
    ))
    expect_setequal(
      declared_edges(fit_inner[[i]], sample_names),
      unname(cell_names[read_cells])
    )
  }
  # m:n fan-in: the coarse hc shard reads (and declares) every fit shard.
  expect_setequal(declared_edges(hc_inner[[1L]], fit_names), fit_names)
  # summary names every hc shard.
  hc_names <- shard_target_names(tg[[3L]], "hc")
  expect_setequal(declared_edges(tg[[4L]], hc_names), hc_names)
})

all_target_names <- function(x) {
  if (inherits(x, "tar_target")) {
    return(x$settings$name)
  }
  if (is.list(x)) {
    return(unlist(lapply(x, all_target_names), use.names = FALSE))
  }
  character(0L)
}

test_that("hive: per-child edges replace the coarse step barrier; no data step", {
  skip_if_not_installed("targets")
  skip_if_not_installed("tarchetypes")
  scenario <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 2L,
    seed = 42L
  )
  tg <- ssd_scenario_targets(scenario)
  all_names <- all_target_names(tg)
  # No step-wide tar_combine() barrier targets, and the fold is kept (no data step).
  expect_false(any(c("sample_done", "fit_done", "hc_done") %in% all_names))
  expect_false(any(grepl("^data_step", all_names)))
  expect_true(all(grepl("^(sample_step|fit_step|hc_step|summary)", all_names)))
})

test_that("hive: cue is threaded onto every shard target", {
  # Needs `targets` for `tar_cue()` here and `tarchetypes` inside the factory.
  skip_if_not_installed("targets")
  skip_if_not_installed("tarchetypes")
  scenario <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 2L,
    seed = 42L
  )
  tg <- ssd_scenario_targets(scenario, cue = targets::tar_cue(depend = FALSE))
  shard_targets <- c(
    tg[[1L]][["sample_step"]],
    tg[[2L]][["fit_step"]],
    tg[[3L]][["hc_step"]]
  )
  for (t in shard_targets) {
    expect_false(t$cue$depend)
  }
  # default (no cue) leaves the standard cue (depend = TRUE)
  tg0 <- ssd_scenario_targets(scenario)
  expect_true(tg0[[1L]][["sample_step"]][[1L]]$cue$depend)
})

test_that("hive: extending nrow caches the sample shard; changing nrow_max rebuilds it", {
  skip_if_not_installed("targets")
  skip_if_not_installed("tarchetypes")
  # The draw size is the scenario's fixed `nrow_max` setting (carried on the
  # sample slice), not max(nrow): adding an `nrow` value within the effective
  # draw size leaves the (inlined) sample shard command byte-identical -> the
  # sample shard stays cached and only the new nrow-keyed fit shards mint.
  # Changing `nrow_max` changes the sample slice -> the draw rebuilds and the
  # per-child edge propagates to the fit shards that read it. The byte-level
  # multi-run assertion lives in shard-atomic-rewrite / path-axis-growth.
  base <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 1L,
    seed = 42L,
    nrow = 6L,
    dists = ssd_distset(lnorm = "lnorm")
  )
  wide <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 1L,
    seed = 42L,
    nrow = c(6L, 12L),
    dists = ssd_distset(lnorm = "lnorm")
  )
  resized <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 1L,
    seed = 42L,
    nrow = 6L,
    nrow_max = 500L,
    dists = ssd_distset(lnorm = "lnorm")
  )
  s1 <- ssd_scenario_targets(base)[[1L]][["sample_step"]][[1L]]$command$expr
  s2 <- ssd_scenario_targets(wide)[[1L]][["sample_step"]][[1L]]$command$expr
  s3 <- ssd_scenario_targets(resized)[[1L]][["sample_step"]][[1L]]$command$expr
  expect_identical(s1, s2)
  expect_false(identical(s1, s3))
})

test_that("hive: re-make is a full cache hit; a missing Parquet rebuilds only its subtree", {
  skip_targets()
  dir <- withr::local_tempdir()
  setup_targets_fixture(dir, "factory-invalidation-targets.R")
  withr::local_dir(dir)
  tar_make_local()
  # cache-by-existence: a second make with nothing changed is a full hit.
  expect_length(tar_outdated_local(), 0L)
  # delete the sim=1 sample shard's Parquet -> only its subtree (+ summary) rebuilds.
  unlink(grep(
    "/sample/dataset=d/sim=1/part.parquet$",
    list.files("results", recursive = TRUE, full.names = TRUE),
    value = TRUE
  ))
  outdated <- tar_outdated_local()
  expect_true(all(
    c("sample_step_42_d_1", "fit_step_42_d_1", "hc_step_42_d_1", "summary") %in%
      outdated
  ))
  expect_false(any(
    c("sample_step_42_d_2", "fit_step_42_d_2", "hc_step_42_d_2") %in% outdated
  ))
})

test_that("hive: invalidating one parent shard re-runs only the children that read it", {
  skip_targets()
  dir <- withr::local_tempdir()
  setup_targets_fixture(dir, "factory-invalidation-targets.R")
  withr::local_dir(dir)
  tar_make_local()
  # invalidate the sim=1 sample shard; its subtree is outdated, sim=2 stays cached.
  targets::tar_invalidate(tidyselect::any_of("sample_step_42_d_1"))
  outdated <- tar_outdated_local()
  expect_true(all(
    c("sample_step_42_d_1", "fit_step_42_d_1", "hc_step_42_d_1", "summary") %in%
      outdated
  ))
  expect_false(any(
    c("sample_step_42_d_2", "fit_step_42_d_2", "hc_step_42_d_2") %in% outdated
  ))
})

# ---- inner-axis growth: atomic byte-stable rewrite (shard-atomic-rewrite) --

test_that("shard-atomic-rewrite: growing a fit inner axis rewrites the fit shards byte-stably and leaves sample cached", {
  skip_targets()
  dir <- withr::local_tempdir()
  setup_targets_fixture(dir, "inner-growth-targets.R")
  withr::local_dir(dir)

  # First build: one fit task per shard (min_pmix = loose only).
  saveRDS(FALSE, "grow.rds")
  tar_make_local()

  # Capture each affected fit shard's Parquet before the growth.
  fit_shard_dirs <- dirname(grep(
    "/fit/",
    list.files(
      "results",
      pattern = "part.parquet",
      recursive = TRUE,
      full.names = TRUE
    ),
    value = TRUE
  ))
  expect_gt(length(fit_shard_dirs), 0L)
  prior <- lapply(file.path(fit_shard_dirs, "part.parquet"), ssd_read_parquet)
  names(prior) <- fit_shard_dirs
  expect_true(all(vapply(prior, nrow, integer(1L)) == 1L))

  # Grow the fit grid by one `min_pmix` (an inner axis) and re-source: tar_make
  # re-reads `_targets.R`, which now builds the two-value scenario. The layout
  # is unchanged (min_pmix is not a partition_by axis), so the per-layout root
  # is identical and the shards are rewritten in place, not re-pathed.
  saveRDS(TRUE, "grow.rds")
  outdated <- tar_outdated_local()
  # Exactly the fit shards (and their hc/summary subtree) are stale; the sample
  # shards - a step the inner-axis growth does not touch - are not.
  expect_true(all(c("fit_step_42_d_1", "fit_step_42_d_2") %in% outdated))
  expect_false(any(c("sample_step_42_d_1", "sample_step_42_d_2") %in% outdated))

  tar_make_local()

  # The sample shards are reported cached by targets and are NOT rebuilt.
  progress <- targets::tar_progress()
  sample_progress <- progress$progress[grepl("^sample_step_", progress$name)]
  expect_identical(unique(sample_progress), "skipped")

  for (d in fit_shard_dirs) {
    # Whole-file overwrite: still a single Parquet per shard (no in-place append).
    expect_length(list.files(d, pattern = "[.]parquet$"), 1L)
    after <- ssd_read_parquet(file.path(d, "part.parquet"))
    # The shard now carries the larger task set: loose + strict.
    expect_identical(nrow(after), 2L)

    prior_tbl <- prior[[d]][order(prior[[d]]$fit_id), ]
    kept <- after[after$fit_id %in% prior_tbl$fit_id, ]
    kept <- kept[order(kept$fit_id), ]
    # The row present before the growth reads back byte-identical to the prior
    # Parquet (the prior task's (seed, primer) unchanged -> same serialised blob).
    expect_identical(kept$fit_id, prior_tbl$fit_id)
    expect_identical(kept$fit_blob, prior_tbl$fit_blob)
    # The shard differs from the prior Parquet only by the added min_pmix row.
    expect_identical(nrow(after) - nrow(kept), 1L)
  }
})

# ---- per-step minimal scenario slice (step-scenario-slice) -----------------

test_that("task-shards: changing an hc-only scenario option rebuilds only hc and summary", {
  skip_targets()
  dir <- withr::local_tempdir()
  setup_targets_fixture(dir, "slice-invalidation-targets.R")
  withr::local_dir(dir)
  saveRDS(
    list(dists = ssd_distset(lnorm = "lnorm"), samples = FALSE),
    "opts.rds"
  )
  tar_make_local()
  expect_length(tar_outdated_local(), 0L)

  # Flip the hc-only `samples` scenario option: only the hc slice changes, so
  # only the hc shard's command moves; the sample/fit slices (and commands) are
  # untouched.
  saveRDS(
    list(dists = ssd_distset(lnorm = "lnorm"), samples = TRUE),
    "opts.rds"
  )
  outdated <- tar_outdated_local()
  expect_true(all(c("hc_step_42_d_1", "summary") %in% outdated))
  expect_false(any(c("sample_step_42_d_1", "fit_step_42_d_1") %in% outdated))
})

test_that("task-shards: changing a fit-only scenario option leaves sample cached", {
  skip_targets()
  dir <- withr::local_tempdir()
  setup_targets_fixture(dir, "slice-invalidation-targets.R")
  withr::local_dir(dir)
  saveRDS(
    list(dists = ssd_distset(lnorm = "lnorm"), samples = FALSE),
    "opts.rds"
  )
  tar_make_local()
  expect_length(tar_outdated_local(), 0L)

  # Flip the fit-only `dists` scenario option: the fit slice changes (and
  # cascades into the hc shard that reads the fit shard, and summary), but the
  # sample slice does not, so the sample shard stays cached.
  saveRDS(
    list(dists = ssd_distset(set = c("lnorm", "gamma")), samples = FALSE),
    "opts.rds"
  )
  outdated <- tar_outdated_local()
  expect_true(all(
    c("fit_step_42_d_1", "hc_step_42_d_1", "summary") %in% outdated
  ))
  expect_false("sample_step_42_d_1" %in% outdated)
})

test_that("task-shards: appending a dataset mints new shards and caches existing ones", {
  skip_targets()
  dir <- withr::local_tempdir()
  file.copy(
    test_path("fixtures", "dataset-growth-targets.R"),
    file.path(dir, "_targets.R")
  )
  withr::local_dir(dir)
  saveRDS(list(d1 = numeric_dataset()), "datasets.rds")
  tar_make_local()
  expect_length(tar_outdated_local(), 0L)

  # Append a second dataset: its shards are new path cells; the per-dataset
  # `sample` slice keeps d1's shard commands byte-identical, so only d2's shards
  # (and `summary`) are outdated while every d1 shard stays cached.
  saveRDS(list(d1 = numeric_dataset(), d2 = numeric_dataset()), "datasets.rds")
  outdated <- tar_outdated_local()
  expect_true(all(
    c(
      "sample_step_42_d2_1",
      "fit_step_42_d2_1",
      "hc_step_42_d2_1",
      "summary"
    ) %in%
      outdated
  ))
  expect_false(any(
    c("sample_step_42_d1_1", "fit_step_42_d1_1", "hc_step_42_d1_1") %in%
      outdated
  ))
})

test_that("task-shards: factory per-task results equal the baseline (slice drops no field)", {
  skip_targets()
  dir <- withr::local_tempdir()
  setup_targets_fixture(dir, "slice-invalidation-targets.R")
  withr::local_dir(dir)
  saveRDS(
    list(dists = ssd_distset(lnorm = "lnorm"), samples = FALSE),
    "opts.rds"
  )
  tar_make_local()

  scenario <- ssd_define_scenario(
    ssd_scenario_data(d = numeric_dataset()),
    nsim = 1L,
    seed = 42L,
    nrow = 6L,
    dists = ssd_distset(lnorm = "lnorm"),
    partition_by = list(
      sample = c("dataset", "sim"),
      fit = c("dataset", "sim"),
      hc = c("dataset", "sim")
    )
  )
  base <- ssd_run_scenario_baseline(scenario)

  # fit objects equal the baseline's (so the fit slice dropped no consumed field,
  # and the sample slice fed the same draw upstream). all.equal ignores the nil
  # TMB external pointers a deserialised fitdists carries.
  fit_pipe <- ssd_read_parquet(file.path(
    grep("/fit$", list.dirs("results", recursive = TRUE), value = TRUE),
    "**",
    "part.parquet"
  ))
  for (k in seq_along(base$fit$fit_id)) {
    fp <- decode_obj(fit_pipe$fit_blob[match(
      base$fit$fit_id[k],
      fit_pipe$fit_id
    )])
    expect_true(isTRUE(all.equal(fp, base$fit$fits[[k]])))
  }

  # hc estimates equal the baseline's (the hc slice dropped no consumed field).
  hc_pipe <- ssd_read_parquet(file.path(
    grep("/hc$", list.dirs("results", recursive = TRUE), value = TRUE),
    "**",
    "part.parquet"
  ))
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
