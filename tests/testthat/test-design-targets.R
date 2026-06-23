# `ssd_design_targets()`: the ragged-union design pipeline. Drives real
# `tar_make()` runs (gated by `skip_targets()`; the worker `library(ssdsims)`s,
# so the package must be installed - true under R CMD check).

design_pb <- list(
  sample = c("dataset", "sim"),
  fit = c("dataset", "sim"),
  hc = c("dataset", "sim")
)

# ---- argument validation (no pipeline) -------------------------------------

test_that("ssd_design_targets validates its inputs", {
  expect_error(ssd_design_targets(list()), "ssdsims_design")
  d <- ssd_design(
    a = ssd_define_scenario(
      ssd_scenario_data(d = numeric_dataset()),
      nsim = 1L,
      seed = 42L
    )
  )
  expect_error(ssd_design_targets(d, "results"), "must be empty|check_dots")
  expect_error(ssd_design_targets(d, upload = "nope"), "upload")
})

test_that("scenario-combine: members differing in hc readouts no longer abort", {
  data <- ssd_scenario_data(d = numeric_dataset())
  a <- ssd_define_scenario(data, nsim = 1L, seed = 42L, ci = FALSE)
  b <- ssd_define_scenario(data, nsim = 1L, seed = 42L, ci = TRUE)
  design <- ssd_design(a = a, b = b)
  targets <- ssd_design_targets(design)
  expect_type(targets, "list")
  expect_gt(length(targets), 0L)
})

test_that("scenario-combine: members sharing a seed must agree on nrow_max", {
  data <- ssd_scenario_data(d = numeric_dataset())
  a <- ssd_define_scenario(data, nsim = 1L, seed = 42L, nrow_max = 10L)
  b <- ssd_define_scenario(data, nsim = 1L, seed = 42L, nrow_max = 12L)
  expect_snapshot(error = TRUE, {
    ssd_design_targets(ssd_design(a = a, b = b))
  })
})

# ---- upload shape (no pipeline) --------------------------------------------

flat_target_names <- function(x) {
  if (inherits(x, "tar_target")) {
    return(x$settings$name)
  }
  if (is.list(x)) {
    return(unlist(lapply(x, flat_target_names), use.names = FALSE))
  }
  character(0L)
}

test_that("a dry-run upload pairs one upload target per shard, cell-addressed", {
  skip_if_not_installed("targets")
  skip_if_not_installed("tarchetypes")
  data <- ssd_scenario_data(d = numeric_dataset())
  coarse <- ssd_define_scenario(
    data,
    nsim = 2L,
    seed = 42L,
    nrow = c(5L, 10L),
    dists = ssd_distset(lnorm = "lnorm"),
    partition_by = design_pb
  )
  dense <- ssd_define_scenario(
    data,
    nsim = 2L,
    seed = 42L,
    nrow = c(6L, 7L),
    dists = ssd_distset(lnorm = "lnorm"),
    partition_by = design_pb
  )
  design <- ssd_design(coarse = coarse, dense = dense)

  names <- flat_target_names(ssd_design_targets(
    design,
    upload = ssd_upload_dryrun()
  ))
  for (step in c("sample", "fit", "hc")) {
    n_step <- length(grep(paste0("^", step, "_step_"), names))
    expect_gt(n_step, 0L)
    # one upload target per (deduplicated) shard
    expect_length(grep(paste0("^upload_", step, "_"), names), n_step)
  }
  # shard and upload targets are addressed by cell, never by a member (scenario)
  # name - only the per-member `summary_<name>` targets carry a member name.
  shard_upload <- grep("^(sample|fit|hc)_step_|^upload_", names, value = TRUE)
  expect_false(any(grepl("coarse|dense", shard_upload)))

  # with no upload, no upload targets
  expect_length(
    grep("^upload_", flat_target_names(ssd_design_targets(design))),
    0L
  )
})

# ---- ragged grid: shared cells computed once -------------------------------

test_that("a ragged design shares cells and unions members into one summary", {
  skip_targets()
  dir <- withr::local_tempdir()
  file.copy(
    test_path("fixtures", "design-ragged-targets.R"),
    file.path(dir, "_targets.R")
  )
  withr::local_dir(dir)
  saveRDS(numeric_dataset(), "data.rds")
  tar_make_local()

  progress <- targets::tar_progress()
  built <- progress$name[progress$progress %in% c("completed", "built")]

  # The (dataset, sim) cells are ragged across the two members:
  #   coarse {a,b} x {1,2}; dense {a} x {1,2,3,4}.
  # Union = (a,1) (a,2) (a,3) (a,4) (b,1) (b,2) = 6 cells, with (a,1)/(a,2) shared
  # and built ONCE - so 6 targets per step, not coarse's 4 + dense's 4 = 8.
  for (step in c("sample", "fit", "hc")) {
    expect_length(grep(paste0("^", step, "_step_"), built), 6L)
  }
  # the shared cells (a,1)/(a,2) appear once each (dedup, not one-per-member)
  expect_length(grep("^sample_step_42_a_1$", built), 1L)
  expect_length(grep("^sample_step_42_a_2$", built), 1L)

  # no duplicate target names
  expect_false(anyDuplicated(progress$name) > 0L)

  # the combined summary tags each row with its scenario; both members present.
  summary <- ssd_read_parquet("results/summary.parquet")
  expect_true(all(c("coarse", "dense") %in% summary$scenario))

  coarse_ids <- summary$hc_id[summary$scenario == "coarse"]
  dense_ids <- summary$hc_id[summary$scenario == "dense"]
  # coarse covers datasets {a,b} x sims {1,2}; dense covers {a} x sims {1,2,3,4}.
  expect_true(any(grepl("dataset=b/", coarse_ids)))
  expect_false(any(grepl("dataset=b/", dense_ids)))
  expect_false(any(grepl("/sim=3/|/sim=4/", coarse_ids)))
  expect_true(any(grepl("/sim=3/|/sim=4/", dense_ids)))
  # on the shared (a,1) cell each member keeps its own `nrow` sweep (coarse
  # {5,10}, dense {5,7,8,10}) - the per-member filter of the shared shard.
  a1_coarse <- grep("dataset=a/sim=1/", coarse_ids, value = TRUE)
  a1_dense <- grep("dataset=a/sim=1/", dense_ids, value = TRUE)
  expect_false(any(grepl("/nrow=7/|/nrow=8/", a1_coarse)))
  expect_true(any(grepl("/nrow=7/|/nrow=8/", a1_dense)))
})

# ---- byte-identity vs a standalone run -------------------------------------

test_that("design per-task hc results equal a standalone scenario run", {
  skip_targets()
  skip_if_not_installed("callr")

  # Run each pipeline in its own dir via a fresh process (absolute paths, no
  # shared-session cwd juggling).
  run_pipeline <- function(dir) {
    callr::r(
      function(d) {
        setwd(d)
        suppressWarnings(targets::tar_make(reporter = "silent"))
      },
      args = list(d = dir)
    )
    # The standalone summary lives under its `seed=`/`layout=` tree; the design's
    # combined summary at the base root. Find the (single) `summary.parquet`.
    f <- list.files(
      file.path(dir, "results"),
      pattern = "^summary\\.parquet$",
      recursive = TRUE,
      full.names = TRUE
    )
    ssd_read_parquet(f[[1L]])
  }

  dir1 <- withr::local_tempdir()
  saveRDS(numeric_dataset(), file.path(dir1, "data.rds"))
  writeLines(
    c(
      "library(targets); library(tarchetypes); library(ssdsims)",
      "d <- readRDS('data.rds')",
      "pb <- list(sample = c('dataset','sim'), fit = c('dataset','sim'), hc = c('dataset','sim'))",
      "coarse <- ssd_define_scenario(ssd_scenario_data(a = d, b = d), nsim = 2L,",
      "  seed = 42L, nrow = c(5L, 10L), dists = ssd_distset(lnorm = 'lnorm'),",
      "  partition_by = pb)",
      "ssd_scenario_targets(coarse, root = 'results')"
    ),
    file.path(dir1, "_targets.R")
  )
  standalone <- run_pipeline(dir1)

  dir2 <- withr::local_tempdir()
  saveRDS(numeric_dataset(), file.path(dir2, "data.rds"))
  file.copy(
    test_path("fixtures", "design-ragged-targets.R"),
    file.path(dir2, "_targets.R")
  )
  design <- run_pipeline(dir2)

  # align coarse's rows by hc_id and compare the point estimate
  d_coarse <- design[design$scenario == "coarse", ]
  m <- merge(
    standalone[, c("hc_id", "est")],
    d_coarse[, c("hc_id", "est")],
    by = "hc_id",
    suffixes = c("_alone", "_design")
  )
  expect_gt(nrow(m), 0L)
  expect_equal(m$est_alone, m$est_design)
})

# ---- cache-free upgrade: standalone -> design of one -----------------------

test_that("upgrading a standalone scenario to a design of one reuses its shards", {
  skip_targets()
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  saveRDS(numeric_dataset(), "data.rds")

  preamble <- c(
    "library(targets); library(tarchetypes); library(ssdsims)",
    "data <- ssd_scenario_data(d = readRDS('data.rds'))",
    "pb <- list(sample = c('dataset','sim'), fit = c('dataset','sim'), hc = c('dataset','sim'))",
    "scenario <- ssd_define_scenario(data, nsim = 2L, seed = 42L,",
    "  nrow = c(5L, 10L), dists = ssd_distset(lnorm = 'lnorm'), partition_by = pb)"
  )

  # 1. standalone run into `results`
  writeLines(
    c(preamble, "ssd_scenario_targets(scenario, root = 'results')"),
    "_targets.R"
  )
  tar_make_local()

  # 2. swap to a design of one (same scenario, same root, same `_targets/` store)
  writeLines(
    c(
      preamble,
      "design <- ssd_design(scenario)",
      "ssd_design_targets(design, root = 'results')"
    ),
    "_targets.R"
  )
  outdated <- tar_outdated_local()

  # every sample/fit/hc shard target is cached - the design addresses them
  # identically (same `seed=`/`layout=` root and `seed`-woven names). Only the
  # summary targets (new/changed command) are outdated.
  expect_length(grep("_step_", outdated), 0L)
})

# ---- distinct seeds: separate trees, no sharing ----------------------------

test_that("members with distinct seeds land under separate seed= trees", {
  skip_targets()
  dir <- withr::local_tempdir()
  file.copy(
    test_path("fixtures", "design-seeds-targets.R"),
    file.path(dir, "_targets.R")
  )
  withr::local_dir(dir)
  saveRDS(numeric_dataset(), "data.rds")
  tar_make_local()

  trees <- list.dirs("results", recursive = FALSE)
  expect_true(any(grepl("seed=42", trees)))
  expect_true(any(grepl("seed=43", trees)))

  summary <- ssd_read_parquet("results/summary.parquet")
  expect_setequal(unique(summary$scenario), c("a", "b"))
})

# ---- per-overlap hc readout aggregation ------------------------------------

# Unnest a baseline run's hc estimates into one (fit_id, proportion, est) table -
# the byte-identity oracle for a member's standalone hc results.
baseline_hc <- function(scenario) {
  out <- ssd_run_scenario_baseline(scenario)$hc
  purrr::list_rbind(purrr::map(seq_len(nrow(out)), function(i) {
    h <- out$hc[[i]]
    tibble::tibble(
      fit_id = out$fit_id[[i]],
      proportion = h$proportion,
      est = h$est
    )
  }))
}

run_design_fixture <- function(fixture) {
  dir <- withr::local_tempdir()
  file.copy(test_path("fixtures", fixture), file.path(dir, "_targets.R"))
  withr::local_dir(dir)
  saveRDS(numeric_dataset(), "data.rds")
  tar_make_local()
  ssd_read_parquet("results/summary.parquet")
}

test_that("scenario-combine: readout-only members share the hc cell, filtered", {
  skip_targets()
  summary <- run_design_fixture("design-readout-proportion-targets.R")

  lo <- summary[summary$scenario == "lo", ]
  hi <- summary[summary$scenario == "hi", ]
  # Each member's summary carries exactly its requested proportion.
  expect_equal(unique(lo$proportion), 0.05)
  expect_equal(unique(hi$proportion), 0.1)
  # ... reading the SAME shared hc cell (proportion is not part of the hc_id).
  expect_setequal(lo$hc_id, hi$hc_id)
})

test_that("scenario-combine: a ci mix bootstraps only the overlapping cells", {
  skip_targets()
  summary <- run_design_fixture("design-ci-mixed-targets.R")

  slow <- summary[summary$scenario == "slow", ]
  sim_of <- function(ids) sub(".*sim=([0-9]+).*", "\\1", ids)
  # Only the overlapping sims (1, 2) carry a bootstrap (nboot == 10); the rest are
  # computed ci = FALSE (nboot == 0).
  bootstrapped <- summary$nboot == 10L
  expect_setequal(sim_of(summary$hc_id[bootstrapped]), c("1", "2"))
  expect_setequal(sim_of(summary$hc_id[!bootstrapped]), c("3", "4"))

  # The ci = FALSE member's `est` equals its standalone ci = FALSE value, for the
  # served (overlapping) sims and the un-served ones alike (keyed by the
  # ci-independent fit_id + proportion).
  oracle <- baseline_hc(ssd_define_scenario(
    ssd_scenario_data(a = numeric_dataset()),
    nsim = 4L,
    seed = 42L,
    nrow = 6L,
    dists = ssd_distset(lnorm = "lnorm"),
    ci = FALSE,
    partition_by = design_pb
  ))
  m <- merge(
    slow[, c("fit_id", "proportion", "est")],
    oracle,
    by = c("fit_id", "proportion"),
    suffixes = c("_design", "_alone")
  )
  expect_equal(nrow(m), nrow(oracle))
  expect_equal(m$est_design, m$est_alone)
})

test_that("scenario-combine: a ci = TRUE member's CI matches its standalone run", {
  skip_targets()
  summary <- run_design_fixture("design-ci-mixed-targets.R")
  fast <- summary[summary$scenario == "fast", ]

  oracle <- ssd_run_scenario_baseline(ssd_define_scenario(
    ssd_scenario_data(a = numeric_dataset()),
    nsim = 2L,
    seed = 42L,
    nrow = 6L,
    dists = ssd_distset(lnorm = "lnorm"),
    ci = TRUE,
    nboot = 10L,
    partition_by = design_pb
  ))$hc
  orows <- purrr::list_rbind(purrr::map(seq_len(nrow(oracle)), function(i) {
    h <- oracle$hc[[i]]
    tibble::tibble(hc_id = oracle$hc_id[[i]], lcl = h$lcl, ucl = h$ucl)
  }))
  m <- merge(
    fast[, c("hc_id", "lcl", "ucl")],
    orows,
    by = "hc_id",
    suffixes = c("_design", "_alone")
  )
  expect_gt(nrow(m), 0L)
  expect_equal(m$lcl_design, m$lcl_alone)
  expect_equal(m$ucl_design, m$ucl_alone)
})

test_that("scenario-combine: distset-coverage members share sample and fit", {
  skip_targets()
  dir <- withr::local_tempdir()
  file.copy(
    test_path("fixtures", "design-distset-targets.R"),
    file.path(dir, "_targets.R")
  )
  withr::local_dir(dir)
  saveRDS(numeric_dataset(), "data.rds")
  tar_make_local()

  progress <- targets::tar_progress()
  built <- progress$name[progress$progress %in% c("completed", "built")]
  # One member-shared cell (a, 1): a single sample and fit shard (the design-wide
  # union fit), shared by both members.
  expect_length(grep("^sample_step_", built), 1L)
  expect_length(grep("^fit_step_", built), 1L)

  summary <- ssd_read_parquet("results/summary.parquet")
  one <- summary[summary$scenario == "one", ]
  both <- summary[summary$scenario == "both", ]
  # `one` covers only its single distset; `both` covers both.
  expect_setequal(unique(one$distset), "one")
  expect_setequal(unique(both$distset), c("one", "two"))

  # Each member's hc subset equals its standalone run (distset-subset-invariance:
  # the union fit subset to a member's distset matches fitting that set alone).
  oracle <- baseline_hc(ssd_define_scenario(
    ssd_scenario_data(a = numeric_dataset()),
    nsim = 1L,
    seed = 42L,
    nrow = 10L,
    dists = ssd_distset(one = "lnorm"),
    partition_by = design_pb
  ))
  m <- merge(
    one[, c("fit_id", "proportion", "est")],
    oracle,
    by = c("fit_id", "proportion"),
    suffixes = c("_design", "_alone")
  )
  expect_equal(nrow(m), nrow(oracle))
  expect_equal(m$est_design, m$est_alone)
})

# ---- semi_join hc_ids filter (large id sets) --------------------------------

test_that("ssd_summarise_member uses semi_join to handle >105 hc_ids in DuckDB", {
  # Regression for #201/#203: `filter(hc_id %in% hc_ids)` with >~105 ids
  # exceeds DuckDB's IN-list threshold under stingy prudence and errors.
  # `semi_join` translates natively regardless of set size.
  dir <- withr::local_tempdir()
  n_ids <- 200L
  hc <- tibble::tibble(
    proportion = 0.05,
    est = as.double(seq_len(n_ids)),
    hc_id = sprintf("hc-%04d", seq_len(n_ids)),
    fit_id = sprintf("fit-%04d", seq_len(n_ids))
  )
  ssd_write_parquet(hc, file.path(dir, "hc", "sim=1", "part.parquet"))
  out_path <- file.path(dir, "summary.parquet")
  # Must complete without error, collecting all 200 ids via semi_join in DuckDB.
  ssd_summarise_member(
    dir_hc = file.path(dir, "hc"),
    hc_ids = hc$hc_id,
    path = out_path
  )
  result <- ssd_read_parquet(out_path)
  expect_identical(nrow(result), n_ids)
  expect_setequal(result$hc_id, hc$hc_id)
})
