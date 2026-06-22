# Observed cost analysis: the tar_meta resolver, ssd_analyse_cost(),
# ssd_calibrate_cost_from_run(), and ssd_compare_cost(). The measured paths run
# off a single-core ssd_run_scenario_shards() fixture (no live cluster); the
# store envelope/fallback paths use a synthesised tar_meta()-shaped tibble.

analysis_scenario <- function() {
  ssd_define_scenario(
    ssd_scenario_data(d = data.frame(Conc = exp(seq(-1, 2, length.out = 20)))),
    nsim = 1L,
    seed = 42L,
    nrow = c(5L, 6L),
    ci = TRUE,
    nboot = c(10L, 50L),
    dists = ssd_distset(lnorm = "lnorm")
  )
}

# ---- target-name resolver (task 2) -----------------------------------------

test_that("cost-analysis: the resolver regenerates the factory's shard target names", {
  skip_if_not_installed("targets")
  skip_if_not_installed("tarchetypes")
  sc <- analysis_scenario()
  resolved <- cost_shard_target_names(sc)
  expect_setequal(names(resolved), c("step", "cell", "name"))
  expect_setequal(unique(resolved$step), c("sample", "fit", "hc"))

  tg <- ssd_scenario_targets(sc)
  factory_names <- c(
    purrr::map_chr(tg[[1L]][["sample_step"]], \(t) t$settings$name),
    purrr::map_chr(tg[[2L]][["fit_step"]], \(t) t$settings$name),
    purrr::map_chr(tg[[3L]][["hc_step"]], \(t) t$settings$name)
  )
  # The resolver's names are exactly the factory's shard target names.
  expect_setequal(resolved$name, factory_names)
})

test_that("cost-analysis: the store matcher excludes summary/upload and reports unmatched/NA", {
  skip_if_not_installed("targets")
  skip_if_not_installed("tarchetypes")
  sc <- analysis_scenario()
  names_tbl <- cost_shard_target_names(sc)
  meta <- tibble::tibble(
    name = c(names_tbl$name, "summary", "upload_fit_d_1", "bogus_target"),
    seconds = c(
      seq_len(nrow(names_tbl)),
      5,
      5,
      NA_real_ # an NA-seconds shape on a non-shard name (ignored as non-shard)
    )
  )
  res <- match_store_seconds(meta, names_tbl)
  # summary/upload excluded from the unmatched report; the stray target is named.
  expect_identical(res$unmatched, "bogus_target")
  # every shard name resolved to a (step, cell)
  expect_setequal(res$seconds$name, names_tbl$name)
  expect_true(all(c("step", "cell", "seconds") %in% names(res$seconds)))
})

test_that("cost-analysis: NA-seconds shard targets are dropped from totals and counted", {
  skip_if_not_installed("targets")
  skip_if_not_installed("tarchetypes")
  sc <- analysis_scenario()
  names_tbl <- cost_shard_target_names(sc)
  secs <- rep(10, nrow(names_tbl))
  secs[[1L]] <- NA_real_
  meta <- tibble::tibble(name = names_tbl$name, seconds = secs)
  res <- match_store_seconds(meta, names_tbl)
  expect_identical(res$n_na, 1L)
  expect_identical(nrow(res$seconds), nrow(names_tbl) - 1L)
})

# ---- ssd_analyse_cost() (task 3) -------------------------------------------

test_that("cost-analysis: analyse reads measured per-task cost and attributes it to axes", {
  sc <- analysis_scenario()
  run <- ssd_run_scenario_shards(sc)
  a <- ssd_analyse_cost(sc, root = run$dir)

  expect_s3_class(a, "ssdsims_cost_analysis")
  expect_true(a$measured)
  expect_s3_class(a$total, "difftime")
  expect_gt(as.numeric(a$total, units = "secs"), 0)
  expect_setequal(
    names(a$breakdown),
    c("ci_method", "nboot", "tasks", "seconds")
  )
  # one breakdown cell per (ci_method, nboot) the scenario fans out
  expect_setequal(a$breakdown$nboot, c(10L, 50L))
  # the observed total is the sum of measured hc + fit task durations
  hc <- read_step_timings(run$dir, "hc", "hc_id")
  fit <- read_step_timings(run$dir, "fit", "fit_id")
  expect_equal(
    as.numeric(a$total, units = "secs"),
    sum(hc$seconds) + sum(fit$seconds)
  )
})

test_that("cost-analysis: analyse is read-only (no RNG, no writes)", {
  sc <- analysis_scenario()
  run <- ssd_run_scenario_shards(sc)
  before_files <- list.files(run$dir, recursive = TRUE)
  set.seed(1)
  before_seed <- .Random.seed
  ssd_analyse_cost(sc, root = run$dir)
  expect_identical(.Random.seed, before_seed)
  expect_setequal(list.files(run$dir, recursive = TRUE), before_files)
})

test_that("cost-analysis: a pre-timing run without a store aborts informatively", {
  sc <- analysis_scenario()
  run <- ssd_run_scenario_shards(sc)
  # Strip the timing columns to simulate a pre-instrumentation run.
  strip <- function(step) {
    f <- list.files(
      file.path(run$dir, step),
      pattern = "part[.]parquet$",
      recursive = TRUE,
      full.names = TRUE
    )
    for (p in f) {
      df <- ssd_read_parquet(p)
      ssd_write_parquet(
        df[, setdiff(names(df), c(".start", ".end", ".host"))],
        p
      )
    }
  }
  strip("fit")
  strip("hc")
  expect_error(ssd_analyse_cost(sc, root = run$dir), "no `store`")
})

# ---- per-shard envelope with a synthesised store (task 6.2) ----------------

test_that("cost-analysis: the shard envelope is target seconds minus task durations", {
  skip_if_not_installed("targets")
  skip_if_not_installed("tarchetypes")
  sc <- analysis_scenario()
  run <- ssd_run_scenario_shards(sc)
  hc <- read_step_timings(run$dir, "hc", "hc_id")
  fit <- read_step_timings(run$dir, "fit", "fit_id")
  names_tbl <- cost_shard_target_names(sc)
  # Synthesise a tar_meta()-shaped tibble: each shard's wall seconds is generous,
  # so every per-shard overhead is positive.
  meta <- tibble::tibble(name = names_tbl$name, seconds = 1000)
  env <- shard_envelope(sc, meta, hc, fit)
  expect_true(all(
    c("step", "cell", "name", "seconds", "task_seconds", "overhead") %in%
      names(env$envelope)
  ))
  expect_equal(
    env$envelope$overhead,
    env$envelope$seconds - env$envelope$task_seconds
  )
  # only the timed layers (fit/hc) contribute shards to the envelope
  expect_setequal(unique(env$envelope$step), c("fit", "hc"))
})

# ---- ssd_calibrate_cost_from_run() (task 4) --------------------------------

test_that("cost-analysis: recalibration from a run drops into the estimator", {
  sc <- analysis_scenario()
  run <- ssd_run_scenario_shards(sc)
  cal <- ssd_calibrate_cost_from_run(sc, root = run$dir)
  expect_s3_class(cal, "ssdsims_cost_calibration")
  expect_identical(cal$provenance$source, "run")
  est <- ssd_estimate_cost(sc, cal)
  expect_s3_class(est, "ssdsims_cost_estimate")
  expect_gt(as.numeric(est$total, units = "secs"), 0)
})

test_that("cost-analysis: recalibration rejects a host not present in the run", {
  sc <- analysis_scenario()
  run <- ssd_run_scenario_shards(sc)
  expect_error(
    ssd_calibrate_cost_from_run(sc, root = run$dir, host = "no-such-cpu"),
    "not among"
  )
})

# ---- ssd_compare_cost() (task 5) -------------------------------------------

test_that("cost-analysis: compare places predicted beside observed with ratios", {
  sc <- analysis_scenario()
  run <- ssd_run_scenario_shards(sc)
  cmp <- ssd_compare_cost(
    sc,
    root = run$dir,
    calibration = test_cost_calibration()
  )
  expect_s3_class(cmp, "ssdsims_cost_comparison")
  expect_s3_class(cmp$predicted, "ssdsims_cost_estimate")
  expect_s3_class(cmp$observed, "ssdsims_cost_analysis")
  expect_true(is.numeric(cmp$total_ratio) && cmp$total_ratio > 0)
  expect_true(is.numeric(cmp$longest_ratio) && cmp$longest_ratio > 0)
})

# ---- deterministic print snapshots (task 6.3) ------------------------------

test_that("cost-analysis: analysis and comparison print is informative", {
  analysis <- new_ssdsims_cost_analysis(
    total = as.difftime(3661, units = "secs"),
    longest = as.difftime(120, units = "secs"),
    breakdown = tibble::tibble(
      ci_method = c("weighted_samples", "weighted_samples"),
      nboot = c(50L, 10L),
      tasks = c(2L, 2L),
      seconds = c(2400, 1261)
    ),
    fit_seconds = as.difftime(5, units = "secs"),
    hosts = "Test CPU @ 1.0GHz",
    measured = TRUE,
    envelope = NULL,
    provenance = list(source = "run")
  )
  expect_snapshot(print(analysis))

  comparison <- new_ssdsims_cost_comparison(
    predicted = ssd_estimate_cost(analysis_scenario(), test_cost_calibration()),
    observed = analysis,
    total_ratio = 1.05,
    longest_ratio = 0.91
  )
  expect_snapshot(print(comparison))
})

# ---- in-engine per-task duration (dd$ epoch) --------------------------------

test_that("cost-analysis: per-task seconds computed in DuckDB match difftime", {
  dir <- withr::local_tempdir()
  t0 <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  # Two `hc_id`s: "a" repeats (must collapse to one distinct timing row), "b"
  # once; sub-second gaps prove `dd$epoch` is not truncating to whole seconds.
  timings <- tibble::tibble(
    hc_id = c("a", "a", "b"),
    proportion = c(0.05, 0.1, 0.05),
    .start = rep(t0, 3L),
    .end = t0 + c(1.25, 1.25, 2.5),
    .host = "Test CPU"
  )
  ssd_write_parquet(timings, file.path(dir, "hc", "sim=1", "part.parquet"))

  out <- read_step_timings(dir, "hc", "hc_id")
  expect_true(attr(out, "timed"))
  out <- out[order(out$hc_id), ]
  expect_identical(nrow(out), 2L)
  expect_equal(
    out$seconds,
    as.numeric(difftime(out$.end, out$.start, units = "secs"))
  )
  expect_equal(out$seconds, c(1.25, 2.5))
})
