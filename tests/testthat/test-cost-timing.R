# Timing instrumentation (cost-analysis): the fit/hc shard rows and the baseline
# fit/hc tibbles carry per-task `.start`/`.end`/`.host`; the `sample` layer is
# untouched and stays file-level deterministic; the per-task result columns are
# unchanged by instrumentation (the narrowed byte-identity contract).

timing_scenario <- function() {
  ssd_define_scenario(
    ssd_scenario_data(d = numeric_dataset()),
    nsim = 1L,
    seed = 42L,
    nrow = 6L,
    ci = TRUE,
    nboot = c(10L, 50L),
    dists = ssd_distset(lnorm = "lnorm")
  )
}

result_cols <- function(df) {
  df[, setdiff(names(df), c(".start", ".end", ".host"))]
}

test_that("cost-analysis: fit and hc shards carry per-task timing columns", {
  run <- ssd_run_scenario_shards(timing_scenario())
  fit <- ssd_read_parquet(file.path(run$dir, "fit", "**", "part.parquet"))
  hc <- ssd_read_parquet(file.path(run$dir, "hc", "**", "part.parquet"))

  expect_true(all(c(".start", ".end", ".host") %in% names(fit)))
  expect_true(all(c(".start", ".end", ".host") %in% names(hc)))
  expect_s3_class(fit$.start, "POSIXct")
  expect_s3_class(hc$.end, "POSIXct")
  expect_type(fit$.host, "character")
  expect_true(all(fit$.end >= fit$.start))
  expect_true(all(hc$.end >= hc$.start))
  # On hc the timing repeats across a task's rows (one distinct triple per hc_id).
  per_task <- !duplicated(hc$hc_id)
  expect_identical(sum(per_task), length(unique(hc$hc_id)))
})

test_that("cost-analysis: sample shards carry no timing columns and stay byte-identical", {
  scenario <- timing_scenario()
  run1 <- ssd_run_scenario_shards(scenario, dir = withr::local_tempdir())
  run2 <- ssd_run_scenario_shards(scenario, dir = withr::local_tempdir())

  sample1 <- ssd_read_parquet(file.path(
    run1$dir,
    "sample",
    "**",
    "part.parquet"
  ))
  expect_false(any(c(".start", ".end", ".host") %in% names(sample1)))

  # Same scenario + seed: the `sample` part.parquet is byte-identical across runs
  # (no timing columns to perturb file-level determinism).
  rel <- function(dir) {
    files <- list.files(
      file.path(dir, "sample"),
      pattern = "part\\.parquet$",
      recursive = TRUE
    )
    sort(files)
  }
  files1 <- rel(run1$dir)
  expect_identical(files1, rel(run2$dir))
  md5_1 <- tools::md5sum(file.path(run1$dir, "sample", files1))
  md5_2 <- tools::md5sum(file.path(run2$dir, "sample", files1))
  expect_identical(unname(md5_1), unname(md5_2))
})

test_that("cost-analysis: fit/hc result columns are unchanged by instrumentation across runs", {
  scenario <- timing_scenario()
  run1 <- ssd_run_scenario_shards(scenario, dir = withr::local_tempdir())
  run2 <- ssd_run_scenario_shards(scenario, dir = withr::local_tempdir())

  fit1 <- ssd_read_parquet(file.path(run1$dir, "fit", "**", "part.parquet"))
  fit2 <- ssd_read_parquet(file.path(run2$dir, "fit", "**", "part.parquet"))
  fit1 <- fit1[order(fit1$fit_id), ]
  fit2 <- fit2[order(fit2$fit_id), ]
  # Result columns (timing excluded) are byte-identical; the serialised fit blob
  # is the per-task (seed, primer) result, invariant across runs.
  expect_identical(result_cols(fit1), result_cols(fit2))

  hc1 <- ssd_read_parquet(file.path(run1$dir, "hc", "**", "part.parquet"))
  hc2 <- ssd_read_parquet(file.path(run2$dir, "hc", "**", "part.parquet"))
  ord <- function(df) df[order(df$hc_id, df$dist), ]
  expect_equal(result_cols(ord(hc1))$est, result_cols(ord(hc2))$est)
})

test_that("cost-analysis: the baseline runner carries the same timing columns", {
  base <- ssd_run_scenario_baseline(timing_scenario())
  expect_true(all(c(".start", ".end", ".host") %in% names(base$fit)))
  expect_true(all(c(".start", ".end", ".host") %in% names(base$hc)))
  expect_s3_class(base$fit$.start, "POSIXct")
  expect_true(all(base$hc$.end >= base$hc$.start))
  expect_type(base$hc$.host, "character")
})
