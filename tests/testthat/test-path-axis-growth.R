# Path-axis growth (TARGETS-DESIGN.md section 8.1, end-to-end): appending a
# dataset or growing `nsim` grows an axis that *is* in every step's
# `partition_by`, so it mints new shard targets for the new path cells, leaves
# every existing shard cached, and re-runs `summary` over the enlarged set.
#
# Driven on the shipped default factory (no `cue`). The contract holds on the
# default factory because the two upstream decisions have landed:
# `hive-partitioning` pinned cache-by-existence over per-child upstream edges,
# and `step-scenario-slice` made the `sample` slice per-dataset - together they
# keep an existing shard's command byte-identical across the growth (a new
# dataset / `sim` is a new path cell, not a change to an existing one), so
# `targets` skips it. Build/skip is read from `tar_progress()` after a real
# second `tar_make()` (not `tar_outdated()`'s static estimate), corroborated by
# the cached shards' byte-identical Parquet and a `summary` row count that grew
# with the added shards.

growth_partition_by <- list(
  sample = c("dataset", "sim"),
  fit = c("dataset", "sim"),
  hc = c("dataset", "sim")
)

test_that("path-axis-growth: appending a dataset builds only the new dataset's shards and caches the rest", {
  skip_targets()
  dir <- withr::local_tempdir()
  file.copy(
    test_path("fixtures", "dataset-growth-targets.R"),
    file.path(dir, "_targets.R")
  )
  withr::local_dir(dir)
  saveRDS(list(d1 = numeric_dataset()), "datasets.rds")
  tar_make_local()
  orig <- growth_state()

  # the captured shard names are one per shard of each step's shard table (so the
  # skip/build sets below are derived from `ssd_scenario_*_shards()`, not strings)
  scenario <- ssd_define_scenario(
    ssd_scenario_data(d1 = numeric_dataset()),
    nsim = 1L,
    seed = 42L,
    nrow = 6L,
    dists = "lnorm",
    partition_by = growth_partition_by
  )
  expect_length(
    grep("^sample_step_", orig$shards),
    nrow(ssd_scenario_sample_shards(scenario))
  )
  expect_length(
    grep("^fit_step_", orig$shards),
    nrow(ssd_scenario_fit_shards(scenario))
  )
  expect_length(
    grep("^hc_step_", orig$shards),
    nrow(ssd_scenario_hc_shards(scenario))
  )

  # append a dataset (a path axis for all three steps), `partition_by` unchanged,
  # re-source and `tar_make()` again into the same store/root
  saveRDS(list(d1 = numeric_dataset(), d2 = numeric_dataset()), "datasets.rds")
  tar_make_local()
  progress <- targets::tar_progress()
  completed <- progress$name[progress$progress == "completed"]
  skipped <- progress$name[progress$progress == "skipped"]
  new_shards <- setdiff(completed, "summary")

  # every original shard skipped; only the new dataset's shards built; summary re-ran
  expect_setequal(skipped, orig$shards)
  expect_true(all(grepl("_d2(_|$)", new_shards)))
  expect_length(intersect(new_shards, orig$shards), 0L)
  expect_true("summary" %in% completed)

  # the cached shards' Parquets are byte-identical (no in-place rewrite)
  expect_identical(unname(tools::md5sum(names(orig$md5))), unname(orig$md5))
  # the summary grew over the enlarged shard set (catches a spuriously stable one)
  expect_gt(growth_state()$summary_rows, orig$summary_rows)
})

test_that("path-axis-growth: growing nsim builds only the new sim cells' shards and caches the rest", {
  skip_targets()
  dir <- withr::local_tempdir()
  file.copy(
    test_path("fixtures", "nsim-growth-targets.R"),
    file.path(dir, "_targets.R")
  )
  withr::local_dir(dir)
  saveRDS(numeric_dataset(), "data.rds")
  saveRDS(2L, "nsim.rds")
  tar_make_local()
  orig <- growth_state()

  scenario <- ssd_define_scenario(
    ssd_scenario_data(d = numeric_dataset()),
    nsim = 2L,
    seed = 42L,
    nrow = 6L,
    dists = "lnorm",
    partition_by = growth_partition_by
  )
  expect_length(
    grep("^sample_step_", orig$shards),
    nrow(ssd_scenario_sample_shards(scenario))
  )
  expect_length(
    grep("^fit_step_", orig$shards),
    nrow(ssd_scenario_fit_shards(scenario))
  )
  expect_length(
    grep("^hc_step_", orig$shards),
    nrow(ssd_scenario_hc_shards(scenario))
  )

  # grow `nsim` (adds new `sim` path cells for all three steps), `partition_by`
  # unchanged, re-source and `tar_make()` again into the same store/root
  saveRDS(3L, "nsim.rds")
  tar_make_local()
  progress <- targets::tar_progress()
  completed <- progress$name[progress$progress == "completed"]
  skipped <- progress$name[progress$progress == "skipped"]
  new_shards <- setdiff(completed, "summary")

  # every prior sim's shard skipped; only the added sim's shards built; summary re-ran
  expect_setequal(skipped, orig$shards)
  expect_true(all(grepl("_3$", new_shards)))
  expect_length(intersect(new_shards, orig$shards), 0L)
  expect_true("summary" %in% completed)

  expect_identical(unname(tools::md5sum(names(orig$md5))), unname(orig$md5))
  expect_gt(growth_state()$summary_rows, orig$summary_rows)
})
