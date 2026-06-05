# A numeric-only dataset: avoids factor/character columns so a draw round-trips
# through Parquet byte-identically (the oracle comparison).
sr_dataset <- function() {
  data.frame(Conc = exp(seq(-1, 2, length.out = 20)))
}

# Stack the in-memory baseline's per-step results into one tibble keyed by
# `<step>_id`, for comparison against the read-back shards.
baseline_hc_rows <- function(base) {
  rows <- Map(
    function(tb, id) {
      tb <- tibble::as_tibble(tb)
      tb$hc_id <- id
      tb
    },
    base$hc$hc,
    base$hc$hc_id
  )
  do.call(rbind, rows)
}

# ---- oracle: results match the in-memory baseline (task 5.1) ---------------

test_that("shard-runner: per-task results match the in-memory baseline", {
  scenario <- ssd_define_scenario(
    ssd_data(d = sr_dataset()),
    nsim = 2L,
    seed = 42L,
    nrow = c(5L, 10L),
    rescale = c(FALSE, TRUE),
    dists = c("lnorm", "gamma")
  )
  run <- ssd_run_scenario_shards(scenario, dir = withr::local_tempdir())
  base <- ssd_run_scenario_baseline(scenario)

  # fit: each fit object equals the baseline's (value-level; a deserialised
  # fitdists has nil TMB pointers that waldo flags but all.equal ignores).
  fit_pipe <- ssd_read_parquet(file.path(run$dir, "fit", "**", "part.parquet"))
  for (k in seq_along(base$fit$fit_id)) {
    fp <- decode_obj(fit_pipe$fit_blob[match(
      base$fit$fit_id[k],
      fit_pipe$fit_id
    )])
    expect_true(isTRUE(all.equal(fp, base$fit$fits[[k]])))
  }

  # hc: estimate rows equal, joined on the hc_id identity.
  hc_pipe <- ssd_read_parquet(file.path(run$dir, "hc", "**", "part.parquet"))
  hc_pipe <- hc_pipe[order(hc_pipe$hc_id, hc_pipe$dist), ]
  base_hc <- baseline_hc_rows(base)
  base_hc <- base_hc[order(base_hc$hc_id, base_hc$dist), ]
  expect_equal(hc_pipe$est, base_hc$est)
})

# ---- re-layout: partition_by shifts paths, not results (task 5.2) ----------

test_that("shard-runner: re-layout shifts shard paths but not results", {
  args <- list(
    ssd_data(d = sr_dataset()),
    nsim = 2L,
    nrow = c(5L, 10L),
    seed = 42L,
    rescale = c(FALSE, TRUE),
    dists = "lnorm"
  )
  fine <- do.call(ssd_define_scenario, args)
  coarse <- do.call(
    ssd_define_scenario,
    c(args, list(partition_by = list(fit = c("dataset", "sim"))))
  )
  run_fine <- ssd_run_scenario_shards(fine, dir = withr::local_tempdir())
  run_coarse <- ssd_run_scenario_shards(coarse, dir = withr::local_tempdir())

  # the fit shard layout differs (fine: per (nrow, rescale); coarse: per (dataset, sim))
  rel <- function(run) {
    sort(sub(run$dir, "", run$fit, fixed = TRUE))
  }
  expect_false(identical(rel(run_fine), rel(run_coarse)))
  expect_gt(length(run_fine$fit), length(run_coarse$fit))

  # but per-task hc results are byte-identical, joined on hc_id
  read_hc <- function(run) {
    t <- ssd_read_parquet(file.path(run$dir, "hc", "**", "part.parquet"))
    t[order(t$hc_id, t$dist), c("hc_id", "dist", "est")]
  }
  expect_equal(read_hc(run_fine), read_hc(run_coarse))
})

# ---- m:n fan-in via inner `replace` (task 5.3) -----------------------------

test_that("shard-runner: a fit shard reads several sample shards (inner replace)", {
  # `replace` is a sample PATH axis but a fit INNER axis by default, so one fit
  # shard's tasks span two sample shards.
  scenario <- ssd_define_scenario(
    ssd_data(d = sr_dataset()),
    nsim = 1L,
    seed = 42L,
    nrow = 6L,
    replace = c(FALSE, TRUE),
    dists = "lnorm"
  )
  run <- ssd_run_scenario_shards(scenario, dir = withr::local_tempdir())
  # two sample shards (replace in the sample path), one fit shard (replace inner)
  expect_length(run$sample, 2L)
  expect_length(run$fit, 1L)
  base <- ssd_run_scenario_baseline(scenario)
  fit_pipe <- ssd_read_parquet(file.path(run$dir, "fit", "**", "part.parquet"))
  for (k in seq_along(base$fit$fit_id)) {
    fp <- decode_obj(fit_pipe$fit_blob[match(
      base$fit$fit_id[k],
      fit_pipe$fit_id
    )])
    expect_true(isTRUE(all.equal(fp, base$fit$fits[[k]])))
  }
})

# ---- m:n fan-in via coarse child (task 5.4) --------------------------------

test_that("shard-runner: an hc shard reads several fit shards (coarse hc)", {
  scenario <- ssd_define_scenario(
    ssd_data(d = sr_dataset()),
    nsim = 1L,
    seed = 42L,
    nrow = c(5L, 10L),
    rescale = c(FALSE, TRUE),
    dists = "lnorm"
  )
  run <- ssd_run_scenario_shards(scenario, dir = withr::local_tempdir())
  # default: fit shards on (dataset, sim, nrow, rescale) = 4; hc on (dataset, sim) = 1
  expect_length(run$fit, 4L)
  expect_length(run$hc, 1L)
  base <- ssd_run_scenario_baseline(scenario)
  hc_pipe <- ssd_read_parquet(file.path(run$dir, "hc", "**", "part.parquet"))
  hc_pipe <- hc_pipe[order(hc_pipe$hc_id, hc_pipe$dist), ]
  base_hc <- baseline_hc_rows(base)
  base_hc <- base_hc[order(base_hc$hc_id, base_hc$dist), ]
  expect_equal(hc_pipe$est, base_hc$est)
})

# ---- shard count follows partition_by, not task count (task 5.5) -----------

test_that("shard-runner: shard count equals the path-cell count, not task count", {
  scenario <- ssd_define_scenario(
    ssd_data(d = sr_dataset()),
    nsim = 2L,
    seed = 42L,
    nrow = c(5L, 10L),
    rescale = c(FALSE, TRUE),
    dists = "lnorm"
  )
  run <- ssd_run_scenario_shards(scenario, dir = withr::local_tempdir())
  # sample path (dataset, sim, replace): 1 * 2 * 1 = 2
  expect_length(run$sample, 2L)
  # fit path (dataset, sim, nrow, rescale): 1 * 2 * 2 * 2 = 8
  expect_length(run$fit, 8L)
  # hc path (dataset, sim): 1 * 2 = 2
  expect_length(run$hc, 2L)
  # one file per shard on disk
  expect_length(
    list.files(
      file.path(run$dir, "fit"),
      pattern = "part.parquet",
      recursive = TRUE
    ),
    8L
  )
})

# ---- reproducible and order-independent (task 5.6) -------------------------

test_that("shard-runner: re-running yields identical per-task results", {
  scenario <- ssd_define_scenario(
    ssd_data(d = sr_dataset()),
    nsim = 2L,
    seed = 42L,
    nrow = 6L,
    dists = "lnorm"
  )
  run1 <- ssd_run_scenario_shards(scenario, dir = withr::local_tempdir())
  run2 <- ssd_run_scenario_shards(scenario, dir = withr::local_tempdir())
  read_hc <- function(run) {
    t <- ssd_read_parquet(file.path(run$dir, "hc", "**", "part.parquet"))
    t[order(t$hc_id, t$dist), ]
  }
  expect_equal(read_hc(run1)$est, read_hc(run2)$est)
})

# ---- owns its output tree: a re-layout leaves no stale shards ---------------

test_that("shard-runner: re-running with a changed partition_by leaves no stale shards", {
  dir <- withr::local_tempdir()
  base_args <- list(
    ssd_data(d = sr_dataset()),
    nsim = 1L,
    nrow = c(5L, 10L),
    seed = 42L,
    rescale = c(FALSE, TRUE),
    dists = "lnorm"
  )
  fine <- do.call(ssd_define_scenario, base_args)
  run_fine <- ssd_run_scenario_shards(fine, dir = dir)
  expect_gt(length(run_fine$fit), 1L)

  # coarser fit layout (nrow/rescale become inner) -> fewer fit shards
  coarse <- do.call(
    ssd_define_scenario,
    c(base_args, list(partition_by = list(fit = c("dataset", "sim"))))
  )
  run_coarse <- ssd_run_scenario_shards(coarse, dir = dir)

  # the fit subtree now holds ONLY the current (coarse) layout's shards
  on_disk <- length(list.files(
    file.path(dir, "fit"),
    pattern = "part.parquet",
    recursive = TRUE
  ))
  expect_identical(on_disk, length(run_coarse$fit))
  expect_lt(on_disk, length(run_fine$fit))
})

# ---- missing-parent guard: symmetric with the hc step's fit_id guard ---------

test_that("shard-runner: fit step aborts when a parent sample draw is missing", {
  scenario <- ssd_define_scenario(
    ssd_data(d = sr_dataset()),
    nsim = 1L,
    seed = 42L,
    nrow = 6L,
    dists = "lnorm"
  )
  dir <- withr::local_tempdir()
  sample_dir <- file.path(dir, "sample")
  sample_shards <- ssd_scenario_sample_shards(scenario)
  for (i in seq_len(nrow(sample_shards))) {
    ssd_run_sample_step(sample_shards$tasks[[i]], scenario, sample_dir)
  }

  # Strip the parent draws (keep the shard files, drop their rows) so the fit
  # task's `.sample_id` resolves to no rows - the missing-parent case.
  files <- list.files(
    sample_dir,
    pattern = "part.parquet",
    recursive = TRUE,
    full.names = TRUE
  )
  for (f in files) {
    ssd_write_parquet(ssd_read_parquet(f)[0L, ], f)
  }

  fit_shards <- ssd_scenario_fit_shards(scenario)
  expect_error(
    ssd_run_fit_step(
      fit_shards$tasks[[1L]],
      scenario,
      sample_dir,
      file.path(dir, "fit")
    ),
    "found none in the parent .sample. shard"
  )
})
