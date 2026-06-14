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

# ---- distset: multi-set baseline == shards, byte-identical per task (6.2) ---

test_that("shard-runner: a multi-set scenario agrees with the baseline per (hc_id, distset)", {
  scenario <- ssd_define_scenario(
    ssd_scenario_data(d = sr_dataset()),
    nsim = 2L,
    seed = 42L,
    nrow = 6L,
    dists = ssd_distset(
      BCANZ = ssdtools::ssd_dists_bcanz(),
      Iwasaki = c("burrIII3", "gamma", "llogis", "lnorm", "weibull"),
      lnorm = "lnorm"
    )
  )
  run <- ssd_run_scenario_shards(scenario, dir = withr::local_tempdir())
  base <- ssd_run_scenario_baseline(scenario)

  # The fit step fits the union once; one hc shard per (dataset, sim) holds all
  # three pools (distset bundled by default).
  expect_length(run$fit, nrow(ssd_scenario_fit_tasks(scenario)))
  expect_length(run$hc, 2L) # (dataset, sim) cells

  hc_pipe <- ssd_read_parquet(file.path(run$dir, "hc", "**", "part.parquet"))
  expect_true("distset" %in% names(hc_pipe))
  expect_setequal(unique(hc_pipe$distset), c("BCANZ", "Iwasaki", "lnorm"))

  # The pipeline tags each row with distset; the baseline carries distset on the
  # hc task row, so stack it onto each per-task tibble for the join.
  base_rows <- do.call(
    rbind,
    Map(
      function(tb, id, ds) {
        tb <- tibble::as_tibble(tb)
        tb$hc_id <- id
        tb$distset <- ds
        tb
      },
      base$hc$hc,
      base$hc$hc_id,
      base$hc$distset
    )
  )
  key <- function(t) {
    t[order(t$hc_id, t$dist), c("hc_id", "distset", "dist", "est")]
  }
  expect_equal(key(hc_pipe), key(base_rows))
})

# ---- distset: an empty subset emits no rows; summarise unions survivors (6.5)

test_that("shard-runner: a set whose members all dropped emits zero rows and survivors are unioned", {
  scenario <- ssd_define_scenario(
    ssd_scenario_data(d = sr_dataset()),
    nsim = 1L,
    seed = 42L,
    nrow = 6L,
    dists = ssd_distset(lnorm = "lnorm", gamma = "gamma")
  )
  dir <- withr::local_tempdir()
  run <- ssd_run_scenario_shards(scenario, dir = dir)

  # Simulate the `gamma` members all dropping from the union fit: rewrite each
  # fit shard's blob to a fit of `lnorm` only (so `subset(fit, "gamma")` is
  # empty), keeping the `fit_id` so the hc step still resolves its parent.
  fit_files <- list.files(
    file.path(dir, "fit"),
    pattern = "part.parquet",
    recursive = TRUE,
    full.names = TRUE
  )
  for (f in fit_files) {
    tb <- ssd_read_parquet(f)
    for (i in seq_len(nrow(tb))) {
      local_dqrng_backend()
      lnorm_only <- ssdtools::ssd_fit_dists(
        utils::head(sr_dataset(), 6L),
        dists = "lnorm",
        silent = TRUE,
        nrow = 5L
      )
      tb$fit_blob[i] <- encode_obj(lnorm_only)
    }
    ssd_write_parquet(tb, f)
  }

  # Re-run only the hc step over the doctored fit shards.
  unlink(file.path(dir, "hc"), recursive = TRUE)
  local_duckplyr_config()
  local_dqrng_backend()
  hc_shards <- ssd_scenario_hc_shards(scenario)
  for (i in seq_len(nrow(hc_shards))) {
    ssd_run_hc_step(
      hc_shards$tasks[[i]],
      scenario,
      file.path(dir, "fit"),
      file.path(dir, "hc")
    )
  }

  hc_pipe <- ssd_read_parquet(file.path(dir, "hc", "**", "part.parquet"))
  # gamma all-dropped -> zero rows for that distset; lnorm survives.
  expect_setequal(unique(hc_pipe$distset), "lnorm")
  expect_false("gamma" %in% hc_pipe$distset)

  # ssd_summarise() unions the survivors (the lnorm rows).
  summary_path <- file.path(dir, "summary.parquet")
  ssd_summarise(
    file.path(dir, "sample"),
    file.path(dir, "fit"),
    file.path(dir, "hc"),
    summary_path
  )
  summ <- ssd_read_parquet(summary_path)
  expect_setequal(unique(summ$distset), "lnorm")
  expect_gt(nrow(summ), 0L)
})

# ---- oracle: results match the in-memory baseline (task 5.1) ---------------

test_that("shard-runner: per-task results match the in-memory baseline", {
  scenario <- ssd_define_scenario(
    ssd_scenario_data(d = sr_dataset()),
    nsim = 2L,
    seed = 42L,
    nrow = c(5L, 10L),
    rescale = c(FALSE, TRUE),
    dists = ssd_distset(set = c("lnorm", "gamma"))
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
    ssd_scenario_data(d = sr_dataset()),
    nsim = 2L,
    nrow = c(5L, 10L),
    seed = 42L,
    rescale = c(FALSE, TRUE),
    dists = ssd_distset(lnorm = "lnorm")
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
    ssd_scenario_data(d = sr_dataset()),
    nsim = 1L,
    seed = 42L,
    nrow = 6L,
    replace = c(FALSE, TRUE),
    dists = ssd_distset(lnorm = "lnorm")
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
    ssd_scenario_data(d = sr_dataset()),
    nsim = 1L,
    seed = 42L,
    nrow = c(5L, 10L),
    rescale = c(FALSE, TRUE),
    dists = ssd_distset(lnorm = "lnorm")
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
    ssd_scenario_data(d = sr_dataset()),
    nsim = 2L,
    seed = 42L,
    nrow = c(5L, 10L),
    rescale = c(FALSE, TRUE),
    dists = ssd_distset(lnorm = "lnorm")
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
    ssd_scenario_data(d = sr_dataset()),
    nsim = 2L,
    seed = 42L,
    nrow = 6L,
    dists = ssd_distset(lnorm = "lnorm")
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
    ssd_scenario_data(d = sr_dataset()),
    nsim = 1L,
    nrow = c(5L, 10L),
    seed = 42L,
    rescale = c(FALSE, TRUE),
    dists = ssd_distset(lnorm = "lnorm")
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
    ssd_scenario_data(d = sr_dataset()),
    nsim = 1L,
    seed = 42L,
    nrow = 6L,
    dists = ssd_distset(lnorm = "lnorm")
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

# ---- blob encoding: lossless round-trip through the shard Parquet (task 3.1) -
#
# The byte-identity oracle "read back through the hc path equals the baseline"
# is covered by the per-task-results test above (it decodes each `fit_blob` and
# compares to `ssd_run_scenario_baseline()`); this pins the seam itself - the
# encode/decode round trip and its survival through a Parquet `VARCHAR`.

test_that("shard-runner: a fit object round-trips losslessly through the encode/decode seam and a Parquet VARCHAR", {
  scenario <- ssd_define_scenario(
    ssd_scenario_data(d = sr_dataset()),
    nsim = 1L,
    seed = 42L,
    nrow = 6L,
    dists = ssd_distset(set = c("lnorm", "gamma"))
  )
  fit <- ssd_run_scenario_baseline(scenario)$fit$fits[[1L]]

  # the seam alone is lossless (a deserialised fitdists has nil TMB pointers
  # that waldo flags but all.equal ignores - matching the oracle above).
  expect_true(isTRUE(all.equal(decode_obj(encode_obj(fit)), fit)))

  # and it survives a Parquet VARCHAR round trip: write -> read -> decode, with
  # the blob carried as a string column (no raw/list column, which duckplyr
  # cannot store).
  path <- file.path(withr::local_tempdir(), "part.parquet")
  ssd_write_parquet(
    tibble::tibble(fit_id = "f1", fit_blob = encode_obj(fit)),
    path
  )
  back <- ssd_read_parquet(path)
  expect_type(back$fit_blob, "character")
  expect_true(isTRUE(all.equal(decode_obj(back$fit_blob), fit)))
})

# ---- blob encoding: the blob column projects out without decoding (task 3.2) -

test_that("shard-runner: the summary-style projection drops the blob column without decoding it", {
  path <- file.path(withr::local_tempdir(), "part.parquet")
  # A `fit_blob` value that is NOT a decodable object: if the projection pulled
  # the bytes into R and decoded them, `decode_obj()` would error - so a clean
  # projection that simply omits the column proves no decode happened.
  ssd_write_parquet(
    tibble::tibble(
      fit_id = c("f1", "f2"),
      fit_blob = c("not-a-blob", "garbage")
    ),
    path
  )

  # Mirror the §6 ssd_summarise() projection: drop the heavy column at the
  # DuckDB level, before anything is collected into R.
  projected <- tibble::as_tibble(dplyr::collect(
    dplyr::select(
      duckplyr::read_parquet_duckdb(
        path,
        options = list(hive_partitioning = FALSE)
      ),
      -dplyr::any_of("fit_blob")
    )
  ))

  expect_named(projected, "fit_id")
  expect_false("fit_blob" %in% names(projected))
  expect_identical(projected$fit_id, c("f1", "f2"))
})
