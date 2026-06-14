# Copyright 2025 Australian Government Department of Climate Change,
# Energy, the Environment and Water
#
#    Licensed under the Apache License, Version 2.0 (the "License");
#    you may not use this file except in compliance with the License.
#    You may obtain a copy of the License at
#
#       https://www.apache.org/licenses/LICENSE-2.0
#
#    Unless required by applicable law or agreed to in writing, software
#    distributed under the License is distributed on an "AS IS" BASIS,
#    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#    See the License for the specific language governing permissions and
#    limitations under the License.

# The DuckDB settings the config scope manages, as the engine reports them.
dc_settings <- function() {
  duckplyr_current_settings()
}

# DuckDB renders `memory_limit` in binary units, so "1GB" reads back as
# "953.6 MiB" and "200MB" as "190.7 MiB". Match on the rendered value of the
# value we set rather than parsing units.
dc_rendered_limit <- function(value) {
  old <- dc_settings()$memory_limit
  duckplyr::db_exec(paste0("SET memory_limit = '", value, "'"))
  rendered <- dc_settings()$memory_limit
  duckplyr::db_exec(paste0("SET memory_limit = '", old, "'"))
  rendered
}

# ---- scope behaviour (task 4.1) ---------------------------------------------

test_that("config scope pins one thread, a 1GB default limit, relaxed order", {
  withr::local_envvar(
    SSDSIMS_DUCKDB_THREADS = NA,
    SSDSIMS_DUCKDB_MEMORY_LIMIT = NA
  )
  one_gb <- dc_rendered_limit("1GB")
  before <- dc_settings()
  local({
    local_duckplyr_config()
    settings <- dc_settings()
    expect_identical(settings$threads, 1L)
    expect_identical(settings$memory_limit, one_gb)
    expect_false(settings$preserve_insertion_order)
  })
  expect_identical(
    dc_settings()$preserve_insertion_order,
    before$preserve_insertion_order
  )
})

test_that("config scope honours the env knobs", {
  withr::local_envvar(
    SSDSIMS_DUCKDB_THREADS = "2",
    SSDSIMS_DUCKDB_MEMORY_LIMIT = "200MB"
  )
  rendered <- dc_rendered_limit("200MB")
  local({
    local_duckplyr_config()
    settings <- dc_settings()
    expect_identical(settings$threads, 2L)
    expect_identical(settings$memory_limit, rendered)
  })
})

test_that("a malformed SSDSIMS_DUCKDB_THREADS aborts loudly", {
  withr::local_envvar(SSDSIMS_DUCKDB_THREADS = "lots")
  expect_error(
    local({
      local_duckplyr_config()
    }),
    "SSDSIMS_DUCKDB_THREADS"
  )
})

test_that("a malformed SSDSIMS_DUCKDB_MEMORY_LIMIT surfaces DuckDB's error", {
  withr::local_envvar(SSDSIMS_DUCKDB_MEMORY_LIMIT = "lots")
  expect_error(
    local({
      local_duckplyr_config()
    }),
    "memory_limit|Could not parse|parse",
    ignore.case = TRUE
  )
})

# ---- restore behaviour (task 4.2) -------------------------------------------

test_that("config scope restores custom pre-scope settings", {
  before <- dc_settings()
  withr::defer({
    duckplyr::db_exec(paste0("SET threads TO ", before$threads))
    duckplyr::db_exec(paste0("SET memory_limit = '", before$memory_limit, "'"))
  })
  duckplyr::db_exec("SET threads TO 2")
  duckplyr::db_exec("SET memory_limit = '300MB'")
  custom <- dc_settings()

  withr::local_envvar(
    SSDSIMS_DUCKDB_THREADS = NA,
    SSDSIMS_DUCKDB_MEMORY_LIMIT = NA
  )
  local({
    local_duckplyr_config()
    expect_identical(dc_settings()$threads, 1L)
  })
  expect_identical(dc_settings(), custom)

  # Error exit restores too.
  try(
    local({
      local_duckplyr_config()
      stop("boom")
    }),
    silent = TRUE
  )
  expect_identical(dc_settings(), custom)
})

test_that("nested config scopes restore layer by layer", {
  before <- dc_settings()
  withr::defer({
    duckplyr::db_exec(paste0("SET threads TO ", before$threads))
    duckplyr::db_exec(paste0("SET memory_limit = '", before$memory_limit, "'"))
  })
  duckplyr::db_exec("SET threads TO 2")
  custom <- dc_settings()

  withr::local_envvar(SSDSIMS_DUCKDB_THREADS = NA)
  local({
    local_duckplyr_config()
    outer <- dc_settings()
    expect_identical(outer$threads, 1L)
    local({
      withr::local_envvar(SSDSIMS_DUCKDB_THREADS = "3")
      local_duckplyr_config()
      expect_identical(dc_settings()$threads, 3L)
    })
    # Inner scope exit restores the outer scope's settings.
    expect_identical(dc_settings(), outer)
  })
  # Outer scope exit restores the original custom settings.
  expect_identical(dc_settings(), custom)
})

# ---- telemetry env vars (task 4.3) ------------------------------------------

test_that("config scope silences duckplyr telemetry and restores after", {
  withr::local_envvar(
    DUCKPLYR_FALLBACK_COLLECT = "5",
    DUCKPLYR_FALLBACK_AUTOUPLOAD = NA
  )
  local({
    local_duckplyr_config()
    expect_identical(Sys.getenv("DUCKPLYR_FALLBACK_COLLECT"), "0")
    expect_identical(Sys.getenv("DUCKPLYR_FALLBACK_AUTOUPLOAD"), "0")
  })
  expect_identical(Sys.getenv("DUCKPLYR_FALLBACK_COLLECT"), "5")
  expect_identical(
    Sys.getenv("DUCKPLYR_FALLBACK_AUTOUPLOAD", unset = NA_character_),
    NA_character_
  )
})

# ---- OOM is a loud, catchable R error (task 4.4) ----------------------------

test_that("a write beyond the memory limit errors catchably, session usable", {
  withr::local_envvar(SSDSIMS_DUCKDB_MEMORY_LIMIT = "100MB")
  # 100 x 50k doubles (~40MB of draws) needs ~5x its payload - beyond 100MB
  # (exploration/RESULTS.md) - while staying quick to build.
  df <- tibble::tibble(
    id = seq_len(100L),
    samples = lapply(seq_len(100L), function(i) as.double(seq_len(50000L)))
  )
  local({
    local_duckplyr_config()
    expect_error(
      ssd_write_parquet(df, file.path(withr::local_tempdir(), "x.parquet")),
      "memory|Memory",
      ignore.case = TRUE
    )
  })
  # The session (and the managed connection) remain usable afterwards.
  small <- tibble::tibble(x = 1:3)
  path <- file.path(withr::local_tempdir(), "ok.parquet")
  ssd_write_parquet(small, path)
  expect_identical(ssd_read_parquet(path)$x, 1:3)
})

# ---- runners apply the scope (task 4.6) -------------------------------------

test_that("step runners pin threads to 1 while the body runs", {
  before <- dc_settings()
  withr::defer({
    duckplyr::db_exec(paste0("SET threads TO ", before$threads))
    duckplyr::db_exec(paste0("SET memory_limit = '", before$memory_limit, "'"))
  })
  duckplyr::db_exec("SET threads TO 2")
  withr::local_envvar(
    SSDSIMS_DUCKDB_THREADS = NA,
    SSDSIMS_DUCKDB_MEMORY_LIMIT = NA
  )

  observed <- NULL
  testthat::local_mocked_bindings(
    sample_data_task_primer = function(data, n_max, replace, seed, primer) {
      observed <<- dc_settings()$threads
      data
    }
  )
  scenario <- ssd_define_scenario(
    ssd_scenario_data(d = data.frame(Conc = exp(seq(-1, 2, length.out = 6)))),
    nsim = 1L,
    seed = 42L,
    nrow = 5L,
    dists = "lnorm"
  )
  shards <- ssd_scenario_sample_shards(scenario)
  ssd_run_sample_step(
    shards$tasks[[1L]],
    scenario,
    file.path(withr::local_tempdir(), "sample")
  )
  expect_identical(observed, 1L)
  # And the runner restored the pre-call setting.
  expect_identical(dc_settings()$threads, 2L)
})

# ---- byte-budgeted full summary (task 4.7) ----------------------------------

# A tiny hand-built hc layer: `n` rows of `cell` doubles each, written as one
# shard Parquet under a Hive-ish tree, so `ssd_summarise()` can fan it in
# without running any fits. The byte-budgeted writer flushes between the
# engine's 2048-row vectors, so forcing a split needs n to span several
# vectors (5000 rows of tiny cells split as 2048+2048+904 under a 100KB
# budget - probed on duckdb 1.5.2).
dc_write_hc_layer <- function(dir, n = 5000L, cell = 10L) {
  hc <- tibble::tibble(
    proportion = 0.05,
    est = as.double(seq_len(n)),
    dists = "lnorm",
    samples = lapply(seq_len(n), function(i) as.double(seq_len(cell)) + i),
    hc_id = sprintf("hc-%05d", seq_len(n)),
    fit_id = sprintf("fit-%05d", seq_len(n))
  )
  ssd_write_parquet(hc, file.path(dir, "hc", "sim=1", "part.parquet"))
  hc
}

test_that("full summary writes byte-budgeted row groups, value-identical", {
  dir <- withr::local_tempdir()
  hc <- dc_write_hc_layer(dir)

  paths1 <- ssd_summarise(
    file.path(dir, "sample"),
    file.path(dir, "fit"),
    file.path(dir, "hc"),
    file.path(dir, "summary1.parquet"),
    path_with_samples = file.path(dir, "summary-samples1.parquet"),
    # 5000 rows x 10 doubles: a 100KB budget flushes between the engine's
    # 2048-row vectors, so the union splits without any large fixture.
    samples_row_group_bytes = "100KB"
  )
  before_order <- dplyr::collect(duckplyr::read_sql_duckdb(
    "SELECT current_setting('preserve_insertion_order') AS p"
  ))$p[[1L]]
  paths2 <- ssd_summarise(
    file.path(dir, "sample"),
    file.path(dir, "fit"),
    file.path(dir, "hc"),
    file.path(dir, "summary2.parquet"),
    path_with_samples = file.path(dir, "summary-samples2.parquet"),
    samples_row_group_bytes = "100KB"
  )

  # Multiple byte-sized row groups, not one union-wide group.
  groups <- dplyr::collect(duckplyr::read_sql_duckdb(paste0(
    "SELECT max(row_group_num_rows) AS rows FROM parquet_metadata('",
    paths1[[2L]],
    "') GROUP BY row_group_id"
  )))$rows
  expect_gt(length(groups), 1L)
  expect_lt(max(groups), nrow(hc))

  # Value-identity of the full summary across runs (order by key), and the
  # samples round-trip the hand-built layer.
  full1 <- ssd_read_parquet(paths1[[2L]])
  full2 <- ssd_read_parquet(paths2[[2L]])
  full1 <- full1[order(full1$hc_id), ]
  full2 <- full2[order(full2$hc_id), ]
  rownames(full1) <- rownames(full2) <- NULL
  expect_identical(full1, full2)
  expect_identical(full1$samples, hc$samples[order(hc$hc_id)])

  # Byte-identity of the compact summary across runs; it carries no samples.
  expect_identical(
    unname(tools::md5sum(paths1[[1L]])),
    unname(tools::md5sum(paths2[[1L]]))
  )
  expect_false("samples" %in% names(ssd_read_parquet(paths1[[1L]])))

  # The order setting is back where it was after the unordered write.
  after_order <- dplyr::collect(duckplyr::read_sql_duckdb(
    "SELECT current_setting('preserve_insertion_order') AS p"
  ))$p[[1L]]
  expect_identical(after_order, before_order)
})

# ---- byte-identity under constrained configuration (task 4.5) ---------------

test_that("constrained configuration leaves results byte-identical", {
  scenario <- ssd_define_scenario(
    ssd_scenario_data(d = data.frame(Conc = exp(seq(-1, 2, length.out = 6)))),
    nsim = 1L,
    seed = 42L,
    nrow = 5L,
    dists = "lnorm",
    ci = TRUE,
    nboot = 5,
    samples = TRUE
  )
  run_once <- function(dir) {
    run <- ssd_run_scenario_shards(scenario, dir = dir)
    ssd_summarise(
      file.path(dir, "sample"),
      file.path(dir, "fit"),
      file.path(dir, "hc"),
      file.path(dir, "summary.parquet"),
      path_with_samples = file.path(dir, "summary-samples.parquet")
    )
    shards <- sort(list.files(
      dir,
      pattern = "part[.]parquet$",
      recursive = TRUE
    ))
    list(
      shards = shards,
      hashes = unname(tools::md5sum(file.path(dir, shards))),
      compact = unname(tools::md5sum(file.path(dir, "summary.parquet"))),
      full = ssd_read_parquet(file.path(dir, "summary-samples.parquet"))
    )
  }

  dir1 <- withr::local_tempdir()
  default_knobs <- withr::with_envvar(
    c(SSDSIMS_DUCKDB_THREADS = NA, SSDSIMS_DUCKDB_MEMORY_LIMIT = NA),
    run_once(dir1)
  )
  dir2 <- withr::local_tempdir()
  constrained <- withr::with_envvar(
    c(SSDSIMS_DUCKDB_THREADS = "1", SSDSIMS_DUCKDB_MEMORY_LIMIT = "500MB"),
    run_once(dir2)
  )

  expect_identical(constrained$shards, default_knobs$shards)
  expect_identical(constrained$hashes, default_knobs$hashes)
  expect_identical(constrained$compact, default_knobs$compact)
  full1 <- default_knobs$full[order(default_knobs$full$hc_id), ]
  full2 <- constrained$full[order(constrained$full$hc_id), ]
  rownames(full1) <- rownames(full2) <- NULL
  expect_identical(full2, full1)
})
