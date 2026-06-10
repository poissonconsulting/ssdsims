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

save_csv <- function(x) {
  path <- tempfile(fileext = ".csv")
  readr::write_csv(x, path)
  path
}

expect_snapshot_data <- function(x, name, digits = 6) {
  if (!requireNamespace("readr", quietly = TRUE)) {
    warning("Package 'readr' is required for this test.")
    return(invisible())
  }

  fun <- function(x) if (is.numeric(x)) signif(x, digits = digits) else x
  lapply_fun <- function(x) I(lapply(x, fun))
  x <- dplyr::mutate(x, dplyr::across(where(is.numeric), fun))
  n <- nrow(x)
  x <- dplyr::mutate(x, dplyr::across(where(is.list), \(.x) rep("list", n)))
  path <- save_csv(x)
  testthat::expect_snapshot_file(
    path,
    paste0(name, ".csv"),
    compare = testthat::compare_file_text
  )
}

# ---- targets-pipeline helpers (test-task-shards.R, test-path-axis-growth.R) --

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

# The targets-backed tests build pipelines that write Parquet through duckdb.
# Running that build *in the main test session* deadlocks once enough
# duckdb/duckplyr state has accumulated from the earlier test files (it does not
# when these files run on their own). Spawning a fresh `callr` subprocess per
# `tar_make()`/`tar_outdated()` is the safe-but-slow alternative the suite used
# before.
#
# Instead, route every pipeline call through ONE dedicated, reused `callr`
# session. It keeps the duckdb state out of the main session (so no deadlock),
# and because it only ever runs targets builds it stays as clean as those files
# do in isolation. Within that single session `callr_function = NULL` runs the
# build in-process, so the whole suite pays one R startup rather than one per
# call. The session is closed at suite teardown.
the_targets <- new.env(parent = emptyenv())

targets_session <- function() {
  s <- the_targets$session
  if (is.null(s) || !s$is_alive()) {
    s <- callr::r_session$new()
    the_targets$session <- s
    withr::defer(s$close(), envir = testthat::teardown_env())
  }
  s
}

# Run `fun` (a self-contained function) in the dedicated session, first matching
# its working directory to the caller's -- each test drives its own withr
# tempdir, and the `_targets` store and `results/` tree are resolved relative to
# it.
in_targets_session <- function(fun) {
  targets_session()$run(
    function(wd, fun) {
      setwd(wd)
      fun()
    },
    args = list(getwd(), fun)
  )
}

tar_make_local <- function() {
  in_targets_session(function() {
    suppressWarnings(targets::tar_make(
      reporter = "silent",
      callr_function = NULL
    ))
  })
}

tar_outdated_local <- function() {
  in_targets_session(function() {
    targets::tar_outdated(reporter = "silent", callr_function = NULL)
  })
}

# A small numeric-only dataset: avoids factor/character columns so a draw
# round-trips through Parquet byte-identically (the byte-identity oracle).
numeric_dataset <- function() {
  data.frame(Conc = exp(seq(-1, 2, length.out = 20)))
}

# A snapshot of a completed run for the path-axis-growth test: the shard target
# names the pipeline minted (one per shard, derived from the shard tables, not
# hard-coded), each shard Parquet's md5, and the summary row count. Captured
# before a path-axis growth, then re-read after the second `tar_make()` to assert
# the existing shards were skipped byte-identically and the summary grew. Only
# called from inside a fixture's working directory (a `targets` store + results).
growth_state <- function() {
  shards <- setdiff(targets::tar_progress()$name, "summary")
  files <- sort(list.files(
    "results",
    pattern = "part\\.parquet$",
    recursive = TRUE,
    full.names = TRUE
  ))
  summary_file <- list.files(
    "results",
    pattern = "summary\\.parquet$",
    recursive = TRUE,
    full.names = TRUE
  )
  list(
    shards = shards,
    md5 = tools::md5sum(files),
    summary_rows = nrow(ssd_read_parquet(summary_file))
  )
}
