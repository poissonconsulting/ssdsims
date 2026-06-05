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

# A small numeric-only dataset: avoids factor/character columns so a draw
# round-trips through Parquet byte-identically (the byte-identity oracle).
numeric_dataset <- function() {
  data.frame(Conc = exp(seq(-1, 2, length.out = 20)))
}

# Gate the dqrng-backend tests that spawn a fresh R process via `callr` and
# `library(ssdsims)` (to exercise the conditional-dependency / RNG-slot paths in
# isolation). They need ssdsims installed -- true under R CMD check, but NOT
# under a bare `devtools::test()` where the package is only `load_all()`ed.
# `skip_if_not_installed("ssdsims")` is insufficient: under `load_all()` the
# namespace is loaded, so `requireNamespace()` succeeds even though there is no
# on-disk package for the subprocess to load. Check `installed.packages()`
# instead, which only lists genuinely installed packages.
skip_if_ssdsims_not_installed <- function() {
  testthat::skip_if_not_installed("callr")
  testthat::skip_if_not(
    "ssdsims" %in% rownames(utils::installed.packages()),
    "ssdsims is not installed (run under R CMD check, not devtools::test())"
  )
}

# Skip a randtoolbox-dependent test UNLESS randtoolbox is installed -- checked
# via `installed.packages()`, NOT `skip_if_not_installed()`/`requireNamespace()`.
# The latter would *load* randtoolbox into the test process, co-loading a second
# user-supplied RNG alongside dqrng. That is the exact process-global hazard this
# change guards against: it places randtoolbox's (uninitialised) `user_unif_rand`
# in the slot, so a later `dqrng::register_methods()` segfaults (case3). The
# foreign-hijack test loads randtoolbox only inside its `callr` subprocess.
skip_if_randtoolbox_not_installed <- function() {
  testthat::skip_if_not(
    "randtoolbox" %in% rownames(utils::installed.packages()),
    "randtoolbox is not installed"
  )
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
