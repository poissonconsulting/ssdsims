## Build a targets pipeline whose branching axes are configurable.
##
## The point is to study how the choice of split axis and resulting batch size
## affects (a) total wall time on a fixed compute budget, and (b) the size
## distribution of individual work units, which drives interruption tolerance.
##
## A config is a list with fields:
##   id           – string label, used in output paths
##   size         – {"small","medium","large"}; sets the (proportion,
##                  ci_method, nboot) grid
##   split        – {"split0","split1","split2","split3"}; which axes branch
##                  vs. stay as in-branch vectors
##   nsim         – integer, sims per nrow
##   nrow_levels  – integer vector of `nrow` values
##   proportion   – numeric vector
##   ci_method    – character vector
##   nboot        – integer vector

## Helpers reused from the production workflow.
source("../R/functions.R")

#' Build the branches tibble: one row per dynamic branch.
build_branches_tbl <- function(config) {
  axes_all <- list(
    nrow = config$nrow_levels,
    ci_method = config$ci_method,
    nboot = config$nboot,
    proportion = config$proportion
  )
  branched <- config$split_axes
  if (length(branched) == 0L) {
    grid <- tibble::tibble(.row = 1L)
  } else {
    grid <- do.call(tidyr::expand_grid, axes_all[branched])
  }
  vectored <- setdiff(names(axes_all), branched)
  for (nm in vectored) {
    grid[[nm]] <- replicate(nrow(grid), axes_all[[nm]], simplify = FALSE)
  }
  if (".row" %in% names(grid)) grid$.row <- NULL
  grid
}

#' Run one experiment branch: pick the right pre-computed fit and run
#' ssd_hc_sims with the args carried in this row of the branches tibble.
#' Writes the flattened result to a uniquely-named parquet file.
run_experiment_branch <- function(
  fits_by_nrow,
  nrow_levels,
  branches_tbl,
  out_dir
) {
  ## branches_tbl is a 1-row slice when patterned with map().
  row <- as.list(branches_tbl)
  ## list-cols come through as length-1 lists; unwrap.
  row <- lapply(row, function(x) {
    if (is.list(x) && length(x) == 1L) x[[1]] else x
  })

  fit_idx <- match(row$nrow, nrow_levels)
  fit <- fits_by_nrow[[fit_idx]]

  hc <- ssdsims::ssd_hc_sims(
    fit,
    proportion = row$proportion,
    ci_method = row$ci_method,
    nboot = row$nboot,
    est_method = "multi",
    ci = TRUE,
    samples = TRUE,
    parametric = TRUE,
    delta = Inf
  )
  flat <- flatten_hc_sims(hc)

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  key <- digest::digest(row, algo = "xxhash32")
  out <- file.path(out_dir, paste0("branch-", key, ".parquet"))
  duckplyr::compute_parquet(duckplyr::as_duckdb_tibble(flat), out)
  out
}

#' Top-level pipeline builder. `config` is read from the SSDSIMS_EXP_CONFIG
#' env var by `_targets.R` and lives in globalenv, so worker processes can
#' look it up the same way when they re-source `_targets.R`.
build_pipeline <- function(config) {
  list(
    targets::tar_target(boron, ssddata::ccme_boron),
    targets::tar_target(nrow_levels, config$nrow_levels),
    targets::tar_target(
      fits_by_nrow,
      sims_fits_for(boron, nrow_levels, nsim = config$nsim),
      pattern = map(nrow_levels),
      iteration = "list"
    ),
    targets::tar_target(branches_tbl, build_branches_tbl(config)),
    targets::tar_target(
      branch_results,
      run_experiment_branch(
        fits_by_nrow,
        nrow_levels,
        branches_tbl,
        out_dir = file.path("data", config$id)
      ),
      pattern = map(branches_tbl),
      format = "file"
    )
  )
}
