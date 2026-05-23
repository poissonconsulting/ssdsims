## Build a targets pipeline with the fit-task fan-out happening in
## targets, not in `ssd_fit_dists_sims()`.
##
## Atomic task: one `fit_dists_seed()` call → one parquet file.
##   The parquet contains every HC row downstream of that fit (the
##   cross-product of the experiment's ci_method × nboot × proportion).
##
## A "job" (= dynamic branch) bundles one or more atomic tasks; the
## bundling is controlled by `config$split_axes`, which become the
## group columns of the atomic grid.
##
## A config is a list with fields:
##   id          – string label, used in output paths
##   split       – split name (matches a key of SPLITS in configs.R)
##   split_axes  – character vector of fit-grid columns to group by
##   grid        – list with: nrow_levels, nsim, stream, ci_method,
##                 nboot, proportion

## flatten_hc_sims() comes from the production helpers.
source("../R/functions.R")

#' One row per atomic fit task. The HC sweep is *not* expanded into
#' rows here — it's applied within the task — so the atomic grid stays
#' at the fit-layer granularity.
build_fit_grid <- function(grid) {
  tidyr::expand_grid(
    nrow   = grid$nrow_levels,
    sim    = seq_len(grid$nsim),
    stream = grid$stream
  )
}

#' Run all atomic fit tasks in this branch and write one parquet per
#' task. `branch_rows` is the slice of the grouped fit-grid that
#' dynamic branching handed us — 1+ rows depending on the split.
run_experiment_branch <- function(branch_rows, boron, grid, out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  fit_defaults <- list(
    dists          = ssdtools::ssd_dists_bcanz(),
    rescale        = FALSE,
    computable     = FALSE,
    at_boundary_ok = TRUE,
    min_pmix       = ssdtools::ssd_min_pmix,
    range_shape1   = c(0.05, 20),
    range_shape2   = c(0.05, 20),
    silent         = TRUE
  )

  paths <- character(nrow(branch_rows))
  for (i in seq_len(nrow(branch_rows))) {
    row <- branch_rows[i, , drop = FALSE]

    ## --- Atomic fit task: one fit_dists_seed() call -------------------
    sim_tbl <- ssdsims::ssd_sim_data(
      boron,
      nrow      = row$nrow,
      nsim      = 1L,
      start_sim = row$sim,
      stream    = row$stream
    )
    data_one <- sim_tbl$data[[1]]

    fit <- ssdsims:::fit_dists_seed(
      data           = data_one,
      sim            = row$sim,
      stream         = row$stream,
      seed           = NULL,
      dists          = fit_defaults$dists,
      rescale        = fit_defaults$rescale,
      computable     = fit_defaults$computable,
      at_boundary_ok = fit_defaults$at_boundary_ok,
      min_pmix       = fit_defaults$min_pmix,
      range_shape1   = fit_defaults$range_shape1,
      range_shape2   = fit_defaults$range_shape2,
      silent         = fit_defaults$silent
    )

    ## --- HC sweep for this fit ---------------------------------------
    ## ssd_hc_sims expects a fits tibble; build a 1-row one from the
    ## values we just produced.
    fits_tbl <- tibble::tibble(
      sim            = row$sim,
      stream         = row$stream,
      nrow           = row$nrow,
      replace        = FALSE,
      data           = list(data_one),
      rescale        = fit_defaults$rescale,
      computable     = fit_defaults$computable,
      at_boundary_ok = fit_defaults$at_boundary_ok,
      min_pmix       = list(fit_defaults$min_pmix),
      range_shape1   = list(fit_defaults$range_shape1),
      range_shape2   = list(fit_defaults$range_shape2),
      fits           = list(fit)
    )

    hc <- ssdsims::ssd_hc_sims(
      fits_tbl,
      proportion = grid$proportion,
      ci_method  = grid$ci_method,
      nboot      = grid$nboot,
      est_method = "multi",
      ci         = TRUE,
      samples    = TRUE,
      parametric = TRUE,
      delta      = Inf
    )
    flat <- flatten_hc_sims(hc)

    ## --- One parquet per fit_dists_seed call -------------------------
    key <- digest::digest(
      list(sim = row$sim, stream = row$stream, nrow = row$nrow),
      algo = "xxhash32"
    )
    out <- file.path(
      out_dir,
      sprintf("fit-nrow%02d-sim%03d-%s.parquet", row$nrow, row$sim, key)
    )
    duckplyr::compute_parquet(duckplyr::as_duckdb_tibble(flat), out)
    paths[i] <- out
  }
  paths
}

#' Top-level pipeline builder. `config` is read from the SSDSIMS_EXP_CONFIG
#' env var by `_targets.R` and lives in globalenv, so worker processes can
#' look it up the same way when they re-source `_targets.R`.
build_pipeline <- function(config) {
  tarchetypes::tar_plan(
    targets::tar_target(boron, ssddata::ccme_boron, deployment = "main"),
    targets::tar_target(grid, config$grid, deployment = "main"),
    targets::tar_target(
      out_dir,
      file.path("data", config$id),
      deployment = "main"
    ),

    ## Atomic fit grid grouped by the chosen split axes; iteration =
    ## "group" makes downstream dynamic branching iterate per bucket.
    targets::tar_target(
      fit_grid,
      build_fit_grid(config$grid) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(config$split_axes))) |>
        targets::tar_group(),
      iteration  = "group",
      deployment = "main"
    ),

    ## One branch = one job = N atomic tasks (depending on split).
    ## Output is a vector of parquet paths (one per atomic task in the
    ## branch).
    targets::tar_target(
      branch_results,
      run_experiment_branch(fit_grid, boron, grid, out_dir),
      pattern = map(fit_grid),
      format  = "file"
    )
  )
}
