## targets pipeline for the ssdsims example.
##
## Equivalent to scripts/example.R, but each `nrow` level is its own branch so
## work can be cached / re-run / parallelised independently. Results land as
## hive-partitioned Parquet files (one directory per scenario, partitioned by
## `nrow`) so they can be read with arrow::open_dataset() or duckplyr.
##
## Run with:
##   targets::tar_make()                # build everything
##   targets::tar_make_future(workers = 4)  # parallel (after loading future)
##
## Then see collect.R for reading the Parquet output back with duckplyr.

library(targets)

tar_option_set(
  packages = c(
    "ssdsims",
    "ssddata",
    "ssdtools",
    "dplyr",
    "tidyr",
    "tibble",
    "purrr",
    "arrow"
  ),
  format = "qs",
  memory = "transient",
  garbage_collection = TRUE,
  storage = "worker",
  retrieval = "worker"
)

tar_source("R")

## Where Parquet output lives (relative to the targets project root).
data_root <- "data"

list(
  ## ---- Source data --------------------------------------------------------
  tar_target(boron, ssddata::ccme_boron),

  ## ---- Basic example (scripts/example.R lines 1-9) ------------------------
  tar_target(basic_sims, ssdsims::ssd_sim_data(boron)),
  tar_target(basic_fits, ssdsims::ssd_fit_dists_sims(basic_sims)),
  tar_target(basic_hcs, ssdsims::ssd_hc_sims(basic_fits)),
  tar_target(
    basic_parquet,
    write_partition(
      flatten_hc_sims(basic_hcs),
      root = file.path(data_root, "basic"),
      nrow_value = unique(basic_hcs$nrow)
    ),
    format = "file"
  ),

  ## ---- Scenarios: one branch per `nrow` -----------------------------------
  ## Both scenarios share (sims, fits) for the same `nrow`, so we factor those
  ## upstream rather than calling ssd_run_scenario() twice (which would
  ## re-simulate and re-fit). With identical seed/stream/start_sim the data
  ## is the same either way.
  tar_target(nrow_levels, c(5L, 6L, 10L, 20L, 50L)),

  tar_target(
    fits_by_nrow,
    sims_fits_for(boron, nrow_value = nrow_levels, nsim = 2L),
    pattern = map(nrow_levels),
    iteration = "list"
  ),

  ## ---- Scenario 1: ci = TRUE, samples = TRUE ------------------------------
  tar_target(
    scenario1_parquet,
    write_partition(
      flatten_hc_sims(scenario1_hcs_for(fits_by_nrow)),
      root = file.path(data_root, "scenario1"),
      nrow_value = nrow_levels
    ),
    pattern = map(fits_by_nrow, nrow_levels),
    format = "file"
  ),

  ## ---- Scenario 2: ci = FALSE, samples = FALSE ----------------------------
  tar_target(
    scenario2_parquet,
    write_partition(
      flatten_hc_sims(scenario2_hcs_for(fits_by_nrow)),
      root = file.path(data_root, "scenario2"),
      nrow_value = nrow_levels
    ),
    pattern = map(fits_by_nrow, nrow_levels),
    format = "file"
  ),

  ## ---- All Parquet outputs ------------------------------------------------
  tar_target(
    parquet_paths,
    c(basic_parquet, scenario1_parquet, scenario2_parquet),
    format = "file"
  )
)
