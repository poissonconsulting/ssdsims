## Helper functions for the ssdsims targets workflow.
##
## These convert the nested ssdsims results into flat tibbles suitable for
## Parquet, and write each branch into a hive-partitioned directory tree so
## the whole dataset can be read back with arrow / duckplyr.

#' Drop list-columns of complex objects and unnest the hc tibble.
flatten_hc_sims <- function(x) {
  drop <- intersect(c("data", "fits", "args"), names(x))
  if (length(drop)) x <- x[, setdiff(names(x), drop), drop = FALSE]
  tidyr::unnest(x, cols = "hc")
}

#' Write a single-nrow flattened result as one Parquet file under
#' `<root>/nrow=<value>/data.parquet` (hive layout).
#'
#' The `nrow` column is removed from the file body because its value is
#' carried by the directory name; DuckDB/arrow restore it on read.
write_partition <- function(df, root, nrow_value) {
  if (!nrow(df)) {
    return(character(0))
  }
  branch_dir <- file.path(root, paste0("nrow=", nrow_value))
  if (dir.exists(branch_dir)) unlink(branch_dir, recursive = TRUE)
  dir.create(branch_dir, recursive = TRUE, showWarnings = FALSE)
  df$nrow <- NULL
  out <- file.path(branch_dir, "data.parquet")
  arrow::write_parquet(df, out)
  out
}

## ---- Pipeline steps --------------------------------------------------------

#' Simulate, fit, and compute HCs for a single `nrow` (basic example).
basic_hcs_for <- function(data, nrow_value, nsim = 2L) {
  sims <- ssdsims::ssd_sim_data(data, nrow = nrow_value, nsim = nsim)
  fits <- ssdsims::ssd_fit_dists_sims(sims)
  ssdsims::ssd_hc_sims(fits)
}

#' Scenario 1: bootstrapped CIs with samples retained.
#'
#' Mirrors the first `ssd_run_scenario()` call in scripts/example.R, but is
#' factored so the (sims, fits) pair can be reused across scenarios for the
#' same `nrow` value.
scenario1_hcs_for <- function(fits) {
  ssdsims::ssd_hc_sims(
    fits,
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = TRUE,
    est_method = "multi",
    ci_method = c(
      "arithmetic_samples",
      "geometric_samples",
      "GMACL",
      "MACL",
      "multi_fixed",
      "multi_free",
      "weighted_samples"
    ),
    parametric = TRUE,
    nboot = c(1L, 5L, 10L, 50L, 100L, 500L),
    samples = TRUE,
    delta = Inf
  )
}

#' Scenario 2: point estimates only, no CIs, no samples.
scenario2_hcs_for <- function(fits) {
  ssdsims::ssd_hc_sims(
    fits,
    proportion = c(0.01, 0.05, 0.1, 0.2),
    est_method = c("arithmetic", "geometric", "multi"),
    ci = FALSE,
    parametric = TRUE,
    nboot = c(1L, 5L, 10L, 50L, 100L, 500L),
    delta = Inf
  )
}

#' Run sim → fit for one `nrow`, used as the shared upstream of both scenarios.
sims_fits_for <- function(data, nrow_value, nsim = 2L) {
  sims <- ssdsims::ssd_sim_data(data, nrow = nrow_value, nsim = nsim)
  ssdsims::ssd_fit_dists_sims(sims)
}
