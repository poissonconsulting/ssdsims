## Driver: loop over experiment configurations, run the pipeline once per
## config, capture per-target metadata into results/. Each config gets a
## clean store and clean data dir.
##
## Run from this directory:
##   Rscript run_all.R [config_id ...]
##
## With no args, runs every config in configs.R. With args, runs only the
## named configs (handy when iterating).

suppressPackageStartupMessages({
  library(dplyr)
  library(jsonlite)
})

source("configs.R")

results_dir <- "results"
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

selected <- commandArgs(trailingOnly = TRUE)
if (length(selected)) {
  unknown <- setdiff(selected, names(configs))
  if (length(unknown)) {
    stop("unknown config ids: ", paste(unknown, collapse = ", "))
  }
  configs <- configs[selected]
}

summary_path <- file.path(results_dir, "summary.csv")
meta_path <- file.path(results_dir, "per_target.csv")

run_one <- function(cfg) {
  message("== running config: ", cfg$id, " ==")

  ## tar_destroy() spawns a callr subprocess that re-sources `_targets.R`,
  ## which expects SSDSIMS_EXP_CONFIG to be set — so set it first.
  Sys.setenv(SSDSIMS_EXP_CONFIG = jsonlite::toJSON(cfg, auto_unbox = TRUE))

  if (dir.exists("_targets")) {
    targets::tar_destroy(ask = FALSE)
  }
  unlink("data", recursive = TRUE)

  t0 <- Sys.time()
  ok <- TRUE
  err_msg <- NA_character_
  tryCatch(
    targets::tar_make(reporter = "silent"),
    error = function(e) {
      ok <<- FALSE
      err_msg <<- conditionMessage(e)
    }
  )
  t1 <- Sys.time()
  wall <- as.numeric(difftime(t1, t0, units = "secs"))

  ## per-target meta
  meta <- targets::tar_meta(
    fields = c("name", "type", "parent", "seconds", "bytes", "warnings", "error")
  )
  meta$config_id <- cfg$id
  meta$size <- cfg$size
  meta$split <- cfg$split

  ## count parquet output
  pq <- list.files("data", pattern = "\\.parquet$", recursive = TRUE,
                   full.names = TRUE)
  pq_bytes <- sum(file.info(pq)$size, na.rm = TRUE)

  branch_meta <- dplyr::filter(meta, type == "branch", parent == "branch_results")
  summary_row <- tibble::tibble(
    config_id = cfg$id,
    size = cfg$size,
    split = cfg$split,
    nsim = cfg$nsim,
    split_axes = paste(cfg$split_axes, collapse = "+"),
    n_proportion = length(cfg$proportion),
    n_ci_method = length(cfg$ci_method),
    n_nboot = length(cfg$nboot),
    n_atomic_units = length(cfg$nrow_levels) * length(cfg$ci_method) *
      length(cfg$nboot) * length(cfg$proportion),
    n_branches = nrow(branch_meta),
    wall_secs = wall,
    branch_secs_mean = if (nrow(branch_meta)) mean(branch_meta$seconds) else NA_real_,
    branch_secs_median = if (nrow(branch_meta)) median(branch_meta$seconds) else NA_real_,
    branch_secs_max = if (nrow(branch_meta)) max(branch_meta$seconds) else NA_real_,
    branch_secs_min = if (nrow(branch_meta)) min(branch_meta$seconds) else NA_real_,
    branch_secs_sd = if (nrow(branch_meta)) sd(branch_meta$seconds) else NA_real_,
    branch_secs_total = if (nrow(branch_meta)) sum(branch_meta$seconds) else NA_real_,
    parquet_count = length(pq),
    parquet_bytes = pq_bytes,
    ok = ok,
    error = err_msg
  )

  list(summary = summary_row, meta = meta)
}

summary_all <- list()
meta_all <- list()
for (cfg in configs) {
  res <- run_one(cfg)
  summary_all[[cfg$id]] <- res$summary
  meta_all[[cfg$id]] <- res$meta
  ## checkpoint after each config in case we're interrupted
  readr::write_csv(dplyr::bind_rows(summary_all), summary_path)
  readr::write_csv(dplyr::bind_rows(meta_all), meta_path)
  message("  -> ", res$summary$ok, " in ", round(res$summary$wall_secs, 1),
          "s, ", res$summary$n_branches, " branches")
}

message("\nDone. Wrote ", summary_path, " and ", meta_path, ".")
