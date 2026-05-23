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

n_workers <- as.integer(Sys.getenv("SSDSIMS_EXP_WORKERS", unset = "2"))

## Atomic = one fit_dists_seed call = one (nrow, sim, stream) tuple.
n_atomic_fits <- function(grid) {
  length(grid$nrow_levels) * grid$nsim * length(grid$stream)
}

## HC combinations applied within each fit.
n_hc_per_fit <- function(grid) {
  length(grid$ci_method) * length(grid$nboot) * length(grid$proportion)
}

run_one <- function(cfg) {
  message("== running config: ", cfg$id, " ==")

  ## tar_destroy() spawns a callr subprocess that re-sources `_targets.R`,
  ## which expects SSDSIMS_EXP_CONFIG to be set — so set it first.
  Sys.setenv(SSDSIMS_EXP_CONFIG = jsonlite::toJSON(cfg, auto_unbox = TRUE))

  if (dir.exists("_targets")) {
    targets::tar_destroy(ask = FALSE)
  }
  ## Per-config data dir; keep other configs' data so we can compare
  ## results across splits in the analysis qmds.
  unlink(file.path("data", cfg$id), recursive = TRUE)

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
  wall <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  meta <- targets::tar_meta(
    fields = c(
      "name",
      "type",
      "parent",
      "seconds",
      "bytes",
      "warnings",
      "error"
    )
  )
  meta$config_id <- cfg$id
  meta$split <- cfg$split

  pq <- list.files(
    file.path("data", cfg$id),
    pattern = "\\.parquet$",
    recursive = TRUE,
    full.names = TRUE
  )
  pq_bytes <- sum(file.info(pq)$size, na.rm = TRUE)

  branch_meta <- dplyr::filter(
    meta,
    type == "branch",
    parent == "branch_results"
  )

  agg <- function(f) {
    if (nrow(branch_meta)) f(branch_meta$seconds) else NA_real_
  }
  summary_row <- tibble::tibble(
    config_id = cfg$id,
    split = cfg$split,
    split_axes = paste(cfg$split_axes, collapse = "+"),
    n_workers = n_workers,
    nsim = cfg$grid$nsim,
    n_proportion = length(cfg$grid$proportion),
    n_ci_method = length(cfg$grid$ci_method),
    n_nboot = length(cfg$grid$nboot),
    n_atomic_fits = n_atomic_fits(cfg$grid),
    n_hc_per_fit = n_hc_per_fit(cfg$grid),
    n_branches = nrow(branch_meta),
    n_fits_per_branch = if (nrow(branch_meta)) {
      n_atomic_fits(cfg$grid) / nrow(branch_meta)
    } else {
      NA_real_
    },
    wall_secs = wall,
    branch_secs_mean = agg(mean),
    branch_secs_median = agg(stats::median),
    branch_secs_max = agg(max),
    branch_secs_min = agg(min),
    branch_secs_sd = agg(stats::sd),
    branch_secs_total = agg(sum),
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
  readr::write_csv(dplyr::bind_rows(summary_all), summary_path)
  readr::write_csv(dplyr::bind_rows(meta_all), meta_path)
  message(
    "  -> ",
    res$summary$ok,
    " in ",
    round(res$summary$wall_secs, 1),
    "s, ",
    res$summary$n_branches,
    " branches"
  )
}

message("\nDone. Wrote ", summary_path, " and ", meta_path, ".")
