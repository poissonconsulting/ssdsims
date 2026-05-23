## Parameterised pipeline used for the split / batch-size experiments.
##
## The runner (run_all.R) sets SSDSIMS_EXP_CONFIG to a JSON string before
## calling tar_make(); we parse it here and call build_pipeline().

library(targets)
library(tarchetypes)

## Workers are intentionally capped at 2 so the baseline timings are
## comparable across machines. Override with SSDSIMS_EXP_WORKERS.
n_workers <- as.integer(Sys.getenv("SSDSIMS_EXP_WORKERS", unset = "2"))

tar_option_set(
  packages = c(
    "ssdsims",
    "ssddata",
    "ssdtools",
    "dplyr",
    "tidyr",
    "tibble",
    "purrr",
    "duckplyr",
    "digest"
  ),
  format             = "qs",
  memory             = "transient",
  garbage_collection = TRUE,
  storage            = "worker",
  retrieval          = "worker",
  controller         = crew::crew_controller_local(workers = n_workers)
)

tar_source("R")

config_json <- Sys.getenv("SSDSIMS_EXP_CONFIG", unset = "")
if (!nzchar(config_json)) {
  stop(
    "SSDSIMS_EXP_CONFIG env var is required ",
    "(JSON describing the experiment config)."
  )
}
config <- jsonlite::fromJSON(config_json, simplifyVector = TRUE)
## fromJSON coerces vectors fine; ensure the right types:
config$grid$nrow_levels <- as.integer(config$grid$nrow_levels)
config$grid$nboot       <- as.integer(config$grid$nboot)
config$grid$proportion  <- as.numeric(config$grid$proportion)
config$grid$ci_method   <- as.character(config$grid$ci_method)
config$grid$nsim        <- as.integer(config$grid$nsim)
config$split_axes       <- as.character(config$split_axes)

build_pipeline(config)
