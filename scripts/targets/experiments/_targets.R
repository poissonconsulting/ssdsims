## Parameterised pipeline used for the split / batch-size experiments.
##
## The runner (run_all.R) sets SSDSIMS_EXP_CONFIG to a JSON string before
## calling tar_make(); we parse it here and call build_pipeline().

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
    "duckplyr",
    "digest"
  ),
  format = "qs",
  memory = "transient",
  garbage_collection = TRUE,
  storage = "worker",
  retrieval = "worker",
  controller = crew::crew_controller_local(
    workers = max(1L, parallel::detectCores())
  )
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
config$nrow_levels <- as.integer(config$nrow_levels)
config$nboot <- as.integer(config$nboot)
config$proportion <- as.numeric(config$proportion)
config$ci_method <- as.character(config$ci_method)
config$split_axes <- as.character(config$split_axes)
config$nsim <- as.integer(config$nsim)

build_pipeline(config)
