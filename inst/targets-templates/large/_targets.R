# ssdsims large targets pipeline (TARGETS-DESIGN.md section 6).
#
# Copy this directory's files (`_targets.R`, `scenario.R`, `run.R`,
# `run-serial.R`) to your project root, edit `scenario.R`, then `source("run.R")`
# (targets) or `source("run-serial.R")` (single core, no targets).
#
# The whole pipeline is built by the `ssd_scenario_targets()` target factory:
# one named, `format = "file"`, `error = "null"` target per `partition_by` path
# cell per step (static branching), ordered `sample -> fit -> hc -> summary`,
# written under the per-layout `scenario_results_dir()` root.

library(targets)
library(tarchetypes)

# DuckDB resources: the step runners cap duckplyr/DuckDB at a single thread
# and 1GB per process while they run (so local workers share the machine
# fairly). The same environment variables as the cluster template apply here -
# e.g. `Sys.setenv(SSDSIMS_DUCKDB_MEMORY_LIMIT = "3GB")` before `tar_make()` for
# shards with a large nested `samples` payload; see ?ssd_summarise and the
# cluster template's controller.R for the sizing rule.

# Two pipeline-wide options:
#   * controller — parallelise the (independent) shard targets across local
#     workers with a mirai-backed `crew` controller. `targets` dispatches each
#     ready shard target to a worker, so the fit/hc shards of this wider study
#     run concurrently. Tune `workers` to your machine; swap in `crew.cluster`
#     controllers for SLURM/PBS etc. (`cluster-pipeline`, section 11). Needs the
#     `crew` package installed.
#   * error = "continue" — keep-going (`make -k`): an errored target skips only
#     its dependents while every other reachable shard still builds, so one bad
#     branch never aborts the parallel run — it just leaves a gap the summary
#     unions over. The factory's shard targets carry the stronger `error =
#     "null"` on top; fail-fast pre-flight checks belong in a separate script the
#     user runs before `tar_make()`, not here (TARGETS-DESIGN.md §6.2).
tar_option_set(
  controller = crew::crew_controller_local(workers = 8L),
  error = "continue"
)

# `scenario` is defined in scenario.R — shared with run-serial.R so both drivers
# run the same study.
source("scenario.R")

ssd_scenario_targets(scenario)
