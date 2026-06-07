# ssdsims small targets pipeline (TARGETS-DESIGN.md section 6).
#
# Copy this directory's files (`_targets.R`, `scenario.R`, `run.R`,
# `run-serial.R`) to your project root, edit `scenario.R`, then `source("run.R")`
# (targets) or `source("run-serial.R")` (single core, no targets).
#
# The whole pipeline is built by the `ssd_scenario_targets()` target factory:
# one named, `format = "file"`, `error = "null"` target per `partition_by` path
# cell per step (static branching), ordered `sample -> fit -> hc -> summary`,
# written under the per-layout `scenario_results_dir()` root. So this file is
# just "build a scenario and call the factory".

library(targets)
library(tarchetypes)

# Keep-going (`make -k`) is the pipeline default: an errored target skips only
# its dependents while every other reachable shard still builds, so one bad
# branch never aborts the run — it just leaves a gap the summary unions over.
# The factory's shard targets carry the stronger `error = "null"` on top.
# Fail-fast pre-flight checks belong in a separate script run before
# `tar_make()`, not here (TARGETS-DESIGN.md §6.2).
tar_option_set(error = "continue")

# `scenario` is defined in scenario.R — shared with run-serial.R so both drivers
# run the same study.
source("scenario.R")

ssd_scenario_targets(scenario)
