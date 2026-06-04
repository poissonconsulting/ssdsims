# ssdsims local targets pipeline (TARGETS-DESIGN.md section 6).
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

# `scenario` is defined in scenario.R — shared with run-serial.R so both drivers
# run the same study.
source("scenario.R")

ssd_scenario_targets(scenario)
