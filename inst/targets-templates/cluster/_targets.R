# ssdsims cluster targets pipeline (TARGETS-DESIGN.md section 4 / section 12
# `cluster-pipeline`).
#
# COPY-EDIT-RUN. Copy this directory's files (`controller.R`, `functions.R`,
# `scenario.R`, `preflight.R`, `_targets.R`, `run.R`, `run-serial.R`) to your
# project root, edit `controller.R` for your cluster, then `source("run.R")`.
# The "zero to a running cluster job" guide in this directory's README walks the
# whole path, from your site's own (non-R) SLURM instructions to a running job.
# (To run the same study *locally* without a scheduler, use the `large/`
# template instead — same factory + scenario, a local controller.)
#
# Three ingredients assemble into the cluster run (TARGETS-DESIGN.md section 4);
# only the second is cluster-specific:
#   A. SHAPE   — the per-shard targets, built verbatim by `ssd_scenario_targets()`
#                (the same factory call `large/` uses). THIS FILE.
#   B. BACKEND — the SLURM `crew` controller. `controller.R` (the one block you
#                edit); the connectivity/prerequisite check lives in `preflight.R`.
#   C. CONTENT — the scenario object (`scenario.R`): seed, datasets, grids.
# Because only B changes between clusters, the per-task `sample`/`fit`/`hc`
# results are byte-identical to the local `large/` pipeline for the same
# scenario (the storage layout is a free re-layout).
#
# This file stays a clean scenario-and-factory definition: the controller is in
# `controller.R` and the cluster preflight is in `preflight.R` (run by `run.R`
# before `tar_make()`).

library(targets)
library(tarchetypes)

# Ingredient B — the SLURM controller (the one editable block; see controller.R).
source("controller.R")
tar_option_set(controller = controller)

# Ingredient C — the scenario (edited to taste; shared with run-serial.R).
source("scenario.R")

# Ingredient A — the whole per-shard pipeline, reused VERBATIM from the shared
# factory (one `format = "file"`, `error = "null"` target per `partition_by`
# cell per step, ordered sample -> fit -> hc -> summary). Scheduler-independent.
ssd_scenario_targets(scenario)
