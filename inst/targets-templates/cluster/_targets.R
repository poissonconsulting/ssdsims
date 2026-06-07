# ssdsims cluster targets pipeline (TARGETS-DESIGN.md section 4 / section 12
# `cluster-pipeline`).
#
# COPY-EDIT-RUN. Copy this directory's files (`controller.R`, `_targets.R`,
# `preflight.R`, `run.R`) to your project root, edit `controller.R` for your
# cluster, then `source("run.R")`. The "zero to a running cluster job" guide in
# this directory's README walks the whole path, from your site's own (non-R)
# SLURM instructions to a running job. (To run the same study *locally* without
# a scheduler, use the `large/` template instead — same factory + scenario, a
# local controller.)
#
# Three ingredients assemble into the cluster run (TARGETS-DESIGN.md section 4);
# only the second is cluster-specific:
#   A. SHAPE   — the per-shard targets, built verbatim by `ssd_scenario_targets()`
#                (the same factory call `large/` uses). THIS FILE.
#   B. BACKEND — the SLURM `crew` controller. `controller.R` (the one block you
#                edit); the connectivity/prerequisite check lives in `preflight.R`.
#   C. CONTENT — the scenario object, defined inline below.
# Because only B changes between clusters, the per-task `sample`/`fit`/`hc`
# results are byte-identical to the local `large/` pipeline for the same
# scenario (the storage layout is a free re-layout).

library(targets)
library(tarchetypes)
library(ssdsims)

# Ingredient B — the SLURM controller (the one editable block; see controller.R).
# `error = "continue"` is the cluster-independent keep-going (`make -k`) default:
# one failed shard/job skips only its dependents while the rest of the sweep
# still runs (TARGETS-DESIGN.md §6.2). The connectivity/prerequisite fail-fast
# guard is the separate `preflight.R` the user runs before `tar_make()` (the
# `run.R` driver runs it for you), NOT a target here.
source("controller.R")
tar_option_set(controller = controller, error = "continue")

# Ingredient C — THE SCENARIO. Edit to taste. The same shape as
# `large/scenario.R`: a wider study sweeping sample sizes, hazard proportions,
# estimation and CI methods, with bootstrap CIs. Scheduler-independent.
#
# MINIMAL FIRST JOB: for your first cluster run, follow the README's step 3 and
# shrink this block (e.g. a single `nrow`, one method) to a cheap, fast job that
# confirms the controller + preflight before launching the full sweep. Then
# (step 4) expand it to your own study, leaving the controller untouched.
scenario <- ssd_define_scenario(
  ssddata::ccme_boron,
  nsim = 2L,
  seed = 42L,
  nrow = c(5L, 10L), # c(5L, 6L, 10L, 20L, 50L),
  est_method = c("arithmetic", "geometric", "multi"),
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = TRUE,
  nboot = c(5, 50), # c(1, 5, 10, 50, 100, 500), # * 100,
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
  samples = TRUE,
  bundle = list(
    hc = "ci_method"
  )
)

# Ingredient A — the whole per-shard pipeline, reused VERBATIM from the shared
# factory (one `format = "file"`, `error = "null"` target per `partition_by`
# cell per step, ordered sample -> fit -> hc -> summary). Scheduler-independent.
#
# OPTIONAL — cloud upload. To ship each shard to an object store as it is
# produced (so results are readable off the cluster), pass an `upload`
# destination by name. It pairs each shard with an `upload_<step>` target
# (content-hashed, so unchanged shards are not re-uploaded) and runs the
# `ssd_test_upload()` preflight up front. Credentials stay external (`AZURE_*`
# env vars) and must reach the WORKERS, not just the login node — set them via
# the controller's `script_lines` (above). A missing credential fails loud. See
# the "Uploading Shards to Cloud Storage" vignette.
#
#   ssd_scenario_targets(
#     scenario,
#     upload = ssd_upload_azure(
#       url = "https://<account>.blob.core.windows.net",
#       container = "ssdsims-results"
#     )
#   )
#
# Default (no upload): the clean DAG, no `upload_<step>` targets.
ssd_scenario_targets(scenario)
