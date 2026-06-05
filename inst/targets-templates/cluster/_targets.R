# ssdsims cluster targets pipeline (TARGETS-DESIGN.md section 4 / section 12
# `cluster-pipeline`).
#
# COPY-EDIT-RUN. Copy this directory's files (`_targets.R`, `functions.R`,
# `scenario.R`, `run.R`, `run-serial.R`) to your project root, edit the ONE
# controller block below for your cluster, then `source("run.R")`. The "zero to
# a running cluster job" guide in this directory's README walks the whole path,
# from your site's own (non-R) SLURM instructions to a running job. (To run the
# same study *locally* without a scheduler, use the `large/` template instead —
# same factory + scenario, a `crew::crew_controller_local()` controller.)
#
# Three ingredients assemble into this file (TARGETS-DESIGN.md section 4); only
# the second is cluster-specific:
#   A. SHAPE   — how the per-shard targets are wired. Provided verbatim by the
#                `ssd_scenario_targets()` factory (the same call `large/` uses).
#   B. BACKEND — the SLURM `crew` controller (queue, module loads, scratch,
#                workers, walltime). THE ONE EDITABLE BLOCK, below.
#   C. CONTENT — the scenario object (`scenario.R`): seed, datasets, grids.
#                Scheduler-independent, identical to `large/scenario.R`.
# Because only B changes between clusters, the per-task `sample`/`fit`/`hc`
# results are byte-identical to the local `large/` pipeline for the same
# scenario (the storage layout is a free re-layout). The probe body and the
# shard-gating glue live in `functions.R` (sourced below), so this file stays a
# readable pipeline definition.

library(targets)
library(tarchetypes)

# Helper functions: the connectivity/prerequisite probe and the probe-gating
# glue (see functions.R).
source("functions.R")

# ===========================================================================
# INGREDIENT B — THE SLURM CONTROLLER. *** THE ONE BLOCK YOU EDIT. ***
#
# Retargeting another SLURM cluster edits ONLY this block; the scenario and the
# `ssd_scenario_targets()` call below stay unchanged. Drafted from the crew
# labs' validated working block (TARGETS-DESIGN.md section 4 ingredient B). Map
# your site's own SLURM usage instructions onto these arguments (see the
# README's mapping table):
#
#   site instruction                  ->  argument
#   --------------------------------      --------------------------------
#   partition / queue                 ->  crew_options_slurm(partition=)
#   `module load R/4.x` (+ any deps)  ->  crew_options_slurm(script_lines=)
#   account / allocation              ->  crew_options_slurm(script_lines=)  (an
#                                          `#SBATCH --account=...` line)
#   scratch filesystem                ->  crew_options_slurm(script_lines=)  (an
#                                          `export TMPDIR=/scratch/...` line)
#   walltime limit                    ->  crew_options_slurm(time_minutes=)
#   cores per task                    ->  crew_options_slurm(cpus_per_task=)
#   memory per cpu                    ->  crew_options_slurm(memory_gigabytes_per_cpu=)
#
# See `?crew.cluster::crew_controller_slurm` and `?crew.cluster::crew_options_slurm`.
# ===========================================================================

# The R version your `module load` provides — the probe checks the worker
# resolves it (set to NULL to skip the version check).
expected_r_version <- "4.3"

controller <- crew.cluster::crew_controller_slurm(
  # ---- general crew settings (cluster-independent) ----
  workers = 4L, # max concurrent SLURM jobs (= concurrent shards; see packing note)
  seconds_idle = 60, # let an idle worker exit (release the SLURM allocation)
  # ---- SLURM resources: EDIT for your site ----
  options_cluster = crew.cluster::crew_options_slurm(
    partition = "short", # <-- your partition/queue
    time_minutes = 60, # <-- walltime per worker (minutes)
    cpus_per_task = 1, # <-- cores per worker
    memory_gigabytes_per_cpu = 4, # <-- memory per cpu (GB)
    script_lines = c(
      # Lines injected verbatim into the sbatch script. Put your `module load`
      # (so R + the ssdsims binary path resolve on the worker), your account,
      # and your scratch path here.
      "#SBATCH --account=YOUR_ALLOCATION", # <-- your account/allocation
      "module load R/4.3", # <-- the module that provides R + deps
      "export TMPDIR=/scratch/$USER/ssdsims" # <-- writable scratch for tempdir()
    )
  )
)

tar_option_set(controller = controller)

# Shard-target <-> SLURM-job packing (TARGETS-DESIGN.md section 11 Q6).
# `targets` + `crew` dispatch the independent per-shard targets across SLURM
# jobs; shards are the unit of parallelism either way. How many shard targets
# ride in one SLURM job is a `crew` knob, NOT hard-coded as 1:1:
#   * `workers` (above) caps concurrent SLURM jobs.
#   * `tasks_max` on the controller packs up to N shard targets per worker/job
#     (the default lets one worker serve many shards over its lifetime -> many
#     shards to one job). Set `tasks_max = 1L` for one shard target per job.
# Independent shards still run concurrently across the live workers regardless.

# ===========================================================================
# INGREDIENT C — THE SCENARIO. Defined in scenario.R, edited to taste; shared
# with run-serial.R so both drivers run the same study. Scheduler-independent.
# ===========================================================================
source("scenario.R")

# ===========================================================================
# INGREDIENT A — THE SHAPE. The whole per-shard pipeline, built by the SAME
# factory `large/` uses (one `format = "file"`, `error = "null"` target per
# `partition_by` cell per step, ordered sample -> fit -> hc -> summary). Reused
# VERBATIM: the shape is scheduler-independent.
# ===========================================================================
scenario_targets <- ssd_scenario_targets(scenario)

# Probe target (functions.R::ssdsims_cluster_probe): building it dispatches ONE
# crew worker — i.e. one SLURM job — and verifies the worker prerequisites
# before any scenario shard runs. `expected_r_version` is captured from the
# editable block above as a literal, so the probe carries no dependency on the
# controller.
probe_target <- tar_target_raw(
  "probe",
  substitute(
    ssdsims_cluster_probe(expected = v),
    list(v = expected_r_version)
  ),
  deployment = "worker" # run ON a worker (a SLURM job), not the main process
)

# Gate the scenario's entry-step (`sample`) shards on the probe, so `tar_make()`
# builds the probe first and a probe failure blocks the whole DAG
# (functions.R::gate_targets). scenario_targets is
# list(sample_block, fit_block, hc_block, summary).
scenario_targets[[1]] <- gate_targets(scenario_targets[[1]])

list(
  probe_target,
  scenario_targets
)
