# ssdsims cluster targets pipeline (TARGETS-DESIGN.md section 4 / section 12
# `cluster-pipeline`).
#
# COPY-EDIT-RUN. Copy this directory's files (`_targets.R`, `scenario.R`,
# `run.R`, `run-serial.R`) to your project root, edit the ONE controller block
# below for your cluster, then `source("run.R")`. The "zero to a running
# cluster job" guide in this directory's README walks the whole path, from your
# site's own (non-R) SLURM instructions to a running job.
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
# scenario (the storage layout is a free re-layout).

library(targets)
library(tarchetypes)

# ---------------------------------------------------------------------------
# Off-cluster smoke path (set by run.R; see its guard).
#
# `run.R` sets the `SSDSIMS_CLUSTER_LOCAL` environment variable when
# `crew.cluster` is not installed or no SLURM queue is reachable, so the
# pipeline shape can be validated on a laptop with a local controller (mirroring
# the crew labs' local fallback, section 4). On a cluster the variable is unset
# and the SLURM controller below is used.
# ---------------------------------------------------------------------------
use_local_smoke <- nzchar(Sys.getenv("SSDSIMS_CLUSTER_LOCAL"))

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

controller <- if (use_local_smoke) {
  # Off-cluster smoke path: same shape, local workers, no scheduler.
  crew::crew_controller_local(workers = 2L)
} else {
  crew.cluster::crew_controller_slurm(
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
}

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

# ---------------------------------------------------------------------------
# CONNECTIVITY + WORKER-PREREQUISITE PROBE (TARGETS-DESIGN.md section 4
# ingredient B).
#
# Building this target dispatches ONE crew worker — i.e. one SLURM job — and,
# inside that job, verifies the worker can actually run ssdsims before any
# scenario shard runs. It checks, and aborts with an actionable message naming
# the failed check, that:
#   * R resolves at the expected version  (else: module-load / install-path),
#   * `library(ssdsims)` loads             (else: the ManyLinux binary path),
#   * the scratch `tempdir()` is writable  (else: storage / scratch config).
# A no-job-dispatched failure surfaces as a crew launch error (queue/account
# wiring). It returns a small witness (worker R version + node id).
#
# `expected_r_version` is captured from the editable block above as a literal,
# so the probe carries no dependency on the controller.
# On the off-cluster smoke path the worker is the local R, not the cluster's
# `module load` R, so skip the version check there (NULL); on the cluster it
# checks the worker resolves the expected version.
probe_expected <- if (use_local_smoke) NULL else expected_r_version
probe_target <- tar_target_raw(
  "probe",
  substitute(
    ssdsims_cluster_probe(expected = v),
    list(v = probe_expected)
  ),
  deployment = "worker" # run ON a worker (a SLURM job), not the main process
)

# The probe is a self-contained function so it ships inside the worker's
# globals (it does not depend on ssdsims being loadable — that is what it
# checks).
ssdsims_cluster_probe <- function(expected = NULL) {
  node <- Sys.info()[["nodename"]]
  r_version <- as.character(getRversion())
  # (1) R version resolves as expected (module-load / install-path).
  if (!is.null(expected) && !startsWith(r_version, expected)) {
    stop(
      "Cluster probe FAILED [R version]: worker R is ",
      r_version,
      " but expected ",
      expected,
      ".x. Fix the `module load R/...` line in the ",
      "controller's `script_lines` (or set `expected_r_version <- NULL`).",
      call. = FALSE
    )
  }
  # (2) ssdsims loads on the worker (the ManyLinux binary install path).
  if (!requireNamespace("ssdsims", quietly = TRUE)) {
    stop(
      "Cluster probe FAILED [ssdsims unavailable]: the worker on node '",
      node,
      "' cannot load `ssdsims`. Fix the worker install/module path (the ",
      "ManyLinux binary path; see this template's README, backend B) so the ",
      "dependency tree resolves without compiling.",
      call. = FALSE
    )
  }
  # (3) scratch / tempdir() is writable (storage config).
  probe_file <- file.path(tempdir(), "ssdsims-cluster-probe.txt")
  ok <- tryCatch(
    {
      writeLines(node, probe_file)
      file.exists(probe_file)
    },
    error = function(e) FALSE
  )
  if (!isTRUE(ok)) {
    stop(
      "Cluster probe FAILED [scratch not writable]: cannot write to tempdir() '",
      tempdir(),
      "' on node '",
      node,
      "'. Fix the scratch path (the ",
      "`export TMPDIR=...` line in `script_lines`).",
      call. = FALSE
    )
  }
  unlink(probe_file)
  # Witness: returned so downstream targets can depend on a green probe.
  list(node = node, r_version = r_version, time = Sys.time())
}

# Gate the scenario shards on the probe: prepend a reference to `probe` to every
# `sample`-step target's command (the `sample` step is the pipeline entry, so
# gating it makes the probe a transitive upstream of every fit/hc/summary
# target). `tar_make()` therefore builds the probe FIRST, and a probe failure
# stops the scenario shards from running — the cluster-wiring/prerequisite
# problem surfaces, not an obscure scenario error. The factory itself is
# untouched; this only adds an edge from `probe` into the shards it already
# minted. `gate_targets()` walks the (nested) `tar_map()` list the factory
# returns and rebuilds each `tar_target` with `{ probe; <original command> }`.
gate_targets <- function(node, probe_name = "probe") {
  if (inherits(node, "tar_target")) {
    gated <- as.call(list(
      as.name("{"),
      as.name(probe_name),
      node$command$expr[[1]]
    ))
    return(tar_target_raw(
      node$settings$name,
      gated,
      format = node$settings$format,
      error = node$settings$error,
      cue = node$cue
    ))
  }
  if (is.list(node)) {
    return(lapply(node, gate_targets, probe_name = probe_name))
  }
  node
}

# scenario_targets is list(sample_block, fit_block, hc_block, summary); gate only
# the `sample` block (the entry step) — that gates the whole DAG.
scenario_targets[[1]] <- gate_targets(scenario_targets[[1]])

list(
  probe_target,
  scenario_targets
)
