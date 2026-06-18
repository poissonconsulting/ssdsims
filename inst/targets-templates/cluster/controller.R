# INGREDIENT B — THE SLURM CONTROLLER. *** THE ONE BLOCK YOU EDIT. ***
#
# Sourced by BOTH `_targets.R` (the pipeline) and `preflight.R` (the
# connectivity/prerequisite check), so the controller is configured in ONE
# place. Retargeting another SLURM cluster edits only this file; the scenario
# and the `ssd_scenario_targets()` call stay unchanged (TARGETS-DESIGN.md
# section 4: only the controller and resource specs change between clusters).
#
# Drafted from the crew labs' validated working block (section 4 ingredient B).
# Map your site's own SLURM usage instructions onto these arguments (see the
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
#
# NON-SLURM CLUSTERS: this targets SLURM, but `crew.cluster` also provides
# `crew_controller_sge()` / `crew_controller_pbs()` / `crew_controller_lsf()`
# (each with a matching `crew_options_*()`). To retarget one, swap the two
# constructors below — the factory, the scenario, and the preflight are
# unchanged. `script_lines` (your `module load` / account / `TMPDIR`) carries
# over verbatim; the named resource args differ (e.g. `cpus_per_task` -> `cores`,
# `partition`/`time_minutes` move into `script_lines` directives). Untested here;
# the README's "Targeting a non-SLURM cluster" table has the mapping.

# The R version your `module load` provides — `preflight.R` checks the worker
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
      "export TMPDIR=/scratch/$USER/ssdsims", # <-- writable scratch for tempdir()
      # DuckDB memory on the worker. ssdsims caps duckplyr/DuckDB at 1GB per
      # worker by default (and at a single thread, so `cpus_per_task = 1`
      # above is correct as shipped). Raise the cap here, keeping headroom
      # for R's own footprint within the job: R holds a shard's draws while
      # DuckDB ingests them, so budget roughly half the job for DuckDB (3GB
      # of a 4GB job). A higher cap is only ever needed for shards carrying a
      # large nested `samples` payload (rule of thumb: a shard with P bytes
      # of draws needs ~5 x P); the summary target never needs it (its writer
      # is byte-budgeted - see ?ssd_summarise). A too-low cap fails the shard
      # loud and isolated (error = "null"), never the whole job.
      "export SSDSIMS_DUCKDB_MEMORY_LIMIT=3GB" # <-- DuckDB cap per worker
    )
  )
)

# Shard-target <-> SLURM-job packing (TARGETS-DESIGN.md section 11 Q6).
# `targets` + `crew` dispatch the independent per-shard targets across SLURM
# jobs; shards are the unit of parallelism either way. How many shard targets
# ride in one SLURM job is a `crew` option, NOT hard-coded as 1:1:
#   * `workers` (above) caps concurrent SLURM jobs.
#   * `tasks_max` on the controller packs up to N shard targets per worker/job
#     (the default lets one worker serve many shards over its lifetime -> many
#     shards to one job). Set `tasks_max = 1L` for one shard target per job.
# Independent shards still run concurrently across the live workers regardless.
