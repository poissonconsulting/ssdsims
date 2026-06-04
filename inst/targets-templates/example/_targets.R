# ssdsims example targets pipeline (TARGETS-DESIGN.md section 6).
#
# Static branching: the scenario is a plain construction-time object built HERE,
# at sourcing time - NOT a tar_target. Everything downstream is a pure function
# of it, so the shard set is fixed before any target runs and
# `tarchetypes::tar_map()` mints one named, addressable target per shard. The
# three `ssd_scenario_*_shards()` tables are computed now (sourcing time) and
# fed to `tar_map(values = )`.
#
# Each step target is `format = "file"` (its value IS the shard's Parquet path,
# so downstream branches receive a path and hand it to duckplyr, with no R
# round-trip) and `error = "null"` (a failing WHOLE shard records its error and
# goes NULL without aborting the run - the other shards still build and
# `summary` unions whatever landed, section 6.2). Finer PARTIAL survival (a bad
# task yielding a shorter shard, the survivor-union, the `warn = 2` muffling)
# and the predicate-pushdown Hive query layer are deferred to
# `shard-failure-survival` / `hive-partitioning`.
#
# Copy this directory's files (`_targets.R`, `scenario.R`, `run.R`,
# `run-serial.R`) to your project root, edit `scenario.R`, then `source("run.R")`
# (targets) or `source("run-serial.R")` (single core, no targets).

library(targets)
library(tarchetypes)

# `scenario` is defined in scenario.R — shared with run-serial.R so both drivers
# run the same study. It attaches ssdsims and builds the scenario at sourcing
# time (a plain object, not a target).
source("scenario.R")

# One row per shard, computed now. Each row carries its partition_by path-axis
# values (the tar_map target-name suffix and Hive path) plus a `tasks`
# list-column of the task rows that shard runs.
sample_shards <- ssd_scenario_sample_shards(scenario)
fit_shards <- ssd_scenario_fit_shards(scenario)
hc_shards <- ssd_scenario_hc_shards(scenario)

# `names = ` are the step's default partition_by path axes; keep them in sync
# with `scenario$partition_by` if you override it. Each `tar_map()` mints one
# named target per shard at sourcing time.
#
# A step body reads its upstream shards from disk *by partition path*, so there
# is no automatic target edge to order the steps. We add one explicit barrier
# per step with `tar_combine()` (it depends on all that step's shard targets)
# and reference the upstream barrier in the next step's command (the leading
# `sample_done` / `fit_done`), so `targets` runs sample -> fit -> hc -> summary
# in order while still parallelising the shards within each step.
sample_targets <- tar_map(
  values = sample_shards,
  names = c(dataset, sim, replace),
  tar_target(
    sample_step,
    ssd_run_sample_step(tasks, scenario, out_dir = "results/sample"),
    format = "file",
    error = "null"
  )
)
fit_targets <- tar_map(
  values = fit_shards,
  names = c(dataset, sim, nrow, rescale),
  tar_target(
    fit_step,
    {
      sample_done # order after all sample shards (read by partition path)
      ssd_run_fit_step(
        tasks,
        scenario,
        sample_dir = "results/sample",
        out_dir = "results/fit"
      )
    },
    format = "file",
    error = "null"
  )
)
hc_targets <- tar_map(
  values = hc_shards,
  names = c(dataset, sim),
  tar_target(
    hc_step,
    {
      fit_done # order after all fit shards (read by partition path)
      ssd_run_hc_step(
        tasks,
        scenario,
        fit_dir = "results/fit",
        out_dir = "results/hc"
      )
    },
    format = "file",
    error = "null"
  )
)

list(
  sample_targets,
  tar_combine(sample_done, sample_targets),
  fit_targets,
  tar_combine(fit_done, fit_targets),
  hc_targets,
  tar_combine(hc_done, hc_targets),
  # `summary` reads the hc result directory (so it unions whatever shards
  # landed, section 6.2); referencing `hc_done` orders it after the hc shards.
  tar_target(
    summary,
    {
      hc_done
      ssd_summarize(
        dir_sample = "results/sample",
        dir_fit = "results/fit",
        dir_hc = "results/hc",
        path = "results/summary.parquet"
      )
    },
    format = "file"
  )
)
