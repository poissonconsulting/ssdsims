# Run the ssdsims design targets pipeline.
#
# The package ships this under
# `system.file("targets-templates", "design", package = "ssdsims")`. Copy the
# directory's files (`design.R`, `_targets.R`, `run.R`) to your project root, edit
# `design.R`, then run this driver - interactively with `source("run.R")` or from
# a shell with `Rscript run.R`.
#
# It builds one target per shard (writing
# `results/seed=<value>/layout=<hash>/<step>/<axis=value>/.../part.parquet`,
# cells shared across members built once) and the combined
# `results/summary.parquet` (the union of the per-member summaries, tagged with a
# `scenario` column), then reports the summary path and a peek at the
# hazard-concentration estimates.

library(targets)

# Allow running from the project root (e.g. Positron / VS Code
# Cmd+Shift+Enter), not just from this directory.
if (dir.exists("inst/targets-templates/design")) {
  withr::local_dir(
    "inst/targets-templates/design",
    .local_envir = parent.frame(2)
  )
}

# Build the pipeline. `_targets.R` defines one named target per shard, so
# independent shards (across members and within) can run in parallel; to scale
# out, configure a controller (e.g. `crew` / `crew.cluster`) via
# `tar_option_set()` in `_targets.R` - the shard set and `tar_make()` here are
# unchanged.
tar_make()

# The top-level `summary` target is `format = "file"`, so its value is the path
# to the combined `results/summary.parquet` (the per-member summaries unioned
# with a `scenario` identity column).
summary_path <- tar_read(summary)
cat("Combined summary written to:", summary_path, sep = "\n")

# Peek at the combined estimates (duckplyr is an ssdsims dependency). The
# `scenario` column tags each row with its member's name within the design.
print(duckplyr::read_parquet_duckdb(summary_path[[1L]]))
