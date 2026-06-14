# ssdsims design targets pipeline.
#
# Copy this directory's files (`_targets.R`, `design.R`, `run.R`) to your project
# root, edit `design.R`, then `source("run.R")` (or `Rscript run.R`).
#
# A design is the de-duplicated union of its members' regular grids - the
# irregular (ragged) grid. The whole pipeline is built by the
# `ssd_design_targets()` factory: one named, `format = "file"`, `error = "null"`
# target per shard cell (static branching), with cells shared across members
# built once, written under `<root>/seed=<value>/layout=<hash>/...`. Each member
# gets a `summary_<name>` target that filters the shared shards to its own cells,
# and a single top-level `summary` unions those into `results/summary.parquet`
# with a `scenario` identity column. So this file is just "build a design and
# call the factory".
#
# Migration note: a one-off `ssd_scenario_targets(scenario)` run upgrades to a
# design for free - wrap the scenario in `ssd_design()` and call this factory.
# Both root shards under the same `scenario_results_dir(scenario, root)` tree and
# weave the `seed` into target names, so the shards already on disk are reused
# (no recompute); only the summary targets are new.

library(targets)
library(tarchetypes)

# DuckDB resources: the step runners cap duckplyr/DuckDB at a single thread and
# 1GB per process while they run. Set e.g.
# `Sys.setenv(SSDSIMS_DUCKDB_MEMORY_LIMIT = "3GB")` before `tar_make()` for
# shards with a large nested `samples` payload; see ?ssd_summarise and the
# cluster template's controller.R for the sizing rule.

# Keep-going (`make -k`) is the pipeline default: an errored target skips only
# its dependents while every other reachable shard still builds, so one bad
# branch never aborts the run - it just leaves a gap the summary unions over.
# The factory's shard targets carry the stronger `error = "null"` on top.
tar_option_set(error = "continue")

# `design` is defined in design.R.
source("design.R")

ssd_design_targets(design)
