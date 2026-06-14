# Single-core sharded runner (TARGETS-DESIGN.md section 5/section 6): the
# "shards without targets" rung between the in-memory baseline runner
# (`ssd_run_scenario_baseline()`, which threads results in memory by foreign
# key) and the static-branching targets pipeline. It runs the three steps in
# order, but materialises each step's results as Hive-partitioned Parquet shards
# and links steps by reading parent shards back from disk via duckplyr - the
# same write -> glob-read -> filter loop the targets path uses, proven in plain
# R. It reuses the per-shard step runners (`ssd_run_{sample,fit,hc}_step()`) and
# the shard grouping (`ssd_scenario_*_shards()`), so per-task RNG and results are
# byte-identical to the in-memory baseline (partitioning is a free re-layout).
#
# The m:n parent-shard relationship (a child shard may read several parent
# shards, and a parent shard may feed several child shards - intrinsic to the
# section 5 coarsening defaults) is resolved at the read layer, not by
# constraining `partition_by`: each step runner opens, per task, the parent
# shard at that task's `<parent>_id` identity projected onto the parent's path
# axes, and filters to the rows it needs. No parent-consistency rule is required.

#' Run a Scenario over Hive-partitioned Parquet Shards (single core)
#'
#' Executes a scenario's three task steps in dependency order - `sample`, then
#' `fit`, then `hc` - materialising each step's results as **one Parquet per
#' `partition_by` path cell** under a Hive-partitioned tree
#' `<dir>/<step>/<axis=value>/.../part.parquet`, and linking steps by reading the
#' parent step's shards back via `duckplyr` (predicate pushdown), rather than
#' threading results in memory. This is the single-core, `targets`-free sibling
#' of [ssd_run_scenario_baseline()] and the first consumer of `partition-by`'s
#' path/inner split ([scenario_dataset()]'s sibling `scenario_partition_axes()`).
#'
#' It reuses the per-task seed-and-run wrappers, so for a fixed `scenario$seed`
#' it is reproducible and **order-independent**, and its per-task results are
#' **byte-identical** to [ssd_run_scenario_baseline()] - `partition_by` is a free
#' re-layout that moves only file paths, never results. The **m:n** parent-shard
#' dependency (a child shard reading several parent shards, or a parent shard
#' feeding several children, per the section 5 coarsening defaults) is resolved
#' at read time: each task opens the parent shard at its `<parent>_id` identity
#' projected onto the parent's path axes and filters to the rows it needs.
#'
#' No `targets`, `crew`, manifest, or cloud upload - this is the plain-R storage
#' loop only, de-risking `hive-partitioning`/`task-tables`.
#'
#' @param scenario An `ssdsims_scenario` from [ssd_define_scenario()].
#' @param dir A results root to write the Hive-partitioned shards under; created
#'   if absent. Defaults to a per-run session temp dir (the shards are left on
#'   disk for inspection and reuse). The runner **owns** the `sample`/`fit`/`hc`
#'   subtrees under `dir` and clears them on each run, so replaying a scenario
#'   with a changed `partition_by`/`bundle` never leaves stale-granularity shards
#'   beside the new ones. (The `targets` pipeline instead isolates each layout
#'   under its own [scenario_results_dir()] root.)
#' @return An `ssdsims_shard_run` object: a list with `dir` and the written
#'   `sample`, `fit`, and `hc` shard Parquet paths (one per shard).
#' @seealso [ssd_run_scenario_baseline()] (the in-memory reference oracle),
#'   [ssd_scenario_sample_shards()], [ssd_run_sample_step()].
#' @export
#' @examples
#' library(dqrng)
#' scenario <- ssd_define_scenario(
#'   ssd_scenario_data(ssddata::ccme_boron),
#'   nsim = 1L,
#'   nrow = 6L,
#'   seed = 42L,
#'   dists = "lnorm"
#' )
#' run <- ssd_run_scenario_shards(scenario)
#' run$hc
ssd_run_scenario_shards <- function(
  scenario,
  dir = tempfile("ssdsims-shards-")
) {
  chk::chk_s3_class(scenario, "ssdsims_scenario")
  chk::chk_string(dir)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  sample_dir <- file.path(dir, "sample")
  fit_dir <- file.path(dir, "fit")
  hc_dir <- file.path(dir, "hc")

  # Own the output tree: clear each step's prior shards before writing. A shard's
  # Hive-path depth/axes depend on `partition_by`/`bundle`, so without this a
  # re-run with a changed split would leave stale-granularity shards beside the
  # new ones and the glob readers would union both. Wiping first guarantees the
  # tree always matches the current scenario (this runner recomputes every shard
  # anyway). Only the three step subtrees are removed - the rest of `dir` is left
  # untouched (e.g. a sibling `summary.parquet`).
  unlink(c(sample_dir, fit_dir, hc_dir), recursive = TRUE)

  # One backend scope for the whole run; each step runner's own
  # `local_dqrng_backend()` is then a reentrant no-op, and each task installs
  # its `(seed, primer)` exactly once - so results match the in-memory runner.
  # The duckplyr config scope nests safely (each layer restores the one
  # above), so opening it here too keeps the whole run configured even when
  # the step runners are bypassed.
  local_duckplyr_config()
  local_dqrng_backend()

  list(
    dir = dir,
    sample = run_shards(scenario, "sample", sample_dir),
    fit = run_shards(scenario, "fit", fit_dir, upstream = sample_dir),
    hc = run_shards(scenario, "hc", hc_dir, upstream = fit_dir)
  ) |>
    structure(class = "ssdsims_shard_run")
}

# Run every shard of one step in a plain loop, returning the written paths.
# A loop (not `purrr::map`) keeps internal frames out of any error header.
run_shards <- function(scenario, step, out_dir, upstream = NULL) {
  shards <- switch(
    step,
    sample = ssd_scenario_sample_shards(scenario),
    fit = ssd_scenario_fit_shards(scenario),
    hc = ssd_scenario_hc_shards(scenario)
  )
  paths <- character(nrow(shards))
  for (i in seq_len(nrow(shards))) {
    tasks <- shards$tasks[[i]]
    paths[i] <- switch(
      step,
      sample = ssd_run_sample_step(tasks, scenario, out_dir),
      fit = ssd_run_fit_step(tasks, scenario, upstream, out_dir),
      hc = ssd_run_hc_step(tasks, scenario, upstream, out_dir)
    )
  }
  paths
}

#' @export
#' @noRd
print.ssdsims_shard_run <- function(x, ...) {
  cat("<ssdsims_shard_run>\n")
  cat("  dir: ", x$dir, "\n", sep = "")
  for (step in c("sample", "fit", "hc")) {
    cat(sprintf("  %-6s shards: %d\n", step, length(x[[step]])))
  }
  invisible(x)
}
