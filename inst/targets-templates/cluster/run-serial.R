# Run the SAME scenario single core, WITHOUT targets (no cluster, no scheduler),
# via the shard store (`ssd_run_scenario_shards()`) — then compare to the
# targets/cluster results.
#
# This is the off-cluster oracle: because both drivers `source("scenario.R")`
# (the same study) and `partition_by` is a free re-layout, the single-core
# per-task estimates must be byte-identical to the cluster run's. From the
# project root: `source("run-serial.R")` or `Rscript run-serial.R`. Run `run.R`
# (the targets pipeline) first if you want the comparison.

# Allow running from the repo root (e.g. Positron / VS Code Cmd+Shift+Enter),
# not just from this cluster directory.
if (dir.exists("inst/targets-templates/cluster")) {
  withr::local_dir(
    "inst/targets-templates/cluster",
    .local_envir = parent.frame(2)
  )
}

source("scenario.R")

# Single core: writes one Parquet per shard under results-serial/<step>/...,
# the same Hive layout the targets pipeline writes to results/.
run <- ssd_run_scenario_shards(scenario, dir = "results-serial")
serial_summary <- ssd_summarize(
  file.path(run$dir, "sample"),
  file.path(run$dir, "fit"),
  file.path(run$dir, "hc"),
  file.path(run$dir, "summary.parquet")
)
cat("Single-core summary:", serial_summary, "\n")
print(run)

# Compare to the cluster/targets run (results/summary.parquet from run.R), if
# present. The per-task estimates must be byte-identical — only the controller
# (local vs SLURM) differs between the two drivers.
targets_summary <- file.path(scenario_results_dir(scenario), "summary.parquet")
if (file.exists(targets_summary)) {
  read_sorted <- function(path) {
    tbl <- tibble::as_tibble(dplyr::collect(
      duckplyr::read_parquet_duckdb(
        path,
        options = list(hive_partitioning = FALSE)
      )
    ))
    tbl[order(tbl$hc_id, tbl$dist, tbl$proportion), ]
  }
  same <- isTRUE(all.equal(
    read_sorted(serial_summary)$est,
    read_sorted(targets_summary)$est
  ))
  cat("single-core vs cluster estimates identical:", same, "\n")
} else {
  message(
    "No targets results at '",
    targets_summary,
    "'. Run `source(\"run.R\")` first to compare."
  )
}
