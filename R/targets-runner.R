# Per-shard step runners and the summary fan-in for the targets pipeline
# (TARGETS-DESIGN.md section 6). These are the bodies `tar_map()` calls, one
# target per shard. Each runner takes a shard's `tasks` (the list-column of task
# rows from `ssd_scenario_<step>_shards()`), runs the bundled tasks with the
# *same* per-task primitives the baseline runner uses
# (`*_data_task_primer()`), reads any upstream shard from Parquet by partition
# path, and writes one Parquet per shard - returning the shard's path (the
# `format = "file"` contract). Because a task's result is fully determined by
# its `(seed, primer)` and is order-independent (`task-lists`), the pipeline's
# per-task results are byte-identical to `ssd_run_scenario_baseline()` regardless
# of how tasks bundle into shards. Execution is reused; sharding and Parquet
# I/O are the only new parts.

# ---- Parquet I/O internals (duckplyr) --------------------------------------
#
# A thin seam over `duckplyr` so the engine is swappable. Non-tabular per-task
# results (a `fitdists` object) are serialised to an ASCII string column via
# `encode_obj()`/`decode_obj()` - duckplyr cannot store a raw/list column, and
# an ASCII serialisation round-trips losslessly through a Parquet `VARCHAR`.

#' Write a data frame to a single Parquet file (creating parent dirs).
#' @noRd
ssd_write_parquet <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  duckplyr::compute_parquet(duckplyr::as_duckdb_tibble(df), path)
  path
}

#' Read a Parquet path (a file or a glob) back into a plain tibble.
#'
#' `hive_partitioning = FALSE` so only the file's own columns return - the
#' shard's `key=value` directory path would otherwise be auto-discovered and
#' injected as extra columns, corrupting a round-tripped draw. Each shard
#' Parquet already carries every column it needs (the ids), so the Hive path is
#' for addressing/pushdown, not for reconstructing columns here.
#' @noRd
ssd_read_parquet <- function(path) {
  tibble::as_tibble(dplyr::collect(duckplyr::read_parquet_duckdb(
    path,
    options = list(hive_partitioning = FALSE)
  )))
}

#' Serialise an arbitrary R object to an ASCII string (Parquet-storable).
#' @noRd
encode_obj <- function(x) {
  rawToChar(serialize(x, connection = NULL, ascii = TRUE))
}

#' Inverse of [encode_obj()].
#' @noRd
decode_obj <- function(s) {
  unserialize(charToRaw(s))
}

# The shard's Hive partition path, e.g. "dataset=boron/sim=1/replace=FALSE",
# from its task rows' path-axis values (all rows in a shard share them) via the
# existing `path_key()` - no duplicated split logic.
shard_path <- function(tasks, scenario, step) {
  path_key(tasks[1L, , drop = FALSE], scenario$partition_by[[step]])
}

# The partition path of the upstream shard a single task row depends on (the
# parent step's path axes), used to open the parent Parquet by partition path.
upstream_path <- function(task_row, scenario, parent) {
  path_key(task_row, scenario$partition_by[[parent]])
}

#' Run a sample Shard
#'
#' Runs the `sample` tasks bundled into one shard: under one
#' [local_dqrng_backend()] scope, reads each task's dataset off the scenario via
#' [scenario_dataset()], draws `n_max` rows with the per-task `(seed, primer)`
#' through `sample_data_task_primer()`, and writes one Parquet at the shard's
#' Hive partition path. Each task's draw is tagged with its `sample_id` and a
#' `.row` order index so a downstream `fit` shard can isolate and re-order it.
#'
#' @param tasks A tibble of the shard's task rows (the `tasks` list-column of a
#'   row of [ssd_scenario_sample_shards()]), each carrying its axis values,
#'   `sample_id`, `seed`, and `primer`.
#' @param scenario The `ssdsims_scenario` (a referenced global in `_targets.R`).
#' @param out_dir The `sample` results root (e.g. `"results/sample"`).
#' @return The shard's Parquet path (the `format = "file"` contract).
#' @seealso [ssd_run_fit_step()], [ssd_run_hc_step()], [ssd_scenario_sample_shards()].
#' @export
ssd_run_sample_step <- function(tasks, scenario, out_dir) {
  chk::chk_s3_class(scenario, "ssdsims_scenario")
  local_dqrng_backend()
  draws <- vector("list", nrow(tasks))
  for (i in seq_len(nrow(tasks))) {
    t <- tasks[i, ]
    data <- scenario_dataset(scenario, t$dataset)
    draw <- sample_data_task_primer(
      data,
      t$n_max,
      t$replace,
      t$seed,
      t$primer[[1L]]
    )
    draw <- tibble::as_tibble(draw)
    draw$.sample_id <- t$sample_id
    draw$.row <- seq_len(nrow(draw))
    draws[[i]] <- draw
  }
  out <- file.path(
    out_dir,
    shard_path(tasks, scenario, "sample"),
    "part.parquet"
  )
  ssd_write_parquet(dplyr::bind_rows(draws), out)
}

#' Run a fit Shard
#'
#' Runs the `fit` tasks bundled into one shard: for each task, opens its parent
#' `sample` shard's Parquet by partition path, isolates that task's draw by
#' `sample_id` (restoring row order), truncates it inline (`head(sample, nrow)`,
#' RNG-free, section 5), and fits with the per-task `(seed, primer)` through
#' `fit_data_task_primer()` (resolving `min_pmix` off the scenario via
#' [scenario_min_pmix()]). The fitted `fitdists` object is serialised into a
#' `fit_blob` string column keyed by `fit_id`, and one Parquet is written at the
#' shard's partition path.
#'
#' @inheritParams ssd_run_sample_step
#' @param tasks A tibble of the shard's `fit` task rows (from
#'   [ssd_scenario_fit_shards()]), each carrying its fit-grid values, `fit_id`,
#'   the parent `sample` path-axis values, `seed`, and `primer`.
#' @param sample_dir The `sample` results root the parent shards were written to.
#' @param out_dir The `fit` results root (e.g. `"results/fit"`).
#' @return The shard's Parquet path.
#' @export
ssd_run_fit_step <- function(tasks, scenario, sample_dir, out_dir) {
  chk::chk_s3_class(scenario, "ssdsims_scenario")
  local_dqrng_backend()
  rows <- vector("list", nrow(tasks))
  for (i in seq_len(nrow(tasks))) {
    t <- tasks[i, ]
    sp <- upstream_path(t, scenario, "sample")
    sample_tbl <- ssd_read_parquet(file.path(sample_dir, sp, "part.parquet"))
    draw <- sample_tbl[sample_tbl$.sample_id == t$sample_id, ]
    draw <- draw[order(draw$.row), ]
    draw$.sample_id <- NULL
    draw$.row <- NULL
    data <- utils::head(draw, t$nrow)
    fit <- fit_data_task_primer(
      data = data,
      scenario = scenario,
      dists = scenario$fit$dists,
      rescale = t$rescale,
      computable = t$computable,
      at_boundary_ok = t$at_boundary_ok,
      min_pmix = t$min_pmix,
      range_shape1 = t$range_shape1[[1L]],
      range_shape2 = t$range_shape2[[1L]],
      seed = t$seed,
      primer = t$primer[[1L]]
    )
    rows[[i]] <- tibble::tibble(fit_id = t$fit_id, fit_blob = encode_obj(fit))
  }
  out <- file.path(out_dir, shard_path(tasks, scenario, "fit"), "part.parquet")
  ssd_write_parquet(dplyr::bind_rows(rows), out)
}

#' Run an hc Shard
#'
#' Runs the `hc` tasks bundled into one shard: for each task, opens its parent
#' `fit` shard's Parquet by partition path, isolates that task's fit by `fit_id`,
#' deserialises the `fitdists` object, and estimates the hazard concentration
#' with the per-task `(seed, primer)` through `hc_data_task_primer()`. Each
#' task's hc tibble (one or more rows - the `proportion` fan-out and the
#' `ci = FALSE` collapse, section 1.2) is tagged with its `hc_id` and parent
#' `fit_id`, stacked, and written as one Parquet at the shard's partition path.
#'
#' @inheritParams ssd_run_sample_step
#' @param tasks A tibble of the shard's `hc` task rows (from
#'   [ssd_scenario_hc_shards()]), each carrying its hc-grid values, `hc_id`, the
#'   parent `fit` path-axis values and `fit_id`, `seed`, and `primer`.
#' @param fit_dir The `fit` results root the parent shards were written to.
#' @param out_dir The `hc` results root (e.g. `"results/hc"`).
#' @return The shard's Parquet path.
#' @export
ssd_run_hc_step <- function(tasks, scenario, fit_dir, out_dir) {
  chk::chk_s3_class(scenario, "ssdsims_scenario")
  local_dqrng_backend()
  rows <- vector("list", nrow(tasks))
  for (i in seq_len(nrow(tasks))) {
    t <- tasks[i, ]
    fp <- upstream_path(t, scenario, "fit")
    fit_tbl <- ssd_read_parquet(file.path(fit_dir, fp, "part.parquet"))
    blob <- fit_tbl$fit_blob[fit_tbl$fit_id == t$fit_id]
    fits <- decode_obj(blob)
    hc <- hc_data_task_primer(
      fits = fits,
      proportion = scenario$hc$proportion,
      ci = t$ci,
      nboot = t$nboot,
      est_method = t$est_method,
      ci_method = t$ci_method,
      parametric = t$parametric,
      seed = t$seed,
      primer = t$primer[[1L]]
    )
    hc <- tibble::as_tibble(hc)
    hc$hc_id <- t$hc_id
    hc$fit_id <- t$fit_id
    rows[[i]] <- hc
  }
  out <- file.path(out_dir, shard_path(tasks, scenario, "hc"), "part.parquet")
  ssd_write_parquet(dplyr::bind_rows(rows), out)
}

#' Summarise a Run's hc Estimates Across Shards
#'
#' Fans in the run's results without pulling shard target values back into R or
#' recomputing anything: reads every `hc` shard Parquet under `dir_hc` (a Hive
#' glob) with `duckplyr` - the analysis-ready per-task hazard-concentration
#' estimates - unions them, and writes `path`. Because it reads the result
#' directory (not the shard targets), it sees whatever shards landed, so it
#' unions the survivors of a partially-failed run (`error = "null"`, section
#' 6.2). `dir_sample` and `dir_fit` are accepted for signature symmetry with the
#' three result layers; the `sample` draws and serialised `fit` objects are not
#' summary material, so the combined summary is the `hc` layer.
#'
#' @param dir_sample The `sample` results root.
#' @param dir_fit The `fit` results root.
#' @param dir_hc The `hc` results root.
#' @param path The output Parquet path for the combined summary.
#' @return The summary Parquet path (the `format = "file"` contract).
#' @details
#' In a `targets` pipeline a directory read carries no dependency edge, so order
#' `summary` after the shards by referencing an upstream barrier in its command
#' (see the shipped `_targets.R` template's `tar_combine()` barriers). Reading
#' the directory - rather than the shard target values - is what lets it union
#' whatever shards landed (the survivors of a partially-failed run, section 6.2).
#' @export
ssd_summarize <- function(dir_sample, dir_fit, dir_hc, path) {
  glob <- file.path(dir_hc, "**", "part.parquet")
  hc <- ssd_read_parquet(glob)
  ssd_write_parquet(hc, path)
}
