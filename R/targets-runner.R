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

#' Read a Parquet path - a file, a glob, or a vector of files - back into a
#' plain tibble (duckplyr combines a multi-file set in one DuckDB scan).
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

#' Layout-keyed Results Root for a Scenario
#'
#' Returns `<root>/layout=<hash>`, where the hash is derived from the scenario's
#' `partition_by`. A step's Hive shard path depth and axes are a function of
#' `partition_by`/`bundle`, so writing two different layouts into one root would
#' leave shards of *different granularity* side by side - and the depth-agnostic
#' glob the readers use (`<step>/**/part.parquet`) would then union stale and
#' current shards, double-counting tasks. Keying the results root on the layout
#' isolates each `partition_by` into its own subtree: re-running a scenario with
#' a changed `partition_by`/`bundle` writes to a *fresh* root (never mixing
#' granularities), while re-running the *same* layout reuses the root
#' (idempotent and cache-friendly - the same shard paths are simply rewritten).
#'
#' The `targets` pipeline writes under this root (see the shipped `_targets.R`
#' template). The single-core [ssd_run_scenario_shards()] takes the complementary
#' approach: it *owns* and clears a fixed `dir` on each run.
#'
#' @inheritParams scenario_dataset
#' @param root The results root directory (default `"results"`).
#' @return The layout-keyed path `file.path(root, paste0("layout=", <hash>))`.
#' @seealso [ssd_run_scenario_shards()], [ssd_summarize()].
#' @export
#' @examples
#' scenario <- ssd_define_scenario(ssddata::ccme_boron, nsim = 1L, seed = 42L)
#' scenario_results_dir(scenario)
scenario_results_dir <- function(scenario, root = "results") {
  chk::chk_s3_class(scenario, "ssdsims_scenario")
  chk::chk_string(root)
  file.path(
    root,
    paste0("layout=", substr(rlang::hash(scenario$partition_by), 1L, 12L))
  )
}

# The shard's Hive partition path, e.g. "dataset=boron/sim=1/replace=FALSE",
# from its task rows' path-axis values (all rows in a shard share them) via the
# existing `path_key()` - no duplicated split logic.
shard_path <- function(tasks, scenario, step) {
  path_key(tasks[1L, , drop = FALSE], scenario$partition_by[[step]])
}

# Read a child shard's parent shards once each and return them combined into one
# tibble. A child shard's tasks may span several parent shards (the m:n
# relationship: a coarser parent keeps an axis the child shards on, or the child
# is coarser and spans many parents); we read the *distinct* set of parent shard
# paths the shard's tasks reference - each parent once, served to every task in
# the shard that needs it - never opening an unrelated shard. duckplyr combines
# the file set in a single DuckDB scan; per-task rows are then isolated in memory
# by the `<parent>_id` identity.
read_parent_shards <- function(tasks, scenario, parent, parent_dir) {
  paths <- unique(path_key(tasks, scenario$partition_by[[parent]]))
  files <- file.path(parent_dir, paths, "part.parquet")
  ssd_read_parquet(files)
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
#' @examples
#' scenario <- ssd_define_scenario(ssddata::ccme_boron, nsim = 1L, seed = 42L)
#' shards <- ssd_scenario_sample_shards(scenario)
#' dir <- tempfile()
#' ssd_run_sample_step(shards$tasks[[1L]], scenario, file.path(dir, "sample"))
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
#' Runs the `fit` tasks bundled into one shard: reads the distinct set of parent
#' `sample` shards the shard's tasks reference (each once - they may span several
#' sample shards), isolates each task's draw by `sample_id` (restoring row
#' order), truncates it inline (`head(sample, nrow)`,
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
#' @examples
#' \donttest{
#' scenario <- ssd_define_scenario(
#'   ssddata::ccme_boron,
#'   nsim = 1L,
#'   nrow = 6L,
#'   seed = 42L,
#'   dists = "lnorm"
#' )
#' dir <- tempfile()
#' ssd_run_sample_step(
#'   ssd_scenario_sample_shards(scenario)$tasks[[1L]],
#'   scenario,
#'   file.path(dir, "sample")
#' )
#' ssd_run_fit_step(
#'   ssd_scenario_fit_shards(scenario)$tasks[[1L]],
#'   scenario,
#'   file.path(dir, "sample"),
#'   file.path(dir, "fit")
#' )
#' }
ssd_run_fit_step <- function(tasks, scenario, sample_dir, out_dir) {
  chk::chk_s3_class(scenario, "ssdsims_scenario")
  local_dqrng_backend()
  # Read each distinct parent `sample` shard once, then isolate each task's draw
  # in memory by `sample_id` (tasks in this shard may span several sample shards).
  sample_tbl <- read_parent_shards(tasks, scenario, "sample", sample_dir)
  rows <- vector("list", nrow(tasks))
  for (i in seq_len(nrow(tasks))) {
    t <- tasks[i, ]
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
#' Runs the `hc` tasks bundled into one shard: reads the distinct set of parent
#' `fit` shards the shard's tasks reference (each once - an hc shard typically
#' spans several fit shards), isolates each task's fit by `fit_id`,
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
#' @examples
#' \donttest{
#' scenario <- ssd_define_scenario(
#'   ssddata::ccme_boron,
#'   nsim = 1L,
#'   nrow = 6L,
#'   seed = 42L,
#'   dists = "lnorm"
#' )
#' dir <- tempfile()
#' ssd_run_sample_step(
#'   ssd_scenario_sample_shards(scenario)$tasks[[1L]],
#'   scenario,
#'   file.path(dir, "sample")
#' )
#' ssd_run_fit_step(
#'   ssd_scenario_fit_shards(scenario)$tasks[[1L]],
#'   scenario,
#'   file.path(dir, "sample"),
#'   file.path(dir, "fit")
#' )
#' ssd_run_hc_step(
#'   ssd_scenario_hc_shards(scenario)$tasks[[1L]],
#'   scenario,
#'   file.path(dir, "fit"),
#'   file.path(dir, "hc")
#' )
#' }
ssd_run_hc_step <- function(tasks, scenario, fit_dir, out_dir) {
  chk::chk_s3_class(scenario, "ssdsims_scenario")
  local_dqrng_backend()
  # Read each distinct parent `fit` shard once, then isolate each task's fit in
  # memory by `fit_id` (an hc shard typically spans several fit shards).
  fit_tbl <- read_parent_shards(tasks, scenario, "fit", fit_dir)
  rows <- vector("list", nrow(tasks))
  for (i in seq_len(nrow(tasks))) {
    t <- tasks[i, ]
    blob <- fit_tbl$fit_blob[fit_tbl$fit_id == t$fit_id]
    if (length(blob) != 1L) {
      chk::abort_chk(
        "Expected exactly one `fit` result for fit_id ",
        encodeString(t$fit_id, quote = "\""),
        ", found ",
        length(blob),
        " in the parent `fit` shard(s) (missing or duplicate result)."
      )
    }
    fits <- decode_obj(blob)
    hc <- hc_data_task_primer(
      fits = fits,
      proportion = scenario$hc$proportion,
      ci = t$ci,
      nboot = t$nboot,
      est_method = t$est_method,
      ci_method = t$ci_method,
      parametric = t$parametric,
      samples = scenario$hc$samples,
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
#' @examples
#' \donttest{
#' scenario <- ssd_define_scenario(
#'   ssddata::ccme_boron,
#'   nsim = 1L,
#'   nrow = 6L,
#'   seed = 42L,
#'   dists = "lnorm"
#' )
#' # Materialise the shards single-core, then fan in the hc layer.
#' run <- ssd_run_scenario_shards(scenario)
#' ssd_summarize(
#'   file.path(run$dir, "sample"),
#'   file.path(run$dir, "fit"),
#'   file.path(run$dir, "hc"),
#'   file.path(run$dir, "summary.parquet")
#' )
#' }
ssd_summarize <- function(dir_sample, dir_fit, dir_hc, path) {
  glob <- file.path(dir_hc, "**", "part.parquet")
  # Project out the `dists`/`samples` list-columns at the DuckDB level (so the
  # potentially-large retained `samples` draws are never pulled into R): the
  # summary is the analysis-ready estimate table; the draws stay in the hc shards.
  hc <- tibble::as_tibble(dplyr::collect(
    dplyr::select(
      duckplyr::read_parquet_duckdb(
        glob,
        options = list(hive_partitioning = FALSE)
      ),
      -dplyr::any_of(c("dists", "samples"))
    )
  ))
  ssd_write_parquet(hc, path)
}

#' Build the Targets Pipeline for a Scenario
#'
#' A **target factory**: returns the list of `targets` objects that runs a
#' scenario as a static-branching Hive-sharded pipeline (TARGETS-DESIGN.md
#' section 6), so a whole `_targets.R` reduces to *build a scenario and call
#' this*:
#'
#' ```r
#' library(targets)
#' library(tarchetypes)
#' library(ssdsims)
#' scenario <- ssd_define_scenario(ssddata::ccme_boron, nsim = 2L, seed = 42L)
#' ssd_scenario_targets(scenario)
#' ```
#'
#' For each step it `tarchetypes::tar_map()`s one named, `format = "file"`,
#' `error = "null"` target per `partition_by` path cell (the `names` are the
#' step's path axes), wires `sample -> fit -> hc -> summary` ordering with
#' `tar_combine()` barriers (a step body reads its parents from disk by partition
#' path, so there is no automatic edge), and writes every shard and the summary
#' under the per-layout [scenario_results_dir()] root (so a changed
#' `partition_by`/`bundle` never mixes shard granularities). `scenario` is
#' referenced as a global, so editing it invalidates the dependent shards.
#'
#' To parallelise the shards, set a controller (e.g. a mirai-backed
#' `crew::crew_controller_local()`) with `targets::tar_option_set()` in
#' `_targets.R` before calling this - the target set is unchanged.
#'
#' @inheritParams scenario_dataset
#' @param root The results root the shards and summary are written under;
#'   defaults to the per-layout [scenario_results_dir()].
#' @return A list of `targets` target objects, for `_targets.R` to return.
#' @seealso [scenario_results_dir()], [ssd_run_scenario_shards()] (the
#'   single-core, `targets`-free equivalent).
#' @export
#' @examples
#' \dontrun{
#' # _targets.R
#' library(targets)
#' library(tarchetypes)
#' library(ssdsims)
#' scenario <- ssd_define_scenario(ssddata::ccme_boron, nsim = 2L, seed = 42L)
#' ssd_scenario_targets(scenario)
#' }
ssd_scenario_targets <- function(
  scenario,
  root = scenario_results_dir(scenario)
) {
  chk::chk_s3_class(scenario, "ssdsims_scenario")
  chk::chk_string(root)
  rlang::check_installed(c("targets", "tarchetypes"))

  sample_dir <- file.path(root, "sample")
  fit_dir <- file.path(root, "fit")
  hc_dir <- file.path(root, "hc")
  summary_path <- file.path(root, "summary.parquet")

  # One `tar_map` per step: a named, format="file", error="null" target per
  # `partition_by` path cell. `tar_target_raw()` + `bquote()` injects the result
  # dirs as literals while leaving `scenario`/`tasks`/the step barriers as
  # symbols (resolved as targets globals / mapped values).
  step_map <- function(step, command) {
    shards <- switch(
      step,
      sample = ssd_scenario_sample_shards(scenario),
      fit = ssd_scenario_fit_shards(scenario),
      hc = ssd_scenario_hc_shards(scenario)
    )
    tarchetypes::tar_map(
      values = shards,
      names = tidyselect::all_of(scenario_partition_axes(scenario, step)$path),
      targets::tar_target_raw(
        paste0(step, "_step"),
        command,
        format = "file",
        error = "null"
      )
    )
  }

  sample_targets <- step_map(
    "sample",
    bquote(ssd_run_sample_step(tasks, scenario, .(sample_dir)))
  )
  fit_targets <- step_map(
    "fit",
    bquote({
      sample_done # order after all sample shards (read by partition path)
      ssd_run_fit_step(tasks, scenario, .(sample_dir), .(fit_dir))
    })
  )
  hc_targets <- step_map(
    "hc",
    bquote({
      fit_done # order after all fit shards (read by partition path)
      ssd_run_hc_step(tasks, scenario, .(fit_dir), .(hc_dir))
    })
  )

  list(
    sample_targets,
    tarchetypes::tar_combine(sample_done, sample_targets),
    fit_targets,
    tarchetypes::tar_combine(fit_done, fit_targets),
    hc_targets,
    tarchetypes::tar_combine(hc_done, hc_targets),
    # `summary` reads the hc directory (unions whatever landed); `hc_done` orders
    # it after the hc shards.
    targets::tar_target_raw(
      "summary",
      bquote({
        hc_done
        ssd_summarize(.(sample_dir), .(fit_dir), .(hc_dir), .(summary_path))
      }),
      format = "file"
    )
  )
}
