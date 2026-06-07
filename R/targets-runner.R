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
#
# This ASCII-`VARCHAR` encoding is the deliberate, benchmark-backed choice for
# the blob (the `blob-storage-format` change): on a representative `fitdists` it
# is lossless (the byte-identity oracle), it is a plain string column duckplyr
# can store *and project out* without decoding, and it needs no extra
# dependency. The two alternatives lost on those constraints - binary
# `serialize(ascii = FALSE)` as base64 text came in ~1.5x *larger* (base64's
# 4/3 expansion plus Parquet's own column compression of the ASCII string
# erased any binary win), and `jsonlite::serializeJSON()` is not lossless (it
# cannot reconstruct the TMB objective closure carried in `model$fn`).

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
#'
#' The single encode/decode seam through which a non-tabular per-task result
#' enters and leaves the shard Parquet. The encoding is lossless
#' (`decode_obj(encode_obj(x))` recovers `x`) and yields a single string-column
#' value the duckplyr engine can store and project out without decoding.
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

# The minimal `ssdsims_scenario`-classed sub-object the named step's per-shard
# runner consumes - a deterministic, hashable projection of the scenario so a
# step's `tar_map()` command depends only on the fields that step reads, not the
# bare `scenario` global (so editing a step-irrelevant field leaves the other
# steps' shards cached). Reading the three runners pins each step's consumed set:
#
#   sample -> the datasets it draws from (read via `scenario_dataset()`) +
#             `partition_by$sample` (its `shard_path()` axis). `datasets`
#             restricts the carried datasets to those the *shard* reads (its
#             `unique(tasks$dataset)`); since `dataset` is a path axis, a sample
#             shard reads one dataset, so the slice carries only that one. This
#             keeps a sample shard's slice independent of the *other* datasets,
#             so appending a dataset mints a new shard and leaves the existing
#             shards' slices (and thus their cached Parquets) untouched.
#   fit    -> `fit$dists` + the `min_pmix` functions (resolved via
#             `scenario_min_pmix()` in the fit primer) + `partition_by` for
#             `sample` (parent read) and `fit` (own path). Carries no datasets
#             (fit reads its parent `sample` shards off disk), so it is already
#             independent of the dataset set.
#   hc     -> `hc$proportion` + `hc$samples` + `partition_by` for `fit` (parent
#             read) and `hc` (own path).
#
# `seed`/`primer` are not sliced - they ride in each shard's `tasks` list-column.
# The `min_pmix` functions hash by name (carried for `fit` execution but not part
# of task identity), so carrying them does not couple a `fit` shard to a
# function-body edit. The class tag is preserved so each runner's
# `chk_s3_class()` and the `scenario_dataset()`/`scenario_min_pmix()` accessors
# work unchanged on the slice. A pure function of the scenario's already-
# materialised fields (no environment capture), so two computations are
# byte-identical and produce the same dependency hash.
scenario_step_slice <- function(
  scenario,
  step,
  datasets = names(scenario$data)
) {
  chk::chk_s3_class(scenario, "ssdsims_scenario")
  step <- rlang::arg_match0(step, c("sample", "fit", "hc"))
  partition_by <- scenario$partition_by
  slice <- switch(
    step,
    sample = list(
      data = scenario$data[datasets],
      partition_by = partition_by["sample"]
    ),
    fit = list(
      fit = list(dists = scenario$fit$dists),
      min_pmix_fns = scenario$min_pmix_fns,
      partition_by = partition_by[c("sample", "fit")]
    ),
    hc = list(
      hc = list(
        proportion = scenario$hc$proportion,
        samples = scenario$hc$samples
      ),
      partition_by = partition_by[c("fit", "hc")]
    )
  )
  structure(slice, class = "ssdsims_scenario")
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
    if (nrow(draw) == 0L) {
      chk::abort_chk(
        "Expected a `sample` draw for sample_id ",
        encodeString(t$sample_id, quote = "\""),
        ", found none in the parent `sample` shard(s) (missing parent result)."
      )
    }
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
#' task's hc tibble (one or more rows - the `proportion` fan-out, with the
#' scalar `ci` applied uniformly and bootstrap-only knobs `NA` when
#' `ci = FALSE`) is tagged with its `hc_id` and parent
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

  # FIXME: Ensure the samples column is unnamed upstream
  rows <- dplyr::bind_rows(rows)
  rows$samples <- purrr::map(rows$samples, unname)

  ssd_write_parquet(rows, out)
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
#' In a `targets` pipeline a directory read carries no dependency edge, so
#' [ssd_scenario_targets()] orders `summary` after the shards by naming every
#' `hc` shard target in its command (it re-runs when any `hc` shard's bytes
#' change). Reading the directory - rather than the shard target values - is what
#' lets it union whatever shards landed (the survivors of a partially-failed run,
#' section 6.2).
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

# ---- per-child upstream edges (Option 3, TARGETS-DESIGN.md sections 6, 8) ---
#
# The shard invalidation model is content-hash over the `format = "file"`
# Parquet outputs, read observably as cache-by-existence: a shard is up to date
# iff its Parquet exists and the bytes its body depends on (its task rows, the
# scenario, and the *named* parent shard targets it reads) are unchanged. To
# make that precise across the m:n child<-parent fan-in, each child shard target
# names only the specific parent shard target(s) its tasks read - not a coarse
# step-wide barrier - so rewriting one parent shard invalidates only the child
# shards that read it.

# The tar_map() target names minted for a step's shards, keyed by the shard's
# Hive partition path cell. `mapped` is the tar_map() return
# (`list(<step>_step = list(<target per shard, in `shards` row order>))`); the
# path cells are computed from `shards` in the same row order, so the result is
# one source of truth mapping cell -> target name.
shard_cell_names <- function(mapped, shards, scenario, step) {
  in_order <- mapped[[paste0(step, "_step")]]
  nms <- purrr::map_chr(in_order, function(t) t$settings$name)
  # All rows of a shard share its path cell (it is the grouping key), so the
  # first row gives the cell - mirrors `shard_path()`.
  cells <- purrr::map_chr(
    shards$tasks,
    function(tasks) {
      path_key(tasks[1L, , drop = FALSE], scenario$partition_by[[step]])
    }
  )
  rlang::set_names(nms, cells)
}

# For each child shard, the language block that names the parent shard target(s)
# its tasks read - the per-child upstream edge spliced into the child's command
# so `targets` records the dependency. The parent cell set is
# `unique(path_key(tasks, partition_by[[parent]]))`, the *same* projection
# `read_parent_shards()` uses to read them, so the dependency graph and the read
# agree (one source of truth).
child_parent_edges <- function(
  child_shards,
  scenario,
  parent,
  parent_cell_names
) {
  purrr::map(child_shards$tasks, function(tasks) {
    cells <- unique(path_key(tasks, scenario$partition_by[[parent]]))
    nms <- parent_cell_names[cells]
    if (anyNA(nms)) {
      chk::abort_chk(
        "A ",
        parent,
        " parent shard has no target for: ",
        paste(cells[is.na(nms)], collapse = ", "),
        "."
      )
    }
    edge_block(unname(nms))
  })
}

# A `{ a; b; ... }` block of bare target-name symbols. Referenced (and discarded)
# in a command purely so `targets` records the upstream edges; the values are the
# parents' `format = "file"` paths (cheap strings).
edge_block <- function(names) {
  rlang::call2("{", !!!rlang::syms(names))
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
#' The shard and summary targets carry `error = "null"` so a shard whose body
#' fails entirely goes `NULL` (its error readable via `tar_meta()`) without
#' aborting the run, and `ssd_summarize()` unions whatever landed
#' (TARGETS-DESIGN.md section 6.2). The shipped `_targets.R` templates pair this
#' with a pipeline-wide **keep-going** default (`tar_option_set(error =
#' "continue")`, the `make -k` analogue) so an errored target skips only its
#' dependents while the rest of the shards still build; fail-fast pre-flight
#' checks (upload/cluster connectivity) belong in a separate script the user
#' runs *before* `tar_make()`, not in this DAG.
#'
#' For each step it `tarchetypes::tar_map()`s one named, `format = "file"`,
#' `error = "null"` target per `partition_by` path cell (the `names` are the
#' step's path axes), and writes every shard and the summary under the
#' per-layout [scenario_results_dir()] root (so a changed `partition_by`/`bundle`
#' never mixes shard granularities). Each step's command depends only on the
#' **minimal scenario slice** its runner consumes (`scenario_step_slice()`)
#' rather than the bare `scenario` global, so editing a field a step does not
#' read leaves the other steps' shards cached. The `sample` slice is built
#' **per shard**, carrying only the dataset(s) that shard reads, so appending a
#' dataset mints a new shard and leaves every existing shard cached.
#'
#' @section Invalidation model:
#' The shard targets use **content-hash invalidation over their `format =
#' "file"` Parquet outputs** (TARGETS-DESIGN.md section 8), observable as
#' **cache-by-existence**: a shard is up to date iff its Parquet exists *and* the
#' inputs its body depends on - its task rows, the step's minimal scenario slice
#' (`scenario_step_slice()`), and the parent shard target(s) it reads - are
#' unchanged. A missing Parquet rebuilds; a recomputed shard whose bytes are
#' byte-identical leaves its dependents skipped.
#'
#' Instead of a coarse `sample -> fit -> hc` `tar_combine()` barrier (which marks
#' the *whole* downstream step out of date when any one parent shard changes),
#' each child shard target names **only the specific parent shard target(s) its
#' tasks read** (the Option-3 per-child upstream edges of section 6), computed at
#' sourcing time as `unique(path_key(tasks, partition_by[[parent]]))` - the same
#' projection the runner uses to read them. So rewriting one parent
#' shard re-runs only the child shards that read it. `summary` reads the whole
#' `hc` directory, so it names every `hc` shard (it re-runs when any `hc` shard's
#' bytes change, and unions the survivors of a partially-failed run).
#'
#' @section Pinning trusted shards (`cue`):
#' Pass `cue = targets::tar_cue(depend = FALSE)` to **pin** the shard targets
#' against upstream dependency/code changes (an edited per-task primitive, a
#' bumped `ssdtools`), so trusted shards are not rebuilt by a code edit
#' (TARGETS-DESIGN.md section 8.3). The carve-outs still hold: a shard rebuilds
#' if its `format = "file"` Parquet is missing, if its task-table grouping
#' changes (the grouping is part of the command, so path-axis and inner-axis
#' growth still apply under the pin), or if it previously errored. Force a
#' refresh of chosen shards with `targets::tar_invalidate()` (or by deleting
#' their Parquet), overriding the pin (section 8.4). The default (`NULL`) is
#' `targets`' standard cue.
#'
#' The `head(sample, nrow)` truncation stays folded into the `fit` step (no
#' materialised `data` shard): a `fit` shard is keyed by `fit_id`, which includes
#' `nrow`, so extending `nrow` mints new `fit` shards and caches the rest, and a
#' widened `max(nrow)` changes the `sample` shard's `n_max` task row, so its
#' bytes change and the per-child edge propagates to exactly the `fit` shards
#' that read the wider draw - no stale short draw is produced.
#'
#' To parallelise the shards, set a controller (e.g. a mirai-backed
#' `crew::crew_controller_local()`) with `targets::tar_option_set()` in
#' `_targets.R` before calling this - the target set is unchanged.
#'
#' @inheritParams scenario_dataset
#' @param root The results root the shards and summary are written under;
#'   defaults to the per-layout [scenario_results_dir()].
#' @param cue An optional `targets::tar_cue()` applied to every shard target
#'   (e.g. `targets::tar_cue(depend = FALSE)` to pin trusted shards against code
#'   changes). `NULL` (default) uses `targets`' standard cue.
#' @return A list of `targets` target objects, for `_targets.R` to return.
#' @seealso [scenario_results_dir()], [ssd_run_scenario_shards()] (the
#'   single-core, `targets`-free equivalent).
#' @export
#' @autoglobal
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
  root = scenario_results_dir(scenario),
  cue = NULL
) {
  chk::chk_s3_class(scenario, "ssdsims_scenario")
  chk::chk_string(root)
  rlang::check_installed(c("targets", "tarchetypes"))

  sample_dir <- file.path(root, "sample")
  fit_dir <- file.path(root, "fit")
  hc_dir <- file.path(root, "hc")
  summary_path <- file.path(root, "summary.parquet")

  sample_shards <- ssd_scenario_sample_shards(scenario)
  fit_shards <- ssd_scenario_fit_shards(scenario)
  hc_shards <- ssd_scenario_hc_shards(scenario)

  # One `tar_map` per step: a named, format="file", error="null" target per
  # `partition_by` path cell. `tar_target_raw()` + `rlang::expr()` injects the
  # result dirs (and the step-global `fit`/`hc` slices) as literals (`!!`) while
  # leaving `tasks`, the per-child `.parents` edge block, and the per-shard
  # `.slice` (the `sample` step's per-dataset slice) as symbols resolved as
  # mapped values. Depending on the slice - not the bare `scenario` global - is
  # what scopes each step's dependency hash to the fields its runner reads.
  step_map <- function(step, shards, command) {
    tarchetypes::tar_map(
      values = shards,
      names = tidyselect::all_of(scenario_partition_axes(scenario, step)$path),
      targets::tar_target_raw(
        paste0(step, "_step"),
        command,
        format = "file",
        error = "null",
        cue = cue
      )
    )
  }

  # Each step's command depends only on its minimal scenario slice (not the bare
  # `scenario` global), so editing a field outside a step's slice no longer
  # invalidates that step's shards. The `fit`/`hc` slices carry no datasets and
  # are step-global, so they are spliced (`!!`) once into the step's command.
  fit_slice <- scenario_step_slice(scenario, "fit")
  hc_slice <- scenario_step_slice(scenario, "hc")

  # sample: a leaf step, no upstream shards. The slice carries the datasets, so
  # it is built *per shard* (carrying only the dataset(s) that shard reads) and
  # carried as a `.slice` mapped value - appending a dataset then mints a new
  # shard and leaves the existing shards' commands (and cached Parquets) intact.
  sample_shards$.slice <- purrr::map(
    sample_shards$tasks,
    function(tasks) {
      scenario_step_slice(scenario, "sample", unique(tasks$dataset))
    }
  )
  sample_targets <- step_map(
    "sample",
    sample_shards,
    rlang::expr(ssd_run_sample_step(tasks, .slice, !!sample_dir))
  )
  sample_names <- shard_cell_names(
    sample_targets,
    sample_shards,
    scenario,
    "sample"
  )

  # fit: each shard names only the sample shard(s) its tasks read.
  fit_shards$.parents <- child_parent_edges(
    fit_shards,
    scenario,
    "sample",
    sample_names
  )
  fit_targets <- step_map(
    "fit",
    fit_shards,
    rlang::expr({
      .parents # per-child edges to the sample shards this fit shard reads
      ssd_run_fit_step(tasks, !!fit_slice, !!sample_dir, !!fit_dir)
    })
  )
  fit_names <- shard_cell_names(fit_targets, fit_shards, scenario, "fit")

  # hc: each shard names only the fit shard(s) its tasks read.
  hc_shards$.parents <- child_parent_edges(
    hc_shards,
    scenario,
    "fit",
    fit_names
  )
  hc_targets <- step_map(
    "hc",
    hc_shards,
    rlang::expr({
      .parents # per-child edges to the fit shards this hc shard reads
      ssd_run_hc_step(tasks, !!hc_slice, !!fit_dir, !!hc_dir)
    })
  )
  hc_names <- shard_cell_names(hc_targets, hc_shards, scenario, "hc")

  # `summary` reads the whole hc directory (unions whatever landed), so it names
  # every hc shard: it re-runs when any hc shard's bytes change, and survives a
  # partially-failed run (`error = "null"`).
  summary_target <- targets::tar_target_raw(
    "summary",
    rlang::expr({
      !!edge_block(unname(hc_names)) # order/value-depend on every hc shard
      ssd_summarize(!!sample_dir, !!fit_dir, !!hc_dir, !!summary_path)
    }),
    format = "file"
  )

  list(sample_targets, fit_targets, hc_targets, summary_target)
}
