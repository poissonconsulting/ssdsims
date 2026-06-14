# `ssd_design_targets()`: run a design (a collection of scenarios) as one
# static-branching `targets` pipeline.
#
# A design is the *de-duplicated union* of its members' regular per-step task
# tables - the irregular (ragged) grid. Shards are addressed by **cell** under a
# legible `seed=<value>/layout=<hash>` root (no per-scenario prefix, no opaque
# content key): a cell several members share resolves to one target, computed
# once. Correctness of naked cell addressing rests on the `ssd_design()`
# name->value consistency contract (same name => same value, uniform
# `partition_by`); the only off-axis input that legitimately varies across members
# is `seed`, which becomes a path level and a target-name component.
#
# This factory reuses the single-scenario machinery (`scenario_step_slice()`,
# `scenario_partition_axes()`, `shard_cell_names()`, `child_parent_edges()`, the
# `ssd_run_*_step()` runners, `ssd_summarise()`'s DuckDB projection) - it changes
# only addressing (the `seed=` root, the seed name component) and the fan-in
# (per-member filtered summaries unioned into one combined summary).

# The `seed=`/`layout=` results root for a design member's seed group: the
# single-scenario `scenario_results_dir()` (`<root>/layout=<hash>`) with a
# leading `seed=<value>` level, so members with different seeds (which share no
# draws) never collide while members sharing a seed share their coincident cells.
design_results_dir <- function(scenario, root) {
  file.path(
    root,
    paste0("seed=", scenario$seed),
    paste0("layout=", substr(rlang::hash(scenario$partition_by), 1L, 12L))
  )
}

# Union the members' per-step shard tables and de-duplicate by `<step>_id` (the
# full task identity): one row per shard cell, its `tasks` the union of the
# members' tasks at that cell. Within a seed group the seed is shared and the
# primer derives from the cell, so tasks with equal `<step>_id` are byte-identical
# - keeping one is sound.
union_shards <- function(members, step) {
  ref <- members[[1L]]
  path_axes <- scenario_partition_axes(ref, step)$path
  id_col <- paste0(step, "_id")
  tbls <- lapply(members, function(s) scenario_shards(s, step))
  all <- dplyr::bind_rows(tbls)
  grouped <- dplyr::group_by(all, dplyr::across(dplyr::all_of(path_axes)))
  keys <- dplyr::group_keys(grouped)
  splits <- dplyr::group_split(grouped)
  keys$tasks <- lapply(splits, function(shard_rows) {
    tasks <- dplyr::bind_rows(shard_rows$tasks)
    tasks[!duplicated(tasks[[id_col]]), , drop = FALSE]
  })
  tibble::as_tibble(keys)
}

# Build the synthetic reference scenario that drives the slice/partition machinery
# for a seed group. `data`, `min_pmix_fns`, and `distsets` may differ in
# *coverage* across members (the ragged grid) and are unioned (consistent values
# by the `ssd_design()` contract). The slice-determining *settings* - `nrow_max`,
# the fit `dists` union, and the hc readouts - must be uniform within the group in
# this (irregular-grid) build: a shared cell then has byte-identical content under
# pure cell-union. (Per-overlap readout aggregation, which relaxes the hc-readout
# requirement, is layered on separately.)
design_reference_scenario <- function(members, call = rlang::caller_env()) {
  ref <- members[[1L]]
  for (i in seq_along(members)[-1L]) {
    s <- members[[i]]
    require_uniform(ref, s, "nrow_max", ref$nrow_max, s$nrow_max, call)
    require_uniform(ref, s, "dists", ref$fit$dists, s$fit$dists, call)
    require_uniform(ref, s, "ci", ref$hc$ci, s$hc$ci, call)
    require_uniform(
      ref,
      s,
      "proportion",
      ref$hc$proportion,
      s$hc$proportion,
      call
    )
    require_uniform(
      ref,
      s,
      "est_method",
      ref$hc$est_method,
      s$hc$est_method,
      call
    )
    require_uniform(ref, s, "samples", ref$hc$samples, s$hc$samples, call)
  }
  ref$data <- union_named(lapply(members, function(s) s$data))
  ref$datasets <- names(ref$data)
  ref$min_pmix_fns <- union_named(lapply(members, function(s) s$min_pmix_fns))
  ref$hc$distsets <- union_named(lapply(members, function(s) s$hc$distsets))
  ref
}

require_uniform <- function(ref, s, what, a, b, call = rlang::caller_env()) {
  if (!identical(a, b)) {
    chk::abort_chk(
      "Members sharing a seed must agree on `",
      what,
      "` (per-overlap aggregation of differing hc readouts across members is ",
      "not yet supported in `ssd_design_targets()`); scenario seed ",
      ref$seed,
      " has divergent `",
      what,
      "`.",
      call = call
    )
  }
  invisible(NULL)
}

# Union of named lists, keeping the first binding per name (values are consistent
# across members by the `ssd_design()` contract, so first-wins is well-defined).
union_named <- function(lists) {
  out <- list()
  for (l in lists) {
    for (nm in names(l)) {
      if (is.null(out[[nm]])) {
        out[nm] <- list(l[[nm]])
      }
    }
  }
  out
}

#' Combine Per-scenario Summaries into One Design Summary
#'
#' The design pipeline's fan-in: unions the named per-scenario compact summary
#' Parquets into one combined summary, tagging each row with a `scenario` identity
#' column equal to the member's name within the design. The union is performed at
#' the DuckDB level (each file read lazily via `duckplyr`, tagged, and
#' `union_all`-ed straight back out), so no per-scenario summary is collected into
#' R. Per-scenario files that did not land are skipped, so the combined summary
#' unions the surviving members (the keep-going property of [ssd_summarise()]).
#'
#' @param summaries A **named** character vector of per-scenario compact summary
#'   Parquet paths; the names are the scenario names within the design.
#' @param path The output Parquet path for the combined design summary (the
#'   `format = "file"` contract).
#' @return `path` (the `format = "file"` contract).
#' @seealso [ssd_design_targets()], [ssd_summarise()].
#' @export
ssd_summarise_design <- function(summaries, path) {
  chk::chk_named(summaries)
  local_duckplyr_config()
  existing <- summaries[file.exists(summaries)]
  if (!length(existing)) {
    ssd_write_parquet(tibble::tibble(scenario = character(0)), path)
    return(path)
  }
  parts <- purrr::imap(existing, function(p, nm) {
    tbl <- duckplyr::read_parquet_duckdb(
      p,
      options = list(hive_partitioning = FALSE)
    )
    dplyr::mutate(tbl, scenario = nm)
  })
  combined <- purrr::reduce(parts, dplyr::union_all)
  ssd_write_parquet(combined, path)
  path
}

#' Summarise One Design Member from the Shared hc Shards
#'
#' The per-scenario fan-in used by [ssd_design_targets()]: reads the (shared) hc
#' shards under `dir_hc`, filters to the member's hc task identities (`hc_ids`),
#' projects out the `dists`/`samples` list-columns at the DuckDB level, and writes
#' the member's compact summary at `path`. Filtering by `hc_id` selects exactly the
#' member's rows from the shards it shares with other members of the design. The
#' read, filter, projection, and write all happen inside DuckDB (never collecting
#' into R), mirroring [ssd_summarise()].
#'
#' @param dir_hc The (shared) `hc` results root of the member's seed group.
#' @param hc_ids The member's hc task identities (`hc_id`s) to keep.
#' @param path The output Parquet path for the member's compact summary (the
#'   `format = "file"` contract).
#' @return `path`.
#' @seealso [ssd_design_targets()], [ssd_summarise_design()], [ssd_summarise()].
#' @export
#' @autoglobal
ssd_summarise_member <- function(dir_hc, hc_ids, path) {
  local_duckplyr_config()
  glob <- file.path(dir_hc, "**", "part.parquet")
  hc_shards <- duckplyr::read_parquet_duckdb(
    glob,
    options = list(hive_partitioning = FALSE)
  )
  hc <- dplyr::filter(hc_shards, hc_id %in% hc_ids)
  hc <- dplyr::select(hc, -dplyr::any_of(c("dists", "samples")))
  ssd_write_parquet(hc, path)
  path
}

#' Build the Targets Pipeline for a Design
#'
#' A **target factory** (the multi-scenario sibling of [ssd_scenario_targets()]):
#' returns the list of `targets` objects that runs a [ssd_design()] - a named
#' collection of scenarios - as one static-branching, Hive-sharded pipeline, so a
#' whole `_targets.R` reduces to *build a design and call this*.
#'
#' A design is the **de-duplicated union** of its members' regular per-step task
#' sets - the irregular (ragged) grid. Members are grouped by `seed`; within a
#' group the union shard tables are computed (one target per cell, a cell shared
#' by several members built **once**) and written under a legible
#' `<root>/seed=<value>/layout=<hash>` tree, with the `seed` woven into the target
#' names so cells never collide across seed groups. Each member then gets a
#' `summary_<name>` target that filters the shared shards to its own task
#' identities, and the top-level `summary` target unions those into
#' `<root>/summary.parquet` with a `scenario` identity column
#' ([ssd_summarise_design()]).
#'
#' @section Migration from a single scenario:
#' Growing a one-off [ssd_scenario_targets()] run into a study is a one-line
#' switch: wrap the scenario with [ssd_design()] and call `ssd_design_targets()`.
#' The per-task results are byte-identical to the standalone run; the only cost is
#' a one-time recompute into the design's `seed=`-levelled tree (the addressing
#' gains the `seed=` level the standalone `layout=` tree lacks) - **safe but
#' recomputing**. Later members are added by extending the `ssd_design(...)` call;
#' the cells they share (within a seed) stay cached.
#'
#' @section Varying the seed:
#' Members may use different `seed`s (e.g. repeating the exploration under several
#' master seeds); they land under separate `seed=` trees and share nothing.
#' Members sharing a `seed` share their coincident cells (common random numbers).
#'
#' @param design An `ssdsims_design` from [ssd_design()].
#' @param ... Unused; must be empty (forces `root`/`upload`/`cue` to be named).
#' @param root The results root the shards and summaries are written under.
#' @param upload An optional upload destination from [ssd_upload_azure()] or
#'   [ssd_upload_dryrun()], or `NULL` (default) for no upload targets.
#' @param cue An optional `targets::tar_cue()` applied to every shard target.
#' @return A list of `targets` target objects, for `_targets.R` to return.
#' @seealso [ssd_design()], [ssd_scenario_targets()], [ssd_summarise_design()].
#' @export
#' @autoglobal
#' @examples
#' \dontrun{
#' # _targets.R
#' library(targets)
#' library(tarchetypes)
#' library(ssdsims)
#' data <- ssd_scenario_data(ssddata::ccme_boron)
#' coarse <- ssd_define_scenario(data, nsim = 2L, seed = 42L, nrow = c(5L, 10L))
#' dense <- ssd_define_scenario(data, nsim = 2L, seed = 42L, nrow = c(6L, 7L, 8L))
#' design <- ssd_design(coarse, dense)
#' ssd_design_targets(design)
#' }
ssd_design_targets <- function(
  design,
  ...,
  root = "results",
  upload = NULL,
  cue = NULL
) {
  call <- environment()
  rlang::check_dots_empty()
  chk::chk_s3_class(design, "ssdsims_design")
  chk::chk_string(root)
  if (!is.null(upload) && !inherits(upload, "ssdsims_upload")) {
    chk::abort_chk(
      "`upload` must be `NULL` or an upload destination from ",
      "`ssd_upload_azure()` or `ssd_upload_dryrun()`.",
      call = call
    )
  }
  rlang::check_installed(c("targets", "tarchetypes"))

  seeds <- vapply(design, function(s) s$seed, integer(1L))
  uniq_seeds <- unique(seeds)

  shard_targets <- list()
  member_info <- list()
  for (sd in uniq_seeds) {
    members <- design[seeds == sd]
    ref <- design_reference_scenario(members, call = call)
    sroot <- design_results_dir(ref, root)
    group <- design_group_targets(members, ref, sroot, upload, cue)
    shard_targets <- c(shard_targets, group$targets)
    for (nm in names(members)) {
      member_info[[nm]] <- list(
        hc_dir = group$hc_dir,
        hc_names = group$hc_names,
        hc_ids = ssd_scenario_hc_tasks(members[[nm]])$hc_id,
        summary_path = file.path(root, paste0("summary-", nm, ".parquet"))
      )
    }
  }

  # Per-member compact summaries: each filters the shared hc shards to its own
  # task identities (depending on its seed group's hc shard targets for ordering).
  summary_member_targets <- purrr::imap(member_info, function(info, nm) {
    targets::tar_target_raw(
      paste0("summary_", nm),
      rlang::expr({
        !!edge_block(unname(info$hc_names))
        ssd_summarise_member(!!info$hc_dir, !!info$hc_ids, !!info$summary_path)
      }),
      format = "file"
    )
  })

  # The combined design summary: union the per-member compact summaries with a
  # `scenario` identity column (depends on every per-member summary target).
  summaries_named <- vapply(
    member_info,
    function(i) i$summary_path,
    character(1L)
  )
  summary_target <- targets::tar_target_raw(
    "summary",
    rlang::expr({
      !!edge_block(paste0("summary_", names(member_info)))
      ssd_summarise_design(
        !!summaries_named,
        !!file.path(root, "summary.parquet")
      )
    }),
    format = "file"
  )

  c(shard_targets, unname(summary_member_targets), list(summary_target))
}

# Build one seed group's shard targets (sample/fit/hc) from the union shard
# tables, rooted at `sroot` and named with the group's `seed` so cells never
# collide across seed groups. Mirrors `ssd_scenario_targets()`'s per-step
# assembly, driven by the reference scenario `ref`.
design_group_targets <- function(members, ref, sroot, upload, cue) {
  sample_dir <- file.path(sroot, "sample")
  fit_dir <- file.path(sroot, "fit")
  hc_dir <- file.path(sroot, "hc")

  step_map <- function(step, shards, command) {
    nms <- tidyselect::all_of(c(
      "seed",
      scenario_partition_axes(ref, step)$path
    ))
    step_target <- targets::tar_target_raw(
      paste0(step, "_step"),
      command,
      format = "file",
      error = "null",
      cue = cue
    )
    if (is.null(upload)) {
      return(tarchetypes::tar_map(values = shards, names = nms, step_target))
    }
    upload_target <- targets::tar_target_raw(
      paste0("upload_", step),
      rlang::expr(ssd_upload_shard(
        !!rlang::sym(paste0(step, "_step")),
        !!upload
      )),
      format = "file",
      error = "null",
      cue = cue
    )
    tarchetypes::tar_map(
      values = shards,
      names = nms,
      step_target,
      upload_target
    )
  }

  fit_slice <- scenario_step_slice(ref, "fit")

  sample_shards <- union_shards(members, "sample")
  sample_shards$seed <- ref$seed
  sample_shards$.slice <- purrr::map(sample_shards$tasks, function(tasks) {
    scenario_step_slice(ref, "sample", unique(tasks$dataset))
  })
  sample_targets <- step_map(
    "sample",
    sample_shards,
    rlang::expr(ssd_run_sample_step(tasks, .slice, !!sample_dir))
  )
  sample_names <- shard_cell_names(sample_targets, sample_shards, ref, "sample")

  fit_shards <- union_shards(members, "fit")
  fit_shards$seed <- ref$seed
  fit_shards$.parents <- child_parent_edges(
    fit_shards,
    ref,
    "sample",
    sample_names
  )
  fit_targets <- step_map(
    "fit",
    fit_shards,
    rlang::expr({
      .parents
      ssd_run_fit_step(tasks, !!fit_slice, !!sample_dir, !!fit_dir)
    })
  )
  fit_names <- shard_cell_names(fit_targets, fit_shards, ref, "fit")

  hc_shards <- union_shards(members, "hc")
  hc_shards$seed <- ref$seed
  hc_shards$.parents <- child_parent_edges(hc_shards, ref, "fit", fit_names)
  hc_shards$.slice <- purrr::map(hc_shards$tasks, function(tasks) {
    scenario_step_slice(ref, "hc", distsets = unique(tasks$distset))
  })
  hc_targets <- step_map(
    "hc",
    hc_shards,
    rlang::expr({
      .parents
      ssd_run_hc_step(tasks, .slice, !!fit_dir, !!hc_dir)
    })
  )
  hc_names <- shard_cell_names(hc_targets, hc_shards, ref, "hc")

  list(
    targets = list(sample_targets, fit_targets, hc_targets),
    hc_dir = hc_dir,
    hc_names = hc_names
  )
}
