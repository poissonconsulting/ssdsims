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
# by the `ssd_design()` contract). The fit `dists` union is reconciled by fitting
# the **design-wide union** of the members' `dists` once per fit cell, each member
# subsetting via its `distset` hc axis (sound by distset-subset-invariance:
# per-dist fits are independent, so fitting extra dists does not change a member's
# subset hc). The four non-axis hc readouts (`proportion`, `est_method`, `ci`,
# `samples`) MAY differ too and are reconciled by the per-overlap aggregation
# (`design_hc_assembly()`); only the layout-shaping `nrow_max` (and `partition_by`,
# enforced by `ssd_design()`) remain uniform-required here.
design_reference_scenario <- function(members, call = rlang::caller_env()) {
  ref <- members[[1L]]
  for (i in seq_along(members)[-1L]) {
    s <- members[[i]]
    require_uniform(ref, s, "nrow_max", ref$nrow_max, s$nrow_max, call)
  }
  ref$data <- union_named(lapply(members, function(s) s$data))
  ref$datasets <- names(ref$data)
  ref$min_pmix_fns <- union_named(lapply(members, function(s) s$min_pmix_fns))
  ref$hc$distsets <- union_named(lapply(members, function(s) s$hc$distsets))
  # Fit the design-wide union of the members' `dists` once per fit cell; each
  # member subsets it to its own `distset` members at the hc step. (The fit `dists`
  # do not enter the `fit_id`, so a wider union shares the fit cells; the hc
  # `distset` axis isolates each member's subset.)
  ref$fit$dists <- sort(unique(unlist(
    lapply(members, function(s) s$fit$dists),
    use.names = FALSE
  )))
  ref
}

require_uniform <- function(ref, s, what, a, b, call = rlang::caller_env()) {
  if (!identical(a, b)) {
    chk::abort_chk(
      "Members sharing a seed must agree on `",
      what,
      "`; scenario seed ",
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

# Assemble the hc shard table for a seed group with per-overlap readout
# aggregation, returning the shards plus, per member, the serving `hc_id`s and the
# `proportion`/`est_method` row filters its summary applies.
#
# When every member shares the four non-axis hc settings
# (`proportion`/`est_method`/`ci`/`samples`) the plain cell-union (`union_shards()`)
# is byte-identical to a standalone run and cache-compatible with it, so it is
# taken unchanged (no per-task demand columns, no filtering). Otherwise the
# members' hc tasks are unioned, **tagged with each member's readout demand**, and
# grouped by hc **cell** (`hc_id` = fit identity + `nboot`/`ci_method`/`parametric`
# /`distset`). For each cell the demand is reduced - `union` `proportion`/
# `est_method`, `any` `ci`/`samples` - and attached to the shard's tasks so the
# runner computes the maximal readout set; the per-member summary then filters its
# slice.
#
# `ci` NA-collapse routing: a `ci = FALSE` task collapses `nboot`/`ci_method`/
# `parametric` to `NA`, so its cell never coincides with a `ci = TRUE` task's. The
# point `est` is analytical and bootstrap-config-invariant, so a `ci = FALSE`
# cell's demand is folded into a coincident `ci = TRUE` cell at the same
# `(fit_id, distset)` when one exists (the `ci = FALSE` member then reads its
# served `est` from that `ci = TRUE` shard); only otherwise is a standalone
# `ci = FALSE` cell minted. The computed cells are therefore every `ci = TRUE`
# cell plus the `ci = FALSE` cells with no overlapping `ci = TRUE` cell.
design_hc_assembly <- function(members, ref) {
  nms <- names(members)
  readout_key <- function(s) {
    list(s$hc$proportion, s$hc$est_method, s$hc$ci, s$hc$samples)
  }
  uniform <- all(vapply(
    members[-1L],
    function(s) identical(readout_key(s), readout_key(members[[1L]])),
    logical(1L)
  ))
  est_method_varies <- !all(vapply(
    members[-1L],
    function(s) identical(s$hc$est_method, members[[1L]]$hc$est_method),
    logical(1L)
  ))
  no_filter <- rlang::set_names(vector("list", length(nms)), nms)

  if (uniform) {
    # The slice carries the (uniform) readouts; the runner reads them - no
    # per-task demand columns, byte-identical to the cell-union path.
    return(list(
      shards = union_shards(members, "hc"),
      serving = lapply(members, function(s) ssd_scenario_hc_tasks(s)$hc_id),
      proportion = no_filter,
      est_method = no_filter
    ))
  }

  # Flat, demand-tagged hc tasks across all members (the `(seed, primer)`
  # decoration mirrors `scenario_shards()`; the readout demand is the same for
  # every row of a member, as those are scenario-level settings).
  flat <- list()
  for (nm in nms) {
    s <- members[[nm]]
    tbl <- tibble::as_tibble(ssd_scenario_hc_tasks(s))
    tbl$seed <- s$seed
    tbl$primer <- task_primers(tbl, "hc")
    tbl$.dem_prop <- rep(list(s$hc$proportion), nrow(tbl))
    tbl$.dem_em <- rep(list(s$hc$est_method), nrow(tbl))
    tbl$.dem_ci <- s$hc$ci
    tbl$.dem_samples <- s$hc$samples
    flat[[nm]] <- tbl
  }
  all_hc <- dplyr::bind_rows(flat)

  # Reduce the demand per hc cell. The expected shape is **most cells touched by
  # one member** (a singleton `hc_id`): such a cell needs no reduction - its row
  # *is* the cell, its demand already final - so only the multi-member cells go
  # through the grouped set-union, optimising the common case. (`all_hc` is
  # R-resident - it carries list-columns like `primer` that cannot originate in
  # DuckDB - so the cheap count stays in dplyr; see this change's
  # `exploration/FINDINGS.md`.)
  demand_cols <- c(".dem_prop", ".dem_em", ".dem_ci", ".dem_samples")
  sizes <- dplyr::summarise(
    dplyr::group_by(all_hc, dplyr::across(dplyr::all_of("hc_id"))),
    .n = dplyr::n(),
    .groups = "drop"
  )
  multi_ids <- sizes$hc_id[sizes$.n > 1L]
  is_multi <- all_hc$hc_id %in% multi_ids
  single_cells <- all_hc[!is_multi, , drop = FALSE]
  multi_rows <- all_hc[is_multi, , drop = FALSE]
  if (nrow(multi_rows)) {
    # One grouped summarise does every multi-member set-union; the constant
    # axis/primer columns rejoin from the first row of each `hc_id`.
    reduced <- dplyr::summarise(
      dplyr::group_by(multi_rows, dplyr::across(dplyr::all_of("hc_id"))),
      .dem_prop = list(sort(unique(unlist(.data[[".dem_prop"]])))),
      .dem_em = list(unique(unlist(.data[[".dem_em"]]))),
      .dem_ci = any(.data[[".dem_ci"]]),
      .dem_samples = any(.data[[".dem_samples"]]),
      .groups = "drop"
    )
    skel <- multi_rows[
      !duplicated(multi_rows$hc_id),
      setdiff(names(multi_rows), demand_cols),
      drop = FALSE
    ]
    multi_cells <- dplyr::left_join(skel, reduced, by = "hc_id")
  } else {
    multi_cells <- single_cells[0L, , drop = FALSE]
  }
  cells <- dplyr::bind_rows(single_cells, multi_cells)

  # `ci` routing: fold each `ci = FALSE` cell's demand into a coincident
  # `ci = TRUE` cell (same `fit_id`, `distset`) when one exists. Expressed as a
  # join - each `ci = FALSE` cell maps to the `first()` matching `ci = TRUE` cell
  # - rather than a per-cell scan over the `ci = TRUE` cells.
  ci_true <- cells[cells$.dem_ci, , drop = FALSE]
  ci_false <- cells[!cells$.dem_ci, , drop = FALSE]
  route <- character(0L) # served `ci = FALSE` hc_id -> serving `ci = TRUE` hc_id
  if (nrow(ci_true) && nrow(ci_false)) {
    serving_lookup <- dplyr::summarise(
      dplyr::group_by(
        ci_true,
        dplyr::across(dplyr::all_of(c(
          "fit_id",
          "distset"
        )))
      ),
      serving = dplyr::first(.data[["hc_id"]]),
      .groups = "drop"
    )
    routed <- dplyr::left_join(
      ci_false[c("hc_id", "fit_id", "distset", demand_cols)],
      serving_lookup,
      by = c("fit_id", "distset")
    )
    served <- routed[!is.na(routed$serving), , drop = FALSE]
    route <- rlang::set_names(served$serving, served$hc_id)
    if (nrow(served)) {
      # Fold each served cell's demand into its serving `ci = TRUE` cell, grouped
      # by the serving hc_id so a cell served by several `ci = FALSE` members is
      # folded once.
      fold <- dplyr::summarise(
        dplyr::group_by(served, dplyr::across(dplyr::all_of("serving"))),
        add_prop = list(unique(unlist(.data[[".dem_prop"]]))),
        add_em = list(unique(unlist(.data[[".dem_em"]]))),
        add_samples = any(.data[[".dem_samples"]]),
        .groups = "drop"
      )
      pos <- match(fold$serving, ci_true$hc_id)
      for (k in seq_len(nrow(fold))) {
        i <- pos[[k]]
        ci_true$.dem_prop[[i]] <- sort(unique(c(
          ci_true$.dem_prop[[i]],
          fold$add_prop[[k]]
        )))
        ci_true$.dem_em[[i]] <- unique(c(
          ci_true$.dem_em[[i]],
          fold$add_em[[k]]
        ))
        ci_true$.dem_samples[i] <- ci_true$.dem_samples[i] ||
          fold$add_samples[[k]]
      }
    }
  }
  unserved_false <- ci_false[
    !(ci_false$hc_id %in% names(route)),
    ,
    drop = FALSE
  ]
  computed <- dplyr::bind_rows(ci_true, unserved_false)

  # Per member: the serving hc_id of each of its tasks (own cell, or the routed
  # `ci = TRUE` cell for a served `ci = FALSE` task), and its readout filters.
  serving <- lapply(members, function(s) {
    ids <- ssd_scenario_hc_tasks(s)$hc_id
    mapped <- route[ids]
    ifelse(is.na(mapped), ids, unname(mapped))
  })

  # Expose the per-task demand under the runner's argument names and shard it.
  computed$proportion <- computed$.dem_prop
  computed$est_method <- computed$.dem_em
  computed$ci <- computed$.dem_ci
  computed$samples <- computed$.dem_samples
  computed <- computed[,
    setdiff(names(computed), demand_cols),
    drop = FALSE
  ]
  path_axes <- scenario_partition_axes(ref, "hc")$path
  grp <- dplyr::group_by(computed, dplyr::across(dplyr::all_of(path_axes)))
  shards <- dplyr::group_keys(grp)
  shards$tasks <- dplyr::group_split(grp, .keep = TRUE)

  list(
    shards = tibble::as_tibble(shards),
    serving = serving,
    proportion = lapply(members, function(s) s$hc$proportion),
    est_method = if (est_method_varies) {
      lapply(members, function(s) s$hc$est_method)
    } else {
      no_filter
    }
  )
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
#' shards under `dir_hc`, filters to the member's **serving** hc task identities
#' (`hc_ids`) - its own cells, plus the coincident `ci = TRUE` cell serving each of
#' its `ci = FALSE` cells under [ssd_design_targets()]'s `ci` routing - projects out
#' the `dists`/`samples` list-columns at the DuckDB level, and writes the member's
#' compact summary at `path`. When the design aggregates differing readouts into a
#' shared cell, `proportion`/`est_method` narrow the maximal computed set back to
#' the member's requested readout rows. The read, filter, projection, and write all
#' happen inside DuckDB (never collecting into R), mirroring [ssd_summarise()].
#'
#' @param dir_hc The (shared) `hc` results root of the member's seed group.
#' @param hc_ids The member's serving hc task identities (`hc_id`s) to keep.
#' @param path The output Parquet path for the member's compact summary (the
#'   `format = "file"` contract).
#' @param proportion Optional `proportion` values to keep (the member's requested
#'   proportions when the design aggregates a wider union into the shared cell);
#'   `NULL` (the default) keeps every proportion.
#' @param est_method Optional `est_method` values to keep (the member's requested
#'   methods when the design aggregates a wider union); `NULL` (the default) keeps
#'   every method.
#' @return `path`.
#' @seealso [ssd_design_targets()], [ssd_summarise_design()], [ssd_summarise()].
#' @export
#' @autoglobal
ssd_summarise_member <- function(
  dir_hc,
  hc_ids,
  path,
  proportion = NULL,
  est_method = NULL
) {
  local_duckplyr_config()
  glob <- file.path(dir_hc, "**", "part.parquet")
  hc_shards <- duckplyr::read_parquet_duckdb(
    glob,
    options = list(hive_partitioning = FALSE)
  )
  hc <- dplyr::filter(hc_shards, hc_id %in% hc_ids)
  keep_prop <- proportion
  keep_em <- est_method
  if (!is.null(keep_prop)) {
    hc <- dplyr::filter(hc, proportion %in% keep_prop)
  }
  if (!is.null(keep_em)) {
    hc <- dplyr::filter(hc, est_method %in% keep_em)
  }
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
#' It is **cache-preserving** - a design of one addresses its shards identically
#' to the standalone run (same `seed=`/`layout=` root via [scenario_results_dir()]
#' and the same `seed`-woven target names), so re-running into the same root
#' **reuses every existing shard** (no recompute); only the per-member and combined
#' `summary` targets are new. Later members are added by extending the
#' `ssd_design(...)` call; the cells they share (within a seed) stay cached.
#'
#' @section Varying the seed:
#' Members may use different `seed`s (e.g. repeating the exploration under several
#' master seeds); they land under separate `seed=` trees and share nothing.
#' Members sharing a `seed` share their coincident cells (common random numbers).
#'
#' @section Per-overlap hc readout aggregation:
#' Members of a seed group MAY differ in the four **non-axis** hc readout settings
#' (`proportion`, `est_method`, `ci`, `samples`) and in their fit `dists` union;
#' only the layout-shaping `nrow_max` and `partition_by` stay uniform-required.
#' Differing readouts are reconciled **per shared hc cell, over only the members
#' whose task set contains that cell** - `proportion`/`est_method` are `union`-ed
#' and `ci`/`samples` reduced with `any()` - so the cell computes the maximal
#' readout set in one shard and each member's summary filters its slice. A cell one
#' member reaches keeps that member's (smaller) demand, so the expensive bootstrap
#' runs only where a `ci = TRUE` member has tasks. The draw-shaping hc axes
#' (`nboot`/`ci_method`/`parametric`/`distset`) are **not** aggregated - they stay
#' cell axes (in the per-task primer), so byte-identity holds: a member's per-task
#' results equal its standalone-run results.
#'
#' Because a `ci = FALSE` task collapses `nboot`/`ci_method`/`parametric` to `NA`,
#' its cell never coincides with a `ci = TRUE` task's. The point `est` is analytical
#' and bootstrap-config-invariant, so a `ci = FALSE` cell is **served by a
#' coincident `ci = TRUE` shard** at the same `(fit, distset)` when one exists (the
#' computed hc shards are every `ci = TRUE` cell plus the `ci = FALSE` cells with no
#' overlapping `ci = TRUE` shard); a `ci = TRUE` member's confidence interval still
#' uses its own cell's `(nboot, ci_method, parametric)` primer. Differing fit
#' `dists` unions are reconciled by fitting the **design-wide union** once per fit
#' cell, each member subsetting via its `distset` axis (distset-subset-invariance),
#' so members differing only in `distset` coverage share every `sample`/`fit` shard.
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
    # The same seed-/layout-keyed root the single-scenario factory uses, so a
    # design-of-one addresses shards identically to a standalone run (cache-free
    # upgrade); members with different seeds land under distinct `seed=` trees.
    sroot <- scenario_results_dir(ref, root)
    group <- design_group_targets(members, ref, sroot, upload, cue)
    shard_targets <- c(shard_targets, group$targets)
    for (nm in names(members)) {
      member_info[[nm]] <- list(
        hc_dir = group$hc_dir,
        hc_names = group$hc_names,
        hc_ids = group$serving[[nm]],
        proportion = group$proportion[[nm]],
        est_method = group$est_method[[nm]],
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
        ssd_summarise_member(
          !!info$hc_dir,
          !!info$hc_ids,
          !!info$summary_path,
          proportion = !!info$proportion,
          est_method = !!info$est_method
        )
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

  # The hc step reconciles members that differ in the four non-axis readouts
  # (`proportion`/`est_method`/`ci`/`samples`) by per-overlap aggregation, so its
  # shard build (and the per-member serving ids / readout filters) come from
  # `design_hc_assembly()` rather than the plain cell-union.
  hc <- design_hc_assembly(members, ref)
  hc_shards <- hc$shards
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
    hc_names = hc_names,
    serving = hc$serving,
    proportion = hc$proportion,
    est_method = hc$est_method
  )
}
