# Task-list derivation: expand an `ssdsims_scenario` into the three per-step task
# tables (`sample`, `fit`, `hc`) and run them in order with a baseline loop
# runner. This is the data shape later roadmap steps decorate with
# `(seed, primer)` and group into shards (TARGETS-DESIGN.md section 2, section 5, section 12).
# The derivations are RNG-free and there is no per-task RNG seeding anywhere
# here (the baseline runner draws from the ambient RNG only); no `targets`, no
# shards, no Parquet I/O at this step.
#
# The expensive random draw is its own `sample` task, keyed only by
# `(dataset, sim, replace)`; `nrow` is then a `fit` cross-join axis, and the
# `fit` step truncates its parent sample inline (`head(sample, nrow)`, RNG-free)
# before fitting. So the sub-truncation property (TARGETS-DESIGN.md section 5) is
# preserved structurally - one draw shared across every `nrow` - without `nrow`
# ever multiplying the draw, and without a separate materialised `data` step.
#
# Dependencies between tasks are explicit: each row carries a path-style
# `<step>_id` primary key (the Hive partition path, section 5/section 6) plus its parent
# step's id as a foreign key, so a child task references its parent by a single
# joinable column.

#' Expand a Scenario into Task Tables
#'
#' @description
#' The canonical expansion entry point (`TARGETS-DESIGN.md` section 1/section 2):
#' [ssd_scenario_tasks()] derives the `sample`, `fit`, and `hc` task tables from a
#' scenario in one call and bundles them into an `ssdsims_task_set`. The per-step
#' derivations ([ssd_scenario_sample_tasks()], [ssd_scenario_fit_tasks()],
#' [ssd_scenario_hc_tasks()]) remain available for callers that need a single
#' table; each is equivalent to `ssd_scenario_tasks(scenario, step)` for the
#' matching step.
#'
#' All derivations are RNG-free: they perform no random-number generation and add
#' no `seed`/`primer`/`stream` columns (those arrive in later roadmap steps; see
#' `TARGETS-DESIGN.md` section 2). Each row carries a path-style `<step>_id`
#' primary key (the Hive partition path) and, for non-root steps, its parent
#' step's `<parent>_id` as a joinable foreign key, so a child task references its
#' parent by a single column.
#'
#' @param scenario An `ssdsims_scenario` from [ssd_define_scenario()].
#' @param step Optional single step name (`"sample"`, `"fit"`, or `"hc"`). When
#'   supplied, returns just that step's `ssdsims_tasks` table (the same as the
#'   matching `ssd_scenario_*_tasks()`); when `NULL` (default) returns the full
#'   `ssdsims_task_set`.
#' @return An `ssdsims_task_set` object (a list with `sample`, `fit`, and `hc`
#'   elements, each an `ssdsims_tasks` table), or - when `step` is supplied - the
#'   single `ssdsims_tasks` table for that step. Each `ssdsims_tasks` table is a
#'   classed tibble recording one step, with one row per cell of that step's
#'   cross-join.
#' @export
#' @examples
#' data <- ssd_scenario_data(ssddata::ccme_boron)
#' scenario <- ssd_define_scenario(data, nsim = 3L, seed = 42L)
#' tasks <- ssd_scenario_tasks(scenario)
#' tasks
#' tasks$hc
#' ssd_scenario_tasks(scenario, "hc")
ssd_scenario_tasks <- function(scenario, step = NULL) {
  chk::chk_s3_class(scenario, "ssdsims_scenario")
  if (!is.null(step)) {
    chk::chk_string(step)
    chk::chk_subset(step, c("sample", "fit", "hc"))
    return(switch(
      step,
      sample = ssd_scenario_sample_tasks(scenario),
      fit = ssd_scenario_fit_tasks(scenario),
      hc = ssd_scenario_hc_tasks(scenario)
    ))
  }
  structure(
    list(
      sample = ssd_scenario_sample_tasks(scenario),
      fit = ssd_scenario_fit_tasks(scenario),
      hc = ssd_scenario_hc_tasks(scenario)
    ),
    class = "ssdsims_task_set"
  )
}

#' @describeIn ssd_scenario_tasks Derive just the `sample` task table: one row per
#'   cell of the cross-join of the scenario's dataset names, replicate index
#'   (`1:nsim`), and `replace` values, keyed by `sample_id`. Each row is the
#'   single random draw that every `nrow` value sub-truncates
#'   (`TARGETS-DESIGN.md` section 5), so `nrow` is **not** a sample axis - the
#'   draw is shared. The draw size is the scenario's `nrow_max` setting,
#'   resolved by the runner against each dataset, not a row column: the table
#'   carries only the task identity.
#' @export
#' @examples
#' ssd_scenario_sample_tasks(scenario)
ssd_scenario_sample_tasks <- function(scenario) {
  chk::chk_s3_class(scenario, "ssdsims_scenario")
  new_ssdsims_tasks(sample_task_grid(scenario), step = "sample")
}

#' @describeIn ssd_scenario_tasks Derive just the `fit` task table: cross each
#'   sample-task identity (`dataset`, `sim`, `replace`) with the scenario's `nrow`
#'   values and each row of the scenario's `fit` argument grid (`rescale`,
#'   `computable`, `at_boundary_ok`, `min_pmix` name, `range_shape1`,
#'   `range_shape2`). `nrow` is a genuine `fit` cross-join axis: the `fit` step
#'   truncates its parent sample inline (`head(sample, nrow)`, RNG-free) before
#'   fitting, so the shared draw is sub-truncated without a separate `data` step
#'   (`TARGETS-DESIGN.md` section 5). `min_pmix` is referenced by name, not by
#'   function value (`TARGETS-DESIGN.md` section 1.1). Each row carries a `fit_id`
#'   primary key and a `sample_id` foreign key referencing its parent sample task.
#' @export
#' @examples
#' data <- ssd_scenario_data(ssddata::ccme_boron)
#' scenario <- ssd_define_scenario(
#'   data,
#'   nsim = 3L,
#'   seed = 42L,
#'   rescale = c(FALSE, TRUE)
#' )
#' ssd_scenario_fit_tasks(scenario)
ssd_scenario_fit_tasks <- function(scenario) {
  chk::chk_s3_class(scenario, "ssdsims_scenario")
  new_ssdsims_tasks(fit_task_table(scenario), step = "fit")
}

#' @describeIn ssd_scenario_tasks Derive just the `hc` task table: cross each
#'   fit-task identity with each row of the scenario's `hc` argument grid
#'   (`nboot`, `ci_method`, `parametric`) **and with the scenario's declared
#'   distribution sets** (`distset`, the set *names*). The scenario's scalar `ci`
#'   flag and the `est_method` setting are applied uniformly to every hc row -
#'   neither is a cross-join axis nor an emitted column; the runners read `ci`
#'   from the scenario and every requested `est_method` is summarised within each
#'   task from its single bootstrap sample set. When `ci = FALSE` the
#'   bootstrap-only scenario options (`nboot`, `ci_method`, `parametric`) are
#'   canonically `NA`, leaving `distset` as the only fan-out, so the grid is
#'   exactly `D` hc rows per fit task (one per set); when `ci = TRUE` the grid
#'   fans out across `distset x nboot x ci_method x parametric`. A single-set
#'   collection yields one `distset` value (one hc row per fit task when
#'   `ci = FALSE`). Each row carries an `hc_id` primary key, its `distset` name,
#'   and a `fit_id` foreign key referencing its parent (union) fit task.
#' @export
#' @examples
#' data <- ssd_scenario_data(ssddata::ccme_boron)
#' scenario <- ssd_define_scenario(
#'   data,
#'   nsim = 2L,
#'   seed = 42L,
#'   ci = TRUE,
#'   nboot = c(10L, 100L)
#' )
#' ssd_scenario_hc_tasks(scenario)
ssd_scenario_hc_tasks <- function(scenario) {
  chk::chk_s3_class(scenario, "ssdsims_scenario")
  # Cross the fit identity x hc grid x the declared distribution sets (the
  # `distset` axis, its *names*). `distset` is the last hc axis, so each hc row
  # carries its set name (and its parent `fit_id`); a single-set collection has
  # one `distset` value and so does not multiply the table.
  hc_grid <- dplyr::cross_join(
    hc_grid_tbl(scenario),
    tibble::tibble(distset = names(scenario$hc$distsets))
  )
  new_ssdsims_tasks(
    dplyr::cross_join(fit_task_table(scenario), hc_grid),
    step = "hc"
  )
}

#' Run a Scenario with the Baseline Loop Runner
#'
#' Executes the three task tables in dependency order - `sample`, `fit`, then
#' `hc` - by looping over each table with `purrr::pmap()` and looking up each
#' task's parent result by the parent's `<step>_id` foreign key. The `fit` step
#' truncates its parent sample inline (`head(sample, nrow)`) before fitting. The
#' runner does no task expansion of its own (it consumes [ssd_scenario_tasks()]);
#' it just threads outputs forward and returns the collected per-step results.
#'
#' This is the no-frills baseline: it runs in-process, with **no** `targets`
#' dependency, **no** shard grouping or `partition_by`, and **no** Parquet I/O.
#'
#' It **is** reproducible without an external seed. The runner opens one
#' [local_dqrng_backend()] scope and seeds each `sample`/`fit`/`hc` task exactly
#' once through its `*_data_task_primer()` wrapper, with `seed = scenario$seed`
#' and a per-task primer derived from the task's canonical identity
#' ([task_primer()] over the `task_axes(step)` columns). Because each task's
#' `(seed, primer)` pair fully determines its RNG starting point, two runs of a
#' scenario with a fixed `seed` yield identical results, and a task's result is
#' independent of the order in which tasks run. These same
#' `*_data_task_primer()` wrappers are the per-task entry point a future
#' `targets` shard body and the replay helper (`TARGETS-DESIGN.md` §7) reuse.
#'
#' The scenario retains the data frames it was built from, so the runner reads
#' them directly - no separate `data` argument. `min_pmix` names are resolved to
#' their materialised functions off the scenario via [scenario_min_pmix()]
#' (resolved once, at construction), not by a runtime `ssdtools`/global-env
#' search.
#'
#' @inheritParams ssd_scenario_tasks
#' @return A named list with `sample`, `fit`, and `hc` elements: each the
#'   corresponding task table augmented with a list column of per-task results
#'   (`sample` draws, `fits` objects, and `hc` tibbles).
#' @export
#' @examples
#' data <- ssd_scenario_data(ssddata::ccme_boron)
#' scenario <- ssd_define_scenario(
#'   data,
#'   nsim = 1L,
#'   nrow = 6L,
#'   seed = 42L,
#'   dists = ssd_distset(lnorm = "lnorm")
#' )
#' out <- ssd_run_scenario_baseline(scenario)
#' out$hc
ssd_run_scenario_baseline <- function(scenario) {
  chk::chk_s3_class(scenario, "ssdsims_scenario")
  data <- scenario$data
  seed <- scenario$seed

  tasks <- ssd_scenario_tasks(scenario)

  # One backend scope for the whole run; each task installs its own
  # `(seed, primer)` once via its `*_data_task_primer()` wrapper, so results
  # are reproducible and order-independent for a fixed `scenario$seed`.
  local_dqrng_backend()

  # Per-task timing on the fit/hc result tibbles (cost-analysis): the host is
  # constant for the run; `.start`/`.end` bracket each task body so the baseline
  # carries the same observed-cost columns as the sharded runners.
  host <- cost_cpu_info()

  # --- sample step: one seeded draw per (dataset, sim, replace), sized by the
  # scenario's fixed `nrow_max` setting resolved against the dataset (the
  # effective draw size). The primer is keyed by the sample identity only (no
  # `nrow`), so every `nrow` shares the one draw (TARGETS-DESIGN.md section 5).
  sample_tbl <- tasks$sample
  sample_tbl$sample <- purrr::pmap(
    list(
      data = data[sample_tbl$dataset],
      replace = sample_tbl$replace,
      primer = task_primers(sample_tbl, "sample")
    ),
    \(data, replace, primer) {
      sample_data_task_primer(
        data,
        effective_draw_size(scenario$nrow_max, data, replace),
        replace,
        seed,
        primer
      )
    }
  )
  sample_out <- rlang::set_names(sample_tbl$sample, sample_tbl$sample_id)

  # --- fit step: truncate each parent sample to nrow (head, RNG-free), then
  # seed and fit against the fit-grid row. The fit-grid column names match
  # `fit_data_task_primer()`'s formals, so `pmap()` maps them by name; the
  # inline truncation and the per-task primer are just more columns.
  fit_tbl <- tasks$fit
  fit_args <- fit_tbl[c(
    "rescale",
    "computable",
    "at_boundary_ok",
    "min_pmix",
    "range_shape1",
    "range_shape2"
  )]
  fit_args$data <- purrr::map2(
    fit_tbl$sample_id,
    fit_tbl$nrow,
    \(sample_id, nrow) utils::head(sample_out[[sample_id]], nrow)
  )
  fit_args$primer <- task_primers(fit_tbl, "fit")
  fit_timed <- purrr::pmap(fit_args, function(...) {
    t_start <- utc_now()
    fit <- fit_data_task_primer(
      ...,
      scenario = scenario,
      dists = scenario$fit$dists,
      seed = seed
    )
    list(fit = fit, .start = t_start, .end = utc_now())
  })
  fit_tbl$fits <- purrr::map(fit_timed, "fit")
  fit_tbl$.start <- do.call(c, purrr::map(fit_timed, ".start"))
  fit_tbl$.end <- do.call(c, purrr::map(fit_timed, ".end"))
  fit_tbl$.host <- host
  fit_out <- rlang::set_names(fit_tbl$fits, fit_tbl$fit_id)

  # --- hc step: seed then estimate hc for each fit against its hc-grid row ---
  # `ci`, `est_method`, `proportion`, and `samples` are hc *settings* (not axes):
  # read from the scenario and applied uniformly, not pulled from the task row.
  # Only the bootstrap axes (`nboot`/`ci_method`/`parametric`) vary per task.
  hc_tbl <- tasks$hc
  hc_args <- hc_tbl[c("nboot", "ci_method", "parametric")]
  hc_args$fits <- fit_out[hc_tbl$fit_id]
  # Resolve each task's `distset` name to its set members, so the primer subsets
  # the parent union fit to that pool before averaging (the subset-then-average
  # contract; an empty subset yields zero rows for the cell).
  hc_args$dists <- purrr::map(
    hc_tbl$distset,
    \(nm) scenario_distset(scenario, nm)
  )
  hc_args$primer <- task_primers(hc_tbl, "hc")
  hc_timed <- purrr::pmap(hc_args, function(...) {
    t_start <- utc_now()
    hc <- hc_data_task_primer(
      ...,
      ci = scenario$hc$ci,
      proportion = scenario$hc$proportion,
      est_method = scenario$hc$est_method,
      samples = scenario$hc$samples,
      seed = seed
    )
    list(hc = hc, .start = t_start, .end = utc_now())
  })
  hc_tbl$hc <- purrr::map(hc_timed, "hc")
  hc_tbl$.start <- do.call(c, purrr::map(hc_timed, ".start"))
  hc_tbl$.end <- do.call(c, purrr::map(hc_timed, ".end"))
  hc_tbl$.host <- host

  list(sample = sample_tbl, fit = fit_tbl, hc = hc_tbl)
}

# Per-task primers for a task table: hash each row's canonical name-keyed
# identity (the `task_axes(step)` columns) via `task_primer()`. A plain loop
# (not `purrr::map`) keeps internal frames out of any error header
# (CLAUDE.md error-call-origin rule).
task_primers <- function(tbl, step) {
  axes <- task_axes(step)
  n <- nrow(tbl)
  cols <- lapply(axes, function(a) tbl[[a]])
  is_df <- vapply(cols, is.data.frame, logical(1L))
  is_lst <- vapply(
    cols,
    function(v) is.list(v) && !is.data.frame(v),
    logical(1L)
  )
  primers <- vector("list", n)
  for (i in seq_len(n)) {
    row <- vector("list", length(axes))
    names(row) <- axes
    for (j in seq_along(cols)) {
      v <- cols[[j]]
      row[[j]] <- if (is_df[[j]]) {
        v[i, , drop = FALSE]
      } else if (is_lst[[j]]) {
        v[[i]]
      } else {
        v[[i]]
      }
    }
    primers[[i]] <- task_primer(row)
  }
  primers
}

# ---- internal grid helpers -------------------------------------------------

sample_task_grid <- function(scenario) {
  # The single draw every `nrow` sub-truncates is sized by the scenario's
  # `nrow_max` setting, resolved by the runner against each dataset
  # (TARGETS-DESIGN.md section 5) - not stored as a row column - so the table
  # carries only the task identity and `nrow` never multiplies the draw.
  grid <- tidyr::expand_grid(
    dataset = scenario$datasets,
    sim = seq_len(scenario$nsim),
    replace = scenario$replace
  )
  # Silently discard a `(dataset, replace = FALSE)` cell only when *no* `nrow`
  # is feasible for its permutation draw (every `nrow` exceeds
  # `min(nrow_max, nrow(data))`): a single feasible `nrow` keeps the shared
  # draw alive, and `replace = TRUE` cells are always feasible. This mirrors
  # the per-`nrow` discard in `fit_task_table()` so no orphan `sample` draw is
  # produced with no `fit` consumer.
  keep <- grid$replace |
    vapply(
      grid$dataset,
      function(d) any(scenario$nrow <= false_draw_size(scenario, d)),
      logical(1)
    )
  grid[keep, , drop = FALSE]
}

# Per-dataset effective draw size under `replace = FALSE` (the permutation cap,
# `min(nrow_max, nrow(data))`). The single source of truth for which
# `(dataset, replace = FALSE, nrow)` cells are feasible, shared by the
# `sample`/`fit` grid filters and the constructor's empty-grid guard.
false_draw_size <- function(scenario, dataset) {
  effective_draw_size(scenario$nrow_max, scenario$data[[dataset]], FALSE)
}

fit_grid_tbl <- function(scenario) {
  fit <- scenario$fit
  tidyr::expand_grid(
    rescale = fit$rescale,
    computable = fit$computable,
    at_boundary_ok = fit$at_boundary_ok,
    min_pmix = fit$min_pmix,
    range_shape1 = fit$range_shape1,
    range_shape2 = fit$range_shape2
  )
}

fit_task_table <- function(scenario) {
  # `nrow` is a genuine `fit` cross-join axis (the `fit` step truncates its
  # parent sample inline); the draw it slices was already shared at `sample`.
  base <- tidyr::expand_grid(
    dataset = scenario$datasets,
    sim = seq_len(scenario$nsim),
    replace = scenario$replace,
    nrow = scenario$nrow
  )
  # Silently discard the infeasible `replace = FALSE` cells: a permutation draw
  # cannot exceed `min(nrow_max, nrow(data))`, so a larger `nrow` has no valid
  # `replace = FALSE` draw for that dataset. `replace = TRUE` cells (capped at
  # `nrow_max`, validated at construction) and feasible `replace = FALSE` cells
  # are kept. The grid is therefore the cross-join minus these cells - still a
  # deterministic function of the scenario.
  caps <- vapply(
    base$dataset,
    function(d) false_draw_size(scenario, d),
    integer(1)
  )
  base <- base[base$replace | base$nrow <= caps, , drop = FALSE]
  dplyr::cross_join(base, fit_grid_tbl(scenario))
}

hc_grid_tbl <- function(scenario) {
  hc <- scenario$hc
  # Single grid keyed by the scalar `ci` flag (no `c(FALSE, TRUE)` collapse).
  # `ci` is an hc scenario setting, not an axis or an emitted column: the
  # runners read it from the scenario, so it never enters the per-task primer
  # or the task row.
  if (isFALSE(hc$ci)) {
    # Bootstrap-only scenario options are canonically NA when ci = FALSE so they
    # cannot enter task identity; with `est_method` now an hc setting (not an
    # axis), there is no fan-out axis, so this is exactly one hc row per fit
    # task.
    tidyr::expand_grid(
      nboot = NA_integer_,
      ci_method = NA_character_,
      parametric = NA
    )
  } else {
    tidyr::expand_grid(
      nboot = as.integer(hc$nboot),
      ci_method = hc$ci_method,
      parametric = hc$parametric
    )
  }
}

# ---- task identity (cross-join axes, ids, foreign keys) --------------------

# The cumulative cross-join axes per step; each step extends its parent's axes.
# `nrow` enters at the `fit` step (which truncates its parent sample inline),
# not at `sample` (the shared draw).
task_axes <- function(step) {
  sample <- c("dataset", "sim", "replace")
  fit <- c(
    sample,
    "nrow",
    "rescale",
    "computable",
    "at_boundary_ok",
    "min_pmix",
    "range_shape1",
    "range_shape2"
  )
  # `ci` and `est_method` are hc scenario settings, not axes. `ci` is a scalar
  # flag (the point estimate is invariant to it) and `est_method` is summarised
  # within a task from its single bootstrap sample set (the CI is
  # est_method-invariant, the point `est` analytical), so neither carries
  # task-distinguishing information; both are excluded from the hc vocabulary
  # (and thus the primer/partition split) and ride as settings rather than axes.
  #
  # `distset` (the distribution-set *name*) IS an hc axis: per cell, the runner
  # subsets the parent union fit to the named set's members and re-averages, so
  # distinct sets are distinct hc tasks. It hashes by name only (the members ride
  # on the scenario, isolated by `scenario_distset()`), mirroring `min_pmix`.
  hc <- c(fit, "nboot", "ci_method", "parametric", "distset")
  switch(step, sample = sample, fit = fit, hc = hc)
}

# The parent step feeding each step (sample is the root).
task_parent <- function(step) {
  switch(
    step,
    sample = NA_character_,
    fit = "sample",
    hc = "fit"
  )
}

# A path-style key over `cols`, e.g. "dataset=boron/sim=1/replace=FALSE"
# (the Hive partition path, TARGETS-DESIGN.md section 5/section 6); list columns are rendered
# element-wise so the key is stable.
path_key <- function(tbl, cols) {
  parts <- purrr::map(cols, function(col) {
    v <- tbl[[col]]
    rendered <- if (is.list(v)) {
      purrr::map_chr(v, \(e) paste0(as.character(e), collapse = ","))
    } else {
      as.character(v)
    }
    paste0(col, "=", rendered)
  })
  rlang::exec(paste, !!!parts, sep = "/")
}

# Append the `<step>_id` primary key and (for non-root steps) the parent's
# `<parent>_id` as a foreign key. They go last so the cross-join axes lead.
add_task_ids <- function(tbl, step) {
  tbl[[paste0(step, "_id")]] <- path_key(tbl, task_axes(step))
  parent <- task_parent(step)
  if (!is.na(parent)) {
    tbl[[paste0(parent, "_id")]] <- path_key(tbl, task_axes(parent))
  }
  tbl
}

# ---- internal per-task operations (state-less: no RNG seeding) -------------
#
# The dqrng-path per-task primitives come in two layers (primer-primitives,
# TARGETS-DESIGN.md section 2):
#
#   * state-less ops -- `sample_data_task()` / `fit_data_task()` /
#     `hc_data_task()` -- perform their operation against the *ambient* RNG and
#     take no `seed`/`primer`/`state`/`stream` argument.
#   * seed-and-run wrappers -- `*_data_task_primer()` -- install a per-task
#     `(seed, primer)` once via `local_dqrng_state()` (assuming an active
#     `local_dqrng_backend()`), then call the matching op.
#
# These supersede the legacy L'Ecuyer `slice_sample_state()` /
# `fit_dists_state()` / `hc_state()` in `R/internal.R`, which keep their names
# (and back the old `ssd_run_scenario()` path) until `cleanup-lecuyer`. dqrng is
# the path forward; the legacy `*_state` family is removed in `cleanup-lecuyer`.

sample_data_task <- function(data, n_max, replace) {
  dplyr::slice_sample(data, n = n_max, replace = replace)
}

# The effective draw size for one dataset: the scenario's fixed `nrow_max`
# scenario setting, capped at the dataset size for `replace = FALSE` (the
# high default thus draws the full permutation - the maximal no-churn draw);
# `nrow_max` rows for `replace = TRUE`. Resolved at run time by the runners
# (and mirrored by the constructor's `nrow` validation), never stored on a
# task row, so extending `nrow` within the draw size never re-draws.
effective_draw_size <- function(nrow_max, data, replace) {
  if (isTRUE(replace)) {
    return(nrow_max)
  }
  min(nrow_max, nrow(data))
}

# Resolve a `min_pmix` name to its function off the scenario (the materialised
# store), not via a runtime `ssdtools`/global-env search. The functions were
# supplied as an `ssd_pmix()` collection at construction (`scenario_pmix_spec()`),
# so a cluster worker reads the function straight off the transported scenario.
resolve_min_pmix <- function(scenario, name) {
  scenario_min_pmix(scenario, name)
}

fit_data_task <- function(
  data,
  scenario,
  dists,
  rescale,
  computable,
  at_boundary_ok,
  min_pmix,
  range_shape1,
  range_shape2
) {
  min_pmix_fn <- resolve_min_pmix(scenario, min_pmix)
  ssdtools::ssd_fit_dists(
    data,
    dists = dists,
    rescale = rescale,
    computable = computable,
    at_boundary_ok = at_boundary_ok,
    min_pmix = min_pmix_fn(nrow(data)),
    range_shape1 = range_shape1,
    range_shape2 = range_shape2,
    silent = TRUE,
    nrow = 5L
  )
}

hc_data_task <- function(
  fits,
  proportion,
  ci,
  nboot,
  est_method,
  ci_method,
  parametric,
  samples = FALSE
) {
  hc_collapse_est_methods(
    fits = fits,
    proportion = proportion,
    ci = ci,
    nboot = nboot,
    est_method = est_method,
    ci_method = ci_method,
    parametric = parametric,
    samples = samples
  )
}

# Summarise every requested `est_method` from a SINGLE bootstrap per hc cell.
#
# `est_method` is an hc scenario setting, not a bootstrap axis: the bootstrap
# CI (`se`/`lcl`/`ucl`, and any retained `samples`) is est_method-invariant and
# each method's point `est` is analytical and seed-independent (verified in the
# `est-method-setting` change's `exploration/est-method-invariance.R`). So when
# `ci = TRUE` we bootstrap once,
# with the first requested method, to obtain the shared CI (and samples), then
# attach each method's analytical point estimate -- its bootstrap-free
# `ci = FALSE` `est`, byte-identical to that method's `ci = TRUE` `est`. Rows are
# emitted one block per requested `est_method`, in order; when `ci = FALSE` there
# is no bootstrap and each block is the method's analytical estimate. Per method,
# the output reproduces exactly what a single-method `ssdtools::ssd_hc()` call
# seeded the same way would return, while bootstrapping only once.
#
# `samples` retains the *bootstrap* draws, which only exist when `ci = TRUE`;
# without CI there is nothing to keep, and ssdtools' model-averaging cleanup
# errors if asked to retain a non-existent `samples` column, so the no-CI path
# never requests them regardless of the scenario's `samples` flag.
hc_collapse_est_methods <- function(
  fits,
  proportion,
  ci,
  nboot,
  est_method,
  ci_method,
  parametric,
  samples = FALSE,
  ...
) {
  analytical_est <- function(m) {
    ssdtools::ssd_hc(
      fits,
      proportion = proportion,
      ci = FALSE,
      est_method = m,
      samples = FALSE,
      min_pboot = 0,
      ...
    )$est
  }
  if (isTRUE(ci)) {
    boot <- ssdtools::ssd_hc(
      fits,
      proportion = proportion,
      ci = TRUE,
      nboot = nboot,
      est_method = est_method[[1L]],
      ci_method = ci_method,
      parametric = parametric,
      samples = samples,
      min_pboot = 0,
      ...
    )
    rows <- purrr::map(est_method, function(m) {
      out <- boot
      out$est <- analytical_est(m)
      out$est_method <- m
      out
    })
  } else {
    rows <- purrr::map(est_method, function(m) {
      ssdtools::ssd_hc(
        fits,
        proportion = proportion,
        ci = FALSE,
        est_method = m,
        samples = FALSE,
        min_pboot = 0,
        ...
      )
    })
  }
  purrr::list_rbind(rows)
}

# ---- seed-and-run wrappers (dqrng + primer) --------------------------------
#
# Each wrapper installs the per-task `(seed, primer)` exactly once via
# `local_dqrng_state()` -- assuming an already-active `local_dqrng_backend()`
# scope -- then runs the matching state-less op against the now-set ambient
# RNG, leaving the surrounding RNG unchanged beyond that scope. `primer` is
# computed by the caller (`task_primer()` over the task's canonical identity);
# the wrappers are schema-agnostic. `local_dqrng_state()`'s second argument is
# still named `state` (a leftover, renamed in a separate change); it carries
# the primer.

sample_data_task_primer <- function(data, n_max, replace, seed, primer) {
  local_dqrng_state(seed, primer = primer)
  sample_data_task(data, n_max, replace)
}

fit_data_task_primer <- function(
  data,
  scenario,
  dists,
  rescale,
  computable,
  at_boundary_ok,
  min_pmix,
  range_shape1,
  range_shape2,
  seed,
  primer
) {
  local_dqrng_state(seed, primer = primer)
  fit_data_task(
    data,
    scenario = scenario,
    dists = dists,
    rescale = rescale,
    computable = computable,
    at_boundary_ok = at_boundary_ok,
    min_pmix = min_pmix,
    range_shape1 = range_shape1,
    range_shape2 = range_shape2
  )
}

hc_data_task_primer <- function(
  fits,
  proportion,
  ci,
  nboot,
  est_method,
  ci_method,
  parametric,
  seed,
  primer,
  dists = NULL,
  samples = FALSE
) {
  local_dqrng_state(seed, primer = primer)
  # The single chokepoint where a `distset` cell subsets its parent *union* fit
  # down to the set's members and re-averages, rather than re-fitting. Done here
  # (not per runner) so the baseline, shard, and targets paths stay byte-identical
  # by construction. `strict = FALSE` tolerates a member that dropped out of the
  # union fit (boundary/failed); the subset is RNG-free, so seeding before it
  # leaves the bootstrap byte-identical to fitting the set alone (the same-seed
  # subset-reuse oracle, `exploration/distset-subset-invariance.R`).
  if (!is.null(dists)) {
    fits <- subset(fits, select = dists, strict = FALSE)
  }
  # An all-dropped set yields a zero-length subset; emit zero hc rows for the
  # cell (the survivor model) rather than asking `ssd_hc()` to average nothing.
  if (!length(fits)) {
    return(tibble::tibble())
  }
  hc_data_task(
    fits,
    proportion = proportion,
    ci = ci,
    nboot = nboot,
    est_method = est_method,
    ci_method = ci_method,
    parametric = parametric,
    samples = samples
  )
}

# ---- ssdsims_tasks S3 class ------------------------------------------------

new_ssdsims_tasks <- function(tbl, step) {
  tbl <- add_task_ids(tbl, step)
  tibble::new_tibble(
    tbl,
    step = step,
    axes = task_axes(step),
    nrow = nrow(tbl),
    class = "ssdsims_tasks"
  )
}

#' @export
#' @noRd
print.ssdsims_tasks <- function(x, ...) {
  cat(sprintf("<ssdsims_tasks: %s>\n", attr(x, "step")))
  cat("  axes:  ", paste(attr(x, "axes"), collapse = ", "), "\n", sep = "")
  cat("  tasks: ", nrow(x), "\n", sep = "")
  preview <- x
  attr(preview, "step") <- NULL
  attr(preview, "axes") <- NULL
  class(preview) <- setdiff(class(preview), "ssdsims_tasks")
  print(preview, ...)
  invisible(x)
}

#' @export
#' @noRd
print.ssdsims_task_set <- function(x, ...) {
  cat("<ssdsims_task_set>\n")
  for (step in c("sample", "fit", "hc")) {
    cat(sprintf("  %-6s tasks: %d\n", step, nrow(x[[step]])))
  }
  invisible(x)
}
