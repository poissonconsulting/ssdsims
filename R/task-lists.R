# Task-list derivation: expand an `ssdsims_scenario` into the four per-step task
# tables (`sample`, `data`, `fit`, `hc`) and run them in order with a baseline
# loop runner. This is the data shape later roadmap steps decorate with
# `(seed, primer)` and group into shards (TARGETS-DESIGN.md §2, §5, §12).
# No RNG, no `targets`, no shards, no Parquet I/O at this step.
#
# The expensive random draw is its own `sample` task, keyed only by
# `(dataset, sim, replace)`; `nrow` then becomes an ordinary cross-join axis of
# the (RNG-free) `data` truncation step, which is just `head(sample, nrow)`. So
# the sub-truncation property (TARGETS-DESIGN.md §5) is preserved structurally -
# one draw shared across every `nrow` - without `nrow` ever multiplying the draw.
#
# Dependencies between tasks are explicit: each row carries a path-style
# `<step>_id` primary key (the Hive partition path, §5/§6) plus its parent
# step's id as a foreign key, so a child task references its parent by a single
# joinable column.

#' Derive the sample Task Table from a Scenario
#'
#' Expands an `ssdsims_scenario` into the `sample` task table: one row per cell
#' of the cross-join of the scenario's dataset names, replicate index (`1:nsim`),
#' and `replace` values. Each row is the single random draw of `n_max =
#' max(nrow)` rows that every `nrow` value sub-truncates (`TARGETS-DESIGN.md`
#' §5), so `nrow` is **not** a sample axis - the draw is shared. `n_max` is
#' carried as an ordinary integer column. The derivation performs no
#' random-number generation and adds no `seed`/`primer`/`stream` columns (those
#' arrive in later roadmap steps; see `TARGETS-DESIGN.md` §2).
#'
#' Each row carries a path-style `sample_id` primary key.
#'
#' @param scenario An `ssdsims_scenario` from [ssd_define_scenario()].
#' @return An `ssdsims_tasks` object (a classed tibble recording the `"sample"`
#'   step) with one row per `(dataset, sim, replace)` cell, a `sample_id` key,
#'   and a carried `n_max` column.
#' @export
#' @examples
#' scenario <- ssd_define_scenario(ssddata::ccme_boron, nsim = 3L, seed = 42L)
#' ssd_scenario_sample_tasks(scenario)
ssd_scenario_sample_tasks <- function(scenario) {
  chk::chk_s3_class(scenario, "ssdsims_scenario")
  new_ssdsims_tasks(sample_task_grid(scenario), step = "sample")
}

#' Derive the data Task Table from a Scenario
#'
#' Crosses each sample-task identity (`dataset`, `sim`, `replace`) with the
#' scenario's `nrow` values: each `data` task is the `head(sample, nrow)`
#' truncation of its parent sample. Unlike the draw, this step is RNG-free, so
#' `nrow` is an ordinary cross-join axis here (one truncation per size) without
#' duplicating the underlying draw (`TARGETS-DESIGN.md` §5).
#'
#' Each row carries a `data_id` primary key and a `sample_id` foreign key
#' referencing its parent sample task.
#'
#' @inheritParams ssd_scenario_sample_tasks
#' @return An `ssdsims_tasks` object recording the `"data"` step, with one row
#'   per `(dataset, sim, replace, nrow)` cell.
#' @export
#' @examples
#' scenario <- ssd_define_scenario(
#'   ssddata::ccme_boron,
#'   nsim = 2L,
#'   nrow = c(5L, 10L),
#'   seed = 42L
#' )
#' ssd_scenario_data_tasks(scenario)
ssd_scenario_data_tasks <- function(scenario) {
  chk::chk_s3_class(scenario, "ssdsims_scenario")
  new_ssdsims_tasks(data_task_grid(scenario), step = "data")
}

#' Derive the fit Task Table from a Scenario
#'
#' Crosses each data-task identity (`dataset`, `sim`, `replace`, `nrow`) with
#' each row of the scenario's `fit` argument grid (`rescale`, `computable`,
#' `at_boundary_ok`, `min_pmix` name, `range_shape1`, `range_shape2`).
#' Parent-identity columns are preserved verbatim so the table can be grouped
#' directly downstream. `min_pmix` is referenced by name, not by function value
#' (`TARGETS-DESIGN.md` §1.1).
#'
#' Each row carries a `fit_id` primary key and a `data_id` foreign key
#' referencing its parent data task.
#'
#' @inheritParams ssd_scenario_sample_tasks
#' @return An `ssdsims_tasks` object recording the `"fit"` step, with one row per
#'   data-task identity crossed with the fit grid.
#' @export
#' @examples
#' scenario <- ssd_define_scenario(
#'   ssddata::ccme_boron,
#'   nsim = 3L,
#'   seed = 42L,
#'   rescale = c(FALSE, TRUE)
#' )
#' ssd_scenario_fit_tasks(scenario)
ssd_scenario_fit_tasks <- function(scenario) {
  chk::chk_s3_class(scenario, "ssdsims_scenario")
  new_ssdsims_tasks(fit_task_table(scenario), step = "fit")
}

#' Derive the hc Task Table from a Scenario
#'
#' Crosses each fit-task identity with each row of the scenario's `hc` argument
#' grid (`nboot`, `est_method`, `ci_method`, `parametric`). The expansion honours
#' the construction-time `ci = FALSE` collapse (`TARGETS-DESIGN.md` §1.2): rows
#' where `ci = FALSE` are not multiplied across the bootstrap-only knobs
#' (`nboot`, `ci_method`, `parametric`), which are stored as `NA`, while
#' `ci = TRUE` rows fan out across the full grid.
#'
#' Each row carries an `hc_id` primary key and a `fit_id` foreign key
#' referencing its parent fit task.
#'
#' @inheritParams ssd_scenario_sample_tasks
#' @return An `ssdsims_tasks` object recording the `"hc"` step, with one row per
#'   fit-task identity crossed with the (collapsed) hc grid.
#' @export
#' @examples
#' scenario <- ssd_define_scenario(
#'   ssddata::ccme_boron,
#'   nsim = 2L,
#'   seed = 42L,
#'   ci = c(FALSE, TRUE),
#'   nboot = c(10L, 100L)
#' )
#' ssd_scenario_hc_tasks(scenario)
ssd_scenario_hc_tasks <- function(scenario) {
  chk::chk_s3_class(scenario, "ssdsims_scenario")
  new_ssdsims_tasks(
    dplyr::cross_join(fit_task_table(scenario), hc_grid_tbl(scenario)),
    step = "hc"
  )
}

#' Expand a Scenario into all Four Task Tables
#'
#' The canonical expansion entry point (`TARGETS-DESIGN.md` §1/§2): derives the
#' `sample`, `data`, `fit`, and `hc` task tables from a scenario in one call and
#' bundles them into an `ssdsims_task_set`. The per-step derivations
#' ([ssd_scenario_sample_tasks()], [ssd_scenario_data_tasks()],
#' [ssd_scenario_fit_tasks()], [ssd_scenario_hc_tasks()]) remain available for
#' callers that need a single table.
#'
#' @inheritParams ssd_scenario_sample_tasks
#' @return An `ssdsims_task_set` object: a list with `sample`, `data`, `fit`, and
#'   `hc` elements, each an `ssdsims_tasks` table.
#' @export
#' @examples
#' scenario <- ssd_define_scenario(ssddata::ccme_boron, nsim = 3L, seed = 42L)
#' tasks <- ssd_scenario_tasks(scenario)
#' tasks
#' tasks$hc
ssd_scenario_tasks <- function(scenario) {
  chk::chk_s3_class(scenario, "ssdsims_scenario")
  structure(
    list(
      sample = ssd_scenario_sample_tasks(scenario),
      data = ssd_scenario_data_tasks(scenario),
      fit = ssd_scenario_fit_tasks(scenario),
      hc = ssd_scenario_hc_tasks(scenario)
    ),
    class = "ssdsims_task_set"
  )
}

#' Run a Scenario with the Baseline Loop Runner
#'
#' Executes the four task tables in dependency order - `sample`, `data`, `fit`,
#' then `hc` - by looping over each table with `purrr::pmap()` and looking up
#' each task's parent result by the parent's `<step>_id` foreign key. The runner
#' does no task expansion of its own (it consumes [ssd_scenario_tasks()]); it
#' just threads outputs forward and returns the collected per-step results.
#'
#' This is the no-frills baseline: it runs in-process, with **no** `targets`
#' dependency, **no** shard grouping or `partition_by`, and **no** Parquet I/O.
#' It is also **not** reproducible - no per-task RNG seeding happens here (that
#' arrives with the `state-primitives` roadmap step); pin the ambient RNG (e.g.
#' [withr::with_seed()]) for deterministic draws.
#'
#' The scenario retains the data frames it was built from, so the runner reads
#' them directly - no separate `data` argument. `min_pmix` names are resolved
#' against `ssdtools` until the registry roadmap step lands.
#'
#' @inheritParams ssd_scenario_sample_tasks
#' @return A named list with `sample`, `data`, `fit`, and `hc` elements: each the
#'   corresponding task table augmented with a list column of per-task results
#'   (`sample` draws, `data` truncations, `fits` objects, and `hc` tibbles).
#' @export
#' @examples
#' scenario <- ssd_define_scenario(
#'   ssddata::ccme_boron,
#'   nsim = 1L,
#'   nrow = 6L,
#'   seed = 42L,
#'   dists = "lnorm"
#' )
#' withr::with_seed(42L, {
#'   out <- ssd_run_scenario_baseline(scenario)
#' })
#' out$hc
ssd_run_scenario_baseline <- function(scenario) {
  chk::chk_s3_class(scenario, "ssdsims_scenario")
  data <- scenario$data

  tasks <- ssd_scenario_tasks(scenario)

  # --- sample step: one draw of n_max rows per (dataset, sim, replace) ---
  sample_tbl <- tasks$sample
  sample_tbl$sample <- purrr::pmap(
    sample_tbl[c("dataset", "n_max", "replace")],
    \(dataset, n_max, replace) {
      dplyr::slice_sample(data[[dataset]], n = n_max, replace = replace)
    }
  )
  sample_out <- rlang::set_names(sample_tbl$sample, sample_tbl$sample_id)

  # --- data step: truncate each sample to its nrow (RNG-free) ------------
  data_tbl <- tasks$data
  data_tbl$data <- purrr::map2(
    data_tbl$sample_id,
    data_tbl$nrow,
    \(sample_id, nrow) utils::head(sample_out[[sample_id]], nrow)
  )
  data_out <- rlang::set_names(data_tbl$data, data_tbl$data_id)

  # --- fit step: fit each data truncation against its fit-grid row -------
  # The fit-grid column names match `fit_data_task()`'s formals, so `pmap()`
  # maps them by name; the resolved parent data is just another column.
  fit_tbl <- tasks$fit
  fit_args <- fit_tbl[c(
    "rescale",
    "computable",
    "at_boundary_ok",
    "min_pmix",
    "range_shape1",
    "range_shape2"
  )]
  fit_args$data <- data_out[fit_tbl$data_id]
  fit_tbl$fits <- purrr::pmap(
    fit_args,
    fit_data_task,
    dists = scenario$fit$dists
  )
  fit_out <- rlang::set_names(fit_tbl$fits, fit_tbl$fit_id)

  # --- hc step: estimate hc for each fit against its hc-grid row ----------
  hc_tbl <- tasks$hc
  hc_args <- hc_tbl[c("ci", "nboot", "est_method", "ci_method", "parametric")]
  hc_args$fits <- fit_out[hc_tbl$fit_id]
  hc_tbl$hc <- purrr::pmap(
    hc_args,
    hc_data_task,
    proportion = scenario$hc$proportion
  )

  list(sample = sample_tbl, data = data_tbl, fit = fit_tbl, hc = hc_tbl)
}

# ---- internal grid helpers -------------------------------------------------

sample_task_grid <- function(scenario) {
  grid <- tidyr::expand_grid(
    dataset = scenario$datasets,
    sim = seq_len(scenario$nsim),
    replace = scenario$replace
  )
  # The single draw is `n_max = max(nrow)` rows; every `nrow` value is a
  # sub-truncation of it (TARGETS-DESIGN.md §5), so `n_max` is carried data, not
  # a cross-join axis, and `nrow` never multiplies the draw.
  grid$n_max <- max(scenario$nrow)
  grid
}

data_task_grid <- function(scenario) {
  # `nrow` is a genuine cross-join axis of the truncation step (one `head()` per
  # size); the draw it slices was already shared at the `sample` step.
  tidyr::expand_grid(
    dataset = scenario$datasets,
    sim = seq_len(scenario$nsim),
    replace = scenario$replace,
    nrow = scenario$nrow
  )
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
  dplyr::cross_join(data_task_grid(scenario), fit_grid_tbl(scenario))
}

hc_grid_tbl <- function(scenario) {
  hc <- scenario$hc
  ci <- hc$ci
  parts <- list()
  if (any(ci == FALSE)) {
    # Bootstrap-only knobs collapse to NA when ci = FALSE (TARGETS-DESIGN.md
    # §1.2); est_method stays an axis as it affects the point estimate too.
    parts$false <- tidyr::expand_grid(
      ci = FALSE,
      nboot = NA_integer_,
      est_method = hc$est_method,
      ci_method = NA_character_,
      parametric = NA
    )
  }
  if (any(ci == TRUE)) {
    parts$true <- tidyr::expand_grid(
      ci = TRUE,
      nboot = as.integer(hc$nboot),
      est_method = hc$est_method,
      ci_method = hc$ci_method,
      parametric = hc$parametric
    )
  }
  dplyr::bind_rows(parts)
}

# ---- task identity (cross-join axes, ids, foreign keys) --------------------

# The cumulative cross-join axes per step; each step extends its parent's axes.
task_axes <- function(step) {
  sample <- c("dataset", "sim", "replace")
  data <- c(sample, "nrow")
  fit <- c(
    data,
    "rescale",
    "computable",
    "at_boundary_ok",
    "min_pmix",
    "range_shape1",
    "range_shape2"
  )
  hc <- c(fit, "ci", "nboot", "est_method", "ci_method", "parametric")
  switch(step, sample = sample, data = data, fit = fit, hc = hc)
}

# The parent step feeding each step (sample is the root).
task_parent <- function(step) {
  switch(
    step,
    sample = NA_character_,
    data = "sample",
    fit = "data",
    hc = "fit"
  )
}

# A path-style key over `cols`, e.g. "dataset=boron/sim=1/replace=FALSE"
# (the Hive partition path, TARGETS-DESIGN.md §5/§6); list columns are rendered
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

# ---- internal per-task operations (no RNG seeding) -------------------------

resolve_min_pmix <- function(name, call = rlang::caller_env()) {
  out <- rlang::env_get(rlang::ns_env("ssdtools"), name, default = NULL)
  if (!rlang::is_function(out)) {
    out <- rlang::env_get(
      rlang::global_env(),
      name,
      default = NULL,
      inherit = TRUE
    )
  }
  if (!rlang::is_function(out)) {
    chk::abort_chk(
      "Unable to resolve `min_pmix` name ",
      encodeString(name, quote = "\""),
      " to a function (no min_pmix registry yet; see TARGETS-DESIGN.md s1.1).",
      call = call
    )
  }
  out
}

fit_data_task <- function(
  data,
  dists,
  rescale,
  computable,
  at_boundary_ok,
  min_pmix,
  range_shape1,
  range_shape2
) {
  min_pmix_fn <- resolve_min_pmix(min_pmix)
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
  parametric
) {
  if (isTRUE(ci)) {
    ssdtools::ssd_hc(
      fits,
      proportion = proportion,
      ci = TRUE,
      nboot = nboot,
      est_method = est_method,
      ci_method = ci_method,
      parametric = parametric,
      min_pboot = 0
    )
  } else {
    ssdtools::ssd_hc(
      fits,
      proportion = proportion,
      ci = FALSE,
      est_method = est_method,
      min_pboot = 0
    )
  }
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
  for (step in c("sample", "data", "fit", "hc")) {
    cat(sprintf("  %-6s tasks: %d\n", step, nrow(x[[step]])))
  }
  invisible(x)
}
