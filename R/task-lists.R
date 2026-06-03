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

#' Derive the sample Task Table from a Scenario
#'
#' Expands an `ssdsims_scenario` into the `sample` task table: one row per cell
#' of the cross-join of the scenario's dataset names, replicate index (`1:nsim`),
#' and `replace` values. Each row is the single random draw of `n_max =
#' max(nrow)` rows that every `nrow` value sub-truncates (`TARGETS-DESIGN.md`
#' section 5), so `nrow` is **not** a sample axis - the draw is shared. `n_max` is
#' carried as an ordinary integer column. The derivation performs no
#' random-number generation and adds no `seed`/`primer`/`stream` columns (those
#' arrive in later roadmap steps; see `TARGETS-DESIGN.md` section 2).
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

#' Derive the fit Task Table from a Scenario
#'
#' Crosses each sample-task identity (`dataset`, `sim`, `replace`) with the
#' scenario's `nrow` values and each row of the scenario's `fit` argument grid
#' (`rescale`, `computable`, `at_boundary_ok`, `min_pmix` name, `range_shape1`,
#' `range_shape2`). `nrow` is a genuine `fit` cross-join axis: the `fit` step
#' truncates its parent sample inline (`head(sample, nrow)`, RNG-free) before
#' fitting, so the shared draw is sub-truncated without a separate `data` step
#' (`TARGETS-DESIGN.md` section 5). Parent-identity columns are preserved
#' verbatim so the table can be grouped directly downstream. `min_pmix` is
#' referenced by name, not by function value (`TARGETS-DESIGN.md` section 1.1).
#'
#' Each row carries a `fit_id` primary key and a `sample_id` foreign key
#' referencing its parent sample task.
#'
#' @inheritParams ssd_scenario_sample_tasks
#' @return An `ssdsims_tasks` object recording the `"fit"` step, with one row per
#'   `(dataset, sim, replace, nrow)` identity crossed with the fit grid.
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
#' the construction-time `ci = FALSE` collapse (`TARGETS-DESIGN.md` section 1.2): rows
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

#' Expand a Scenario into all Three Task Tables
#'
#' The canonical expansion entry point (`TARGETS-DESIGN.md` section 1/section 2): derives the
#' `sample`, `fit`, and `hc` task tables from a scenario in one call and
#' bundles them into an `ssdsims_task_set`. The per-step derivations
#' ([ssd_scenario_sample_tasks()], [ssd_scenario_fit_tasks()],
#' [ssd_scenario_hc_tasks()]) remain available for callers that need a single
#' table.
#'
#' @inheritParams ssd_scenario_sample_tasks
#' @return An `ssdsims_task_set` object: a list with `sample`, `fit`, and
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
      fit = ssd_scenario_fit_tasks(scenario),
      hc = ssd_scenario_hc_tasks(scenario)
    ),
    class = "ssdsims_task_set"
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
#' them directly - no separate `data` argument. `min_pmix` names are resolved
#' against `ssdtools` until the registry roadmap step lands.
#'
#' @inheritParams ssd_scenario_sample_tasks
#' @return A named list with `sample`, `fit`, and `hc` elements: each the
#'   corresponding task table augmented with a list column of per-task results
#'   (`sample` draws, `fits` objects, and `hc` tibbles).
#' @export
#' @examples
#' scenario <- ssd_define_scenario(
#'   ssddata::ccme_boron,
#'   nsim = 1L,
#'   nrow = 6L,
#'   seed = 42L,
#'   dists = "lnorm"
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

  # --- sample step: one seeded draw of n_max rows per (dataset, sim, replace).
  # The primer is keyed by the sample identity only (no `nrow`), so every
  # `nrow` shares the one draw (TARGETS-DESIGN.md section 5).
  sample_tbl <- tasks$sample
  sample_tbl$sample <- purrr::pmap(
    list(
      data = data[sample_tbl$dataset],
      n_max = sample_tbl$n_max,
      replace = sample_tbl$replace,
      primer = task_primers(sample_tbl, "sample")
    ),
    \(data, n_max, replace, primer) {
      sample_data_task_primer(data, n_max, replace, seed, primer)
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
  fit_tbl$fits <- purrr::pmap(
    fit_args,
    fit_data_task_primer,
    dists = scenario$fit$dists,
    seed = seed
  )
  fit_out <- rlang::set_names(fit_tbl$fits, fit_tbl$fit_id)

  # --- hc step: seed then estimate hc for each fit against its hc-grid row ---
  hc_tbl <- tasks$hc
  hc_args <- hc_tbl[c("ci", "nboot", "est_method", "ci_method", "parametric")]
  hc_args$fits <- fit_out[hc_tbl$fit_id]
  hc_args$primer <- task_primers(hc_tbl, "hc")
  hc_tbl$hc <- purrr::pmap(
    hc_args,
    hc_data_task_primer,
    proportion = scenario$hc$proportion,
    seed = seed
  )

  list(sample = sample_tbl, fit = fit_tbl, hc = hc_tbl)
}

# Per-task primers for a task table: hash each row's canonical name-keyed
# identity (the `task_axes(step)` columns) via `task_primer()`. A plain loop
# (not `purrr::map`) keeps internal frames out of any error header
# (CLAUDE.md error-call-origin rule).
task_primers <- function(tbl, step) {
  axes <- task_axes(step)
  primers <- vector("list", nrow(tbl))
  for (i in seq_len(nrow(tbl))) {
    primers[[i]] <- task_primer(tbl[i, axes])
  }
  primers
}

# ---- internal grid helpers -------------------------------------------------

sample_task_grid <- function(scenario) {
  grid <- tidyr::expand_grid(
    dataset = scenario$datasets,
    sim = seq_len(scenario$nsim),
    replace = scenario$replace
  )
  # The single draw is `n_max = max(nrow)` rows; every `nrow` value is a
  # sub-truncation of it (TARGETS-DESIGN.md section 5), so `n_max` is carried data, not
  # a cross-join axis, and `nrow` never multiplies the draw.
  grid$n_max <- max(scenario$nrow)
  grid
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
  dplyr::cross_join(base, fit_grid_tbl(scenario))
}

hc_grid_tbl <- function(scenario) {
  hc <- scenario$hc
  ci <- hc$ci
  parts <- list()
  if (any(ci == FALSE)) {
    # Bootstrap-only knobs collapse to NA when ci = FALSE (TARGETS-DESIGN.md
    # section 1.2); est_method stays an axis as it affects the point estimate too.
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
  hc <- c(fit, "ci", "nboot", "est_method", "ci_method", "parametric")
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
      " to a function; there is no `min_pmix` registry yet.",
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
  local_dqrng_state(seed, state = primer)
  sample_data_task(data, n_max, replace)
}

fit_data_task_primer <- function(
  data,
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
  local_dqrng_state(seed, state = primer)
  fit_data_task(
    data,
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
  primer
) {
  local_dqrng_state(seed, state = primer)
  hc_data_task(
    fits,
    proportion = proportion,
    ci = ci,
    nboot = nboot,
    est_method = est_method,
    ci_method = ci_method,
    parametric = parametric
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
