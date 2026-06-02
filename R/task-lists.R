# Task-list derivation: expand an `ssdsims_scenario` into the three per-step
# task tables (`data`, `fit`, `hc`) and run them in order with a baseline
# `purrr::pmap()` loop. This is the data shape later roadmap steps decorate
# with `(seed, primer)` and group into shards (TARGETS-DESIGN.md §2, §5, §12).
# No RNG, no `targets`, no shards, no Parquet I/O at this step.

#' Derive the data Task Table from a Scenario
#'
#' Expands an `ssdsims_scenario` into the `data` task table: one row per cell of
#' the cross-join of the scenario's dataset names, replicate index (`1:nsim`),
#' and `replace` values. The derivation performs no random-number generation and
#' adds no `seed`/`primer`/`stream` columns (those arrive in later roadmap steps;
#' see `TARGETS-DESIGN.md` §2).
#'
#' `nrow` is carried as an ordinary (list) column, **not** a cross-join axis:
#' every `nrow` value is a sub-truncation of the same `n_max`-row draw, so the
#' task count is not multiplied by the number of `nrow` values (`TARGETS-DESIGN.md`
#' §5 / the `nrow-sub-truncation` roadmap step).
#'
#' @param scenario An `ssdsims_scenario` from [ssd_define_scenario()].
#' @return An `ssdsims_tasks` object (a classed
#'   tibble recording the `"data"` step) with one row per `(dataset, sim,
#'   replace)` cell and a carried `nrow` column.
#' @export
#' @examples
#' scenario <- ssd_define_scenario(ssddata::ccme_boron, nsim = 3L, seed = 42L)
#' ssd_scenario_data_tasks(scenario)
ssd_scenario_data_tasks <- function(scenario) {
  chk::chk_s3_class(scenario, "ssdsims_scenario")
  new_ssdsims_tasks(
    data_task_grid(scenario),
    step = "data",
    axes = c("dataset", "sim", "replace")
  )
}

#' Derive the fit Task Table from a Scenario
#'
#' Crosses each data-task identity (`dataset`, `sim`, `replace`, plus the carried
#' `nrow`) with each row of the scenario's `fit` argument grid (`rescale`,
#' `computable`, `at_boundary_ok`, `min_pmix` name, `range_shape1`,
#' `range_shape2`). Parent-identity columns are preserved verbatim so the table
#' can be grouped directly downstream. `min_pmix` is referenced by name, not by
#' function value (`TARGETS-DESIGN.md` §1.1).
#'
#' @inheritParams ssd_scenario_data_tasks
#' @return An `ssdsims_tasks` object recording the
#'   `"fit"` step, with one row per data-task identity crossed with the fit grid.
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
  new_ssdsims_tasks(
    fit_task_table(scenario),
    step = "fit",
    axes = c(
      "dataset",
      "sim",
      "replace",
      "rescale",
      "computable",
      "at_boundary_ok",
      "min_pmix",
      "range_shape1",
      "range_shape2"
    )
  )
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
#' @inheritParams ssd_scenario_data_tasks
#' @return An `ssdsims_tasks` object recording the
#'   `"hc"` step, with one row per fit-task identity crossed with the (collapsed)
#'   hc grid.
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
    step = "hc",
    axes = c(
      "dataset",
      "sim",
      "replace",
      "rescale",
      "computable",
      "at_boundary_ok",
      "min_pmix",
      "range_shape1",
      "range_shape2",
      "ci",
      "nboot",
      "est_method",
      "ci_method",
      "parametric"
    )
  )
}

#' Run a Scenario with the Baseline Loop Runner
#'
#' Executes the three task tables in dependency order - `data`, then `fit`, then
#' `hc` - via `purrr::pmap()` loops, threading each step's output into the next
#' and returning the collected per-step results. This is the no-frills baseline:
#' it runs in-process, with **no** `targets` dependency, **no** shard grouping or
#' `partition_by`, and **no** Parquet I/O. It is also **not** reproducible - no
#' per-task RNG seeding happens here (that arrives with the `state-primitives`
#' roadmap step); pin the ambient RNG (e.g. [withr::with_seed()]) for
#' deterministic draws.
#'
#' Because the scenario stores only dataset *names*, the actual data frames are
#' supplied via `data` (an [ssd_data()] collection or named list) and looked up
#' by name. `min_pmix` names are resolved against `ssdtools` until the registry
#' roadmap step lands.
#'
#' @inheritParams ssd_scenario_data_tasks
#' @param data An [ssd_data()] collection, a named list of data frames, or - for
#'   a single-dataset scenario - one data frame, supplying the data referenced by
#'   the scenario's dataset names.
#' @return A named list with `data`, `fit`, and `hc` elements: each the
#'   corresponding task table augmented with a list column of per-task results
#'   (`data` samples, `fits` objects, and `hc` tibbles respectively).
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
#'   out <- ssd_run_scenario_baseline(scenario, ssddata::ccme_boron)
#' })
#' out$hc
ssd_run_scenario_baseline <- function(scenario, data) {
  call <- environment()
  chk::chk_s3_class(scenario, "ssdsims_scenario")
  data <- resolve_scenario_data(data, scenario, call = call)

  # --- data step: one sample per (dataset, sim, replace) task ------------
  data_tbl <- data_task_grid(scenario)
  data_tbl$data <- purrr::pmap(
    list(data_tbl$dataset, data_tbl$nrow, data_tbl$replace),
    \(dataset, nrow, replace) {
      dplyr::slice_sample(data[[dataset]], n = max(nrow), replace = replace)
    }
  )

  # --- fit step: fit each data sample against its fit-grid row -----------
  fit_tbl <- dplyr::cross_join(data_tbl, fit_grid_tbl(scenario))
  fit_tbl$fits <- purrr::pmap(
    list(
      fit_tbl$data,
      fit_tbl$rescale,
      fit_tbl$computable,
      fit_tbl$at_boundary_ok,
      fit_tbl$min_pmix,
      fit_tbl$range_shape1,
      fit_tbl$range_shape2
    ),
    \(
      data,
      rescale,
      computable,
      at_boundary_ok,
      min_pmix,
      range_shape1,
      range_shape2
    ) {
      fit_data_task(
        data,
        dists = scenario$fit$dists,
        rescale = rescale,
        computable = computable,
        at_boundary_ok = at_boundary_ok,
        min_pmix = min_pmix,
        range_shape1 = range_shape1,
        range_shape2 = range_shape2
      )
    }
  )

  # --- hc step: estimate hc for each fit against its hc-grid row ----------
  hc_tbl <- dplyr::cross_join(fit_tbl, hc_grid_tbl(scenario))
  hc_tbl$hc <- purrr::pmap(
    list(
      hc_tbl$fits,
      hc_tbl$ci,
      hc_tbl$nboot,
      hc_tbl$est_method,
      hc_tbl$ci_method,
      hc_tbl$parametric
    ),
    \(fits, ci, nboot, est_method, ci_method, parametric) {
      hc_data_task(
        fits,
        proportion = scenario$hc$proportion,
        ci = ci,
        nboot = nboot,
        est_method = est_method,
        ci_method = ci_method,
        parametric = parametric
      )
    }
  )

  list(data = data_tbl, fit = fit_tbl, hc = hc_tbl)
}

# ---- internal grid helpers -------------------------------------------------

# `replace` is not (yet) a scenario field; default to FALSE and read it
# forward-compatibly so a later scenario `replace` knob flows through unchanged.
scenario_replace <- function(scenario) {
  scenario$replace %||% FALSE
}

data_task_grid <- function(scenario) {
  grid <- tidyr::expand_grid(
    dataset = scenario$datasets,
    sim = seq_len(scenario$nsim),
    replace = scenario_replace(scenario)
  )
  # `nrow` is carried as data, never a cross-join axis (TARGETS-DESIGN.md §5 /
  # nrow-sub-truncation): stored as a list column so multiple `nrow` values do
  # not multiply the task count.
  grid$nrow <- rep(list(scenario$nrow), nrow(grid))
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
  dplyr::bind_rows(parts$false, parts$true)
}

# ---- internal per-task operations (no RNG seeding) -------------------------

resolve_min_pmix <- function(name, call = rlang::caller_env()) {
  out <- tryCatch(getExportedValue("ssdtools", name), error = function(e) NULL)
  if (is.null(out)) {
    out <- tryCatch(get(name, mode = "function"), error = function(e) NULL)
  }
  if (!is.function(out)) {
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

resolve_scenario_data <- function(data, scenario, call = rlang::caller_env()) {
  if (inherits(data, "ssdsims_data")) {
    data <- unclass(data)
  }
  if (is.data.frame(data)) {
    if (length(scenario$datasets) != 1L) {
      chk::abort_chk(
        "A single data frame can only supply a one-dataset scenario; ",
        "supply a named list or an `ssd_data()` collection.",
        call = call
      )
    }
    data <- stats::setNames(list(data), scenario$datasets)
  }
  if (!is.list(data) || is.null(names(data)) || !all(nzchar(names(data)))) {
    chk::abort_chk(
      "`data` must be an `ssd_data()` collection or a named list of data frames.",
      call = call
    )
  }
  missing <- setdiff(scenario$datasets, names(data))
  if (length(missing)) {
    chk::abort_chk(
      "`data` is missing dataset",
      if (length(missing) > 1L) "s" else "",
      ": ",
      chk::cc(missing),
      ".",
      call = call
    )
  }
  data
}

# ---- ssdsims_tasks S3 class ------------------------------------------------

new_ssdsims_tasks <- function(tbl, step, axes) {
  structure(
    tbl,
    step = step,
    axes = axes,
    class = c("ssdsims_tasks", class(tbl))
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
