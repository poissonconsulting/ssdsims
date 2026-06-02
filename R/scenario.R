#' Define a Simulation Scenario
#'
#' Constructs a purely declarative `ssdsims_scenario` object: the root of
#' the targets-based pipeline (see `TARGETS-DESIGN.md` §1). The object stores
#' only declarative fields - a scalar `seed`, the replicate count `nsim`, the
#' sample sizes `nrow`, the dataset *names*, and the `fit` and `hc` argument
#' grids. It performs **no** random-number generation, **no** task expansion,
#' and has **no** dependency on `targets`.
#'
#' Input data is forwarded through [ssd_data()] for validation (a numeric
#' `Conc` column is required) but the data frames themselves are *not* stored;
#' only their names are kept, so the scenario serialises to a compact manifest.
#'
#' # Dataset input
#'
#' Datasets may be supplied in four forms:
#'
#' 1. A single data frame, name derived from the argument expression:
#'    `ssd_define_scenario(ssddata::ccme_boron, ...)` gives `"ccme_boron"`.
#' 2. A single data frame with an explicit `name=`:
#'    `ssd_define_scenario(ssddata::ccme_boron, name = "boron", ...)`.
#' 3. A named list, names taken from the list:
#'    `ssd_define_scenario(list(boron = ccme_boron, cadmium = ccme_cadmium), ...)`.
#' 4. An unnamed list, names derived per element:
#'    `ssd_define_scenario(list(ccme_boron, ccme_cadmium), ...)`.
#'
#' Supplying both a named list and `name=` is an error.
#'
#' # `ci = FALSE`
#'
#' When `ci = FALSE` is the only confidence-interval value, the bootstrap-only
#' knobs `nboot`, `ci_method`, and `parametric` are meaningless. Passing any of
#' them in that case is an error; set `ci = c(FALSE, TRUE)` to enable bootstrap,
#' or omit the knobs.
#'
#' @inheritParams ssdtools::ssd_fit_dists
#' @inheritParams ssdtools::ssd_hc
#' @inheritParams params
#' @param data A data frame, or a (named or unnamed) list of data frames. Each
#'   is forwarded through [ssd_data()] for validation.
#' @param name An optional dataset name for the single-data-frame form,
#'   overriding the derived name. Must not be combined with a named list.
#' @param min_pmix A list of one or more functions with a single argument that
#'   inputs the number of rows of data and returns a proportion between 0 and
#'   0.5.
#' @param range_shape1 A list of numeric vectors of length two of the lower and
#'   upper bounds for the shape1 parameter.
#' @param range_shape2 A list of numeric vectors of length two of the lower and
#'   upper bounds for the shape2 parameter.
#' @param partition_by An optional named list with `data`, `fit`, and `hc`
#'   character vectors naming the Hive partition axes per step. When `NULL` the
#'   documented per-step defaults are used.
#' @param upload An optional upload specification (a list), or `NULL` for no
#'   upload.
#' @param ... Unused; must be empty.
#' @return An S3 object of class `ssdsims_scenario`.
#' @export
#' @examples
#' ssd_define_scenario(ssddata::ccme_boron, nsim = 100L, nrow = c(5L, 10L), seed = 42L)
ssd_define_scenario <- function(
  data,
  ...,
  name = NULL,
  nsim = 100L,
  nrow = 6L,
  seed = NULL,
  dists = ssdtools::ssd_dists_bcanz(),
  rescale = FALSE,
  computable = FALSE,
  at_boundary_ok = TRUE,
  min_pmix = list(ssdtools::ssd_min_pmix),
  range_shape1 = list(c(0.05, 20)),
  range_shape2 = list(c(0.05, 20)),
  proportion = 0.05,
  ci = FALSE,
  nboot = 1000,
  est_method = "multi",
  ci_method = "weighted_samples",
  parametric = TRUE,
  partition_by = NULL,
  upload = NULL
) {
  data_expr <- rlang::enexpr(data)
  chk::chk_unused(...)

  # --- scalar / vector knob validation ----------------------------------
  chk::chk_whole_number(seed)

  chk::chk_whole_number(nsim)
  chk::chk_gt(nsim)

  chk::chk_whole_numeric(nrow)
  chk::chk_not_any_na(nrow)
  chk::chk_unique(nrow)
  chk::chk_range(nrow, c(5, 1000))
  chk::chk_length(nrow, upper = Inf)

  chk::chk_null_or(name, vld = chk::vld_string)
  chk::chk_null_or(partition_by, vld = chk::vld_list)
  chk::chk_null_or(upload, vld = chk::vld_list)

  # --- fit-grid validation (mirrors ssd_fit_dists_sims) ------------------
  chk::chk_character(dists)
  chk::chk_not_any_na(dists)
  chk::chk_unique(dists)

  chk::chk_logical(rescale)
  chk::chk_not_any_na(rescale)
  chk::chk_unique(rescale)
  chk::chk_length(rescale, upper = 2L)

  chk::chk_logical(computable)
  chk::chk_not_any_na(computable)
  chk::chk_unique(computable)
  chk::chk_length(computable, upper = 2L)

  chk::chk_logical(at_boundary_ok)
  chk::chk_not_any_na(at_boundary_ok)
  chk::chk_unique(at_boundary_ok)
  chk::chk_length(at_boundary_ok, upper = 2L)

  chk::chk_list(min_pmix)
  chk::chk_length(min_pmix, upper = Inf)
  chk::chk_all(min_pmix, chk::chk_function, formals = 1L)
  chk::chk_unique(min_pmix)

  chk::chk_list(range_shape1)
  chk::chk_length(range_shape1, upper = Inf)
  chk::chk_all(range_shape1, chk::chk_double)
  chk::chk_all(range_shape1, chk::chk_length, upper = 2L)
  chk::chk_unique(range_shape1)

  chk::chk_list(range_shape2)
  chk::chk_length(range_shape2, upper = Inf)
  chk::chk_all(range_shape2, chk::chk_double)
  chk::chk_all(range_shape2, chk::chk_length, upper = 2L)
  chk::chk_unique(range_shape2)

  # --- hc-grid validation (mirrors ssd_hc_sims) --------------------------
  chk::chk_numeric(proportion)
  chk::chk_not_any_na(proportion)
  chk::chk_range(proportion, c(0, 1))
  chk::chk_unique(proportion)

  chk::chk_logical(ci)
  chk::chk_not_any_na(ci)
  chk::chk_unique(ci)
  chk::chk_length(ci, upper = 2L)

  chk::chk_whole_numeric(nboot)
  chk::chk_not_any_na(nboot)
  chk::chk_gt(nboot)
  chk::chk_unique(nboot)
  chk::chk_length(nboot, upper = Inf)

  chk::chk_character(est_method)
  chk::chk_not_any_na(est_method)
  chk::chk_unique(est_method)
  chk::chk_subset(est_method, ssdtools::ssd_est_methods())
  chk::chk_length(est_method, upper = Inf)

  chk::chk_character(ci_method)
  chk::chk_not_any_na(ci_method)
  chk::chk_unique(ci_method)
  chk::chk_subset(ci_method, ssdtools::ssd_ci_methods())
  chk::chk_length(ci_method, upper = Inf)

  chk::chk_logical(parametric)
  chk::chk_not_any_na(parametric)
  chk::chk_unique(parametric)
  chk::chk_length(parametric, upper = 2L)

  # --- ci = FALSE rejects bootstrap-only knobs ---------------------------
  if (length(ci) == 1L && isFALSE(ci)) {
    passed <- c(
      if (!missing(nboot)) "nboot",
      if (!missing(ci_method)) "ci_method",
      if (!missing(parametric)) "parametric"
    )
    if (length(passed)) {
      chk::abort_chk(
        "Bootstrap-only knob",
        if (length(passed) > 1L) "s" else "",
        " (",
        chk::cc(passed, conj = " and "),
        ") cannot be set when `ci = FALSE`. ",
        "Set `ci = c(FALSE, TRUE)` to enable bootstrap, or omit the knob",
        if (length(passed) > 1L) "s" else "",
        "."
      )
    }
  }

  # --- dataset names + validation (no data stored) -----------------------
  datasets <- scenario_dataset_names(data, name, data_expr)

  partition_by <- partition_by %||% scenario_default_partition_by()

  structure(
    list(
      seed = as.integer(seed),
      nsim = as.integer(nsim),
      datasets = datasets,
      nrow = as.integer(nrow),
      fit = list(
        dists = dists,
        rescale = rescale,
        computable = computable,
        at_boundary_ok = at_boundary_ok,
        min_pmix = min_pmix,
        range_shape1 = range_shape1,
        range_shape2 = range_shape2
      ),
      hc = list(
        proportion = proportion,
        ci = ci,
        nboot = nboot,
        est_method = est_method,
        ci_method = ci_method,
        parametric = parametric
      ),
      partition_by = partition_by,
      upload = upload
    ),
    class = "ssdsims_scenario"
  )
}

#' Documented per-step `partition_by` defaults.
#' @noRd
scenario_default_partition_by <- function() {
  list(
    data = c("dataset", "sim", "replace"),
    fit = c("dataset", "sim", "rescale"),
    hc = c("dataset", "sim")
  )
}

#' Derive dataset names from input data, validating each via `ssd_data()`.
#'
#' Stores nothing - returns only the character vector of names. The data is
#' forwarded through `ssd_data()` purely for its validation side effect.
#' @noRd
scenario_dataset_names <- function(data, name, data_expr) {
  if (is.data.frame(data)) {
    ssd_data(data)
    if (!is.null(name)) {
      return(name)
    }
    nm <- expr_to_name(data_expr)
    if (is.null(nm)) {
      chk::abort_chk(
        "Unable to derive a dataset name from the data argument; ",
        "supply an explicit `name=`."
      )
    }
    return(nm)
  }

  if (is.list(data)) {
    list_names <- names(data)
    is_named <- !is.null(list_names) && all(nzchar(list_names))
    if (is_named) {
      if (!is.null(name)) {
        chk::abort_chk(
          "`name` must not be supplied with a named list of datasets."
        )
      }
      nms <- list_names
    } else {
      if (!is.null(name)) {
        chk::abort_chk(
          "`name` applies only to a single data frame; ",
          "use a named list for multiple datasets."
        )
      }
      nms <- list_expr_names(data_expr, length(data))
    }
    purrr::walk(data, ssd_data)
    return(nms)
  }

  chk::abort_chk(
    "`data` must be a data frame or a list of data frames."
  )
}

#' Derive a single dataset name from a captured argument expression.
#'
#' Returns `NULL` when no meaningful name can be derived (e.g. a literal
#' `data.frame(...)` call).
#' @noRd
expr_to_name <- function(expr) {
  if (rlang::is_symbol(expr)) {
    return(rlang::as_string(expr))
  }
  if (rlang::is_call(expr, c("::", ":::"))) {
    return(rlang::as_string(expr[[3]]))
  }
  NULL
}

#' Derive per-element names from a captured `list(...)` expression.
#' @noRd
list_expr_names <- function(data_expr, n) {
  if (!rlang::is_call(data_expr, "list")) {
    chk::abort_chk(
      "Unable to derive dataset names from the list argument; ",
      "supply a named list (e.g. `list(boron = ...)`)."
    )
  }
  elems <- rlang::call_args(data_expr)
  nms <- vapply(
    elems,
    function(e) expr_to_name(e) %||% NA_character_,
    character(1),
    USE.NAMES = FALSE
  )
  if (anyNA(nms)) {
    chk::abort_chk(
      "Unable to derive a name for every dataset in the list; ",
      "supply a named list (e.g. `list(boron = ...)`)."
    )
  }
  nms
}

#' Print a Simulation Scenario
#'
#' @param x An `ssdsims_scenario` object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.ssdsims_scenario <- function(x, ...) {
  cat("<ssdsims_scenario>\n")
  cat("  seed:     ", x$seed, "\n", sep = "")
  cat("  datasets: ", paste(x$datasets, collapse = ", "), "\n", sep = "")
  cat("  nsim:     ", x$nsim, "\n", sep = "")
  cat("  nrow:     ", paste(x$nrow, collapse = ", "), "\n", sep = "")
  cat("  fit grid:\n")
  print_grid(x$fit)
  cat("  hc grid:\n")
  print_grid(x$hc)
  invisible(x)
}

#' Print a named list of argument vectors in a snapshot-stable way.
#' @noRd
print_grid <- function(grid) {
  for (nm in names(grid)) {
    cat("    ", nm, ": ", fmt_grid_value(grid[[nm]]), "\n", sep = "")
  }
}

#' Format a grid value without deparsing function bodies (snapshot-stable).
#' @noRd
fmt_grid_value <- function(value) {
  if (is.list(value)) {
    parts <- vapply(
      value,
      function(e) {
        if (is.function(e)) {
          "<fn>"
        } else {
          paste(as.character(e), collapse = ", ")
        }
      },
      character(1)
    )
    return(paste0("{", paste(parts, collapse = "; "), "}"))
  }
  paste(as.character(value), collapse = ", ")
}
