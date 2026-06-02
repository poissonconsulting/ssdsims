#' Assemble and Validate Datasets for a Simulation Scenario
#'
#' Collects one or more datasets into a validated, named collection - the
#' single entry point through which [ssd_define_scenario()] takes dataset
#' input. Each dataset must carry a numeric `Conc` column (the species
#' sensitivity distribution convention); additional columns are preserved.
#'
#' Names are taken from the argument names where supplied, otherwise derived
#' from the argument expression by symbol capture (e.g. `ssddata::ccme_boron`
#' becomes `"ccme_boron"`). A literal with no derivable name (e.g. a bare
#' `data.frame(...)` call) must be given an explicit name.
#'
#' @details
#' `ssd_data()` is intended to grow: the planned `scenario-input-types` change
#' (see `TARGETS-DESIGN.md` section 12) will let each input also be one of the data
#' *generators* `ssd_run_scenario()` accepts today - a `fitdists` or `tmbfit`
#' object, a generator function, or a function-name string - with the data
#' materialised by the dataset registry. For now each input must be a data
#' frame.
#'
#' @param ... One or more data frames, optionally named. Each is validated for
#'   a numeric `Conc` column.
#' @return An `ssdsims_data` object: a named list of validated tibbles.
#' @export
#' @examples
#' ssd_data(ssddata::ccme_boron)
#' ssd_data(boron = ssddata::ccme_boron, cadmium = ssddata::ccme_cadmium)
ssd_data <- function(...) {
  inputs <- rlang::list2(...)
  exprs <- rlang::enexprs(...)
  call <- environment()
  if (length(inputs) == 0L) {
    chk::abort_chk("`ssd_data()` requires at least one dataset.", call = call)
  }
  nms <- ssd_data_names(inputs, exprs, call = call)
  for (i in seq_along(inputs)) {
    inputs[[i]] <- ssd_data_validate(inputs[[i]], name = nms[[i]], call = call)
  }
  names(inputs) <- nms
  structure(inputs, class = "ssdsims_data")
}

#' Validate a single dataset against the `Conc` contract.
#'
#' Returns the input coerced to a tibble; aborts (in the context of `call`, the
#' user-facing function) if it is not a data frame or lacks a numeric `Conc`
#' column. `name`, when supplied, is woven into the message.
#' @noRd
ssd_data_validate <- function(data, name = NULL, call = rlang::caller_env()) {
  what <- if (is.null(name)) "Each dataset" else paste0("Dataset `", name, "`")
  if (!is.data.frame(data)) {
    chk::abort_chk(what, " must be a data frame.", call = call)
  }
  if (!"Conc" %in% names(data)) {
    chk::abort_chk(what, " must have a column named `Conc`.", call = call)
  }
  if (!is.numeric(data$Conc)) {
    chk::abort_chk(what, " must have a numeric `Conc` column.", call = call)
  }
  dplyr::as_tibble(data)
}

#' Derive a unique name vector from `...` values and captured expressions.
#'
#' Uses the argument name where supplied, otherwise symbol capture; aborts (in
#' the context of `call`) when a name cannot be derived or when names collide.
#' @noRd
ssd_data_names <- function(inputs, exprs, call = rlang::caller_env()) {
  nms <- names(inputs)
  if (is.null(nms)) {
    nms <- rep("", length(inputs))
  }
  for (i in seq_along(inputs)) {
    if (!nzchar(nms[[i]])) {
      derived <- expr_to_name(exprs[[i]])
      if (is.null(derived)) {
        chk::abort_chk(
          "Unable to derive a name for dataset ",
          i,
          "; supply a name (e.g. `ssd_data(boron = ...)`).",
          call = call
        )
      }
      nms[[i]] <- derived
    }
  }
  if (anyDuplicated(nms)) {
    chk::abort_chk("Dataset names must be unique.", call = call)
  }
  nms
}
