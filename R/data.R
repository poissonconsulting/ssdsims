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
#' (see `TARGETS-DESIGN.md` §12) will let each input also be one of the data
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
  if (length(inputs) == 0L) {
    chk::abort_chk("`ssd_data()` requires at least one dataset.")
  }
  names(inputs) <- ssd_data_names(inputs, exprs)
  inputs <- purrr::map(inputs, ssd_data_validate)
  structure(inputs, class = "ssdsims_data")
}

#' Validate a single dataset against the `Conc` contract.
#'
#' Returns the input coerced to a tibble; aborts if it is not a data frame or
#' lacks a numeric `Conc` column.
#' @noRd
ssd_data_validate <- function(data) {
  chk::chk_data(data)
  if (!"Conc" %in% names(data)) {
    chk::abort_chk("Each dataset must have a column named `Conc`.")
  }
  chk::chk_numeric(data$Conc, x_name = "Column `Conc`")
  dplyr::as_tibble(data)
}

#' Derive a unique name vector from `...` values and captured expressions.
#'
#' Uses the argument name where supplied, otherwise symbol capture; aborts when
#' a name cannot be derived or when names collide.
#' @noRd
ssd_data_names <- function(inputs, exprs) {
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
          "; supply a name (e.g. `ssd_data(boron = ...)`)."
        )
      }
      nms[[i]] <- derived
    }
  }
  chk::chk_unique(nms, x_name = "Dataset names")
  nms
}
