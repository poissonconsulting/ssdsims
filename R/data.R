#' Default Cost Calibration Object
#'
#' The `ssdsims_cost_calibration` object shipped with the package and returned by
#' [ssd_cost_calibration()]. It carries the per-`ci_method` cost-model
#' coefficients (`base`, `slope`, `n0`), the bounded `nrow_factor`, a
#' `fixed_addend` (sample + fit per-task overhead), and the provenance of the
#' machine it was fitted on. [ssd_estimate_cost()] uses it when no `calibration`
#' is supplied.
#'
#' Because the coefficients are architecture-specific, this default yields a
#' *ballpark* estimate sized for the machine in its provenance. Re-fit on your own
#' machine with [ssd_calibrate_cost()] for a trustworthy estimate. It was
#' produced by `data-raw/cost_calibration.R` (which simply runs
#' [ssd_calibrate_cost()]).
#'
#' @format An `ssdsims_cost_calibration` object: a list with `coefficients` (a
#'   tibble of `ci_method`, `base`, `slope`, `n0`), `nrow_factor` (a tibble of
#'   `nrow`, `factor`), `fixed_addend` (a scalar), and `provenance` (cpu, R
#'   version, ssdtools version, date, sweep grid).
#' @source Fitted by [ssd_calibrate_cost()] during package development
#'   (Intel Xeon @ 2.10 GHz, R 4.5.3, ssdtools 2.6.0.9002).
#' @seealso [ssd_cost_calibration()], [ssd_calibrate_cost()],
#'   [ssd_estimate_cost()].
"ssd_cost_calibration_default"

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
#' `data.frame(...)` call) must be given an explicit name. Names must be
#' unique across the collection.
#'
#' @details
#' Generator-style inputs (a `fitdists` or `tmbfit` object, a generator
#' function, or a function-name string) enter the collection through
#' [ssd_gen()], which materialises each, once, to a reproducible `Conc`
#' tibble. Its result composes with the data-frame inputs in two equivalent
#' ways: passed as an unnamed argument, the collection is flattened in
#' (each materialised tibble becomes a member under its own name); or spliced
#' with `!!!` ([rlang::list2()] splicing), with identical results:
#'
#' ```r
#' ssd_scenario_data(boron = ccme_boron, ssd_gen(synth = ssd_rlnorm, .n = 30, .seed = 1L))
#' ssd_scenario_data(boron = ccme_boron, !!!ssd_gen(synth = ssd_rlnorm, .n = 30, .seed = 1L))
#' ```
#'
#' A materialised generator dataset is an ordinary tibble in the collection,
#' indistinguishable downstream from a data-frame dataset.
#'
#' @param ... One or more data frames, optionally named, and/or [ssd_gen()]
#'   collections (unnamed, or spliced with `!!!`). Each data frame is
#'   validated for a numeric `Conc` column.
#' @return An `ssdsims_data` object: a named list of validated tibbles.
#' @seealso [ssd_gen()], [ssd_define_scenario()], and the
#'   "Generating and Assembling Datasets" vignette
#'   (`vignette("generating-data", package = "ssdsims")`).
#' @export
#' @examples
#' ssd_scenario_data(ssddata::ccme_boron)
#' ssd_scenario_data(boron = ssddata::ccme_boron, cadmium = ssddata::ccme_cadmium)
ssd_scenario_data <- function(...) {
  inputs <- rlang::list2(...)
  exprs <- rlang::enexprs(...)
  call <- environment()
  if (length(inputs) == 0L) {
    chk::abort_chk(
      "`ssd_scenario_data()` requires at least one dataset.",
      call = call
    )
  }
  arg_nms <- names(inputs)
  if (is.null(arg_nms)) {
    arg_nms <- rep("", length(inputs))
  }
  values <- list()
  nms <- character(0)
  # Plain loop (not `purrr`) so validation errors surface as
  # `ssd_scenario_data()`, not a map frame (error-call-origin rule).
  for (i in seq_along(inputs)) {
    input <- inputs[[i]]
    if (inherits(input, "ssdsims_gen")) {
      # An ssd_gen() collection is flattened in: its members are already
      # validated `Conc` tibbles carrying their own names.
      if (nzchar(arg_nms[[i]])) {
        chk::abort_chk(
          "An `ssd_gen()` collection must be supplied unnamed; ",
          "its members carry their own names.",
          call = call
        )
      }
      values <- c(values, unclass(input))
      nms <- c(nms, names(input))
      next
    }
    nm <- arg_nms[[i]]
    if (!nzchar(nm)) {
      nm <- expr_to_name(exprs[[i]])
      if (is.null(nm)) {
        chk::abort_chk(
          "Unable to derive a name for dataset ",
          i,
          "; supply a name (e.g. `ssd_scenario_data(boron = ...)`).",
          call = call
        )
      }
    }
    values <- c(values, list(ssd_data_validate(input, name = nm, call = call)))
    nms <- c(nms, nm)
  }
  if (anyDuplicated(nms)) {
    chk::abort_chk("Dataset names must be unique.", call = call)
  }
  structure(rlang::set_names(values, nms), class = "ssdsims_data")
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
