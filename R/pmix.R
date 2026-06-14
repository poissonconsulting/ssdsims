#' Assemble and Validate `min_pmix` Functions for a Simulation Scenario
#'
#' Collects one or more `min_pmix` functions into a validated, named collection -
#' the single entry point through which [ssd_define_scenario()] takes `min_pmix`
#' input. Each entry must be a single-argument **function** (it inputs the number
#' of rows of data and returns a proportion between 0 and 0.5); a name-string is
#' **not** accepted, so the constructor performs no string-to-function resolution.
#'
#' Names are taken from the argument names where supplied, otherwise derived from
#' the argument expression by symbol capture (e.g. `ssdtools::ssd_min_pmix`
#' becomes `"ssd_min_pmix"`), mirroring [ssd_scenario_data()]. Names must be
#' unique across the collection. The name - not the function value - is what the
#' task path hashes; the functions ride on the scenario for execution, isolated
#' by name via [scenario_min_pmix()].
#'
#' @param ... One or more single-argument functions, optionally named.
#' @return An `ssdsims_pmix` object: a named list of validated single-argument
#'   functions.
#' @seealso [ssd_define_scenario()], [scenario_min_pmix()].
#' @export
#' @examples
#' ssd_pmix(ssdtools::ssd_min_pmix)
#' ssd_pmix(ssd_min_pmix = ssdtools::ssd_min_pmix, strict = function(n) 0.1)
ssd_pmix <- function(...) {
  inputs <- rlang::list2(...)
  exprs <- rlang::enexprs(...)
  call <- environment()
  if (length(inputs) == 0L) {
    chk::abort_chk(
      "`ssd_pmix()` requires at least one `min_pmix` function.",
      call = call
    )
  }
  arg_nms <- names(inputs)
  if (is.null(arg_nms)) {
    arg_nms <- rep("", length(inputs))
  }
  values <- vector("list", length(inputs))
  nms <- character(length(inputs))
  # Plain loop (not `purrr`) so validation errors surface as `ssd_pmix()`, not a
  # map frame (error-call-origin rule).
  for (i in seq_along(inputs)) {
    input <- inputs[[i]]
    if (!is.function(input)) {
      chk::abort_chk(
        "Each `ssd_pmix()` entry must be a single-argument function; ",
        "entry ",
        i,
        " is not a function (a name-string is not accepted - ",
        "pass the function itself).",
        call = call
      )
    }
    if (length(formals(input)) != 1L) {
      chk::abort_chk(
        "Each `ssd_pmix()` entry must take a single argument ",
        "(the number of rows); entry ",
        i,
        " does not.",
        call = call
      )
    }
    nm <- arg_nms[[i]]
    if (!nzchar(nm)) {
      nm <- expr_to_name(exprs[[i]])
      if (is.null(nm)) {
        chk::abort_chk(
          "Unable to derive a name for `ssd_pmix()` entry ",
          i,
          "; supply a name (e.g. `ssd_pmix(strict = ...)`).",
          call = call
        )
      }
    }
    values[[i]] <- input
    nms[[i]] <- nm
  }
  if (anyDuplicated(nms)) {
    chk::abort_chk("`ssd_pmix()` names must be unique.", call = call)
  }
  structure(rlang::set_names(values, nms), class = "ssdsims_pmix")
}

#' @export
#' @noRd
print.ssdsims_pmix <- function(x, ...) {
  cat("<ssdsims_pmix>\n")
  for (nm in names(x)) {
    cat("  ", nm, ": <fn>\n", sep = "")
  }
  invisible(x)
}
