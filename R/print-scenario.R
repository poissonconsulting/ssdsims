#' Print a Scenario
#'
#' Prints a summary of an `ssd_scenario` object: source description,
#' simulation settings, fit parameters and hazard-concentration parameters.
#'
#' @param x An `ssd_scenario` object.
#' @param ... Unused.
#' @return The object invisibly.
#' @export
print.ssd_scenario <- function(x, ...) {
  cat("<ssd_scenario>\n")
  cat("Source:    ", x$source_info$description, "\n", sep = "")
  cat("Jobs:      ", nrow(x$jobs), "\n", sep = "")
  cat("Simulation:\n")
  cat("  nsim:      ", x$nsim, "\n", sep = "")
  cat("  start_sim: ", x$start_sim, "\n", sep = "")
  cat("  stream:    ", x$stream, "\n", sep = "")
  cat("  seed:      ", format_scalar(x$seed), "\n", sep = "")
  cat("Fit:\n")
  for (nm in names(x$fit_params)) {
    cat("  ", nm, ": ", format_param(x$fit_params[[nm]]), "\n", sep = "")
  }
  cat("HC:\n")
  for (nm in names(x$hc_params)) {
    cat("  ", nm, ": ", format_param(x$hc_params[[nm]]), "\n", sep = "")
  }
  invisible(x)
}

format_scalar <- function(x) {
  if (is.null(x)) "NULL" else format(x)
}

format_param <- function(x) {
  if (is.null(x)) return("NULL")
  if (is.function(x)) return("<function>")
  if (is.list(x)) {
    parts <- vapply(x, format_param, character(1))
    return(paste0("[", paste(parts, collapse = ", "), "]"))
  }
  if (length(x) == 0) return(paste0(typeof(x), "(0)"))
  if (length(x) == 1) return(as.character(x))
  paste0("[", paste(as.character(x), collapse = ", "), "]")
}
