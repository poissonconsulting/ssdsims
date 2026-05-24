#' Print a Scenario
#'
#' Prints a structured summary of an `ssdsims_scenario` object: the data
#' generator (with source-specific details), simulation controls, fit
#' parameters, hazard-concentration parameters and any captured extras.
#'
#' @param x An `ssdsims_scenario` object.
#' @param ... Unused.
#' @return The object, invisibly.
#' @export
print.ssdsims_scenario <- function(x, ...) {
  cat("<ssdsims_scenario>\n")
  print_scenario_generator(x$generator)
  print_scenario_sim(x$sim)
  print_scenario_fit(x$fit)
  print_scenario_hc(x$hc)
  print_scenario_extras(x$extras)
  invisible(x)
}

print_scenario_generator <- function(g) {
  cat(sprintf("* Generator: %s\n", g$kind))
  if (g$kind == "data.frame") {
    cat(sprintf(
      "    source:  data.frame [%d x %d]\n",
      nrow(g$x),
      ncol(g$x)
    ))
    cat(sprintf("    nrow:    %s\n", fmt_scenario_vec(g$nrow)))
    cat(sprintf("    replace: %s\n", fmt_scenario_vec(g$replace)))
  } else if (g$kind == "function") {
    cat(sprintf("    nrow: %s\n", fmt_scenario_vec(g$nrow)))
    cat(sprintf("    args: %s\n", fmt_scenario_args(g$args)))
  } else if (g$kind == "fitdists") {
    cat(sprintf(
      "    source:   fitdists [%s]\n",
      paste(names(g$x), collapse = ", ")
    ))
    cat(sprintf("    nrow:     %s\n", fmt_scenario_vec(g$nrow)))
    cat(sprintf("    dist_sim: %s\n", fmt_scenario_vec(g$dist_sim)))
  }
}

print_scenario_sim <- function(s) {
  cat(sprintf(
    "* Sim: nsim=%d, stream=%d, start_sim=%d, seed=%s\n",
    s$nsim,
    s$stream,
    s$start_sim,
    if (is.null(s$seed)) "NULL" else format(s$seed)
  ))
}

print_scenario_fit <- function(f) {
  cat("* Fit:\n")
  cat(sprintf("    dists:          %s\n", fmt_scenario_vec(f$dists)))
  cat(sprintf("    rescale:        %s\n", fmt_scenario_vec(f$rescale)))
  cat(sprintf("    computable:     %s\n", fmt_scenario_vec(f$computable)))
  cat(sprintf(
    "    at_boundary_ok: %s\n",
    fmt_scenario_vec(f$at_boundary_ok)
  ))
  cat(sprintf(
    "    min_pmix:       %s\n",
    fmt_scenario_list(f$min_pmix)
  ))
  cat(sprintf(
    "    range_shape1:   %s\n",
    fmt_scenario_list(f$range_shape1)
  ))
  cat(sprintf(
    "    range_shape2:   %s\n",
    fmt_scenario_list(f$range_shape2)
  ))
}

print_scenario_hc <- function(h) {
  cat("* HC:\n")
  cat(sprintf("    proportion: %s\n", fmt_scenario_vec(h$proportion)))
  cat(sprintf("    ci:         %s\n", fmt_scenario_vec(h$ci)))
  cat(sprintf("    nboot:      %s\n", fmt_scenario_vec(h$nboot)))
  cat(sprintf("    est_method: %s\n", fmt_scenario_vec(h$est_method)))
  cat(sprintf("    ci_method:  %s\n", fmt_scenario_vec(h$ci_method)))
  cat(sprintf("    parametric: %s\n", fmt_scenario_vec(h$parametric)))
}

print_scenario_extras <- function(extras) {
  fit_extras <- extras$fit
  hc_extras <- extras$hc
  if (length(fit_extras) == 0 && length(hc_extras) == 0) {
    return(invisible())
  }
  cat("* Extras:\n")
  if (length(fit_extras)) {
    cat(sprintf("    fit: %s\n", paste(names(fit_extras), collapse = ", ")))
  }
  if (length(hc_extras)) {
    cat(sprintf("    hc:  %s\n", paste(names(hc_extras), collapse = ", ")))
  }
}

fmt_scenario_vec <- function(x) {
  if (is.null(x)) {
    return("NULL")
  }
  if (is.character(x)) {
    return(paste(x, collapse = ", "))
  }
  paste(format(x), collapse = ", ")
}

fmt_scenario_args <- function(args) {
  if (length(args) == 0) {
    return("<empty>")
  }
  parts <- vapply(
    seq_along(args),
    function(i) {
      nm <- names(args)[i]
      val <- args[[i]]
      if (is.numeric(val) && length(val) == 1) {
        sprintf("%s=%s", nm, format(val))
      } else if (is.atomic(val) && length(val) <= 4) {
        sprintf("%s=[%s]", nm, paste(format(val), collapse = ", "))
      } else {
        sprintf("%s=<%s [%d]>", nm, class(val)[1], length(val))
      }
    },
    character(1)
  )
  paste(parts, collapse = ", ")
}

fmt_scenario_list <- function(x) {
  sprintf("<list of %d>", length(x))
}
