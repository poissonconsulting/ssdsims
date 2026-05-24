#' Run a Scenario
#'
#' Executes an `ssd_scenario` object by materializing each job's data,
#' fitting the requested distributions and computing hazard concentrations.
#' The runner contains no logic beyond iteration: it maps each job's
#' generator closure to produce one dataset, then hands the results to
#' [`ssd_fit_dists_sims()`] and [`ssd_hc_sims()`].
#'
#' @param x An `ssd_scenario` object produced by [`ssd_sim_data2()`].
#' @inheritParams params
#' @return A tibble with the data, fits and hazard concentrations for every
#' job (same structure as the output of [`ssd_run_scenario()`]).
#' @export
#' @examples
#' scenario <- ssd_sim_data2(ssddata::ccme_boron, nsim = 2)
#' ssd_run_scenario2(scenario)
#'
ssd_run_scenario2 <- function(x, .progress = FALSE) {
  chk::chk_is(x, "ssd_scenario")

  data <- materialize_jobs(x, .progress = .progress)

  fits <- ssd_fit_dists_sims(
    data,
    dists = x$fit_params$dists,
    rescale = x$fit_params$rescale,
    computable = x$fit_params$computable,
    at_boundary_ok = x$fit_params$at_boundary_ok,
    min_pmix = x$fit_params$min_pmix,
    range_shape1 = x$fit_params$range_shape1,
    range_shape2 = x$fit_params$range_shape2,
    seed = x$seed,
    .progress = .progress
  )

  ssd_hc_sims(
    fits,
    proportion = x$hc_params$proportion,
    ci = x$hc_params$ci,
    nboot = x$hc_params$nboot,
    est_method = x$hc_params$est_method,
    ci_method = x$hc_params$ci_method,
    parametric = x$hc_params$parametric,
    seed = x$seed,
    .progress = .progress
  )
}

materialize_jobs <- function(x, .progress = FALSE) {
  data <- x$jobs
  data$data <- purrr::map(data$gen, function(gen) gen(), .progress = .progress)
  data$gen <- NULL
  data
}
