#' Run Scenario
#'
#' @inheritParams ssdtools::ssd_fit_dists
#' @inheritParams ssdtools::ssd_hc
#' @inheritParams params
#' @param x The object to use for the scenario.
#' @param ... Unused.
#' @return A tibble of nested data sets.
#' @export
ssd_run_scenario <- function(x, ...) UseMethod("ssd_run_scenario")

#' @describeIn ssd_run_scenario Run scenario using data.frame to sample data
#' @export
#' @examples
#' ssd_run_scenario(ssddata::ccme_boron, nsim = 2)
#'
ssd_run_scenario.data.frame <- function(
  x,
  ...,
  nrow = 6L,
  replace = FALSE,
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
  seed = NULL,
  nsim = 100L,
  stream = getOption("ssdsims.stream", 1L),
  start_sim = 1L,
  .progress = FALSE
) {
  data <- ssd_simulate_data(
    x,
    nrow = nrow,
    replace = replace,
    seed = seed,
    nsim = nsim,
    stream = stream,
    start_sim = start_sim,
    .progress = .progress
  )

  run_scenario(
    x = data,
    ...,
    dists = dists,
    rescale = rescale,
    computable = computable,
    at_boundary_ok = at_boundary_ok,
    min_pmix = min_pmix,
    range_shape1 = range_shape1,
    range_shape2 = range_shape2,
    proportion = proportion,
    ci = ci,
    nboot = nboot,
    est_method = est_method,
    ci_method = ci_method,
    parametric = parametric,
    .progress = .progress
  )
}

#' @describeIn ssd_run_scenario Run scenario using fitdists object to generate data
#' @export
#' @examples
#' fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron)
#' ssd_run_scenario(fit, dist_sim = c("lnorm", "top"), nsim = 3)
#'
ssd_run_scenario.fitdists <- function(
  x,
  ...,
  nrow = 6L,
  dist_sim = "top",
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
  seed = NULL,
  nsim = 100L,
  stream = getOption("ssdsims.stream", 1L),
  start_sim = 1L,
  .progress = FALSE
) {
  data <- ssd_simulate_data(
    x,
    nrow = nrow,
    dist_sim = dist_sim,
    seed = seed,
    nsim = nsim,
    stream = stream,
    start_sim = start_sim,
    .progress = .progress
  )

  run_scenario(
    x = data,
    ...,
    dists = dists,
    rescale = rescale,
    computable = computable,
    at_boundary_ok = at_boundary_ok,
    min_pmix = min_pmix,
    range_shape1 = range_shape1,
    range_shape2 = range_shape2,
    proportion = proportion,
    ci = ci,
    nboot = nboot,
    est_method = est_method,
    ci_method = ci_method,
    parametric = parametric,
    .progress = .progress
  )
}

#' @describeIn ssd_run_scenario Run scenario using tmbfit object to generate data
#' @export
#' @examples
#' fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron)
#' ssd_run_scenario(fit[[1]], nsim = 3)
#'
ssd_run_scenario.tmbfit <- function(
  x,
  ...,
  nrow = 6L,
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
  seed = NULL,
  nsim = 100L,
  stream = getOption("ssdsims.stream", 1L),
  start_sim = 1L,
  .progress = FALSE
) {
  args <- ssdtools::estimates(x)
  x <- paste0("ssdtools::ssd_r", x$dist)

  ssd_run_scenario(
    x,
    ...,
    nrow = nrow,
    dists = dists,
    rescale = rescale,
    computable = computable,
    at_boundary_ok = at_boundary_ok,
    min_pmix = min_pmix,
    range_shape1 = range_shape1,
    range_shape2 = range_shape2,
    proportion = proportion,
    ci = ci,
    nboot = nboot,
    est_method = est_method,
    ci_method = ci_method,
    parametric = parametric,
    seed = seed,
    nsim = nsim,
    stream = stream,
    start_sim = start_sim,
    .progress = .progress
  )
}

#' @describeIn ssd_run_scenario Run scenario using name of function to generate sequence of random numbers
#' @export
#' @examples
#' ssd_run_scenario("rlnorm", nsim = 3)
#'
ssd_run_scenario.character <- function(
  x,
  ...,
  nrow = 6L,
  args = list(),
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
  seed = NULL,
  nsim = 100L,
  stream = getOption("ssdsims.stream", 1L),
  start_sim = 1L,
  .progress = FALSE
) {
  chk::chk_string(x)

  x <- eval(parse(text = x))

  ssd_run_scenario(
    x,
    ...,
    args = args,
    nrow = nrow,
    dists = dists,
    rescale = rescale,
    computable = computable,
    at_boundary_ok = at_boundary_ok,
    min_pmix = min_pmix,
    range_shape1 = range_shape1,
    range_shape2 = range_shape2,
    proportion = proportion,
    ci = ci,
    nboot = nboot,
    est_method = est_method,
    ci_method = ci_method,
    parametric = parametric,
    seed = seed,
    nsim = nsim,
    stream = stream,
    start_sim = start_sim,
    .progress = .progress
  )
}


#' @describeIn ssd_run_scenario Run scenario data using function to generate sequence of random numbers
#' @export
#' @examples
#' ssd_run_scenario(ssdtools::ssd_rlnorm, nsim = 3)
#'
ssd_run_scenario.function <- function(
  x,
  ...,
  nrow = 6L,
  args = list(),
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
  seed = NULL,
  nsim = 100L,
  stream = getOption("ssdsims.stream", 1L),
  start_sim = 1L,
  .progress = FALSE
) {
  data <- ssd_simulate_data(
    x,
    nrow = nrow,
    args = args,
    seed = seed,
    nsim = nsim,
    stream = stream,
    start_sim = start_sim,
    .progress = .progress
  )

  run_scenario(
    x = data,
    ...,
    dists = dists,
    rescale = rescale,
    computable = computable,
    at_boundary_ok = at_boundary_ok,
    min_pmix = min_pmix,
    range_shape1 = range_shape1,
    range_shape2 = range_shape2,
    proportion = proportion,
    ci = ci,
    nboot = nboot,
    est_method = est_method,
    ci_method = ci_method,
    parametric = parametric,
    .progress = .progress
  )
}
