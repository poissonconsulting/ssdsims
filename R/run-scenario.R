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

  limits = c(
    replace = 2,
    dists = 20,
    rescale = 2,
    computable = 2,
    at_boundary_ok = 2,
    min_pmix = 2,
    range_shape1 = 2,
    range_shape2 = 2,
    ci = 2,
    nboot = 10,
    est_method = 4,
    ci_method = 10,
    parametric = 2
  ),

  # 1: solved with head()
  nrow = c(6L, 20L, 100L, 200L, 500L),
  max_nrow = 1000L, # want to specify up front to avoid relying on implementation details of slice_sample(). Alternative: use implementation of slice_sample() that we control, relying on sample.int() only.
  # 2
  replace = FALSE,
  # 20
  dists = ssdtools::ssd_dists_bcanz(),
  # 2
  rescale = c(FALSE, TRUE),
  # 2
  computable = FALSE,
  # 2
  at_boundary_ok = TRUE,
  # 2
  min_pmix = list(min_pmix = ssdtools::ssd_min_pmix),
  # 2
  range_shape1 = list(c(0.05, 20)),
  # 2
  range_shape2 = list(c(0.05, 20)),
  proportion = 0.05,
  # 2: ci = FALSE means nboot, ci_method and parametric are ignored, ci = TRUE means they are not
  ci = FALSE,
  # 10: reusing the value for 1000 to compute 10000 is a lot of effort for very limited gain; inferring the value for 1000 from the value for 10000 might be possible; out of scope for now.
  nboot = c(20, 50, 100, 1000, 10000),
  # 4
  est_method = "multi",
  # 10
  ci_method = "weighted_samples",
  # 2
  parametric = TRUE,
  seed = NULL,
  # nsim
  nsim = 100L,
  stream = getOption("ssdsims.stream", 1L),
  start_sim = 1L,
  .progress = FALSE
) {
  data <- ssd_sim_data(
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
  data <- ssd_sim_data(
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
  data <- ssd_sim_data(
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
