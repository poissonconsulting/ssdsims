#' Build a Scenario S3 Object
#'
#' A family of functions to build a rich S3 object that fully describes a
#' simulation scenario. The returned object is compact (no data is generated
#' until [ssd_run_scenario2()] is called) and stores everything required
#' to run the scenario, including the data generator, the
#' [ssdtools::ssd_fit_dists()] arguments and the [ssdtools::ssd_hc()]
#' arguments.
#'
#' Calls to `ssd_sim_data2()` on a `character` or `tmbfit` input are
#' resolved at construction time to their canonical (function, args)
#' form. `data.frame` and `fitdists` inputs are stored as supplied because
#' their materialized data has a different tibble shape.
#'
#' @inheritParams ssdtools::ssd_fit_dists
#' @inheritParams ssdtools::ssd_hc
#' @inheritParams params
#' @param x The object to use for the scenario.
#' @param ... Additional arguments passed to [ssdtools::ssd_fit_dists()] or
#' [ssdtools::ssd_hc()].
#' @return An `ssdsims_scenario` S3 object.
#' @export
ssd_sim_data2 <- function(x, ...) UseMethod("ssd_sim_data2")

#' @describeIn ssd_sim_data2 Build a scenario from a data.frame.
#' @export
#' @examples
#' scenario <- ssd_sim_data2(ssddata::ccme_boron, nsim = 2)
#' scenario
ssd_sim_data2.data.frame <- function(
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
  chk::check_data(
    x,
    values = list(Conc = c(0, Inf, NA_real_)),
    nrow = c(5, 10000)
  )

  chk::chk_whole_numeric(nrow)
  chk::chk_not_any_na(nrow)
  chk::chk_unique(nrow)
  chk::chk_range(nrow, c(5, 1000))
  chk::chk_length(nrow, upper = Inf)

  chk::chk_logical(replace)
  chk::chk_not_any_na(replace)
  chk::chk_unique(replace)
  chk::chk_length(replace, upper = 2L)

  chk::chk_whole_number(stream)
  chk::chk_gt(stream)

  new_ssdsims_scenario(
    generator = list(
      kind = "data.frame",
      x = x,
      nrow = nrow,
      replace = replace
    ),
    extras = list(...),
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

#' @describeIn ssd_sim_data2 Build a scenario from a random number generator function.
#' @export
#' @examples
#' scenario <- ssd_sim_data2(ssdtools::ssd_rlnorm, nsim = 2)
#' scenario
ssd_sim_data2.function <- function(
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
  chk::chk_whole_numeric(nrow)
  chk::chk_not_any_na(nrow)
  chk::chk_unique(nrow)
  chk::chk_range(nrow, c(5, 1000))
  chk::chk_length(nrow, upper = Inf)

  chk::chk_list(args)

  chk::chk_whole_number(stream)
  chk::chk_gt(stream)

  new_ssdsims_scenario(
    generator = list(
      kind = "function",
      fn = x,
      args = args,
      nrow = nrow
    ),
    extras = list(...),
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

#' @describeIn ssd_sim_data2 Build a scenario from a generator name.
#' @export
#' @examples
#' scenario <- ssd_sim_data2("rlnorm", nsim = 2)
#' scenario
ssd_sim_data2.character <- function(x, ...) {
  chk::chk_string(x)
  x <- eval(parse(text = x))
  ssd_sim_data2(x, ...)
}

#' @describeIn ssd_sim_data2 Build a scenario from a tmbfit object.
#' @export
#' @examples
#' fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron)
#' scenario <- ssd_sim_data2(fit[[1]], nsim = 2)
#' scenario
ssd_sim_data2.tmbfit <- function(x, ...) {
  args <- ssdtools::estimates(x)
  fn <- eval(parse(text = paste0("ssdtools::ssd_r", x$dist)))
  ssd_sim_data2(fn, args = args, ...)
}

#' @describeIn ssd_sim_data2 Build a scenario from a fitdists object.
#' @export
#' @examples
#' fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron)
#' scenario <- ssd_sim_data2(fit, dist_sim = c("lnorm", "top"), nsim = 2)
#' scenario
ssd_sim_data2.fitdists <- function(
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
  chk::chk_whole_numeric(nrow)
  chk::chk_not_any_na(nrow)
  chk::chk_unique(nrow)
  chk::chk_range(nrow, c(5, 1000))
  chk::chk_length(nrow, upper = Inf)

  chk::chk_character(dist_sim)
  chk::chk_not_any_na(dist_sim)
  chk::chk_unique(dist_sim)
  chk::chk_subset(dist_sim, c("all", "multi", "top", names(x)))
  chk::chk_length(dist_sim, upper = Inf)

  chk::chk_whole_number(stream)
  chk::chk_gt(stream)

  new_ssdsims_scenario(
    generator = list(
      kind = "fitdists",
      x = x,
      nrow = nrow,
      dist_sim = dist_sim
    ),
    extras = list(...),
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

new_ssdsims_scenario <- function(
  generator,
  extras,
  dists,
  rescale,
  computable,
  at_boundary_ok,
  min_pmix,
  range_shape1,
  range_shape2,
  proportion,
  ci,
  nboot,
  est_method,
  ci_method,
  parametric,
  seed,
  nsim,
  stream,
  start_sim,
  .progress
) {
  extras <- split_extras(extras)

  fit <- list(
    dists = dists,
    rescale = rescale,
    computable = computable,
    at_boundary_ok = at_boundary_ok,
    min_pmix = min_pmix,
    range_shape1 = range_shape1,
    range_shape2 = range_shape2
  )
  hc <- list(
    proportion = proportion,
    ci = ci,
    nboot = nboot,
    est_method = est_method,
    ci_method = ci_method,
    parametric = parametric
  )
  sim <- list(
    seed = seed,
    nsim = nsim,
    stream = stream,
    start_sim = start_sim
  )

  structure(
    list(
      generator = generator,
      fit = fit,
      hc = hc,
      sim = sim,
      extras = extras,
      progress = .progress
    ),
    class = "ssdsims_scenario"
  )
}

split_extras <- function(extras) {
  fit_formals <- methods::formalArgs(ssdtools::ssd_fit_dists)
  hc_formals <- methods::formalArgs(utils::argsAnywhere("ssd_hc.fitdists"))

  fit_extras <- extras[names(extras) %in% fit_formals]
  hc_extras <- extras[names(extras) %in% hc_formals]
  unused <- names(extras)[
    !names(extras) %in% c(fit_formals, hc_formals)
  ]

  n <- length(unused)
  if (n) {
    chk::abort_chk(
      "the following %n argument%s %r unrecognised: ",
      chk::cc(unused),
      n = n
    )
  }
  list(fit = fit_extras, hc = hc_extras)
}

#' @export
print.ssdsims_scenario <- function(x, ...) {
  cat("<ssdsims_scenario>\n")
  cat(
    sprintf(
      "  generator: %s\n  nsim: %d  stream: %d  start_sim: %d\n",
      x$generator$kind,
      x$sim$nsim,
      x$sim$stream,
      x$sim$start_sim
    )
  )
  invisible(x)
}
