#' Build a Scenario Object
#'
#' Builds an `ssd_scenario` object that fully describes a simulation scenario:
#' a tibble of one job per dataset to generate (each carrying a closure that
#' produces the data when called) together with the parameters required to
#' fit distributions and compute hazard concentrations. The scenario is
#' compact because seeds and recipe information are stored once per
#' generation rather than the materialized data.
#'
#' Pass the result to [`ssd_run_scenario2()`] to execute the scenario.
#'
#' @inheritParams ssdtools::ssd_fit_dists
#' @inheritParams ssdtools::ssd_hc
#' @inheritParams params
#' @param x The object to use for the scenario.
#' @param ... Unused.
#' @return An object of class `ssd_scenario`.
#' @export
ssd_sim_data2 <- function(x, ...) UseMethod("ssd_sim_data2")

#' @describeIn ssd_sim_data2 Scenario built by sampling rows from a data.frame.
#' @export
#' @examples
#' ssd_sim_data2(ssddata::ccme_boron, nsim = 2)
#'
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
  start_sim = 1L
) {
  chk::check_data(
    x,
    values = list(Conc = c(0, Inf, NA_real_)),
    nrow = c(5, 10000)
  )
  chk::chk_unused(...)
  chk_nrow_param(nrow)

  chk::chk_logical(replace)
  chk::chk_not_any_na(replace)
  chk::chk_unique(replace)
  chk::chk_length(replace, upper = 2L)

  chk_fit_params(
    dists = dists,
    rescale = rescale,
    computable = computable,
    at_boundary_ok = at_boundary_ok,
    min_pmix = min_pmix,
    range_shape1 = range_shape1,
    range_shape2 = range_shape2
  )
  chk_hc_params(
    proportion = proportion,
    ci = ci,
    nboot = nboot,
    est_method = est_method,
    ci_method = ci_method,
    parametric = parametric
  )
  chk_stream_param(stream)

  stream <- as.integer(stream)
  sims <- sim_seq(start_sim, nsim)
  state_by_sim <- states_by_sim(seed, nsim, stream, start_sim)

  jobs <- tidyr::expand_grid(
    sim = sims,
    stream = stream,
    replace = replace,
    nrow = nrow
  )
  jobs$gen <- purrr::pmap(
    jobs[c("sim", "replace", "nrow")],
    function(sim, replace, nrow) {
      force(sim)
      force(replace)
      force(nrow)
      function() {
        slice_sample_state(
          x,
          n = nrow,
          replace = replace,
          state = state_by_sim[[as.character(sim)]]
        )
      }
    }
  )

  new_ssd_scenario(
    jobs = jobs,
    fit_params = list(
      dists = dists,
      rescale = rescale,
      computable = computable,
      at_boundary_ok = at_boundary_ok,
      min_pmix = min_pmix,
      range_shape1 = range_shape1,
      range_shape2 = range_shape2
    ),
    hc_params = list(
      proportion = proportion,
      ci = ci,
      nboot = nboot,
      est_method = est_method,
      ci_method = ci_method,
      parametric = parametric
    ),
    seed = seed,
    nsim = nsim,
    stream = stream,
    start_sim = start_sim,
    source_info = list(
      type = "data.frame",
      description = paste0(
        "data.frame [",
        nrow(x),
        " x ",
        ncol(x),
        "]"
      )
    )
  )
}

#' @describeIn ssd_sim_data2 Scenario built from a fitdists object.
#' @export
#' @examples
#' fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron)
#' ssd_sim_data2(fit, dist_sim = c("lnorm", "top"), nsim = 2)
#'
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
  start_sim = 1L
) {
  chk::chk_unused(...)
  chk_nrow_param(nrow)

  chk::chk_character(dist_sim)
  chk::chk_not_any_na(dist_sim)
  chk::chk_unique(dist_sim)
  chk::chk_subset(dist_sim, c("all", "multi", "top", names(x)))
  chk::chk_length(dist_sim, upper = Inf)

  if ("all" %in% dist_sim) {
    wch <- which(dist_sim == "all")
    n <- length(dist_sim)
    dist_sim <- c(
      dist_sim[rlang::seq2(1, wch - 1)],
      names(x),
      dist_sim[rlang::seq2(wch + 1, n)]
    )
    dist_sim <- unique(dist_sim)
  }

  chk_fit_params(
    dists = dists,
    rescale = rescale,
    computable = computable,
    at_boundary_ok = at_boundary_ok,
    min_pmix = min_pmix,
    range_shape1 = range_shape1,
    range_shape2 = range_shape2
  )
  chk_hc_params(
    proportion = proportion,
    ci = ci,
    nboot = nboot,
    est_method = est_method,
    ci_method = ci_method,
    parametric = parametric
  )
  chk_stream_param(stream)

  stream <- as.integer(stream)
  sims <- sim_seq(start_sim, nsim)
  state_by_sim <- states_by_sim(seed, nsim, stream, start_sim)
  resolved <- lapply(dist_sim, resolve_dist_sim, fits = x)
  names(resolved) <- dist_sim

  jobs <- tidyr::expand_grid(
    sim = sims,
    stream = stream,
    nrow = nrow,
    dist_sim = dist_sim
  )
  jobs$gen <- purrr::pmap(
    jobs[c("sim", "nrow", "dist_sim")],
    function(sim, nrow, dist_sim) {
      force(sim)
      force(nrow)
      r <- resolved[[dist_sim]]
      function() {
        argsn <- c(r$args, list(n = nrow))
        dplyr::tibble(
          Conc = do_call_state(
            r$fn,
            args = argsn,
            state = state_by_sim[[as.character(sim)]]
          )
        )
      }
    }
  )

  new_ssd_scenario(
    jobs = jobs,
    fit_params = list(
      dists = dists,
      rescale = rescale,
      computable = computable,
      at_boundary_ok = at_boundary_ok,
      min_pmix = min_pmix,
      range_shape1 = range_shape1,
      range_shape2 = range_shape2
    ),
    hc_params = list(
      proportion = proportion,
      ci = ci,
      nboot = nboot,
      est_method = est_method,
      ci_method = ci_method,
      parametric = parametric
    ),
    seed = seed,
    nsim = nsim,
    stream = stream,
    start_sim = start_sim,
    source_info = list(
      type = "fitdists",
      description = paste0("fitdists: ", paste(names(x), collapse = ", "))
    )
  )
}

#' @describeIn ssd_sim_data2 Scenario built from a tmbfit object.
#' @export
#' @examples
#' fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron)
#' ssd_sim_data2(fit[[1]], nsim = 2)
#'
ssd_sim_data2.tmbfit <- function(x, ...) {
  args <- ssdtools::estimates(x)
  fn_name <- paste0("ssdtools::ssd_r", x$dist)
  scenario <- ssd_sim_data2(fn_name, ..., args = args)
  scenario$source_info <- list(
    type = "tmbfit",
    description = paste0("tmbfit (", x$dist, ")")
  )
  scenario
}

#' @describeIn ssd_sim_data2 Scenario built from the name of a random number function.
#' @export
#' @examples
#' ssd_sim_data2("rlnorm", nsim = 2)
#'
ssd_sim_data2.character <- function(x, ...) {
  chk::chk_string(x)
  fn <- eval(parse(text = x))
  ssd_sim_data2(fn, ...)
}

#' @describeIn ssd_sim_data2 Scenario built from a random number function.
#' @export
#' @examples
#' ssd_sim_data2(ssdtools::ssd_rlnorm, nsim = 2)
#'
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
  start_sim = 1L
) {
  chk::chk_unused(...)
  chk_nrow_param(nrow)
  chk::chk_list(args)

  chk_fit_params(
    dists = dists,
    rescale = rescale,
    computable = computable,
    at_boundary_ok = at_boundary_ok,
    min_pmix = min_pmix,
    range_shape1 = range_shape1,
    range_shape2 = range_shape2
  )
  chk_hc_params(
    proportion = proportion,
    ci = ci,
    nboot = nboot,
    est_method = est_method,
    ci_method = ci_method,
    parametric = parametric
  )
  chk_stream_param(stream)

  stream <- as.integer(stream)
  sims <- sim_seq(start_sim, nsim)
  state_by_sim <- states_by_sim(seed, nsim, stream, start_sim)

  jobs <- tidyr::expand_grid(
    sim = sims,
    stream = stream,
    nrow = nrow
  )
  jobs$args <- list(args)
  jobs$gen <- purrr::pmap(
    jobs[c("sim", "nrow")],
    function(sim, nrow) {
      force(sim)
      force(nrow)
      function() {
        argsn <- args
        argsn$n <- nrow
        dplyr::tibble(
          Conc = do_call_state(
            x,
            args = argsn,
            state = state_by_sim[[as.character(sim)]]
          )
        )
      }
    }
  )

  new_ssd_scenario(
    jobs = jobs,
    fit_params = list(
      dists = dists,
      rescale = rescale,
      computable = computable,
      at_boundary_ok = at_boundary_ok,
      min_pmix = min_pmix,
      range_shape1 = range_shape1,
      range_shape2 = range_shape2
    ),
    hc_params = list(
      proportion = proportion,
      ci = ci,
      nboot = nboot,
      est_method = est_method,
      ci_method = ci_method,
      parametric = parametric
    ),
    seed = seed,
    nsim = nsim,
    stream = stream,
    start_sim = start_sim,
    source_info = list(
      type = "function",
      description = "function"
    )
  )
}

new_ssd_scenario <- function(
  jobs,
  fit_params,
  hc_params,
  seed,
  nsim,
  stream,
  start_sim,
  source_info
) {
  structure(
    list(
      jobs = jobs,
      fit_params = fit_params,
      hc_params = hc_params,
      seed = seed,
      nsim = nsim,
      stream = stream,
      start_sim = start_sim,
      source_info = source_info
    ),
    class = "ssd_scenario"
  )
}

states_by_sim <- function(seed, nsim, stream, start_sim) {
  sims <- sim_seq(start_sim, nsim)
  states <- get_lecuyer_cmrg_stream_states(
    seed = seed,
    nsim = nsim,
    stream = stream,
    start_sim = start_sim
  )
  names(states) <- as.character(sims)
  states
}

resolve_dist_sim <- function(d, fits) {
  if (d == "multi") {
    return(list(
      fn = ssdtools::ssd_rmulti_fitdists,
      args = list(fitdists = fits)
    ))
  }
  wch <- d
  if (d == "top") {
    weight <- ssdtools::glance(fits, wt = TRUE)$wt
    wch <- which.max(weight)
  }
  sub <- fits[[wch]]
  list(
    fn = eval(parse(text = paste0("ssdtools::ssd_r", sub$dist))),
    args = ssdtools::estimates(sub)
  )
}

chk_nrow_param <- function(nrow) {
  chk::chk_whole_numeric(nrow)
  chk::chk_not_any_na(nrow)
  chk::chk_unique(nrow)
  chk::chk_range(nrow, c(5, 1000))
  chk::chk_length(nrow, upper = Inf)
}

chk_stream_param <- function(stream) {
  chk::chk_whole_number(stream)
  chk::chk_gt(stream)
}

chk_fit_params <- function(
  dists,
  rescale,
  computable,
  at_boundary_ok,
  min_pmix,
  range_shape1,
  range_shape2
) {
  chk::chk_logical(rescale)
  chk::chk_not_any_na(rescale)
  chk::chk_unique(rescale)
  chk::chk_length(rescale, upper = 2L)

  chk::chk_logical(computable)
  chk::chk_not_any_na(computable)
  chk::chk_unique(computable)
  chk::chk_length(computable, upper = 2L)

  chk::chk_logical(at_boundary_ok)
  chk::chk_not_any_na(at_boundary_ok)
  chk::chk_unique(at_boundary_ok)
  chk::chk_length(at_boundary_ok, upper = 2L)

  chk::chk_list(min_pmix)
  chk::chk_length(min_pmix, upper = Inf)
  chk::chk_all(min_pmix, chk::chk_function, formals = 1L)
  chk::chk_unique(min_pmix)

  chk::chk_list(range_shape1)
  chk::chk_length(range_shape1, upper = Inf)
  chk::chk_all(range_shape1, chk::chk_double)
  chk::chk_all(range_shape1, chk::chk_length, upper = 2L)
  chk::chk_unique(range_shape1)

  chk::chk_list(range_shape2)
  chk::chk_length(range_shape2, upper = Inf)
  chk::chk_all(range_shape2, chk::chk_double)
  chk::chk_all(range_shape2, chk::chk_length, upper = 2L)
  chk::chk_unique(range_shape2)
}

chk_hc_params <- function(
  proportion,
  ci,
  nboot,
  est_method,
  ci_method,
  parametric
) {
  chk::chk_whole_numeric(nboot)
  chk::chk_not_any_na(nboot)
  chk::chk_gt(nboot)
  chk::chk_unique(nboot)
  chk::chk_length(nboot, upper = Inf)

  chk::chk_character(est_method)
  chk::chk_not_any_na(est_method)
  chk::chk_unique(est_method)
  chk::chk_subset(est_method, ssdtools::ssd_est_methods())
  chk::chk_length(est_method, upper = Inf)

  chk::chk_character(ci_method)
  chk::chk_not_any_na(ci_method)
  chk::chk_unique(ci_method)
  chk::chk_subset(ci_method, ssdtools::ssd_ci_methods())
  chk::chk_length(ci_method, upper = Inf)

  chk::chk_logical(parametric)
  chk::chk_not_any_na(parametric)
  chk::chk_unique(parametric)
  chk::chk_length(parametric, upper = 2L)
}
