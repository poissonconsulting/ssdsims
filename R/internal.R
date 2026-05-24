#' Sample Rows from a Data Frame with a Fixed Seed
#'
#' Samples `n` rows from `data` using [dplyr::slice_sample()] under a
#' L'Ecuyer-CMRG seed.
#'
#' The body of the sample call is wrapped in [with_lecuyer_cmrg_seed()] so
#' the RNG state outside the call is unaffected and the result depends only
#' on `seed`. This makes each simulation row of `ssd_sim_data()` independently
#' reproducible.
#'
#' @param data A data frame to sample from.
#' @param n A count of the number of rows to draw.
#' @param replace A flag specifying whether to sample with replacement.
#' @param seed An integer vector that is a valid L'Ecuyer-CMRG seed.
#' @return A data frame with `n` rows.
#' @noRd
slice_sample_seed <- function(data, n, replace, seed) {
  with_lecuyer_cmrg_seed(seed, {
    data |>
      dplyr::slice_sample(n = n, replace = replace)
  })
}

#' Call a Function with a Fixed Seed
#'
#' Invokes [do.call()] on `what` and `args` inside a temporary
#' L'Ecuyer-CMRG seed scope so that the call is reproducible without
#' touching the surrounding RNG state.
#'
#' Used to drive random-number-generating distribution functions (e.g.
#' [ssdtools::ssd_rlnorm()]) for each simulation row of [ssd_sim_data()].
#'
#' @param what A function or function name.
#' @param args A list of arguments to pass to `what`.
#' @param seed An integer vector that is a valid L'Ecuyer-CMRG seed.
#' @return The return value of `do.call(what, args)`.
#' @noRd
do_call_seed <- function(what, args, seed) {
  with_lecuyer_cmrg_seed(seed, {
    do.call(what, args = args)
  })
}

#' Fit Distributions for a Single Simulation
#'
#' Fits the supplied `dists` to one simulated data set using a deterministic
#' L'Ecuyer-CMRG seed derived from `seed`, `sim` and `stream`.
#'
#' The L'Ecuyer-CMRG seed is computed via `get_lecuyer_cmrg_seed_stream()` so
#' that fits across `(stream, sim)` pairs are statistically independent yet
#' fully reproducible. The fit itself is performed by [ssdtools::ssd_fit_dists()].
#' The `min_pmix` argument is a function of `nrow(data)` rather than a value,
#' enabling the minimum mixture proportion to scale with the sample size.
#'
#' @param data A data frame to fit distributions to.
#' @param sim A count of the simulation index.
#' @param stream A count of the stream index.
#' @param seed An integer of the starting seed or `NULL`.
#' @param dists A character vector of distribution names.
#' @param rescale A flag for rescaling the data before fitting.
#' @param computable A flag controlling discarding non-computable fits.
#' @param at_boundary_ok A flag controlling acceptance of boundary fits.
#' @param min_pmix A function returning the minimum mixture proportion.
#' @param range_shape1 A numeric vector of length two of the lower
#' and upper bounds of the shape1 parameter for the burrIII3 distribution.
#' @param range_shape2 A numeric vector of length two of the lower
#' and upper bounds of the shape2 parameter for the burrIII3 distribution.
#' @param silent A flag controlling whether fitting warnings are suppressed.
#' @param ... Additional arguments passed to [ssdtools::ssd_fit_dists()].
#' @return A `fitdists` object.
#' @noRd
fit_dists_seed <- function(
  data,
  sim,
  stream,
  seed,
  dists,
  rescale,
  computable,
  at_boundary_ok,
  min_pmix,
  range_shape1,
  range_shape2,
  silent,
  ...
) {
  seed <- get_lecuyer_cmrg_seed_stream(
    seed = seed,
    start_sim = sim,
    stream = stream
  )

  ## TODO: handle failure of all model to fit!!
  with_lecuyer_cmrg_seed(seed, {
    fit <- ssdtools::ssd_fit_dists(
      data,
      dists = dists,
      rescale = rescale,
      computable = computable,
      at_boundary_ok = at_boundary_ok,
      min_pmix = min_pmix(nrow(data)),
      range_shape1 = range_shape1,
      range_shape2 = range_shape2,
      silent = silent,
      nrow = 5L,
      ...
    )
  })
  fit
}

#' Compute Hazard Concentration for a Single Simulation
#'
#' Calls [ssdtools::ssd_hc()] on a single `fitdists` object using a
#' deterministic L'Ecuyer-CMRG seed derived from `seed`, `sim` and `stream`.
#'
#' Bootstrap sampling is performed inside [with_lecuyer_cmrg_seed()] so the
#' confidence limits are reproducible across parallel workers. The `nboot`,
#' `est_method` and `ci_method` columns are dropped from the returned data
#' frame because they are stored at the row level of the caller's tibble.
#' The minimum bootstrap acceptance proportion is hard-wired to zero so that
#' all simulations always return a value.
#'
#' @param data A `fitdists` object.
#' @param sim A count of the simulation index.
#' @param stream A count of the stream index.
#' @param nboot A count of the number of bootstrap samples.
#' @param est_method A string naming the estimation method.
#' @param ci_method A string naming the confidence interval method.
#' @param seed An integer of the starting seed or `NULL`.
#' @param proportion The hazard concentration proportion(s).
#' @param ci A flag specifying whether to estimate confidence intervals.
#' @param parametric A flag specifying parametric bootstrap.
#' @param save_to A directory path or `NULL`.
#' @param ... Additional arguments passed to [ssdtools::ssd_hc()].
#' @return A data frame of hazard concentration estimates.
#' @noRd
hc_seed <- function(
  data,
  sim,
  stream,
  nboot,
  est_method,
  ci_method,
  seed,
  proportion,
  ci,
  parametric,
  save_to,
  ...
) {
  seed <- get_lecuyer_cmrg_seed_stream(
    seed = seed,
    start_sim = sim,
    stream = stream
  )
  ## TODO: handle failures
  with_lecuyer_cmrg_seed(seed, {
    hc <- ssdtools::ssd_hc(
      data,
      proportion = proportion,
      ci = ci,
      nboot = nboot,
      est_method = est_method,
      ci_method = ci_method,
      parametric = parametric,
      min_pboot = 0,
      ...
    )
  })
  dplyr::select(hc, !c("nboot", "est_method", "ci_method"))
}

#' Fit Distributions and Estimate Hazard Concentrations for a Scenario
#'
#' Internal driver that chains [ssd_fit_dists_sims()] and [ssd_hc_sims()] for
#' the `ssd_run_scenario()` methods.
#'
#' Splits the contents of `...` between arguments that belong to
#' [ssdtools::ssd_fit_dists()] and those that belong to
#' [ssdtools::ssd_hc.fitdists()] using [methods::formalArgs()], aborting with
#' a friendly message if any unrecognised arguments are supplied. The
#' arguments listed explicitly (`dists`, `rescale`, ..., `parametric`) are
#' the simulation-level defaults supplied by each `ssd_run_scenario()` method.
#'
#' @param x A nested tibble of simulated data, as produced by [ssd_sim_data()].
#' @param ... Additional arguments forwarded to the underlying calls.
#' @param dists,rescale,computable,at_boundary_ok,min_pmix,range_shape1,range_shape2
#'   Arguments forwarded to [ssd_fit_dists_sims()].
#' @param proportion,ci,nboot,est_method,ci_method,parametric Arguments
#'   forwarded to [ssd_hc_sims()].
#' @param .progress Whether to show a progress bar.
#' @return A tibble of hazard concentration estimates.
#' @noRd
run_scenario <- function(
  x,
  ...,
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
  .progress = .progress
) {
  .args <- list(...)

  fit_dists_formals <- methods::formalArgs(ssdtools::ssd_fit_dists)
  hc_formals <- methods::formalArgs(utils::argsAnywhere("ssd_hc.fitdists"))

  .args_fit <- .args[names(.args) %in% fit_dists_formals]
  .args_hc <- .args[names(.args) %in% hc_formals]

  .args_unused <- names(.args[
    !names(.args) %in% c(fit_dists_formals, hc_formals)
  ])
  .n <- length(.args_unused)

  if (.n) {
    chk::abort_chk(
      "the following %n argument%s %r unrecognised: ",
      chk::cc(.args_unused),
      n = .n
    )
  }

  .args_fit <- list(
    x = x,
    dists = dists,
    rescale = rescale,
    computable = computable,
    at_boundary_ok = at_boundary_ok,
    min_pmix = min_pmix,
    range_shape1 = range_shape1,
    range_shape2 = range_shape2,
    .progress = .progress
  ) |>
    c(.args_fit)

  x <- do.call(ssd_fit_dists_sims, .args_fit)

  .args_hc <- list(
    x = x,
    proportion = proportion,
    ci = ci,
    nboot = nboot,
    est_method = est_method,
    ci_method = ci_method,
    parametric = parametric,
    .progress = .progress
  ) |>
    c(.args_hc)

  do.call("ssd_hc_sims", .args_hc)
}
