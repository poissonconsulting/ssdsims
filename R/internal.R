slice_sample_state <- function(data, n, replace, state) {
  trace_msg(
    "slice_sample_state",
    n = n,
    replace = replace,
    state = state
  )
  with_lecuyer_cmrg_state(state, {
    data |>
      dplyr::slice_sample(n = n, replace = replace)
  })
}

do_call_seed <- function(what, args, seed) {
  trace_msg("do_call_seed", seed = seed)
  with_lecuyer_cmrg_seed(seed, {
    do.call(what, args = args)
  })
}

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
  state <- get_lecuyer_cmrg_stream_state(
    seed = seed,
    start_sim = sim,
    stream = stream
  )
  trace_msg(
    "fit_dists_seed",
    sim = sim,
    stream = stream,
    seed = seed,
    state = state
  )

  ## TODO: handle failure of all model to fit!!
  with_lecuyer_cmrg_state(state, {
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
  state <- get_lecuyer_cmrg_stream_state(
    seed = seed,
    start_sim = sim,
    stream = stream
  )
  trace_msg(
    "hc_seed",
    sim = sim,
    stream = stream,
    seed = seed,
    nboot = nboot,
    ci = ci,
    state = state
  )
  ## TODO: handle failures
  with_lecuyer_cmrg_state(state, {
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
