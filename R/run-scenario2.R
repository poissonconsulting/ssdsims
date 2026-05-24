#' Run a Scenario
#'
#' Materializes a scenario (built by [ssd_sim_data2()]) by expanding the
#' full task grid (cross-joining the data-generation,
#' [ssdtools::ssd_fit_dists()] and [ssdtools::ssd_hc()] argument vectors)
#' and computing each task independently. Each task produces one row of
#' the output tibble.
#'
#' Compared to [ssd_run_scenario()] this function:
#' - Uses the precomputed L'Ecuyer-CMRG stream states uniformly for all
#'   generator kinds via [`local_lecuyer_cmrg_state()`].
#' - Propagates the master `seed` to every task by construction; no
#'   recursive call ever drops it.
#' - Derives independent data, fit and hc states per (sim, stream) from
#'   the master seed (via L'Ecuyer-CMRG substreams) rather than reading
#'   the global RNG state at fit/hc time.
#' - Has a single full cross-join over all argument vectors, so output
#'   column order is uniform across generator kinds.
#'
#' @param scenario An `ssdsims_scenario` object built by [ssd_sim_data2()].
#' @param ... Unused.
#' @return A tibble of one row per task with `data`, `fits` and `hc`
#' list-columns.
#' @export
#' @examples
#' scenario <- ssd_sim_data2(ssddata::ccme_boron, nsim = 2)
#' ssd_run_scenario2(scenario)
ssd_run_scenario2 <- function(scenario, ...) {
  chk::chk_is(scenario, "ssdsims_scenario")
  chk::chk_unused(...)

  tasks <- build_tasks(scenario)

  if (!nrow(tasks)) {
    tasks$data <- list()
    tasks$fits <- list()
    tasks$hc <- list()
    return(drop_internal_cols(tasks))
  }

  results <- purrr::map(
    seq_len(nrow(tasks)),
    \(i) run_task(tasks[i, , drop = FALSE], scenario),
    .progress = scenario$progress
  )

  tasks$data <- purrr::map(results, "data")
  tasks$fits <- purrr::map(results, "fits")
  tasks$hc <- purrr::map(results, "hc")

  drop_internal_cols(tasks)
}

internal_cols <- c(
  ".state_data",
  ".state_fit",
  ".state_hc",
  ".gen_kind",
  ".gen_x",
  ".gen_fn",
  ".gen_args"
)

drop_internal_cols <- function(tasks) {
  tasks[, !names(tasks) %in% internal_cols, drop = FALSE]
}

build_tasks <- function(scenario) {
  data_grid <- build_data_grid(scenario)

  fit_grid <- tidyr::expand_grid(
    rescale = scenario$fit$rescale,
    computable = scenario$fit$computable,
    at_boundary_ok = scenario$fit$at_boundary_ok,
    min_pmix = scenario$fit$min_pmix,
    range_shape1 = scenario$fit$range_shape1,
    range_shape2 = scenario$fit$range_shape2
  )

  hc_grid <- tidyr::expand_grid(
    nboot = scenario$hc$nboot,
    est_method = scenario$hc$est_method,
    ci_method = scenario$hc$ci_method,
    parametric = scenario$hc$parametric
  )

  data_grid |>
    dplyr::cross_join(fit_grid) |>
    dplyr::cross_join(hc_grid)
}

build_data_grid <- function(scenario) {
  gen <- scenario$generator
  sim <- scenario$sim

  sims <- sim_seq(sim$start_sim, sim$nsim)
  stream <- as.integer(sim$stream)

  data_states <- get_lecuyer_cmrg_stream_states(
    seed = sim$seed,
    nsim = sim$nsim,
    start_sim = sim$start_sim,
    stream = stream
  )
  fit_states <- lapply(data_states, parallel::nextRNGSubStream)
  hc_states <- lapply(fit_states, parallel::nextRNGSubStream)

  state_tbl <- dplyr::tibble(
    sim = sims,
    stream = stream,
    .state_data = data_states,
    .state_fit = fit_states,
    .state_hc = hc_states
  )

  switch(
    gen$kind,
    "data.frame" = build_data_grid_dataframe(gen, sims, stream, state_tbl),
    "function" = build_data_grid_function(gen, sims, stream, state_tbl),
    "fitdists" = build_data_grid_fitdists(gen, sims, stream, state_tbl)
  )
}

build_data_grid_dataframe <- function(gen, sims, stream, state_tbl) {
  grid <- tidyr::expand_grid(
    sim = sims,
    stream = stream,
    replace = gen$replace,
    nrow = gen$nrow
  )
  grid <- dplyr::left_join(grid, state_tbl, by = c("sim", "stream"))
  grid$.gen_kind <- "data.frame"
  grid$.gen_x <- replicate(nrow(grid), gen$x, simplify = FALSE)
  grid
}

build_data_grid_function <- function(gen, sims, stream, state_tbl) {
  grid <- tidyr::expand_grid(
    sim = sims,
    stream = stream,
    nrow = gen$nrow
  )
  grid <- dplyr::left_join(grid, state_tbl, by = c("sim", "stream"))
  grid$args <- replicate(nrow(grid), gen$args, simplify = FALSE)
  grid$.gen_kind <- "function"
  grid$.gen_fn <- replicate(nrow(grid), gen$fn, simplify = FALSE)
  grid
}

build_data_grid_fitdists <- function(gen, sims, stream, state_tbl) {
  dist_sim <- expand_all_dist_sim(gen$dist_sim, gen$x)
  resolutions <- lapply(dist_sim, \(ds) resolve_dist_sim(ds, gen$x))
  names(resolutions) <- dist_sim

  grid <- tidyr::expand_grid(
    sim = sims,
    stream = stream,
    nrow = gen$nrow,
    dist_sim = dist_sim
  )
  grid <- dplyr::left_join(grid, state_tbl, by = c("sim", "stream"))
  grid$.gen_kind <- "function"
  grid$.gen_fn <- lapply(grid$dist_sim, \(ds) resolutions[[ds]]$fn)
  grid$.gen_args <- lapply(grid$dist_sim, \(ds) resolutions[[ds]]$args)
  grid
}

expand_all_dist_sim <- function(dist_sim, fitdists_obj) {
  if (!"all" %in% dist_sim) {
    return(dist_sim)
  }
  wch <- which(dist_sim == "all")
  n <- length(dist_sim)
  out <- c(
    dist_sim[rlang::seq2(1, wch - 1)],
    names(fitdists_obj),
    dist_sim[rlang::seq2(wch + 1, n)]
  )
  unique(out)
}

resolve_dist_sim <- function(ds, fitdists_obj) {
  if (ds == "multi") {
    return(list(
      fn = ssdtools::ssd_rmulti_fitdists,
      args = list(fitdists = fitdists_obj)
    ))
  }
  if (ds == "top") {
    weight <- ssdtools::glance(fitdists_obj, wt = TRUE)$wt
    ds <- names(fitdists_obj)[which.max(weight)]
  }
  tmb <- fitdists_obj[[ds]]
  list(
    fn = eval(parse(text = paste0("ssdtools::ssd_r", tmb$dist))),
    args = ssdtools::estimates(tmb)
  )
}

run_task <- function(task, scenario) {
  data <- generate_task_data(task)
  fit <- fit_task_dists(task, scenario, data)
  hc <- hc_task(task, scenario, fit)
  list(data = data, fits = fit, hc = hc)
}

generate_task_data <- function(task) {
  local_lecuyer_cmrg_state(task$.state_data[[1]])
  if (task$.gen_kind == "data.frame") {
    dplyr::slice_sample(task$.gen_x[[1]], n = task$nrow, replace = task$replace)
  } else {
    args <- if (".gen_args" %in% names(task)) {
      task$.gen_args[[1]]
    } else {
      task$args[[1]]
    }
    args$n <- task$nrow
    dplyr::tibble(Conc = do.call(task$.gen_fn[[1]], args))
  }
}

fit_task_dists <- function(task, scenario, data) {
  local_lecuyer_cmrg_state(task$.state_fit[[1]])
  fit_args <- c(
    list(
      data = data,
      dists = scenario$fit$dists,
      rescale = task$rescale,
      computable = task$computable,
      at_boundary_ok = task$at_boundary_ok,
      min_pmix = task$min_pmix[[1]](nrow(data)),
      range_shape1 = task$range_shape1[[1]],
      range_shape2 = task$range_shape2[[1]],
      silent = TRUE,
      nrow = 5L
    ),
    scenario$extras$fit
  )
  do.call(ssdtools::ssd_fit_dists, fit_args)
}

hc_task <- function(task, scenario, fit) {
  local_lecuyer_cmrg_state(task$.state_hc[[1]])
  hc_args <- c(
    list(
      x = fit,
      proportion = scenario$hc$proportion,
      ci = scenario$hc$ci,
      nboot = task$nboot,
      est_method = task$est_method,
      ci_method = task$ci_method,
      parametric = task$parametric,
      min_pboot = 0
    ),
    scenario$extras$hc
  )
  hc <- do.call(ssdtools::ssd_hc, hc_args)
  dplyr::select(hc, !dplyr::any_of(c("nboot", "est_method", "ci_method")))
}
