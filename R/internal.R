slice_sample_seed <- function(data, n, replace, seed) {
  with_lecuyer_cmrg_seed(seed, {
    data |>
      dplyr::slice_sample(n = n, replace = replace)
  })
}

do_call_seed <- function(what, args, seed) {
  with_lecuyer_cmrg_seed(seed, {
    do.call(what, args = args)
  })
}

fit_dists_seed <- function(data, sim, stream, seed, dists, silent, ...) {
   seed <- get_lecuyer_cmrg_seed_stream(seed = seed, start_sim = sim, stream = stream)
  ## TODO: handle failure of all model to fit!!
  with_lecuyer_cmrg_seed(seed, {
    fit <- ssdtools::ssd_fit_dists(data, dists = dists, silent = silent, nrow = 5L, ...)
  })
  fit
}

hc_seed <- function(data, sim, stream, seed, proportion, ci, save_to, ...) {
  seed <- get_lecuyer_cmrg_seed_stream(seed = seed, start_sim = sim, stream = stream)
  ## TODO: handle failures
  with_lecuyer_cmrg_seed(seed, {
    fit <- ssdtools::ssd_hc(data, proportion = proportion, ci = ci, ...)
  })
  fit 
}
