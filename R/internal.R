filename <- function(stream, sim, prefix, ext = NULL, sep = "_") {
  stream <- stringr::str_pad(stream, width = 9, pad = "0") 
  sim <- stringr::str_pad(sim, width = 9, pad = "0")
  paste(prefix, stream, sim, sep = sep) |>
    paste0(ext)
}

filepath <- function(stream, sim, dir, prefix = "fit", ext = ".rds") {
  file.path(dir, filename(stream, sim, prefix = prefix, ext = ext))
}

slice_sample_seed <- function(data, n, replace, seed) {
  print(seed)
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

fit_dists_seed <- function(data, sim, stream, seed, dists, silent, save_to, ...) {
   seed <- get_lecuyer_cmrg_seed_stream(seed = seed, start_sim = sim, stream = stream)
  ## TODO: handle failure of all model to fit!!
  with_lecuyer_cmrg_seed(seed, {
    fit <- ssdtools::ssd_fit_dists(data, dists = dists, silent = silent, ...)
  })
  if(!is.null(save_to)) {
    saveRDS(fit, filepath(stream, sim, dir = save_to, prefix = "fit", ext = ".rds"))
  }
  fit
}

hc_seed <- function(data, sim, stream, seed, proportion, save_to, ...) {
  seed <- get_lecuyer_cmrg_seed_stream(seed = seed, start_sim = sim, stream = stream)
  ## TODO: handle failure of all model to fit!!
  with_lecuyer_cmrg_seed(seed, {
    fit <- ssdtools::ssd_hc(data, proportion = proportion, ...)
  })
  # work out what to do with save_to
  # if(!is.null(save_to)) {
  #   saveRDS(fit, filepath(stream, sim, dir = save_to, prefix = "fit", ext = ".rds"))
  # }
  fit 
}
