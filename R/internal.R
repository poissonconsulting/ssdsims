filename <- function(i, prefix, ext = NULL, sep = "_") {
  paste0(prefix, sep, stringr::str_pad(i, width = 9, pad = "0"), sep, ext)
}

filepath <- function(i, save_to, prefix = "fit", ext = ".rds") {
  file.path(save_to, filename(i, prefix = prefix, ext = ext))
}

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

fit_dists_seed <- function(data, seed, i, dists, silent, save_to, ...) {
  ## TODO: handle failure of all model to fit!!
  with_lecuyer_cmrg_seed(seed, {
    fit <- ssdtools::ssd_fit_dists(data, dists = dists, silent = silent, ...)
  })
  if(chk::vld_dir(save_to)) {
    saveRDS(fit, filepath(i, save_to, prefix = "fit", ext = ".rds"))
  }
  fit
}
