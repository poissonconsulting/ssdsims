
fit_dists_sim <- function(data, seed, i, dists, silent, save_to, return_items, ...) {
  ## TODO: handle failure of all model to fit!!
  fit <- ssdtools::ssd_fit_dists(data, dists = dists, silent = silent, ...)
  if(chk::vld_string(save_to)) {
    saveRDS(fit, filepath(i, save_to, prefix = "fit", ext = ".rds"))
  }
  if(!return_items) {
    return(list())
  }
  fit
}

#' Fit SSD Distributions to Simulated Data
#' 
#' @inheritParams ssdtools::ssd_fit_dists
#' @inheritParams params
#' @return A list of fitdists objects unless `return_items = FALSE`.
#' @export
ssd_fit_dists_sims <- function(datas, dists = ssdtools::ssd_dists_bcanz(), ..., silent = TRUE, save_to = NULL, return_items = TRUE, seed = NULL, stream = 1L, start_seed = 1L) {
  chk::chk_null_or(save_to, vld = chk::vld_dir)
  chk::chk_flag(return_items)
  chk::chk_list(datas)
  if(!chk::vld_all(datas, chk::vld_data)) {
    chk::chk_all(datas, chk::chk_data)
  }
  if(!length(datas)) {
    return(list())
  }

  n <- length(datas)
  seeds <- ssd_get_seeds_streams(seed = seed, nseeds = n, start_stream = stream, start_seed = start_seed)
  seq <- start_seed:(start_seed + n - 1L)
  purrr::pmap(list(datas, seeds, seq), \(.x, .s, .i) fit_dists_sim(.x, .s, .i, dists = dists, silent = silent, save_to = save_to, return_items = return_items, ...))
}
