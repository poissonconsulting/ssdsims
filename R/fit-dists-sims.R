
fit_dists_sim <- function(data, i, dists, silent, save_to, return_items, ...) {
  ## TODO: handle failure of all model to fit!!
  fit <- ssdtools::ssd_fit_dists(data, dists = dists, silent = silent, ...)
  if(chk::vld_string(save_to)) {
    saveRDS(fit, filepath(i, save_to, prefix = "fit", ext = ".rds"))
  }
  if(return_items) {
    return(fit)
  }
  NULL
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
  
  ## TODO: local_set_seed_stream(seed = seed, start_seed = start_seed, start_stream = stream)
  seq <- seq_len(length(datas))
  purrr::map2(datas, seq, \(.x, .i) fit_dists_sim(.x, .i, dists = dists, silent = silent, save_to = save_to, return_items = return_items, ...))
}
