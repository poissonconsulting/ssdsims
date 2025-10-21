#' Fit SSD Distributions to Simulated Data
#' 
#' @inheritParams ssdtools::ssd_fit_dists
#' @inheritParams params
#' @param ... Additional arguments passed to `ssdtools::ssd_fit_dists()`.
#' @return The sims tibble with a list column fits of fistdist objects.
#' @export
ssd_fit_dists_sims <- function(sims, dists = ssdtools::ssd_dists_bcanz(), ..., seed = NULL, silent = TRUE, save_to = NULL) {
  chk::check_data(sims, values = list(sim = c(1L, 10000000L), stream = c(1L, 10000000L)), key = c("sim", "stream"))
  chk::check_names(sims, "data")
  chk::chk_flag(silent)
  chk::chk_null_or(save_to, vld = chk::vld_dir)

  if(!nrow(sims)) {
    return(dplyr::mutate(sims, dists = list()))
  }
  sims$fits <- purrr::pmap(list(sims$data, sims$sim, sims$stream), \(.x, .sim, .stream) fit_dists_seed(.x, .sim, .stream, seed = seed, dists = dists, silent = silent, save_to = save_to, ...))
  sims
}
