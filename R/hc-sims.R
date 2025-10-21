#' Fit SSD Distributions to Simulated Data
#' 
#' @inheritParams ssdtools::ssd_hc
#' @inheritParams params
#' @param ... Additional arguments passed to `ssdtools::ssd_fit_dists()`.
#' @return The sims tibble with a list column fits of fistdist objects.
#' @export
ssd_hc_sims <- function(sims, proportion = 0.05, ..., seed = NULL, save_to = NULL) {
  chk::check_data(sims, values = list(sim = c(1L, 10000000L), stream = c(1L, 10000000L)), key = c("sim", "stream"))
  chk::check_names(sims, "fits")
  chk::chk_null_or(save_to, vld = chk::vld_dir)

  if(!nrow(sims)) {
    return(dplyr::mutate(sims, hc = list()))
  }
  sims$hc <- purrr::pmap(list(sims$fits, sims$sim, sims$stream), \(.x, .sim, .stream) hc_seed(.x, .sim, .stream, seed = seed, proportion = proportion, save_to = save_to, ...))
  sims
}
