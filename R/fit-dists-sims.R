#' Fit SSD Distributions to Simulated Data
#' 
#' @inheritParams ssdtools::ssd_fit_dists
#' @inheritParams params
#' @param ... Additional arguments passed to `ssdtools::ssd_fit_dists()`.
#' @return The sims tibble with a list column fits of fistdist objects.
#' @export
ssd_fit_dists_sims <- function(sims, dists = ssdtools::ssd_dists_bcanz(), ..., silent = TRUE, save_to = NULL, seed = NULL, stream = 1L, start_sim = 1L) {
  chk::chk_data(sims)
  chk::check_names(sims, c("data"))
  chk::chk_flag(silent)
  chk::chk_null_or(save_to, vld = chk::vld_dir)
  chk::chk_count(start_sim)
  chk::chk_gt(start_sim)

  n <- nrow(sims)
  if(!n) {
    return(dplyr::mutate(sims, dists = list()))
  }
  seeds <- get_lecuyer_cmrg_seed_stream(seed = seed, nseed = n, stream = stream, start_seed = start_sim)
  iss <- seq(start_sim, start_sim + n - 1L) 
  sims$fits <- purrr::pmap(list(sims$data, seeds, iss), \(.x, .s, .i) fit_dists_seed(.x, .s, .i, dists = dists, silent = silent, save_to = save_to, ...))
  sims
}
