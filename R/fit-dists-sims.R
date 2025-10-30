#' Fit SSD Distributions to Simulated Data
#' 
#' @inheritParams ssdtools::ssd_fit_dists
#' @inheritParams params
#' @param x A data frame with sim and stream integer columns and a list column of the data frames to fit distributions to.
#' @param ... Additional arguments passed to `ssdtools::ssd_fit_dists()`.
#' @return The x tibble with a list column fits of fistdist objects.
#' @export
ssd_fit_dists_sims <- function(x, dists = ssdtools::ssd_dists_bcanz(), ..., seed = NULL, silent = TRUE, .progress = FALSE) {
  chk::check_data(x, values = list(sim = c(1L, 10000000L), stream = c(1L, 10000000L)))
  chk::check_names(x, "data")
  chk::chk_flag(silent)

  if(!nrow(x)) {
    return(dplyr::mutate(x, dists = list()))
  }
  x$fits <- purrr::pmap(list(x$data, x$sim, x$stream), \(.x, .sim, .stream) fit_dists_seed(.x, .sim, .stream, seed = seed, dists = dists, silent = silent, ...), .progress = .progress)
  x
}
