#' Fit SSD Distributions to Simulated Data
#' 
#' @inheritParams ssdtools::ssd_hc
#' @inheritParams params
#' @param x A data frame with sim and stream integer columns and a list column of fitdists objects.
#' @param ... Additional arguments passed to `ssdtools::ssd_hc()`.
#' @return The x tibble with a list column hc of data frames produced by applying ssd_hc() to fits.
#' @export
ssd_hc_sims <- function(x, proportion = 0.05, ..., ci = FALSE, seed = NULL, save_to = NULL, .progress = FALSE) {
  chk::check_data(x, values = list(sim = c(1L, 10000000L), stream = c(1L, 10000000L)))
  chk::check_names(x, "fits")
  chk::chk_null_or(save_to, vld = chk::vld_dir)

  if(!nrow(x)) {
    return(dplyr::mutate(x, hc = list()))
  }
  x$hc <- purrr::pmap(list(x$fits, x$sim, x$stream), \(.x, .sim, .stream) hc_seed(.x, .sim, .stream, seed = seed, proportion = proportion, save_to = save_to, ci = ci,...), .progress = .progress)
  x
}
