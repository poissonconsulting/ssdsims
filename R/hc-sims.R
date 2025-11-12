#' Fit SSD Distributions to Simulated Data
#' 
#' @inheritParams ssdtools::ssd_hc
#' @inheritParams params
#' @param x A data frame with sim and stream integer columns and a list column of fitdists objects.
#' @param ... Additional arguments passed to `ssdtools::ssd_hc()`.
#' @return The x tibble with a list column hc of data frames produced by applying ssd_hc() to fits.
#' @export
ssd_hc_sims <- function(x, proportion = 0.05, ..., ci = FALSE, ci_method = "weighted_samples", seed = NULL, save_to = NULL, .progress = FALSE) {
  chk::check_data(x, values = list(sim = c(1L, 10000000L), stream = c(1L, 10000000L)))
  chk::check_names(x, "fits")
  chk::chk_null_or(save_to, vld = chk::vld_dir)

  chk::chk_character(ci_method)
  chk::chk_not_any_na(ci_method)
  chk::chk_unique(ci_method)
  chk::chk_subset(ci_method, ssdtools::ssd_ci_methods())
  chk::chk_length(ci_method, upper = Inf)

  args <- list(...)
  if("min_pboot" %in% names(args)) {
    chk::abort_chk("`min_pboot` is fixed at 0 in ssdsims and cannot be set by the user")
  }

  if(!nrow(x)) {
    return(dplyr::mutate(x, hc = list()))
  }

  x <- x |>
    dplyr::cross_join(tidyr::expand_grid(ci_method = ci_method))

  x$hc <- purrr::pmap(list(x$fits, x$sim, x$stream, x$ci_method), \(.x, .sim, .stream, .ci_method) hc_seed(.x, .sim, .stream, ci_method = .ci_method, seed = seed, proportion = proportion, save_to = save_to, ci = ci,...), .progress = .progress)
  x
}
