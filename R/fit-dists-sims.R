#' Fit SSD Distributions to Simulated Data
#' 
#' @inheritParams ssdtools::ssd_fit_dists
#' @param min_pmix A list of one or more functions with a single argument
#' that inputs the number of rows of data and returns a proportion between 0 and 0.5. 
#' @inheritParams params
#' @param x A data frame with sim and stream integer columns and a list column of the data frames to fit distributions to.
#' @param ... Additional arguments passed to `ssdtools::ssd_fit_dists()`.
#' @return The x tibble with a list column fits of fistdist objects.
#' @export
ssd_fit_dists_sims <- function(x, dists = ssdtools::ssd_dists_bcanz(), ..., rescale = FALSE, computable = FALSE, at_boundary_ok = TRUE, min_pmix = list(ssdtools::ssd_min_pmix), seed = NULL, silent = TRUE, .progress = FALSE) {
  chk::check_data(x, values = list(sim = c(1L, 10000000L), stream = c(1L, 10000000L)))
  chk::check_names(x, "data")

  chk::chk_logical(rescale)
  chk::chk_not_any_na(rescale)
  chk::chk_unique(rescale)
  chk::chk_length(rescale, upper = 2L)

  chk::chk_logical(computable)
  chk::chk_not_any_na(computable)
  chk::chk_unique(computable)
  chk::chk_length(computable, upper = 2L)

  chk::chk_logical(at_boundary_ok)
  chk::chk_not_any_na(at_boundary_ok)
  chk::chk_unique(at_boundary_ok)
  chk::chk_length(at_boundary_ok, upper = 2L)

  chk::chk_list(min_pmix)
  chk::chk_length(min_pmix, upper = Inf)
  chk::chk_all(min_pmix, chk::chk_function, formals = 1L)
  chk::chk_unique(min_pmix)

  chk::chk_flag(silent)

  if(!nrow(x)) {
    return(dplyr::mutate(x, dists = list()))
  }

  x <- x |>
    dplyr::cross_join(tidyr::expand_grid(rescale = rescale, computable = computable, at_boundary_ok = at_boundary_ok, min_pmix = min_pmix))

  x$fits <- purrr::pmap(list(x$data, x$sim, x$stream, x$rescale, x$computable, x$at_boundary_ok, x$min_pmix), \(.x, .sim, .stream, .rescale, .computable, .at_boundary_ok, .min_pmix) fit_dists_seed(.x, .sim, .stream, seed = seed, dists = dists, rescale = .rescale, computable = .computable, at_boundary_ok = .at_boundary_ok, min_pmix = .min_pmix, silent = silent, ...), .progress = .progress)
  x
}
