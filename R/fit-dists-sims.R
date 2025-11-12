#' Fit SSD Distributions to Simulated Data
#' 
#' @inheritParams ssdtools::ssd_fit_dists
#' @param min_pmix A list of one or more functions with a single argument
#' that inputs the number of rows of data and returns a proportion between 0 and 0.5. 
#' @param range_shape1 A list of numeric vectors of length two of the lower
#' and upper bounds for the shape1 parameter.
#' @param range_shape2 A list of numeric vectors of length two of the lower
#' and upper bounds for the shape2 parameter. 
#' @inheritParams params
#' @param x A data frame with sim and stream integer columns and a list column of the data frames to fit distributions to.
#' @param ... Additional arguments passed to `ssdtools::ssd_fit_dists()`.
#' @return The x tibble with a list column fits of fistdist objects.
#' @export
ssd_fit_dists_sims <- function(x, dists = ssdtools::ssd_dists_bcanz(), ..., rescale = FALSE, computable = FALSE, at_boundary_ok = TRUE, min_pmix = list(ssdtools::ssd_min_pmix), range_shape1 = list(c(0.05, 20)), range_shape2 = range_shape1, seed = NULL, silent = TRUE, .progress = FALSE) {
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

  chk::chk_list(range_shape1)
  chk::chk_length(range_shape1, upper = Inf)
  chk::chk_all(range_shape1, chk::chk_double)
  chk::chk_all(range_shape1, chk::chk_length, upper = 2L)
  chk::chk_unique(range_shape1)

  chk::chk_list(range_shape2)
  chk::chk_length(range_shape2, upper = Inf)
  chk::chk_all(range_shape2, chk::chk_double)
  chk::chk_all(range_shape2, chk::chk_length, upper = 2L)
  chk::chk_unique(range_shape2)

  chk::chk_flag(silent)

  if(!nrow(x)) {
    return(dplyr::mutate(x, dists = list()))
  }

  x <- x |>
    dplyr::cross_join(tidyr::expand_grid(rescale = rescale, computable = computable, at_boundary_ok = at_boundary_ok, min_pmix = min_pmix, range_shape1 = range_shape1, range_shape2 = range_shape2))

  x$fits <- purrr::pmap(list(x$data, x$sim, x$stream, x$rescale, x$computable, x$at_boundary_ok, x$min_pmix, x$range_shape1, x$range_shape2), \(.x, .sim, .stream, .rescale, .computable, .at_boundary_ok, .min_pmix, .range_shape1, .range_shape2) fit_dists_seed(.x, .sim, .stream, seed = seed, dists = dists, rescale = .rescale, computable = .computable, at_boundary_ok = .at_boundary_ok, min_pmix = .min_pmix, range_shape1 = .range_shape1, range_shape2 = .range_shape2, silent = silent, ...), .progress = .progress)
  x
}
