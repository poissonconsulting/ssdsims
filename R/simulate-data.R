#' Generate Data for Simulations
#'
#' A family of functions to generate a tibble of nested data sets.
#' 
#' @inheritParams params
#' @param x The object to use for generating the data.
#' @param ... Unused.
#' @return A tibble of nested data sets.
#' @export
ssd_simulate_data <- function(x, ...) UseMethod("ssd_simulate_data")

#' @describeIn ssd_simulate_data Generate data from data.frame
#' @param replace A flag specifying whether to sample with replacement.
#' @export
#' @examples
#' ssd_simulate_data(ssddata::ccme_boron, nrow = 5, nsim = 3)
#' 
ssd_simulate_data.data.frame <- function(x, ..., replace = FALSE, nrow = 6L, seed = NULL, nsim = 100L, stream = getOption("ssdsims.stream", 1L), start_sim = 1L) {
  chk::check_data(
    x, values = list(Conc = c(0,Inf,NA_real_)), nrow = c(5, 10000)
  )
  chk::chk_unused(...)
  chk::chk_flag(replace)
  chk::chk_whole_number(nrow)
  chk::chk_range(nrow, c(5, 1000))
  chk::chk_count(nsim)
  chk::chk_count(start_sim)
  chk::chk_gt(start_sim)

  sims <- seq(start_sim, start_sim + nsim - 1L) 
  seeds <- get_lecuyer_cmrg_seed_stream(seed = seed, nseed = nsim, start_seed = start_sim, stream = stream)
 
  stream <- as.integer(stream)

   purrr::map(seeds, \(seed) slice_sample_seed(x, n = nrow, replace = replace, seed = seed)) |>
      purrr::map2(sims, \(.x, .y) dplyr::mutate(.x, sim = .y, stream = stream)) |>
      dplyr::bind_rows() |>
      tidyr::nest(data = !c("sim", "stream"))
}

#' @describeIn ssd_simulate_data Generate data from fitdists object
#' @export
#' @examples
#' fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron)
#' ssd_simulate_data(fit, nrow = 5, nsim = 3)
#' 
ssd_simulate_data.fitdists <- function(x, ..., dist = "top", nrow = 6L, seed = NULL, nsim = 100L, stream = getOption("ssdsims.stream", 1L), start_sim = 1L) {
  chk::chk_string(dist)
  chk::chk_subset(dist, c("multi", "top", names(x)))
  chk::chk_unused(...)

  if(dist == "multi") {
    ## TODO: implement multi method
    .NotYetImplemented()
  }
  wch <- dist
  if(dist == "top") {
    weight <- ssdtools::glance(x, wt = TRUE)$wt
    wch <- which.max(weight)
  }
  
  ssd_simulate_data(x[[wch]], nrow = nrow, seed = seed, nsim = nsim, stream = stream, start_sim = start_sim)
}

#' @describeIn ssd_simulate_data Generate data from tmbfit object
#' @export
#' @examples
#' fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron)
#' ssd_simulate_data(fit[[1]], nrow = 5, nsim = 3)
#' 
ssd_simulate_data.tmbfit <- function(x, ..., nrow = 6L, seed = NULL, nsim = 100L, stream = getOption("ssdsims.stream", 1L), start_sim = 1L) {
  chk::chk_unused(...)
  
  pars <- ssdtools::estimates(x)
  x <- x$dist
  ssd_simulate_data(x, pars = pars, nrow = nrow, seed = seed, nsim = nsim, stream = stream, start_sim = start_sim)
}

#' @describeIn ssd_simulate_data Generate data using distribution name
#' @param pars A named list of the parameter values.
#' @export
#' @examples
#' ssd_simulate_data("lnorm", nrow = 5, nsim = 3)
#' 
ssd_simulate_data.character <- function(x, ..., pars = list(), nrow = 6L, seed = NULL, nsim = 100L, stream = getOption("ssdsims.stream", 1L), start_sim = 1L) {
  chk::chk_string(x)
  chk::chk_subset(x, ssdtools::ssd_dists_all())
  chk::chk_list(pars)
  chk::chk_unused(...)
  
  fun <- paste0("ssdtools::ssd_r", x)
  fun <- eval(parse(text = fun))
  
  ssd_simulate_data(fun, args = as.list(pars), nrow = nrow, seed = seed, nsim = nsim, stream = stream, start_sim = start_sim)
}

#' @describeIn ssd_simulate_data Generate data using function to generate sequence of random numbers
#' @param args A named list of the argument values.
#' @export
#' @examples
#' ssd_simulate_data(ssdtools::ssd_rlnorm, nrow = 5, nsim = 3)
#'
ssd_simulate_data.function <- function(x, ..., args = list(), nrow = 6L, seed = NULL, nsim = 100L, stream = getOption("ssdsims.stream", 1L), start_sim = 1L) {
  chk::chk_list(args)
  chk::chk_whole_number(nrow)
  chk::chk_range(nrow, c(5, 1000))
  chk::chk_count(nsim)
  chk::chk_count(start_sim)
  chk::chk_gt(start_sim)

  sims <- seq(start_sim, start_sim + nsim - 1L) 
  seeds <- get_lecuyer_cmrg_seed_stream(seed = seed, nseed = nsim, start_seed = start_sim, stream = stream)

  stream <- as.integer(stream)
  args$n <- nrow
    
  purrr::map(sims, \(seed) do_call_seed(x, args = args, seed = seed)) |>
      purrr::map(\(.x) dplyr::tibble(Conc = .x)) |>
      purrr::map2(seq_len(nsim), \(.x, .y) dplyr::mutate(.x, sim = .y, stream = stream)) |>
      dplyr::bind_rows() |>
      tidyr::nest(data = !c("sim", "stream"))

}
