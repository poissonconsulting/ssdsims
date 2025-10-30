#' Generate Data for Simulations
#'
#' A family of functions to generate a tibble of nested data sets.
#' 
#' @inheritParams params
#' @param x The object to use for generating the data.
#' @return A tibble of nested data sets.
#' @export
ssd_simulate_data <- function(x, ...) UseMethod("ssd_simulate_data")

#' @describeIn ssd_simulate_data Generate data by sampling from data.frame
#' @export
#' @examples
#' ssd_simulate_data(ssddata::ccme_boron, nrow = 5, nsim = 3)
#' 
ssd_simulate_data.data.frame <- function(x, ..., replace = FALSE, nrow = 6L, seed = NULL, nsim = 100L, stream = getOption("ssdsims.stream", 1L), start_sim = 1L, .progress = FALSE) {
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

  sims <- sim_seq(start_sim, nsim) 
  seeds <- get_lecuyer_cmrg_seeds_stream(seed = seed, nsim = nsim, start_sim = start_sim, stream = stream)
 
  stream <- as.integer(stream)

   purrr::map(seeds, \(seed) slice_sample_seed(x, n = nrow, replace = replace, seed = seed), .progress = .progress) |>
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
ssd_simulate_data.fitdists <- function(x, ..., dist = "top", nrow = 6L, seed = NULL, nsim = 100L, stream = getOption("ssdsims.stream", 1L), start_sim = 1L, .progress = FALSE) {
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
  
  ssd_simulate_data(x[[wch]], nrow = nrow, seed = seed, nsim = nsim, stream = stream, start_sim = start_sim, .progress = .progress)
}

#' @describeIn ssd_simulate_data Generate data from tmbfit object
#' @export
#' @examples
#' fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron)
#' ssd_simulate_data(fit[[1]], nrow = 5, nsim = 3)
#' 
ssd_simulate_data.tmbfit <- function(x, ..., nrow = 6L, seed = NULL, nsim = 100L, stream = getOption("ssdsims.stream", 1L), start_sim = 1L, .progress = FALSE) {
  chk::chk_unused(...)
  
  args <- ssdtools::estimates(x)
  x <- paste0("ssdtools::ssd_r", x$dist)
  ssd_simulate_data(x, args = args, nrow = nrow, seed = seed, nsim = nsim, stream = stream, start_sim = start_sim, .progress = .progress)
}

#' @describeIn ssd_simulate_data Generate data using name of function
#' @export
#' @examples
#' ssd_simulate_data("rnorm", nrow = 5, nsim = 3)
#' 
ssd_simulate_data.character <- function(x, ..., args = list(), nrow = 6L, seed = NULL, nsim = 100L, stream = getOption("ssdsims.stream", 1L), start_sim = 1L, .progress = FALSE) {
  chk::chk_string(x)
  chk::chk_unused(...)
  
  x <- eval(parse(text = x))
  
  ssd_simulate_data(x, args = args, nrow = nrow, seed = seed, nsim = nsim, stream = stream, start_sim = start_sim, .progress = .progress)
}

#' @describeIn ssd_simulate_data Generate data using function to generate sequence of random numbers
#' @export
#' @examples
#' ssd_simulate_data(ssdtools::ssd_rlnorm, nrow = 5, nsim = 3)
#'
ssd_simulate_data.function <- function(x, ..., args = list(), nrow = 6L, seed = NULL, nsim = 100L, stream = getOption("ssdsims.stream", 1L), start_sim = 1L, .progress = FALSE) {
  chk::chk_function(x)
  chk::chk_unused(...)
  chk::chk_list(args)
  chk::chk_whole_number(nrow)
  chk::chk_range(nrow, c(5, 1000))
  chk::chk_count(nsim)
  chk::chk_count(start_sim)
  chk::chk_gt(start_sim)

  sims <- sim_seq(start_sim, nsim) 
  seeds <- get_lecuyer_cmrg_seeds_stream(seed = seed, nsim = nsim, start_sim = start_sim, stream = stream)

  stream <- as.integer(stream)
  args$n <- nrow
    
  purrr::map(sims, \(seed) do_call_seed(x, args = args, seed = seed), .progress = .progress) |>
      purrr::map(\(.x) dplyr::tibble(Conc = .x)) |>
      purrr::map2(seq_len(nsim), \(.x, .y) dplyr::mutate(.x, sim = .y, stream = stream)) |>
      dplyr::bind_rows() |>
      tidyr::nest(data = !c("sim", "stream"))
}
