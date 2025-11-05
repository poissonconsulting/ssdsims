#' Run Scenario
#'
#' @inheritParams ssdtools::ssd_fit_dists
#' @inheritParams ssdtools::ssd_hc
#' @inheritParams params
#' @param x The object to use for the scenario.
#' @param ... Unused.
#' @return A tibble of nested data sets.
#' @export
ssd_run_scenario <- function(x, ...) UseMethod("ssd_run_scenario")

#' @describeIn ssd_run_scenario Run scenario using data.frame to sample data
#' @export
#' @examples
#' ssd_run_scenario(ssddata::ccme_boron, nsim = 2)
#' 
ssd_run_scenario.data.frame <- function(x, ..., nrow = c(6L, 10L), replace = FALSE, dists = ssdtools::ssd_dists_bcanz(), proportion = 0.05, ci = FALSE, seed = NULL, nsim = 100L, stream = getOption("ssdsims.stream", 1L), start_sim = 1L, .progress = FALSE) {

  data <- ssd_simulate_data(x, nrow = nrow, replace = replace, seed = seed, nsim = nsim, 
    stream = stream, start_sim = start_sim, .progress = .progress)
  
  run_scenario(x = data, ..., dists = dists, proportion = proportion, ci = ci, .progress = .progress)
}

#' @describeIn ssd_run_scenario Run scenario using fitdists object to generate data
#' @export
#' @examples
#' fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron)
#' ssd_run_scenario(fit, dist_sim = c("lnorm", "top"), nsim = 3)
#'
ssd_run_scenario.fitdists <- function(x, ..., nrow = c(6L, 10L), dist_sim = "top", dists = ssdtools::ssd_dists_bcanz(), proportion = 0.05, ci = FALSE, seed = NULL, nsim = 100L, stream = getOption("ssdsims.stream", 1L), start_sim = 1L, .progress = FALSE) {
  chk::chk_character(dist_sim)
  chk::chk_not_any_na(dist_sim)
  chk::chk_length(dist_sim, upper = Inf)
  chk::chk_subset(dist_sim, c("multi", "top", names(x)))

  chk::chk_whole_number(stream)
  chk::chk_gt(stream)

  sims <- sim_seq(start_sim, nsim)
  data <- tidyr::expand_grid(sim = sims, stream = stream, dist_sim = dist_sim, nrow = nrow)

  data$data <- purrr::pmap(as.list(data), \(dist_sim, nrow, sim, stream) ssd_simulate_data(x, dist_sim = dist_sim, nrow = nrow, nsim = 1L, start_sim = sim, stream = stream),.progress = .progress) |> 
    dplyr::bind_rows() |> 
    dplyr::pull("data")

  run_scenario(x = data, ..., dists = dists, proportion = proportion, ci = ci, .progress = .progress)
}

#' @describeIn ssd_run_scenario Run scenario using tmbfit object to generate data
#' @export
#' @examples
#' fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron)
#' ssd_run_scenario(fit[[1]], nsim = 3)
#'
ssd_run_scenario.tmbfit <- function(x, ..., nrow = c(6L, 10L), dists = ssdtools::ssd_dists_bcanz(), proportion = 0.05, ci = FALSE, seed = NULL, nsim = 100L, stream = getOption("ssdsims.stream", 1L), start_sim = 1L, .progress = FALSE) {
  args <- ssdtools::estimates(x)
  x <- paste0("ssdtools::ssd_r", x$dist)

  ssd_run_scenario(x, ..., nrow = nrow, dists = dists, proportion = proportion, ci = ci, seed = seed, nsim = nsim, stream = stream, start_sim = start_sim, .progress = .progress)
}

#' @describeIn ssd_run_scenario Run scenario using name of function to generate sequence of random numbers
#' @export
#' @examples
#' ssd_run_scenario("rlnorm", nsim = 3)
#'
ssd_run_scenario.character <- function(x, ..., nrow = c(6L, 10L), args = list(), dists = ssdtools::ssd_dists_bcanz(), proportion = 0.05, ci = FALSE, seed = NULL, nsim = 100L, stream = getOption("ssdsims.stream", 1L), start_sim = 1L, .progress = FALSE) {
  chk::chk_string(x)

  x <- eval(parse(text = x))

  ssd_run_scenario(x, ..., args = args, nrow = nrow, dists = dists, proportion = proportion, ci = ci, seed = seed, nsim = nsim, stream = stream, start_sim = start_sim, .progress = .progress)
}


#' @describeIn ssd_run_scenario Run scenario data using function to generate sequence of random numbers
#' @export
#' @examples
#' ssd_run_scenario(ssdtools::ssd_rlnorm, nsim = 3)
#'
ssd_run_scenario.function <- function(x, ...,  nrow = c(6L, 10L), args = list(),dists = ssdtools::ssd_dists_bcanz(), proportion = 0.05, ci = FALSE, seed = NULL, nsim = 100L, stream = getOption("ssdsims.stream", 1L), start_sim = 1L, .progress = FALSE) {
  chk::chk_function(x)
  chk::chk_unused(...)

  chk::chk_list(args)

  chk::chk_whole_numeric(nrow)
  chk::chk_not_any_na(nrow)
  chk::chk_range(nrow, c(5, 1000))
  chk::chk_unique(nrow)
  chk::chk_length(nrow, upper = 995)

  chk::chk_whole_number(stream)
  chk::chk_gt(stream)

  sims <- sim_seq(start_sim, nsim)
  data <- tidyr::expand_grid(sim = sims, stream = stream, nrow = nrow)

  data$data <- purrr::pmap(as.list(data), \(nrow, sim, stream) ssd_simulate_data(x, args = args, nrow = nrow, nsim = 1L, start_sim = sim, stream = stream),.progress = .progress) |> 
    dplyr::bind_rows() |> 
    dplyr::pull("data")

  run_scenario(x = data, ..., dists = dists, proportion = proportion, ci = ci, .progress = .progress)
}