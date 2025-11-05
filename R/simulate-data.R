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
ssd_simulate_data.data.frame <- function(x, ..., nrow = 6L, replace = FALSE, seed = NULL, nsim = 100L, stream = getOption("ssdsims.stream", 1L), start_sim = 1L, .progress = FALSE) {
  chk::check_data(
    x, values = list(Conc = c(0,Inf,NA_real_)), nrow = c(5, 10000)
  )
  chk::chk_unused(...)
  
  chk::chk_whole_numeric(nrow)
  chk::chk_not_any_na(nrow)
  chk::chk_unique(nrow)
  chk::chk_range(nrow, c(5, 1000))
  chk::chk_length(nrow, upper = Inf)
  
  chk::chk_logical(replace)
  chk::chk_not_any_na(replace)
  chk::chk_unique(replace)
  chk::chk_length(replace, upper = 2L)
  
  chk::chk_whole_number(stream)
  chk::chk_gt(stream)
  
  sims <- sim_seq(start_sim, nsim)
  stream <- as.integer(stream)
  
  data <- tidyr::expand_grid(sim = sims, stream = stream, replace = replace, nrow = nrow)
  
  if(nrow(data) == nsim) {
    seeds <- get_lecuyer_cmrg_seeds_stream(seed = seed, nsim = nsim, start_sim = start_sim, stream = stream)
    
    data <- purrr::map(seeds, \(seed) slice_sample_seed(x, n = nrow, replace = replace, seed = seed), .progress = .progress) |>
    purrr::map2(sims, \(.x, .y) dplyr::mutate(.x, sim = .y, stream = stream)) |>
    dplyr::bind_rows() |>
    tidyr::nest(data = !c("sim", "stream")) |>
    dplyr::mutate(nrow = nrow, replace = replace) |>
    dplyr::select("sim", "stream", "nrow", "replace", "data")
    
    return(data)
  }
  
  data$data <- purrr::pmap(as.list(data), \(replace, nrow, sim, stream) ssd_simulate_data(x, replace = replace, nrow = nrow, nsim = 1L, start_sim = sim, stream = stream),.progress = .progress) |> 
  dplyr::bind_rows() |> 
  dplyr::pull("data")
  
  data
}

#' @describeIn ssd_simulate_data Generate data from fitdists object
#' @export
#' @examples
#' fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron)
#' ssd_simulate_data(fit, nrow = 5, nsim = 3)
#' 
ssd_simulate_data.fitdists <- function(x, ..., nrow = 6L, dist_sim = "top", seed = NULL, nsim = 100L, stream = getOption("ssdsims.stream", 1L), start_sim = 1L, .progress = FALSE) {
  chk::chk_unused(...)
  
  chk::chk_whole_numeric(nrow)
  chk::chk_not_any_na(nrow)
  chk::chk_unique(nrow)
  chk::chk_range(nrow, c(5, 1000))
  chk::chk_length(nrow, upper = Inf)
  
  chk::chk_character(dist_sim)
  chk::chk_not_any_na(dist_sim)
  chk::chk_unique(dist_sim)
  chk::chk_subset(dist_sim, c("all", "multi", "top", names(x)))
  chk::chk_length(dist_sim, upper = Inf)

  if("all" %in% dist_sim) {
    wch <- which(dist_sim == "all")
    n <- length(dist_sim)
    dist_sim <- c(dist_sim[seq_up(1, wch-1)], names(x), dist_sim[seq_up(wch+1,n)])
    dist_sim <- unique(dist_sim)
  }
  
  chk::chk_whole_number(stream)
  chk::chk_gt(stream)
  
  sims <- sim_seq(start_sim, nsim)
  stream <- as.integer(stream)
  
  data <- tidyr::expand_grid(sim = sims, stream = stream, nrow = nrow, dist_sim = dist_sim)
  
  if(nrow(data) == nsim) {
    
    if(dist_sim == "multi") {
      fun <- ssdtools::ssd_rmulti_fitdists
      args <- list(fitdists = x)
      datas <- ssd_simulate_data(fun, args = args, nrow = nrow, seed = seed, nsim = nsim, stream = stream, start_sim = start_sim, .progress = .progress)
      return(datas)
    }
    wch <- dist_sim
    if(dist_sim == "top") {
      weight <- ssdtools::glance(x, wt = TRUE)$wt
      wch <- which.max(weight)
    }
    x <- x[[wch]]
    data <- ssd_simulate_data(x, nrow = nrow, seed = seed, nsim = nsim, stream = stream, start_sim = start_sim, .progress = .progress)
    return(data)
  }
  data$data <- purrr::pmap(as.list(data), \(nrow, dist_sim, sim, stream) ssd_simulate_data(x, nrow = nrow, dist_sim = dist_sim, nsim = 1L, start_sim = sim, stream = stream),.progress = .progress) |> 
  dplyr::bind_rows() |> 
  dplyr::pull("data")
  data
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
ssd_simulate_data.character <- function(x, ..., nrow = 6L, args = list(), seed = NULL, nsim = 100L, stream = getOption("ssdsims.stream", 1L), start_sim = 1L, .progress = FALSE) {
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
ssd_simulate_data.function <- function(x, ..., nrow = 6L, args = list(), seed = NULL, nsim = 100L, stream = getOption("ssdsims.stream", 1L), start_sim = 1L, .progress = FALSE) {
  chk::chk_unused(...)
  
  chk::chk_whole_numeric(nrow)
  chk::chk_not_any_na(nrow)
  chk::chk_unique(nrow)
  chk::chk_range(nrow, c(5, 1000))
  chk::chk_length(nrow, upper = Inf)
  
  chk::chk_list(args)
  
  chk::chk_whole_number(stream)
  chk::chk_gt(stream)
  
  sims <- sim_seq(start_sim, nsim) 
  stream <- as.integer(stream)
  
  data <- tidyr::expand_grid(sim = sims, stream = stream, nrow = nrow)
  
  if(nrow(data) == nsim) {
    
    seeds <- get_lecuyer_cmrg_seeds_stream(seed = seed, nsim = nsim, start_sim = start_sim, stream = stream)
    
    argsn <- args
    argsn$n <- nrow
    
    data <- purrr::map(sims, \(seed) do_call_seed(x, args = argsn, seed = seed), .progress = .progress) |>
    purrr::map(\(.x) dplyr::tibble(Conc = .x)) |>
    purrr::map2(seq_len(nsim), \(.x, .y) dplyr::mutate(.x, sim = .y, stream = stream)) |>
    dplyr::bind_rows() |>
    tidyr::nest(data = !c("sim", "stream")) |>
    dplyr::mutate(nrow = nrow, args = list(args)) |>
    dplyr::select("sim", "stream", "nrow", "args", "data")
    
    return(data)
  }
  
  data$data <- purrr::pmap(as.list(data), \(nrow, sim, stream) ssd_simulate_data(x, args = args, nrow = nrow, nsim = 1L, start_sim = sim, stream = stream),.progress = .progress) |> 
  dplyr::bind_rows() |> 
  dplyr::pull("data")
  data
}
