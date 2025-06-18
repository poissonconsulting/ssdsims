#' Generate Data for Simulations
#'
#' @param x The object to use for generating the data.
#' @param nrow A integer vector of the number of rows in the generated data.
#' @param nsims A count of the number of data sets to generate.
#' @param ... Unused.
#' @return A tibble of generate datasets.
#' @export
ssd_generate_data <- function(x, ...) UseMethod("ssd_generate_data")

#' @describeIn ssd_generate_data Generate data from data.frame
#' @param replace A flag specifying whether to sample with replacement.
#' @export
#' @examples
#' ssd_generate_data(ssddata::ccme_boron, nrow = 5, nsims = 3)
#' 
ssd_generate_data.data.frame <- function(x, ..., replace = FALSE, nrow = 6L, nsims = 100L) {
  chk::check_data(
    x, values = list(Conc = c(0,Inf,NA_real_)), nrow = c(5, 10000)
  )
  chk::chk_flag(replace)
  chk::chk_whole_numeric(nrow)
  chk::chk_range(nrow, c(5, 1000))
  chk::chk_length(nrow, upper = 100)
  chk::chk_not_any_na(nrow)
  chk::chk_count(nsims)
  chk::chk_range(nsims, c(1, 10000))
  chk::chk_unused(...)
  
  if(length(nrow) == 1) {
    data <- nsims |>
      seq_len() |>
      purrr::map(\(n) dplyr::slice_sample(x, n = nrow, replace = replace)) |>
      purrr::map(\(.x) dplyr::mutate(.x, row = seq_len(nrow))) |>
      purrr::map2(seq_len(nsims), \(.x, .y) dplyr::mutate(.x, sim = .y)) |>
      dplyr::bind_rows() |>
      dplyr::mutate(nrow = nrow,
                    replace = replace)
    return(data)
  }
  nrow |>
    purrr::map(\(.x) ssd_generate_data(x, replace = replace, nrow = .x, nsims = nsims)) |>
    dplyr::bind_rows()
}

#' @describeIn ssd_generate_data Generate data from fitdists object
#' @param dist A string specifying the distribution in the fitdists object or
#' `"top"` to use the distribution with most weight or `"multi"` to treat
#' the distributions as a single distribution.
#' @export
#' @examples
#' fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron)
#' ssd_generate_data(fit, nrow = 5, nsims = 3)
#' 
ssd_generate_data.fitdists <- function(x, ..., dist = "top", nrow = 6L, nsims = 100L) {
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
  
  ssd_generate_data(x[[wch]], nrow = nrow, nsims = nsims)
}

#' @describeIn ssd_generate_data Generate data from tmbfit object
#' @export
#' @examples
#' fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron)
#' ssd_generate_data(fit[[1]], nrow = 5, nsims = 3)
#' 
ssd_generate_data.tmbfit <- function(x, ..., nrow = 6L, nsims = 100L) {
  chk::chk_unused(...)
  
  pars <- ssdtools::estimates(x)
  x <- x$dist
  ssd_generate_data(x, pars = pars, nrow = nrow, nsims = nsims)
}

#' @describeIn ssd_generate_data Generate data using distribution name
#' @param pars A named list of the parameter values.
#' @export
#' @examples
#' ssd_generate_data("lnorm", nrow = 5, nsims = 3)
#' 
ssd_generate_data.character <- function(x, ..., pars = list(), nrow = 6L, nsims = 100L) {
  chk::chk_string(x)
  chk::chk_subset(x, ssdtools::ssd_dists_all())
  chk::chk_list(pars)
  chk::chk_unused(...)
  
  fun <- paste0("ssdtools::ssd_r", x)
  fun <- eval(parse(text = fun))
  
  ssd_generate_data(fun, args = as.list(pars), nrow = nrow, nsims = nsims)
}

#' @describeIn ssd_generate_data Generate data using function
#' @param args A named list of the argument values.
#' @export
#' @examples
#' ssd_generate_data(ssdtools::ssd_rlnorm, nrow = 5, nsims = 3)
#'
ssd_generate_data.function <- function(x, ..., args = list(), nrow = 6L, nsims = 100L) {
  chk::chk_list(args)
  chk::chk_whole_numeric(nrow)
  chk::chk_range(nrow, c(5, 1000))
  chk::chk_length(nrow, upper = 100)
  chk::chk_not_any_na(nrow)
  chk::chk_count(nsims)
  chk::chk_range(nsims, c(1, 10000))
  chk::chk_unused(...)
  
  if(length(nrow) == 1) {
    args$n <- nrow
    
    data <- nsims |>
      seq_len() |>
      purrr::map(\(n) do.call(x, args = args)) |>
      purrr::map(\(.x) dplyr::tibble(Conc = .x)) |>
      purrr::map(\(.x) dplyr::mutate(.x, row = seq_len(nrow))) |>
      purrr::map2(seq_len(nsims), \(.x, .y) dplyr::mutate(.x, sim = .y)) |>
      dplyr::bind_rows() |>
      dplyr::mutate(nrow = nrow,
                    args = list(args))
    return(data)
  }
  nrow |>
    purrr::map(\(.x) ssd_generate_data(x, args = args, nrow = .x, nsims = nsims)) |>
    dplyr::bind_rows()
}
