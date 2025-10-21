#' Run Scenario
#'
#' 
#' @inheritParams params
#' @param x The object to use for the scenario.
#' @param ... Unused.
#' @return A tibble of nested data sets.
#' @export
ssd_run_scenario <- function(x, ...) UseMethod("ssd_run_scenario")

#' @describeIn ssd_run_scenario Run from data.frame
#' @export
#' @examples
#' ssd_run_scenario(ssddata::ccme_boron, nsim = 2)
#' 
ssd_run_scenario.data.frame <- function(x, ..., replace = FALSE, nrow = c(5L, 10L), seed = NULL, nsim = 100L, stream = getOption("ssdsims.stream", 1L), start_sim = 1L) {

  chk::check_data(
    x, values = list(Conc = c(0,Inf,NA_real_)), nrow = c(5, 10000)
  )
  chk::chk_unused(...)
  chk::chk_logical(replace)
  chk::chk_not_any_na(replace)
  chk::chk_unique(replace)
  chk::chk_length(replace, upper = 2L)

  chk::chk_whole_numeric(nrow)
  chk::chk_not_any_na(nrow)
  chk::chk_range(nrow, c(5, 1000))
  chk::chk_unique(nrow)
  chk::chk_length(nrow, upper = 995)

  sims <- sim_seq(nsim = nsim, start_sim = start_sim)
  data <- tidyr::expand_grid(sim = sims, stream = stream, replace = replace, nrow = nrow)
  data
}