#' Simulation Index Sequence
#'
#' Returns the integer sequence of simulation indices starting from
#' `start_sim` of length `nsim`.
#'
#' Validates that `start_sim` is a positive count and `nsim` is a count before constructing the
#' sequence. Used to assemble the `sim` column of the nested tibbles produced
#' by the [ssd_sim_data()] family.
#'
#' @inheritParams params
#' @return An integer vector of consecutive positive numbers of length `nsim`.
#' @noRd
sim_seq <- function(start_sim, nsim) {
  chk::chk_count(nsim)
  chk::chk_count(start_sim)
  chk::chk_gt(start_sim)

  seq(start_sim, start_sim + nsim - 1L)
}
