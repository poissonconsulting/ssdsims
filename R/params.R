#' Parameter Descriptions for Package
#'
#' Default parameter descriptions which may be overridden in individual
#' functions.
#'
#' A flag is a non-missing logical scalar.
#'
#' A string is a non-missing character scalar.
#'
#' @param dist A string specifying the distribution in the fitdists object or
#' `"top"` to use the distribution with most weight or `"multi"` to treat
#' the distributions as a single distribution.
#' @param nrow A integer vector of the number of rows in the generated data.
#' @param nsim A count of the number of data sets to generate.
#' @param sims A list of simulated data.
#' @param nseed A count of the number of new seeds to get for each stream.
#' @param return_items A flag specifying whether to return a list of the generated items.
#' @param save_to A string of the existing directory to save objects to or NULL.
#' @param seed An integer of the starting seed or NULL.
#' @param start_sim A count of the number of the simulation to start from.
#' @param start_seed A count of the number of the seed to start getting seeds from.
#' @param stream A count of the stream number.
#' @param ... Unused.
#' @keywords internal
#' @aliases parameters arguments args
#' @usage NULL
# nocov start
params <- function(...) NULL
# nocov end
