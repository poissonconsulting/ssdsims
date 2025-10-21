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
#' @param nrow A count of the number of rows in the generated data which must be between 5 and 1,000,
#' @param nsim A count of the number of data sets to generate. 
#' @param replace A flag specifying whether to sample with replacement.
#' @param seed An integer of the starting seed or NULL.
#' @param start_sim A count of the number of the simulation to start from.
#' @param stream A count of the stream number.
#' @param ... Unused.
#' @keywords internal
#' @aliases parameters arguments args
#' @usage NULL
# nocov start
params <- function(...) NULL
# nocov end
