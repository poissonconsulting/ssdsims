#' ssdsims Glossary
#'
#' Terminology used throughout `ssdsims`.
#'
#' @section RNG terms:
#' \describe{
#'   \item{seed}{A scalar integer passed to [`base::set.seed()`] to
#'     initialize the RNG (e.g. `42`). Exposed via the `seed` argument of
#'     [`ssd_sim_data()`], [`ssd_fit_dists_sims()`], [`ssd_hc_sims()`] and
#'     [`ssd_run_scenario()`], and consumed by [`with_lecuyer_cmrg_seed()`]
#'     and [`local_lecuyer_cmrg_seed()`].}
#'   \item{state}{A `.Random.seed`-style integer vector representing the
#'     full internal state of the RNG. For L'Ecuyer-CMRG this is a length-7
#'     integer vector (see [`parallel::nextRNGStream()`]). States are produced
#'     by [`parallel::nextRNGStream()`] /
#'     [`parallel::nextRNGSubStream()`] and by
#'     `get_lecuyer_cmrg_seed_stream()`; they are consumed by
#'     [`with_lecuyer_cmrg_state()`] and [`local_lecuyer_cmrg_state()`].
#'     A state cannot be passed to [`base::set.seed()`] because
#'     `set.seed()` only consumes a single integer.}
#'   \item{stream}{An independent sequence of pseudo-random numbers within
#'     the L'Ecuyer-CMRG RNG, advanced via
#'     [`parallel::nextRNGStream()`]. Streams are designed to be
#'     statistically independent across distinct values of the `stream`
#'     argument.}
#'   \item{sub-stream}{A subdivision of a stream, advanced via
#'     [`parallel::nextRNGSubStream()`]. Each simulation (`sim`) within a
#'     given `stream` uses a distinct sub-stream.}
#' }
#'
#' @section Simulation terms:
#' \describe{
#'   \item{`sim`}{The index of a simulation replicate within a `stream`.}
#'   \item{`nsim`}{The number of simulation replicates to perform.}
#'   \item{`start_sim`}{The starting `sim` index; sub-streams are advanced
#'     to this position before data is generated.}
#'   \item{`stream` (argument)}{The index of the L'Ecuyer-CMRG stream to
#'     use; distinct `stream` values give statistically independent
#'     sequences.}
#'   \item{`nrow`}{The number of rows (species) in each simulated dataset.}
#'   \item{`replace`}{Whether the resampling that generates simulated data
#'     is performed with replacement.}
#' }
#'
#' @section SSD terms:
#' \describe{
#'   \item{SSD}{Species sensitivity distribution: a distribution of
#'     species-level toxicity endpoints used in ecological risk assessment.}
#'   \item{`dists`}{The parametric distributions fit to the SSD data
#'     (e.g. `lnorm`, `gamma`, `llogis`); see
#'     [`ssdtools::ssd_fit_dists()`].}
#'   \item{`fits`}{A `fitdists` object holding one or more fitted
#'     distributions.}
#'   \item{`hc`}{Hazard concentration: a quantile of the SSD at a given
#'     `proportion` (e.g. `hc5` is the 5\% quantile); see
#'     [`ssdtools::ssd_hc()`].}
#'   \item{`proportion`}{The proportion of species affected at which the
#'     hazard concentration is computed.}
#'   \item{`ci`}{Whether to compute confidence intervals on hazard
#'     concentrations.}
#'   \item{`ci_method`}{The method used to compute confidence intervals
#'     (e.g. `multi_fixed`, `weighted_samples`).}
#'   \item{`nboot`}{The number of bootstrap replicates used when computing
#'     confidence intervals.}
#' }
#'
#' @name ssdsims-glossary
#' @aliases glossary
NULL
