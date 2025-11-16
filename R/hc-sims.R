#' Fit SSD Distributions to Simulated Data
#'
#' @inheritParams ssdtools::ssd_hc
#' @inheritParams params
#' @param x A data frame with sim and stream integer columns and a list column of fitdists objects.
#' @param ... Additional arguments passed to `ssdtools::ssd_hc()`.
#' @return The x tibble with a list column hc of data frames produced by applying ssd_hc() to fits.
#' @export
ssd_hc_sims <- function(
  x,
  proportion = 0.05,
  ...,
  ci = FALSE,
  nboot = 1000,
  est_method = "multi",
  ci_method = "weighted_samples",
  parametric = TRUE,
  seed = NULL,
  save_to = NULL,
  .progress = FALSE
) {
  chk::check_data(
    x,
    values = list(sim = c(1L, 10000000L), stream = c(1L, 10000000L))
  )
  chk::check_names(x, "fits")
  chk::chk_null_or(save_to, vld = chk::vld_dir)

  chk::chk_whole_numeric(nboot)
  chk::chk_not_any_na(nboot)
  chk::chk_gt(nboot)
  chk::chk_unique(nboot)
  chk::chk_length(nboot, upper = Inf)

  chk::chk_character(est_method)
  chk::chk_not_any_na(est_method)
  chk::chk_unique(est_method)
  chk::chk_subset(est_method, ssdtools::ssd_est_methods())
  chk::chk_length(est_method, upper = Inf)

  chk::chk_character(ci_method)
  chk::chk_not_any_na(ci_method)
  chk::chk_unique(ci_method)
  chk::chk_subset(ci_method, ssdtools::ssd_ci_methods())
  chk::chk_length(ci_method, upper = Inf)

  chk::chk_logical(parametric)
  chk::chk_not_any_na(parametric)
  chk::chk_unique(parametric)
  chk::chk_length(parametric, upper = 2L)

  args <- list(...)
  if ("min_pboot" %in% names(args)) {
    chk::abort_chk(
      "`min_pboot` is fixed at 0 in ssdsims and cannot be set by the user"
    )
  }

  if (!nrow(x)) {
    return(dplyr::mutate(x, hc = list()))
  }

  x <- x |>
    dplyr::cross_join(tidyr::expand_grid(
      nboot = nboot,
      est_method = est_method,
      ci_method = ci_method,
      parametric = parametric
    ))

  x$hc <- purrr::pmap(
    list(
      x$fits,
      x$sim,
      x$stream,
      x$nboot,
      x$est_method,
      x$ci_method,
      x$parametric
    ),
    \(.x, .sim, .stream, .nboot, .est_method, .ci_method, .parametric) {
      hc_seed(
        .x,
        .sim,
        .stream,
        nboot = .nboot,
        est_method = .est_method,
        ci_method = .ci_method,
        parametric = .parametric,
        seed = seed,
        proportion = proportion,
        save_to = save_to,
        ci = ci,
        ...
      )
    },
    .progress = .progress
  )
  x
}
