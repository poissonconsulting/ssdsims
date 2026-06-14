# Cost estimation: a calibrate -> estimate workflow that predicts, *before* a
# scenario is launched, roughly how much compute it costs and how long its
# single longest task runs. The hc bootstrap dominates `ci = TRUE`; the fit and
# data-sampling steps are comparatively cheap (TARGETS-DESIGN.md). Per-task time
# follows a simple model fit per `ci_method`:
#
#   time = (base + slope * max(nboot, n0)) * nrow_factor(nrow)
#
# `proportion` and `est_method` are *free* - one bootstrap per
# `nboot x ci_method x parametric` cell serves every `proportion`/`est_method`
# (both are hc settings, not task axes, since `est-method-setting`), so they
# never multiply the estimate. The coefficients are architecture-
# specific, so the package ships both a *method* to re-measure them
# (`ssd_calibrate_cost()`) and a *default* fitted during development
# (`ssd_cost_calibration()`). `ssd_estimate_cost()` only *reads* the scenario's
# read-only hc task expansion - it never fits, bootstraps, draws random numbers,
# or writes anything.

# ---- ssdsims_cost_calibration S3 object ------------------------------------

#' Construct an `ssdsims_cost_calibration` object.
#'
#' Small, serialisable S3 object carrying the fitted per-`ci_method`
#' coefficients, the bounded `nrow` factor, a fixed sample+fit per-task addend,
#' and provenance. `ssd_calibrate_cost()` builds it on a target machine; the
#' shipped default is read via `ssd_cost_calibration()`.
#' @noRd
new_ssdsims_cost_calibration <- function(
  coefficients,
  nrow_factor,
  fixed_addend,
  provenance
) {
  structure(
    list(
      coefficients = tibble::as_tibble(coefficients),
      nrow_factor = tibble::as_tibble(nrow_factor),
      fixed_addend = fixed_addend,
      provenance = provenance
    ),
    class = "ssdsims_cost_calibration"
  )
}

#' @export
#' @noRd
format.ssdsims_cost_calibration <- function(x, ...) {
  prov <- x$provenance
  coef <- x$coefficients[order(x$coefficients$slope), ]
  coef_lines <- sprintf(
    "    %-18s base %6.2fs  %7.2f ms/boot  n0 %3.0f",
    coef$ci_method,
    coef$base,
    coef$slope * 1000,
    coef$n0
  )
  nrf <- x$nrow_factor
  nrf_line <- paste(
    sprintf("%g:%.2f", nrf$nrow, nrf$factor),
    collapse = "  "
  )
  c(
    "<ssdsims_cost_calibration>",
    "  per-ci_method cost model  time = (base + slope * max(nboot, n0)) * nrow_factor",
    coef_lines,
    sprintf("  nrow_factor:   %s", nrf_line),
    sprintf("  fixed_addend:  %.2fs (sample + fit per task)", x$fixed_addend),
    "  provenance:",
    sprintf("    cpu:       %s", prov$cpu),
    sprintf("    R:         %s", prov$r_version),
    sprintf("    ssdtools:  %s", prov$ssdtools_version),
    sprintf("    date:      %s", format(prov$date)),
    sprintf(
      "    sweep:     nboot {%s} x nrow {%s} x %d ci_methods",
      paste(prov$sweep_grid$nboot, collapse = ", "),
      paste(prov$sweep_grid$nrow, collapse = ", "),
      length(prov$sweep_grid$ci_method)
    ),
    "  Ballpark only: coefficients are architecture-specific - recalibrate with",
    "  ssd_calibrate_cost() on the target machine for a trustworthy estimate."
  )
}

#' @export
#' @noRd
print.ssdsims_cost_calibration <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  cat("\n")
  invisible(x)
}

#' Default Cost Calibration
#'
#' Returns the `ssdsims_cost_calibration` object shipped with the package - the
#' calibration fitted during development (see
#' [ssd_cost_calibration_default]) and used by [ssd_estimate_cost()] when no
#' `calibration` is supplied. Because the coefficients are architecture-specific,
#' an estimate built on this default is a *ballpark* sized for the machine in its
#' provenance; rerun [ssd_calibrate_cost()] on your own machine for a
#' trustworthy estimate.
#'
#' @return The shipped `ssdsims_cost_calibration` object.
#' @seealso [ssd_calibrate_cost()] to re-fit on a target machine and
#'   [ssd_estimate_cost()] to apply a calibration to a scenario.
#' @export
#' @examples
#' ssd_cost_calibration()
ssd_cost_calibration <- function() {
  env <- new.env(parent = emptyenv())
  utils::data(
    "ssd_cost_calibration_default",
    package = "ssdsims",
    envir = env
  )
  env$ssd_cost_calibration_default
}

# ---- calibration harness ---------------------------------------------------

#' Calibrate the Per-task Cost Model on the Current Machine
#'
#' Runs a small, fixed benchmark sweep on the *current* architecture - tiny
#' `nboot` values, all `ssdtools::ssd_ci_methods()`, and a couple of `nrow`
#' values - times each `ssdtools::ssd_hc()` call, fits the per-`ci_method` cost
#' model `time = (base + slope * max(nboot, n0)) * nrow_factor(nrow)`, and returns
#' a versioned `ssdsims_cost_calibration` object carrying the fitted coefficients
#' and provenance. Producing an architecture-specific estimator is nothing more
#' than this single call plus passing the result to [ssd_estimate_cost()].
#'
#' The sweep is self-contained and dependency-light: it draws data by resampling
#' a reference dataset (`ssddata::ccme_boron` by default) and times with base
#' [system.time()]. It takes minutes - not the hours a real scenario costs -
#' because the `nboot` values are tiny (the slope and floor are estimable from
#' small bootstraps). The one-time research that discovered the model's *form*
#' (which axes are free, the `max(nboot, n0)` shape, the non-monotonic `nrow`
#' factor) is preserved under the change's `exploration/` directory; it is not
#' rerun here.
#'
#' @param nboot An integer vector of tiny bootstrap sizes to sweep (the slope and
#'   floor are fit over these).
#' @param nrow An integer vector of at least two sample sizes to fit one model
#'   per (the `nrow_factor` is derived from these).
#' @param data A reference data frame with a numeric `Conc` column to resample
#'   (default `ssddata::ccme_boron`).
#' @param seed A whole number seeding the resampling so the sweep is reproducible.
#' @return An `ssdsims_cost_calibration` object with per-`ci_method`
#'   `base`/`slope`/`n0` coefficients, a bounded `nrow_factor`, a `fixed_addend`,
#'   and provenance (cpu, R version, ssdtools version, date, sweep grid).
#' @seealso [ssd_cost_calibration()] for the shipped default and
#'   [ssd_estimate_cost()] to apply the result.
#' @export
#' @examples
#' \dontrun{
#' calibration <- ssd_calibrate_cost()
#' calibration
#' }
ssd_calibrate_cost <- function(
  nboot = c(20L, 50L, 100L, 200L),
  nrow = c(5L, 10L, 20L, 50L),
  data = NULL,
  seed = 42L
) {
  chk::chk_whole_numeric(nboot)
  chk::chk_gt(nboot)
  chk::chk_unique(nboot)
  chk::chk_length(nboot, length = 2L, upper = Inf)
  chk::chk_whole_numeric(nrow)
  chk::chk_gt(nrow)
  chk::chk_unique(nrow)
  chk::chk_length(nrow, length = 2L, upper = Inf)
  chk::chk_whole_number(seed)
  if (is.null(data)) {
    if (!requireNamespace("ssddata", quietly = TRUE)) {
      chk::abort_chk(
        "`ssd_calibrate_cost()` needs the `ssddata` package for its default ",
        "reference dataset; install it or pass `data`."
      )
    }
    data <- ssddata::ccme_boron
  }
  chk::chk_data(data)
  if (!"Conc" %in% names(data)) {
    chk::abort_chk("`data` must have a numeric `Conc` column.")
  }
  nboot <- as.integer(nboot)
  nrow <- as.integer(nrow)
  ci_methods <- ssdtools::ssd_ci_methods()

  conc <- data$Conc
  # Pre-fit one bcanz model per `nrow`, timing the fit so a representative
  # sample+fit overhead can be reported as `fixed_addend`. `nrow = 5L` relaxes
  # ssd_fit_dists()'s 6-row floor, matching ssdsims' internal fit call.
  withr::local_seed(seed)
  fits <- vector("list", length(nrow))
  fit_secs <- numeric(length(nrow))
  for (i in seq_along(nrow)) {
    d <- data.frame(Conc = sample(conc, nrow[[i]], replace = TRUE))
    fit_secs[[i]] <- as.numeric(system.time(
      fit <- ssdtools::ssd_fit_dists(
        d,
        dists = ssdtools::ssd_dists_bcanz(),
        nrow = 5L
      )
    )["elapsed"])
    fits[[i]] <- fit
  }

  # Sweep ssd_hc() over nrow x ci_method x nboot, timing each call.
  rows <- list()
  k <- 0L
  for (i in seq_along(nrow)) {
    for (cim in ci_methods) {
      for (nb in nboot) {
        elapsed <- as.numeric(system.time(suppressWarnings(ssdtools::ssd_hc(
          fits[[i]],
          ci = TRUE,
          nboot = nb,
          ci_method = cim,
          parametric = TRUE,
          min_pboot = 0
        )))["elapsed"])
        k <- k + 1L
        rows[[k]] <- data.frame(
          nrow = nrow[[i]],
          ci_method = cim,
          nboot = nb,
          time = elapsed
        )
      }
    }
  }
  sweep <- do.call(rbind, rows)

  coefficients <- calibrate_coefficients(sweep, ci_methods)
  nrow_factor <- calibrate_nrow_factor(sweep, coefficients)

  provenance <- list(
    cpu = cost_cpu_info(),
    r_version = R.version.string,
    ssdtools_version = as.character(utils::packageVersion("ssdtools")),
    date = Sys.Date(),
    sweep_grid = list(nboot = nboot, nrow = nrow, ci_method = ci_methods)
  )

  new_ssdsims_cost_calibration(
    coefficients = coefficients,
    nrow_factor = nrow_factor,
    fixed_addend = mean(fit_secs),
    provenance = provenance
  )
}

# Fit `time ~ pmax(nboot, n0)` per `ci_method`, choosing `n0` by minimising
# residual SD over a small grid (the protocol from the session sweeps). Pools
# over `nrow` (the nrow_factor captures residual nrow variation separately).
calibrate_coefficients <- function(sweep, ci_methods) {
  # `n0` is a bootstrap floor, so it must stay below the largest swept `nboot`;
  # capping the grid there also keeps `pmax(nboot, n0)` non-constant, so the
  # per-ci_method `lm()` is never singular.
  n0_grid <- seq(0L, max(sweep$nboot) - 1L, by = 5L)
  rows <- lapply(ci_methods, function(cim) {
    d <- sweep[sweep$ci_method == cim, ]
    sigmas <- vapply(
      n0_grid,
      function(n0) summary(stats::lm(time ~ pmax(nboot, n0), d))$sigma,
      numeric(1)
    )
    n0 <- n0_grid[[which.min(sigmas)]]
    fit <- stats::lm(time ~ pmax(nboot, n0), d)
    tibble::tibble(
      ci_method = cim,
      base = unname(stats::predict(fit, data.frame(nboot = 0L))),
      slope = unname(stats::coef(fit)[[2]]),
      n0 = as.numeric(n0)
    )
  })
  dplyr::bind_rows(rows)
}

# Derive the bounded, non-monotonic `nrow` factor: the mean ratio of observed
# time to the per-ci_method prediction *without* the factor, per `nrow`. Centred
# near 1 (the base/slope are the pooled-nrow coefficients), so a typical nrow
# estimates near the pooled prediction; cheap small-`nrow` fits (most bcanz
# dists fail) and the easing at large `nrow` show up as factors below 1.
calibrate_nrow_factor <- function(sweep, coefficients) {
  idx <- match(sweep$ci_method, coefficients$ci_method)
  pred0 <- (coefficients$base[idx] +
    coefficients$slope[idx] * pmax(sweep$nboot, coefficients$n0[idx]))
  ratio <- sweep$time / pred0
  agg <- tapply(ratio, sweep$nrow, mean)
  tibble::tibble(
    nrow = as.integer(names(agg)),
    factor = as.numeric(agg)
  )
}

# CPU description from /proc/cpuinfo (Linux) with a portable fallback; no
# benchmarkme dependency.
cost_cpu_info <- function() {
  if (file.exists("/proc/cpuinfo")) {
    lines <- readLines("/proc/cpuinfo", warn = FALSE)
    model <- grep("^model name", lines, value = TRUE)
    if (length(model)) {
      return(trimws(sub("^model name\\s*:\\s*", "", model[[1]])))
    }
  }
  sysname <- Sys.info()[["machine"]]
  if (!is.null(sysname) && nzchar(sysname)) sysname else "unknown CPU"
}

# ---- estimator -------------------------------------------------------------

#' Estimate a Scenario's Compute Cost and Longest Task
#'
#' Predicts, *before* a scenario is launched, roughly how much compute it costs
#' and how long its single longest task runs. It expands the scenario into its hc
#' task table read-only (via [ssd_scenario_hc_tasks()], without running any fit or
#' bootstrap), applies the calibrated per-task cost model, and returns the
#' ballpark serial **total** cost and the **duration of the longest single task**,
#' plus a per-axis breakdown of which `ci_method`/`nboot` cells dominate.
#'
#' `proportion` and `est_method` are *free* axes: one bootstrap per
#' `nboot x ci_method x parametric` cell serves every `proportion`/`est_method`,
#' so adding values along those axes does not change the estimate. The estimator
#' does **not** execute the scenario, draw random numbers, or alter any result.
#'
#' The longest task is the irreducible wall-time floor under any amount of
#' parallelism; wall-time under `n` workers is roughly
#' `max(longest_task, total / n)`, computed by the caller.
#'
#' @param scenario An `ssdsims_scenario` from [ssd_define_scenario()].
#' @param calibration An `ssdsims_cost_calibration`; defaults to the shipped
#'   [ssd_cost_calibration()]. Pass the result of [ssd_calibrate_cost()] for an
#'   architecture-specific estimate.
#' @return An `ssdsims_cost_estimate` object: a list with `total` and `longest`
#'   (both `difftime` time quantities), a `breakdown` tibble grouped by
#'   `ci_method` x `nboot`, and the calibration's `provenance`.
#' @seealso [ssd_calibrate_cost()] and [ssd_cost_calibration()].
#' @export
#' @examples
#' data <- ssd_scenario_data(ssddata::ccme_boron)
#' scenario <- ssd_define_scenario(
#'   data,
#'   nsim = 10L,
#'   seed = 42L,
#'   ci = TRUE,
#'   nboot = c(1000L, 5000L, 10000L, 50000L),
#'   nrow = c(5L, 10L, 20L, 50L)
#' )
#' ssd_estimate_cost(scenario)
ssd_estimate_cost <- function(scenario, calibration = ssd_cost_calibration()) {
  chk::chk_s3_class(scenario, "ssdsims_scenario")
  chk::chk_s3_class(calibration, "ssdsims_cost_calibration")

  hc <- ssd_scenario_hc_tasks(scenario)

  # One bootstrap per cost-bearing cell. Since `est-method-setting`, neither
  # `est_method` nor `proportion` is an hc task axis - `est_method` is an hc
  # *setting* summarised within a task from its single bootstrap sample set, and
  # `proportion` is vectorised into one `ssd_hc()` call - so the task expansion
  # already carries "one bootstrap per `nboot x ci_method x parametric` cell" and
  # neither axis multiplies the estimate. `ci` rides as a carried column and
  # selects bootstrap vs the cheap analytical addend.
  cost_axes <- task_axes("hc")
  tasks <- dplyr::distinct(hc[c(cost_axes, "ci")])
  tasks$seconds <- cost_task_seconds(tasks, calibration)

  breakdown <- dplyr::arrange(
    dplyr::summarise(
      tasks,
      tasks = dplyr::n(),
      seconds = sum(.data$seconds),
      .by = c("ci_method", "nboot")
    ),
    dplyr::desc(.data$seconds)
  )

  structure(
    list(
      total = as.difftime(sum(tasks$seconds), units = "secs"),
      longest = as.difftime(max(tasks$seconds), units = "secs"),
      breakdown = breakdown,
      provenance = calibration$provenance
    ),
    class = "ssdsims_cost_estimate"
  )
}

# Per-task seconds for each cost-bearing cell. For `ci = TRUE`:
#   (base + slope * max(nboot, n0)) * nrow_factor(nrow) + fixed_addend
# For `ci = FALSE` there is no bootstrap, so the task is the cheap analytical
# `fixed_addend` only. Vectorised over rows.
cost_task_seconds <- function(tasks, calibration) {
  coef <- calibration$coefficients
  idx <- match(tasks$ci_method, coef$ci_method)
  boot <- (coef$base[idx] +
    coef$slope[idx] * pmax(tasks$nboot, coef$n0[idx])) *
    nrow_factor_at(calibration, tasks$nrow)
  ifelse(
    tasks$ci,
    boot + calibration$fixed_addend,
    calibration$fixed_addend
  )
}

# The bounded `nrow` factor at arbitrary `nrow`: linear interpolation between
# calibrated points, clamped (rule = 2) to the measured range so it is never
# extrapolated beyond the data. A single calibrated point degenerates to a flat
# factor.
nrow_factor_at <- function(calibration, nrow) {
  nrf <- calibration$nrow_factor
  if (base::nrow(nrf) == 1L) {
    return(rep(nrf$factor, length(nrow)))
  }
  stats::approx(nrf$nrow, nrf$factor, xout = nrow, rule = 2)$y
}

#' @export
#' @noRd
format.ssdsims_cost_estimate <- function(x, ...) {
  prov <- x$provenance
  bd <- x$breakdown
  bd_lines <- sprintf(
    "    %-18s nboot %6s  %4d tasks  %s",
    bd$ci_method,
    ifelse(is.na(bd$nboot), "-", as.character(bd$nboot)),
    bd$tasks,
    vapply(as.numeric(bd$seconds), format_duration, character(1))
  )
  c(
    "<ssdsims_cost_estimate>  (ballpark, serial)",
    sprintf(
      "  total compute:  %s",
      format_duration(as.numeric(x$total, units = "secs"))
    ),
    sprintf(
      "  longest task:   %s",
      format_duration(as.numeric(x$longest, units = "secs"))
    ),
    "  breakdown (ci_method x nboot, by total cost):",
    bd_lines,
    sprintf(
      "  calibration:    %s | R %s | ssdtools %s | %s",
      prov$cpu,
      sub("^R version ([0-9.]+).*", "\\1", prov$r_version),
      prov$ssdtools_version,
      format(prov$date)
    ),
    "  Ballpark only - recalibrate with ssd_calibrate_cost() on the target machine."
  )
}

#' @export
#' @noRd
print.ssdsims_cost_estimate <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  cat("\n")
  invisible(x)
}

# Render a duration in seconds in human-friendly units (days/hours/minutes/
# seconds), for the cost-estimate print method.
format_duration <- function(seconds) {
  if (is.na(seconds)) {
    return("NA")
  }
  if (seconds >= 86400) {
    sprintf("%.1f days", seconds / 86400)
  } else if (seconds >= 3600) {
    sprintf("%.1f hours", seconds / 3600)
  } else if (seconds >= 60) {
    sprintf("%.1f min", seconds / 60)
  } else {
    sprintf("%.1f s", seconds)
  }
}
