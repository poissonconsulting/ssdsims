# Observed cost analysis: read the per-task `.start`/`.end`/`.host` timings a run
# left in its `fit`/`hc` shard Parquets (the cost-analysis instrumentation),
# attribute the observed compute to the scenario's axes, compare it against the
# `ssd_estimate_cost()` prediction, and recalibrate the per-task cost model from
# the measured durations. All functions here are read-only: they read result
# Parquets (and, optionally, a `targets` meta store), never running the pipeline,
# fitting a distribution, drawing random numbers, or writing a file.
#
# The design-level (`ssdsims_design`) methods are gated on `scenario-combine` and
# are not built here; a prototype of their rollup seam lives under the change's
# `exploration/design-rollup-seam/`.

# ---- reading measured timings off the shards -------------------------------

# Read one step's per-task timing rows off the shard glob, projecting only the
# id and timing columns at the DuckDB level (never decoding the `fit_blob`/
# `samples`/`dists` payloads). On `hc` the timing repeats across a task's rows,
# so the distinct `<id>` row is the per-task timing. Returns `NULL` when the glob
# matches no shard (an unrun step), and carries no timing columns flag when the
# shards predate the instrumentation.
read_step_timings <- function(root, step, id) {
  glob <- file.path(root, step, "**", "part.parquet")
  rel <- tryCatch(
    duckplyr::read_parquet_duckdb(
      glob,
      options = list(hive_partitioning = FALSE)
    ),
    error = function(e) NULL
  )
  if (is.null(rel)) {
    return(NULL)
  }
  cols <- colnames(rel)
  if (!all(c(".start", ".end", ".host") %in% cols)) {
    # Pre-timing shards: return the ids only, flagged as carrying no timings.
    out <- tibble::as_tibble(dplyr::collect(dplyr::distinct(
      dplyr::select(rel, dplyr::all_of(id))
    )))
    attr(out, "timed") <- FALSE
    return(out)
  }
  out <- dplyr::select(rel, dplyr::all_of(c(id, ".start", ".end", ".host")))
  out <- tibble::as_tibble(dplyr::collect(dplyr::distinct(out)))
  out$seconds <- as.numeric(difftime(out$.end, out$.start, units = "secs"))
  attr(out, "timed") <- TRUE
  out
}

# The Hive path cell each task row maps to, for the named step's `partition_by`
# (the same projection `shard_path()`/the readers use), so per-task durations can
# be grouped to the shard target that produced them.
task_cells <- function(tasks, scenario, step) {
  path_key(tasks, scenario$partition_by[[step]])
}

# ---- targets meta resolver (tar_meta layer) --------------------------------

# Regenerate the shard target names the factory mints, keyed by Hive path cell,
# by rebuilding each step's `tar_map()` with the *same* `values`/`names` the
# factory uses and reading the minted names through `shard_cell_names()` - never
# string-parsing a name (axis values can contain separators). Returns a tibble of
# `step`, `cell`, `name`.
cost_shard_target_names <- function(scenario) {
  rlang::check_installed(c("targets", "tarchetypes"))
  steps <- c("sample", "fit", "hc")
  rows <- lapply(steps, function(step) {
    shards <- switch(
      step,
      sample = ssd_scenario_sample_shards(scenario),
      fit = ssd_scenario_fit_shards(scenario),
      hc = ssd_scenario_hc_shards(scenario)
    )
    # Mirror the factory's naming: the `seed` is woven into every shard target
    # name (`scenario_results_dir()` keys the tree on `seed=` too), so the
    # resolver must carry the `seed` column and lead the `names` with it.
    shards$seed <- scenario$seed
    mapped <- tarchetypes::tar_map(
      values = shards,
      names = tidyselect::all_of(
        c("seed", scenario_partition_axes(scenario, step)$path)
      ),
      targets::tar_target_raw(paste0(step, "_step"), quote(NULL))
    )
    nm <- shard_cell_names(mapped, shards, scenario, step)
    tibble::tibble(step = step, cell = names(nm), name = unname(nm))
  })
  dplyr::bind_rows(rows)
}

# Read the `targets` meta store's per-target `name`/`seconds` (the only
# long-stable columns this depends on).
read_store_meta <- function(store) {
  rlang::check_installed("targets")
  meta <- targets::tar_meta(store = store, fields = "seconds")
  tibble::as_tibble(meta[, c("name", "seconds")])
}

# Match a `tar_meta()`-shaped `meta` (`name`, `seconds`) to the regenerated shard
# target names, resolving each shard target to its `(step, cell)`. The combined
# `summary` and any `upload_<step>` targets are excluded from shard attribution;
# `NA`-seconds (errored/unbuilt) targets are dropped from totals; unmatched stored
# targets are reported, never silently dropped. Pure in `meta`/`shard_names` so
# the envelope path is testable with a synthesised meta tibble.
match_store_seconds <- function(meta, shard_names) {
  shard_meta <- meta[meta$name %in% shard_names$name, ]
  n_na <- sum(is.na(shard_meta$seconds))
  shard_meta <- shard_meta[!is.na(shard_meta$seconds), ]
  resolved <- dplyr::inner_join(shard_meta, shard_names, by = "name")
  non_shard <- meta$name[!meta$name %in% shard_names$name]
  unmatched <- non_shard[!grepl("^(summary|upload_)", non_shard)]
  list(
    seconds = tibble::as_tibble(resolved),
    n_na = n_na,
    unmatched = unmatched
  )
}

# ---- the cost-analysis object ----------------------------------------------

new_ssdsims_cost_analysis <- function(
  total,
  longest,
  breakdown,
  fit_seconds,
  hosts,
  measured,
  envelope,
  provenance
) {
  structure(
    list(
      total = total,
      longest = longest,
      breakdown = breakdown,
      fit_seconds = fit_seconds,
      hosts = hosts,
      measured = measured,
      envelope = envelope,
      provenance = provenance
    ),
    class = "ssdsims_cost_analysis"
  )
}

#' Analyse a Run's Observed Compute Cost
#'
#' Reads the per-task `.start`/`.end`/`.host` timings a completed run left in its
#' `fit`/`hc` shard Parquets (the cost-analysis instrumentation), and attributes
#' the **observed** compute to the scenario's `ci_method` x `nboot` axes - the
#' measured counterpart to [ssd_estimate_cost()]'s prediction. It is strictly
#' read-only: it reads result Parquets (and, optionally, a `targets` meta store),
#' and never runs the pipeline, fits a distribution, draws random numbers, or
#' writes a file. The observed **total** is serial-equivalent compute (the sum of
#' per-task durations), distinct from elapsed wall time under parallel workers.
#'
#' Given a `targets` `store`, it additionally reads each shard target's wall
#' `seconds` from `targets::tar_meta()` and reports the per-shard **envelope
#' overhead** (`target seconds - sum(task durations)`: parent read, Parquet write,
#' and dispatch), the number that informs `partition_by` tuning. The combined
#' `summary` and `upload_<step>` targets are excluded; errored/unbuilt
#' (`NA`-seconds) targets are dropped from totals; unmatched stored targets are
#' reported, never silently dropped.
#'
#' @param scenario An `ssdsims_scenario` from [ssd_define_scenario()].
#' @param root The run's results root (the `fit`/`hc` shard trees live under it).
#'   Defaults to [scenario_results_dir()] for `scenario`.
#' @param store Optional path to the run's `targets` data store, to add the
#'   per-shard envelope overhead. `NULL` (the default) reads the shards only.
#' @return An `ssdsims_cost_analysis`: a list with `total` and `longest` (both
#'   `difftime`), a `breakdown` tibble grouped by `ci_method` x `nboot`,
#'   `fit_seconds` (the measured fit addend), the `hosts` seen, a `measured` flag,
#'   the per-shard `envelope` (when a `store` is given), and provenance.
#' @seealso [ssd_estimate_cost()], [ssd_compare_cost()],
#'   [ssd_calibrate_cost_from_run()].
#' @export
ssd_analyse_cost <- function(
  scenario,
  root = scenario_results_dir(scenario),
  store = NULL
) {
  UseMethod("ssd_analyse_cost")
}

#' @export
ssd_analyse_cost.ssdsims_scenario <- function(
  scenario,
  root = scenario_results_dir(scenario),
  store = NULL
) {
  chk::chk_string(root)
  if (!is.null(store)) {
    chk::chk_string(store)
  }

  hc <- read_step_timings(root, "hc", "hc_id")
  fit <- read_step_timings(root, "fit", "fit_id")
  if (is.null(hc)) {
    chk::abort_chk(
      "No `hc` shards found under ",
      encodeString(root, quote = "\""),
      "; nothing to analyse (has the run completed?)."
    )
  }
  measured <- isTRUE(attr(hc, "timed"))

  # Attribute hc durations to the cost axes (`ci_method` x `nboot`) by joining the
  # per-task timing to the scenario's hc task table on `hc_id` (the table also
  # supplies `nrow` for recalibration). The shard echoes `ci_method`/`nboot`, but
  # the task table is the authoritative axis source.
  hc_tasks <- ssd_scenario_hc_tasks(scenario)
  axes <- hc_tasks[, c("hc_id", "nrow", "ci_method", "nboot")]

  if (!measured) {
    return(analyse_inferred(scenario, root, store, hc, fit, axes))
  }

  hc <- dplyr::left_join(hc, axes, by = "hc_id")
  breakdown <- dplyr::arrange(
    dplyr::summarise(
      hc,
      tasks = dplyr::n(),
      seconds = sum(.data$seconds),
      .by = c("ci_method", "nboot")
    ),
    dplyr::desc(.data$seconds)
  )
  fit_seconds <- if (is.null(fit)) 0 else sum(fit$seconds)
  hc_total <- sum(hc$seconds)
  all_task_seconds <- c(hc$seconds, if (!is.null(fit)) fit$seconds)
  hosts <- unique(c(hc$.host, if (!is.null(fit)) fit$.host))

  envelope <- NULL
  unmatched <- character()
  n_na <- 0L
  if (!is.null(store)) {
    env <- shard_envelope(scenario, read_store_meta(store), hc, fit)
    envelope <- env$envelope
    unmatched <- env$unmatched
    n_na <- env$n_na
  }

  new_ssdsims_cost_analysis(
    total = as.difftime(hc_total + fit_seconds, units = "secs"),
    longest = as.difftime(max(all_task_seconds), units = "secs"),
    breakdown = breakdown,
    fit_seconds = as.difftime(fit_seconds, units = "secs"),
    hosts = hosts,
    measured = TRUE,
    envelope = envelope,
    provenance = list(
      source = "run",
      root = root,
      store = store,
      date = Sys.Date(),
      n_hc_tasks = nrow(hc),
      n_fit_tasks = if (is.null(fit)) 0L else nrow(fit),
      unmatched_targets = unmatched,
      na_targets = n_na
    )
  )
}

# Per-shard envelope overhead: target wall seconds minus the summed measured task
# durations in that shard's cell, for `fit` and `hc` (the timed layers).
shard_envelope <- function(scenario, meta, hc, fit) {
  res <- match_store_seconds(meta, cost_shard_target_names(scenario))
  hc_tasks <- ssd_scenario_hc_tasks(scenario)
  fit_tasks <- ssd_scenario_fit_tasks(scenario)
  # Map each task's duration to its shard cell, then sum per (step, cell).
  hc_cell <- data.frame(
    hc_id = hc_tasks$hc_id,
    cell = task_cells(hc_tasks, scenario, "hc"),
    stringsAsFactors = FALSE
  )
  hc_dur <- dplyr::left_join(hc[, c("hc_id", "seconds")], hc_cell, by = "hc_id")
  per_cell_hc <- dplyr::summarise(
    hc_dur,
    task_seconds = sum(.data$seconds),
    step = "hc",
    .by = "cell"
  )
  per_cell <- per_cell_hc
  if (!is.null(fit)) {
    fit_cell <- data.frame(
      fit_id = fit_tasks$fit_id,
      cell = task_cells(fit_tasks, scenario, "fit"),
      stringsAsFactors = FALSE
    )
    fit_dur <- dplyr::left_join(
      fit[, c("fit_id", "seconds")],
      fit_cell,
      by = "fit_id"
    )
    per_cell_fit <- dplyr::summarise(
      fit_dur,
      task_seconds = sum(.data$seconds),
      step = "fit",
      .by = "cell"
    )
    per_cell <- dplyr::bind_rows(per_cell_hc, per_cell_fit)
  }
  envelope <- dplyr::inner_join(
    res$seconds,
    per_cell,
    by = c("step", "cell")
  )
  envelope$overhead <- envelope$seconds - envelope$task_seconds
  list(
    envelope = tibble::as_tibble(envelope[, c(
      "step",
      "cell",
      "name",
      "seconds",
      "task_seconds",
      "overhead"
    )]),
    unmatched = res$unmatched,
    n_na = res$n_na
  )
}

# Pre-timing fallback: the shards carry no `.start`/`.end`, so observed per-task
# durations are unavailable. With a `store`, attribute each shard's wall seconds
# to its tasks in proportion to the *predicted* per-task cost and mark the result
# inferred; without a store there is nothing measured to report.
analyse_inferred <- function(scenario, root, store, hc, fit, axes) {
  if (is.null(store)) {
    chk::abort_chk(
      "The `fit`/`hc` shards under ",
      encodeString(root, quote = "\""),
      " carry no timing columns (a pre-instrumentation run) and no `store` was ",
      "given, so there is no observed cost to report. Re-run with the current ",
      "package to capture per-task timings, or pass the run's `targets` `store`."
    )
  }
  res <- match_store_seconds(
    read_store_meta(store),
    cost_shard_target_names(scenario)
  )
  # Predicted per-task cost gives the attribution weights.
  est <- ssd_estimate_cost(scenario)
  hc_tasks <- ssd_scenario_hc_tasks(scenario)
  pred <- dplyr::left_join(
    est$breakdown,
    dplyr::summarise(
      hc_tasks,
      tasks = dplyr::n(),
      .by = c("ci_method", "nboot")
    ),
    by = c("ci_method", "nboot")
  )
  total_seconds <- sum(res$seconds$seconds)
  new_ssdsims_cost_analysis(
    total = as.difftime(total_seconds, units = "secs"),
    longest = as.difftime(NA_real_, units = "secs"),
    breakdown = est$breakdown,
    fit_seconds = as.difftime(NA_real_, units = "secs"),
    hosts = NA_character_,
    measured = FALSE,
    envelope = NULL,
    provenance = list(
      source = "store (inferred)",
      root = root,
      store = store,
      date = Sys.Date(),
      unmatched_targets = res$unmatched,
      na_targets = res$n_na
    )
  )
}

#' @export
format.ssdsims_cost_analysis <- function(x, ...) {
  bd <- x$breakdown
  bd_lines <- sprintf(
    "    %-22s nboot %6s  %4d tasks  %s",
    ifelse(is.na(bd$ci_method), "-", bd$ci_method),
    ifelse(is.na(bd$nboot), "-", as.character(bd$nboot)),
    bd$tasks,
    vapply(as.numeric(bd$seconds), format_duration, character(1))
  )
  tag <- if (isTRUE(x$measured)) "measured" else "inferred (pre-timing)"
  lines <- c(
    sprintf("<ssdsims_cost_analysis>  (observed, serial-equivalent; %s)", tag),
    sprintf(
      "  total compute:  %s",
      format_duration(as.numeric(x$total, units = "secs"))
    ),
    sprintf(
      "  longest task:   %s",
      format_duration(as.numeric(x$longest, units = "secs"))
    ),
    "  breakdown (ci_method x nboot, by total cost):",
    bd_lines
  )
  if (!is.null(x$envelope)) {
    lines <- c(
      lines,
      sprintf(
        "  shard envelope: %d shards, %s overhead total",
        nrow(x$envelope),
        format_duration(sum(x$envelope$overhead))
      )
    )
  }
  if (length(x$hosts) && !anyNA(x$hosts)) {
    lines <- c(
      lines,
      sprintf("  host(s):        %s", paste(x$hosts, collapse = ", "))
    )
  }
  lines
}

#' @export
print.ssdsims_cost_analysis <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  cat("\n")
  invisible(x)
}

# ---- recalibration from a run ----------------------------------------------

#' Recalibrate the Cost Model from an Observed Run
#'
#' Re-fits the per-task cost model from a run's **measured** hc task durations
#' (the cost-analysis timings), returning an `ssdsims_cost_calibration` of the
#' same shape [ssd_calibrate_cost()] produces - so it drops straight into
#' [ssd_estimate_cost()] - but derived from real measurements rather than the
#' synthetic micro-benchmark. The fixed addend comes from the measured fit task
#' durations. Read-only: no pipeline, no RNG, no writes.
#'
#' Because the calibration is architecture-specific, timings from different
#' `.host` values are never silently pooled: a run spanning more than one host
#' requires an explicit `host`, or the function aborts listing the hosts found.
#'
#' @inheritParams ssd_analyse_cost
#' @param host Optional CPU description (a `.host` value) to select when the run
#'   spans more than one host. `NULL` (the default) requires a single-host run.
#' @return An `ssdsims_cost_calibration` with run-derived provenance.
#' @seealso [ssd_calibrate_cost()], [ssd_analyse_cost()].
#' @export
ssd_calibrate_cost_from_run <- function(
  scenario,
  root = scenario_results_dir(scenario),
  host = NULL
) {
  chk::chk_s3_class(scenario, "ssdsims_scenario")
  chk::chk_string(root)
  if (!is.null(host)) {
    chk::chk_string(host)
  }
  hc <- read_step_timings(root, "hc", "hc_id")
  fit <- read_step_timings(root, "fit", "fit_id")
  if (is.null(hc) || !isTRUE(attr(hc, "timed"))) {
    chk::abort_chk(
      "Recalibration needs measured hc task timings; the `hc` shards under ",
      encodeString(root, quote = "\""),
      " are missing or carry no timing columns."
    )
  }
  hosts <- unique(hc$.host)
  if (is.null(host)) {
    if (length(hosts) > 1L) {
      chk::abort_chk(
        "The run spans more than one `.host` (",
        paste0("`", hosts, "`", collapse = ", "),
        "); pass `host` to pick one - architecture-specific calibrations are ",
        "not pooled across hosts."
      )
    }
    host <- hosts[[1L]]
  } else if (!host %in% hosts) {
    chk::abort_chk(
      "`host` ",
      encodeString(host, quote = "\""),
      " is not among the run's hosts (",
      paste0("`", hosts, "`", collapse = ", "),
      ")."
    )
  }
  hc <- hc[hc$.host == host, ]
  axes <- ssd_scenario_hc_tasks(scenario)[, c(
    "hc_id",
    "nrow",
    "ci_method",
    "nboot"
  )]
  sweep <- dplyr::left_join(hc[, c("hc_id", "seconds")], axes, by = "hc_id")
  sweep <- data.frame(
    nrow = sweep$nrow,
    ci_method = sweep$ci_method,
    nboot = sweep$nboot,
    time = sweep$seconds,
    stringsAsFactors = FALSE
  )
  # Drop ci = FALSE rows (NA bootstrap axes): they carry the analytical addend,
  # not a bootstrap cost, so they do not inform the bootstrap coefficients.
  sweep <- sweep[!is.na(sweep$ci_method) & !is.na(sweep$nboot), ]
  if (!nrow(sweep)) {
    chk::abort_chk(
      "No bootstrap (`ci = TRUE`) hc tasks in the run to recalibrate from."
    )
  }
  ci_methods <- unique(sweep$ci_method)
  coefficients <- calibrate_coefficients(sweep, ci_methods)
  nrow_factor <- calibrate_nrow_factor(sweep, coefficients)
  fixed_addend <- if (is.null(fit) || !isTRUE(attr(fit, "timed"))) {
    0
  } else {
    mean(fit$seconds[fit$.host == host])
  }
  new_ssdsims_cost_calibration(
    coefficients = coefficients,
    nrow_factor = nrow_factor,
    fixed_addend = fixed_addend,
    provenance = list(
      cpu = host,
      r_version = R.version.string,
      ssdtools_version = as.character(utils::packageVersion("ssdtools")),
      date = Sys.Date(),
      source = "run",
      root = root
    )
  )
}

# ---- predicted vs observed comparison --------------------------------------

new_ssdsims_cost_comparison <- function(
  predicted,
  observed,
  total_ratio,
  longest_ratio
) {
  structure(
    list(
      predicted = predicted,
      observed = observed,
      total_ratio = total_ratio,
      longest_ratio = longest_ratio
    ),
    class = "ssdsims_cost_comparison"
  )
}

#' Compare Predicted Against Observed Compute Cost
#'
#' Places the [ssd_estimate_cost()] prediction beside the [ssd_analyse_cost()]
#' observation for one scenario+run and reports the predicted and observed total
#' compute, the predicted and observed longest task, and the predicted/observed
#' ratio for each. Read-only.
#'
#' @inheritParams ssd_analyse_cost
#' @param calibration An `ssdsims_cost_calibration` for the prediction; defaults
#'   to the shipped [ssd_cost_calibration()].
#' @return An `ssdsims_cost_comparison`.
#' @seealso [ssd_estimate_cost()], [ssd_analyse_cost()].
#' @export
ssd_compare_cost <- function(
  scenario,
  root = scenario_results_dir(scenario),
  store = NULL,
  calibration = ssd_cost_calibration()
) {
  chk::chk_s3_class(scenario, "ssdsims_scenario")
  chk::chk_s3_class(calibration, "ssdsims_cost_calibration")
  predicted <- ssd_estimate_cost(scenario, calibration)
  observed <- ssd_analyse_cost(scenario, root = root, store = store)
  ratio <- function(obs, pred) {
    p <- as.numeric(pred, units = "secs")
    if (is.na(p) || p == 0) NA_real_ else as.numeric(obs, units = "secs") / p
  }
  new_ssdsims_cost_comparison(
    predicted = predicted,
    observed = observed,
    total_ratio = ratio(observed$total, predicted$total),
    longest_ratio = ratio(observed$longest, predicted$longest)
  )
}

#' @export
format.ssdsims_cost_comparison <- function(x, ...) {
  fd <- function(d) format_duration(as.numeric(d, units = "secs"))
  fr <- function(r) if (is.na(r)) "NA" else sprintf("%.2fx", r)
  c(
    "<ssdsims_cost_comparison>  (predicted vs observed)",
    sprintf(
      "  total compute:  predicted %s | observed %s | obs/pred %s",
      fd(x$predicted$total),
      fd(x$observed$total),
      fr(x$total_ratio)
    ),
    sprintf(
      "  longest task:   predicted %s | observed %s | obs/pred %s",
      fd(x$predicted$longest),
      fd(x$observed$longest),
      fr(x$longest_ratio)
    )
  )
}

#' @export
print.ssdsims_cost_comparison <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  cat("\n")
  invisible(x)
}
