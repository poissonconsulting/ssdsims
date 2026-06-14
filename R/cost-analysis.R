# Observed cost analysis: read the per-task `.start`/`.end`/`.host` timings a run
# left in its `fit`/`hc` shard Parquets (the cost-analysis instrumentation),
# attribute the observed compute to the scenario's axes, compare it against the
# `ssd_estimate_cost()` prediction, and recalibrate the per-task cost model from
# the measured durations. All functions here are read-only: they read result
# Parquets (and, optionally, a `targets` meta store), never running the pipeline,
# fitting a distribution, drawing random numbers, or writing a file.
#
# The three functions are generics: the scenario-level methods read one run's
# shard tree, and the design-level (`ssdsims_design`) methods roll observed cost
# up across a design's members - each read out of its seed group's SHARED
# `seed=/layout=` tree by filtering to the member's task ids (the collection
# rollup seam at the bottom of this file is promoted from the change's
# `exploration/design-rollup-seam/`).

# ---- reading measured timings off the shards -------------------------------

# Read one step's per-task timing rows off the shard glob, projecting only the
# id and timing columns at the DuckDB level (never decoding the `fit_blob`/
# `samples`/`dists` payloads). On `hc` the timing repeats across a task's rows,
# so the distinct `<id>` row is the per-task timing. Returns `NULL` when the glob
# matches no shard (an unrun step), and carries no timing columns flag when the
# shards predate the instrumentation.
read_step_timings <- function(root, step, id, keep_ids = NULL) {
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
  # A design member reads its own rows out of the shared seed-group shards by its
  # task ids (the `ssd_summarise_member()` projection), filtered at the DuckDB
  # level before anything is pulled into R.
  if (!is.null(keep_ids)) {
    rel <- dplyr::filter(rel, .data[[id]] %in% keep_ids)
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

  m <- measured_member(scenario, hc, fit)

  envelope <- NULL
  unmatched <- character()
  n_na <- 0L
  if (!is.null(store)) {
    env <- shard_envelope(scenario, read_store_meta(store), m$hc, fit)
    envelope <- env$envelope
    unmatched <- env$unmatched
    n_na <- env$n_na
  }

  new_ssdsims_cost_analysis(
    total = as.difftime(m$total, units = "secs"),
    longest = as.difftime(m$longest, units = "secs"),
    breakdown = m$breakdown,
    fit_seconds = as.difftime(m$fit_seconds, units = "secs"),
    hosts = m$hosts,
    measured = TRUE,
    envelope = envelope,
    provenance = list(
      source = "run",
      root = root,
      store = store,
      date = Sys.Date(),
      n_hc_tasks = m$n_hc,
      n_fit_tasks = m$n_fit,
      unmatched_targets = unmatched,
      na_targets = n_na
    )
  )
}

# Per-member measured cost from already-read (optionally id-filtered) hc/fit
# timing tibbles: join the hc timing to the scenario's `ci_method`/`nboot`/`nrow`
# axes on `hc_id`, summarise the `ci_method` x `nboot` breakdown (by total cost),
# and reduce to total/longest/fit-addend. Shared by the scenario method and the
# design rollup, which filters each member's ids out of the shared shards first.
measured_member <- function(scenario, hc, fit) {
  axes <- ssd_scenario_hc_tasks(scenario)[, c(
    "hc_id",
    "nrow",
    "ci_method",
    "nboot"
  )]
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
  list(
    hc = hc,
    breakdown = breakdown,
    total = hc_total + fit_seconds,
    fit_seconds = fit_seconds,
    longest = max(all_task_seconds),
    hosts = unique(c(hc$.host, if (!is.null(fit)) fit$.host)),
    n_hc = nrow(hc),
    n_fit = if (is.null(fit)) 0L else nrow(fit)
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
  # Design observed cost (a `scenario` column tags each member); the scenario-level
  # rendering below is byte-unchanged when no `scenario` column is present.
  if ("scenario" %in% names(bd)) {
    lines <- format_design_breakdown(
      bd,
      x$total,
      x$longest,
      x$provenance$n_contributing
    )
  } else {
    bd_lines <- sprintf(
      "    %-22s nboot %6s  %4d tasks  %s",
      ifelse(is.na(bd$ci_method), "-", bd$ci_method),
      ifelse(is.na(bd$nboot), "-", as.character(bd$nboot)),
      bd$tasks,
      vapply(as.numeric(bd$seconds), format_duration, character(1))
    )
    tag <- if (isTRUE(x$measured)) "measured" else "inferred (pre-timing)"
    lines <- c(
      sprintf(
        "<ssdsims_cost_analysis>  (observed, serial-equivalent; %s)",
        tag
      ),
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
  }
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
  UseMethod("ssd_calibrate_cost_from_run")
}

#' @export
ssd_calibrate_cost_from_run.ssdsims_scenario <- function(
  scenario,
  root = scenario_results_dir(scenario),
  host = NULL
) {
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
  host <- select_run_host(hc$.host, host)
  hc <- hc[hc$.host == host, ]
  sweep <- member_sweep(scenario, hc)
  if (!nrow(sweep)) {
    chk::abort_chk(
      "No bootstrap (`ci = TRUE`) hc tasks in the run to recalibrate from."
    )
  }
  sweep <- sweep[, c("nrow", "ci_method", "nboot", "time")]
  coefficients <- calibrate_coefficients(sweep, unique(sweep$ci_method))
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

# A run's measured hc tasks as a calibration sweep frame: join the per-task hc
# timing to the scenario's `nrow`/`ci_method`/`nboot` axes, keep `.host`, and drop
# the `ci = FALSE` rows (NA bootstrap axes carry the analytical addend, not a
# bootstrap cost, so they do not inform the bootstrap coefficients). The shape
# `calibrate_coefficients()` / `pool_calibration_from_frames()` consume.
member_sweep <- function(scenario, hc) {
  axes <- ssd_scenario_hc_tasks(scenario)[, c(
    "hc_id",
    "nrow",
    "ci_method",
    "nboot"
  )]
  s <- dplyr::left_join(
    hc[, c("hc_id", "seconds", ".host")],
    axes,
    by = "hc_id"
  )
  s <- tibble::tibble(
    nrow = s$nrow,
    ci_method = s$ci_method,
    nboot = s$nboot,
    time = s$seconds,
    .host = s$.host
  )
  s[!is.na(s$ci_method) & !is.na(s$nboot), ]
}

# Single-host selection shared by the run/design recalibrations: a `NULL` host
# requires exactly one host in the run; an explicit host must be present. The
# calibration is architecture-specific, so hosts are never silently pooled.
select_run_host <- function(hosts, host) {
  hosts <- unique(hosts)
  if (is.null(host)) {
    if (length(hosts) > 1L) {
      chk::abort_chk(
        "The run spans more than one `.host` (",
        paste0("`", hosts, "`", collapse = ", "),
        "); pass `host` to pick one - architecture-specific calibrations are ",
        "not pooled across hosts."
      )
    }
    return(hosts[[1L]])
  }
  if (!host %in% hosts) {
    chk::abort_chk(
      "`host` ",
      encodeString(host, quote = "\""),
      " is not among the run's hosts (",
      paste0("`", hosts, "`", collapse = ", "),
      ")."
    )
  }
  host
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
  UseMethod("ssd_compare_cost")
}

#' @export
ssd_compare_cost.ssdsims_scenario <- function(
  scenario,
  root = scenario_results_dir(scenario),
  store = NULL,
  calibration = ssd_cost_calibration()
) {
  chk::chk_s3_class(calibration, "ssdsims_cost_calibration")
  predicted <- ssd_estimate_cost(scenario, calibration)
  observed <- ssd_analyse_cost(scenario, root = root, store = store)
  cost_comparison(predicted, observed)
}

# Predicted-vs-observed comparison from a prediction and an observation (scenario
# or design): the obs/pred ratio for the total and the longest task.
cost_comparison <- function(predicted, observed) {
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

# ============================================================================
# Design-level rollup (ssdsims_design): aggregate observed cost across a
# design's member scenarios. `scenario-combine` writes one SHARED
# `<root>/seed=<value>/layout=<hash>` tree per seed group (a cell shared by
# several members built once, seed woven into the shard target names), so each
# member's cost is recovered by filtering the shared shards to its `hc_id`/
# `fit_id`s - the `ssd_summarise_member()` projection - never a per-member tree.
# The design total is per-member accounting (a shared cell counts once per
# member). The collection-agnostic seam below (combine/totals/pool) is promoted
# from the change's `exploration/design-rollup-seam/` proof of work.
# ============================================================================

# ---- collection-agnostic rollup seam (promoted from exploration) -----------

# Row-bind per-member cost breakdowns into one design breakdown with a leading
# `scenario` column holding each member's name; `NULL`/empty members drop out.
combine_cost_breakdowns <- function(named_breakdowns) {
  chk::chk_list(named_breakdowns)
  names <- rlang::names2(named_breakdowns)
  if (length(named_breakdowns) && any(!nzchar(names))) {
    chk::abort_chk("`named_breakdowns` must be fully named.")
  }
  keep <- !purrr::map_lgl(
    named_breakdowns,
    \(bd) is.null(bd) || nrow(bd) == 0L
  )
  named_breakdowns <- named_breakdowns[keep]
  if (!length(named_breakdowns)) {
    return(tibble::tibble(scenario = character()))
  }
  dplyr::bind_rows(named_breakdowns, .id = "scenario")
}

# Reduce per-member observed costs to design totals: total = Σ member totals,
# longest = max member longest (both `difftime` seconds), skipping `NA` members
# (no readable run) and reporting the contributing-member count. Inputs are named
# numeric vectors of seconds keyed by member name.
design_cost_totals <- function(member_totals, member_longests) {
  chk::chk_numeric(member_totals)
  chk::chk_numeric(member_longests)
  contributing <- !is.na(member_totals)
  longest <- if (any(contributing)) {
    max(member_longests[contributing], na.rm = TRUE)
  } else {
    NA_real_
  }
  list(
    total = as.difftime(sum(member_totals[contributing]), units = "secs"),
    longest = as.difftime(longest, units = "secs"),
    n_contributing = sum(contributing)
  )
}

# Pool per-member measured sweep frames into one host-aware recalibration. Each
# frame carries `nrow`, `ci_method`, `nboot`, `time`, `.host`; the named list is
# keyed by member. Scenario *name* never enters the cost model, so frames at the
# same `.host` pool soundly; frames spanning more than one `.host` are never
# pooled silently. Reuses the landed `calibrate_coefficients()` /
# `calibrate_nrow_factor()` / `new_ssdsims_cost_calibration()`.
pool_calibration_from_frames <- function(
  member_frames,
  fixed_addend,
  host = NULL
) {
  chk::chk_list(member_frames)
  chk::chk_number(fixed_addend)
  if (!is.null(host)) {
    chk::chk_string(host)
  }
  sweep <- dplyr::bind_rows(member_frames, .id = "scenario")
  required <- c("nrow", "ci_method", "nboot", "time", ".host")
  if (!all(required %in% names(sweep))) {
    chk::abort_chk(
      "Each member frame must have columns ",
      paste0("`", required, "`", collapse = ", "),
      "."
    )
  }
  host <- select_run_host(sweep$.host, host)
  sweep <- sweep[sweep$.host == host, c("nrow", "ci_method", "nboot", "time")]
  coefficients <- calibrate_coefficients(sweep, unique(sweep$ci_method))
  nrow_factor <- calibrate_nrow_factor(sweep, coefficients)
  new_ssdsims_cost_calibration(
    coefficients = coefficients,
    nrow_factor = nrow_factor,
    fixed_addend = fixed_addend,
    provenance = list(
      cpu = host,
      r_version = R.version.string,
      ssdtools_version = as.character(utils::packageVersion("ssdtools")),
      date = Sys.Date(),
      source = "design-run",
      members = rlang::names2(member_frames)
    )
  )
}

# Render a design breakdown (per-`scenario` rows and design totals) as lines,
# reusing `format_duration()`.
format_design_breakdown <- function(breakdown, total, longest, n_contributing) {
  tasks <- if ("tasks" %in% names(breakdown)) {
    breakdown$tasks
  } else {
    rep(NA_integer_, nrow(breakdown))
  }
  bd_lines <- sprintf(
    "    %-12s %-22s nboot %6s  %4s tasks  %s",
    breakdown$scenario,
    ifelse(is.na(breakdown$ci_method), "-", breakdown$ci_method),
    ifelse(is.na(breakdown$nboot), "-", as.character(breakdown$nboot)),
    ifelse(is.na(tasks), "-", as.character(tasks)),
    vapply(as.numeric(breakdown$seconds), format_duration, character(1))
  )
  c(
    sprintf(
      "<ssdsims_cost_analysis>  (design observed, serial-equivalent; %d members)",
      n_contributing
    ),
    sprintf(
      "  total compute:  %s  (per-member accounting; shared cells counted per member)",
      format_duration(as.numeric(total, units = "secs"))
    ),
    sprintf(
      "  longest task:   %s",
      format_duration(as.numeric(longest, units = "secs"))
    ),
    "  breakdown (scenario x ci_method x nboot, by total cost):",
    bd_lines
  )
}

# ---- design-aware reads -----------------------------------------------------

# The combined design summary as a per-task hc timing surface, if usable: a tibble
# of `scenario`, `hc_id`, timing and `seconds`, distinct per `(scenario, hc_id)`.
# `NULL` when the file is absent or lacks the `scenario`/timing columns (the
# caller then falls back to per-member shard reads).
design_fast_hc <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  rel <- tryCatch(
    duckplyr::read_parquet_duckdb(
      path,
      options = list(hive_partitioning = FALSE)
    ),
    error = function(e) NULL
  )
  if (is.null(rel)) {
    return(NULL)
  }
  if (
    !all(c("scenario", "hc_id", ".start", ".end", ".host") %in% colnames(rel))
  ) {
    return(NULL)
  }
  out <- dplyr::distinct(dplyr::select(
    rel,
    dplyr::all_of(c("scenario", "hc_id", ".start", ".end", ".host"))
  ))
  out <- tibble::as_tibble(dplyr::collect(out))
  out$seconds <- as.numeric(difftime(out$.end, out$.start, units = "secs"))
  out
}

# One member's hc timings: from the combined-summary fast path (filtered to the
# member's `scenario` name) when available, else from the member's seed-group hc
# tree filtered to its `hc_id`s.
member_hc_timings <- function(fast, name, sroot, hc_ids) {
  if (!is.null(fast)) {
    hc <- fast[
      fast$scenario == name,
      setdiff(names(fast), "scenario"),
      drop = FALSE
    ]
    if (nrow(hc)) {
      attr(hc, "timed") <- TRUE
      return(hc)
    }
  }
  read_step_timings(sroot, "hc", "hc_id", keep_ids = hc_ids)
}

#' @export
ssd_analyse_cost.ssdsims_design <- function(
  scenario,
  root = "results",
  store = NULL
) {
  design <- scenario
  chk::chk_string(root)
  if (!is.null(store)) {
    chk::chk_string(store)
  }
  fast <- design_fast_hc(file.path(root, "summary.parquet"))

  member_breakdowns <- list()
  member_totals <- stats::setNames(rep(NA_real_, length(design)), names(design))
  member_longests <- member_totals
  fit_total <- 0
  hosts <- character()
  for (name in names(design)) {
    member <- design[[name]]
    sroot <- scenario_results_dir(member, root)
    hc_ids <- ssd_scenario_hc_tasks(member)$hc_id
    fit_ids <- ssd_scenario_fit_tasks(member)$fit_id
    hc <- member_hc_timings(fast, name, sroot, hc_ids)
    fit <- read_step_timings(sroot, "fit", "fit_id", keep_ids = fit_ids)
    if (is.null(hc) || !isTRUE(attr(hc, "timed")) || !nrow(hc)) {
      next
    }
    m <- measured_member(member, hc, fit)
    member_breakdowns[[name]] <- m$breakdown
    member_totals[[name]] <- m$total
    member_longests[[name]] <- m$longest
    fit_total <- fit_total + m$fit_seconds
    hosts <- unique(c(hosts, m$hosts))
  }
  totals <- design_cost_totals(member_totals, member_longests)

  envelope <- NULL
  unmatched <- character()
  n_na <- 0L
  if (!is.null(store)) {
    env <- design_shard_envelope(design, read_store_meta(store), root)
    envelope <- env$envelope
    unmatched <- env$unmatched
    n_na <- env$n_na
  }

  new_ssdsims_cost_analysis(
    total = totals$total,
    longest = totals$longest,
    breakdown = combine_cost_breakdowns(member_breakdowns),
    fit_seconds = as.difftime(fit_total, units = "secs"),
    hosts = hosts,
    measured = TRUE,
    envelope = envelope,
    provenance = list(
      source = "design-run",
      root = root,
      store = store,
      date = Sys.Date(),
      members = names(design),
      n_contributing = totals$n_contributing,
      unmatched_targets = unmatched,
      na_targets = n_na
    )
  )
}

# ---- design store resolver + per-shared-shard envelope ----------------------

# Regenerate every seed group's shard target names, keyed by `(step, cell)` with
# the `seed` woven in (`<step>_step_<seed>_<pathcell>`, the `ssd_design_targets()`
# naming - `tar_map()` over the union shard tables keyed on `c("seed", <axes>)`).
# Returns `step`, `cell`, `name` for the sample/fit/hc shared shards.
cost_design_shard_target_names <- function(design) {
  rlang::check_installed(c("targets", "tarchetypes"))
  seeds <- vapply(design, function(s) s$seed, integer(1L))
  rows <- list()
  for (sd in unique(seeds)) {
    members <- design[seeds == sd]
    ref <- design_reference_scenario(members)
    for (step in c("sample", "fit", "hc")) {
      shards <- union_shards(members, step)
      shards$seed <- ref$seed
      mapped <- tarchetypes::tar_map(
        values = shards,
        names = tidyselect::all_of(c(
          "seed",
          scenario_partition_axes(ref, step)$path
        )),
        targets::tar_target_raw(paste0(step, "_step"), quote(NULL))
      )
      nm <- shard_cell_names(mapped, shards, ref, step)
      rows[[length(rows) + 1L]] <- tibble::tibble(
        step = step,
        cell = names(nm),
        name = unname(nm)
      )
    }
  }
  dplyr::bind_rows(rows)
}

# Σ measured task durations per shared (fit/hc) shard, with its seed-woven target
# name: read each seed group's step tree FULLY (the once-built union tasks), map
# each task id to its Hive cell from the union shard tables, sum per cell.
design_timed_cell_seconds <- function(design, root) {
  seeds <- vapply(design, function(s) s$seed, integer(1L))
  rows <- list()
  for (sd in unique(seeds)) {
    members <- design[seeds == sd]
    ref <- design_reference_scenario(members)
    sroot <- scenario_results_dir(ref, root)
    for (step in c("fit", "hc")) {
      id <- paste0(step, "_id")
      shards <- union_shards(members, step)
      shards$seed <- ref$seed
      mapped <- tarchetypes::tar_map(
        values = shards,
        names = tidyselect::all_of(c(
          "seed",
          scenario_partition_axes(ref, step)$path
        )),
        targets::tar_target_raw(paste0(step, "_step"), quote(NULL))
      )
      nm <- shard_cell_names(mapped, shards, ref, step)
      timings <- read_step_timings(sroot, step, id)
      if (is.null(timings) || !isTRUE(attr(timings, "timed"))) {
        next
      }
      idcell <- dplyr::bind_rows(lapply(shards$tasks, function(tasks) {
        tibble::tibble(
          id = tasks[[id]],
          cell = path_key(tasks, ref$partition_by[[step]])
        )
      }))
      names(idcell)[[1L]] <- id
      dur <- dplyr::left_join(timings[, c(id, "seconds")], idcell, by = id)
      per_cell <- dplyr::summarise(
        dur,
        task_seconds = sum(.data$seconds),
        .by = "cell"
      )
      per_cell$step <- step
      per_cell$name <- nm[per_cell$cell]
      rows[[length(rows) + 1L]] <- per_cell
    }
  }
  dplyr::bind_rows(rows)
}

# Design-level per-shared-shard envelope overhead (once per shared fit/hc shard,
# since shards are shared across a seed group's members), with the combined
# `summary`, per-member `summary_<name>`, and `upload_<step>` targets excluded
# from attribution and unmatched/NA stored targets reported.
design_shard_envelope <- function(design, meta, root) {
  res <- match_store_seconds(meta, cost_design_shard_target_names(design))
  per_cell <- design_timed_cell_seconds(design, root)
  envelope <- dplyr::inner_join(per_cell, meta, by = "name")
  envelope <- envelope[!is.na(envelope$seconds), ]
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

# ---- design compare + recalibrate -------------------------------------------

#' @export
ssd_compare_cost.ssdsims_design <- function(
  scenario,
  root = "results",
  store = NULL,
  calibration = ssd_cost_calibration()
) {
  design <- scenario
  chk::chk_s3_class(calibration, "ssdsims_cost_calibration")
  ests <- lapply(design, ssd_estimate_cost, calibration)
  predicted <- list(
    total = as.difftime(
      sum(vapply(ests, \(e) as.numeric(e$total, units = "secs"), numeric(1))),
      units = "secs"
    ),
    longest = as.difftime(
      max(vapply(ests, \(e) as.numeric(e$longest, units = "secs"), numeric(1))),
      units = "secs"
    )
  )
  observed <- ssd_analyse_cost(design, root = root, store = store)
  cost_comparison(predicted, observed)
}

#' @export
ssd_calibrate_cost_from_run.ssdsims_design <- function(
  scenario,
  root = "results",
  host = NULL
) {
  design <- scenario
  chk::chk_string(root)
  if (!is.null(host)) {
    chk::chk_string(host)
  }
  frames <- list()
  fit_addends <- numeric()
  for (name in names(design)) {
    member <- design[[name]]
    sroot <- scenario_results_dir(member, root)
    hc_ids <- ssd_scenario_hc_tasks(member)$hc_id
    fit_ids <- ssd_scenario_fit_tasks(member)$fit_id
    hc <- read_step_timings(sroot, "hc", "hc_id", keep_ids = hc_ids)
    if (is.null(hc) || !isTRUE(attr(hc, "timed")) || !nrow(hc)) {
      next
    }
    sweep <- member_sweep(member, hc)
    if (!nrow(sweep)) {
      next
    }
    frames[[name]] <- sweep
    fit <- read_step_timings(sroot, "fit", "fit_id", keep_ids = fit_ids)
    if (!is.null(fit) && isTRUE(attr(fit, "timed")) && nrow(fit)) {
      fit_addends <- c(fit_addends, mean(fit$seconds))
    }
  }
  if (!length(frames)) {
    chk::abort_chk(
      "No design member had measured bootstrap (`ci = TRUE`) hc tasks to ",
      "recalibrate from under ",
      encodeString(root, quote = "\""),
      "."
    )
  }
  fixed_addend <- if (length(fit_addends)) mean(fit_addends) else 0
  pool_calibration_from_frames(frames, fixed_addend = fixed_addend, host = host)
}
