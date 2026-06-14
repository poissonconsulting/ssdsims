# Design-level cost-analysis rollup seam.
#
# These internal helpers are the collection-agnostic core of design-level cost
# analysis: they aggregate per-member scenario cost results into a design-level
# view, independent of how the member collection was constructed. They depend
# only on already-landed code (the `cost-estimate.R` fitters/formatters and
# `scenario_results_dir()`), so they can ship and be tested ahead of the
# `ssd_design()` collection (the `scenario-combine` change) and the scenario-level
# `ssd_analyse_cost()` family (the `cost-analysis-targets` change). When those
# land, the thin `ssdsims_design` methods unpack a design into the normalised
# `(scenario, results-root, scenario-analysis)` members these helpers consume and
# delegate here - they do not re-implement the aggregation.

# Row-bind per-member cost breakdowns into one design breakdown, tagged with a
# leading `scenario` column holding each member's name. Members that are `NULL`
# or empty (e.g. did not run) drop out. The input is a named list of breakdown
# tibbles (each `ci_method` x `nboot`, as produced by the scenario-level cost
# functions); the names become the `scenario` values.
combine_cost_breakdowns <- function(named_breakdowns) {
  chk::chk_list(named_breakdowns)
  names <- rlang::names2(named_breakdowns)
  if (length(named_breakdowns) && any(!nzchar(names))) {
    chk::abort_chk("`named_breakdowns` must be fully named.")
  }
  keep <- !purrr::map_lgl(
    named_breakdowns,
    \(bd) is.null(bd) || base::nrow(bd) == 0L
  )
  named_breakdowns <- named_breakdowns[keep]
  if (!length(named_breakdowns)) {
    return(tibble::tibble(scenario = character()))
  }
  dplyr::bind_rows(named_breakdowns, .id = "scenario")
}

# Reduce per-member observed costs to design totals: observed total compute is
# the sum of the members' totals, observed longest task is the maximum of the
# members' longest tasks, both returned as `difftime` seconds. Members with `NA`
# (no readable run) are skipped and the count of contributing members reported.
# Inputs are named numeric vectors of seconds keyed by member name.
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
# frame carries the columns the scenario-level recalibration produces - `nrow`,
# `ci_method`, `nboot`, `time` (measured seconds) and `.host` - and the named
# list is keyed by member name. Because scenario *name* never enters the cost
# model, frames from different members at the same `.host` pool soundly; because
# the calibration is architecture-specific, frames spanning more than one `.host`
# are never pooled silently - the caller selects a `host` or the function aborts
# listing the hosts found. Reuses the landed `calibrate_coefficients()` /
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
  hosts <- unique(sweep$.host)
  if (is.null(host)) {
    if (length(hosts) > 1L) {
      chk::abort_chk(
        "The design run spans more than one `.host` (",
        paste0("`", hosts, "`", collapse = ", "),
        "); pass `host` to pick one - architecture-specific calibrations are ",
        "not pooled across hosts."
      )
    }
    host <- hosts[[1]]
  } else if (!host %in% hosts) {
    chk::abort_chk(
      "`host` ",
      paste0("`", host, "`"),
      " is not among the run's hosts (",
      paste0("`", hosts, "`", collapse = ", "),
      ")."
    )
  }
  sweep <- sweep[sweep$.host == host, c("nrow", "ci_method", "nboot", "time")]
  ci_methods <- unique(sweep$ci_method)
  coefficients <- calibrate_coefficients(sweep, ci_methods)
  nrow_factor <- calibrate_nrow_factor(sweep, coefficients)
  provenance <- list(
    cpu = host,
    r_version = R.version.string,
    ssdtools_version = as.character(utils::packageVersion("ssdtools")),
    date = Sys.Date(),
    source = "design-run",
    members = rlang::names2(member_frames)
  )
  new_ssdsims_cost_calibration(
    coefficients = coefficients,
    nrow_factor = nrow_factor,
    fixed_addend = fixed_addend,
    provenance = provenance
  )
}

# Derive each member's design addressing from a named set of scenarios and a
# root: the `<name>_` target-name prefix and the `scenario=<name>` results root
# (the addressing `scenario-combine` will mint), plus the layout-keyed results
# directory under that root via the landed `scenario_results_dir()`. Computed
# without an `ssd_design()` object so it is testable now.
design_member_addressing <- function(scenarios, root = "results") {
  chk::chk_list(scenarios)
  chk::chk_string(root)
  names <- rlang::names2(scenarios)
  if (length(scenarios) && any(!nzchar(names))) {
    chk::abort_chk("`scenarios` must be fully named.")
  }
  rows <- purrr::imap(scenarios, function(scenario, name) {
    chk::chk_s3_class(scenario, "ssdsims_scenario")
    scenario_root <- file.path(root, paste0("scenario=", name))
    # Resolve the layout-keyed dir before the tibble: a `scenario` column would
    # otherwise shadow the `scenario` argument in tibble()'s data mask.
    dir <- scenario_results_dir(scenario, root = scenario_root)
    tibble::tibble(
      scenario = name,
      prefix = paste0(name, "_"),
      root = scenario_root,
      results_dir = dir
    )
  })
  dplyr::bind_rows(rows)
}

# Render the design-level observed cost (per-`scenario` breakdown and design
# totals) as character lines, reusing `format_duration()`. A pure string helper,
# not an S3 method on the `ssdsims_cost_analysis` class (that class is owned by
# the `cost-analysis-targets` change); the design-aware print methods landing
# with Phase B call this. The breakdown carries `scenario`, `ci_method`, `nboot`,
# and `seconds`, and optionally `tasks`.
format_design_breakdown <- function(breakdown, total, longest) {
  tasks <- if ("tasks" %in% names(breakdown)) {
    breakdown$tasks
  } else {
    rep(NA_integer_, base::nrow(breakdown))
  }
  bd_lines <- sprintf(
    "    %-12s %-18s nboot %6s  %4s tasks  %s",
    breakdown$scenario,
    breakdown$ci_method,
    ifelse(is.na(breakdown$nboot), "-", as.character(breakdown$nboot)),
    ifelse(is.na(tasks), "-", as.character(tasks)),
    vapply(as.numeric(breakdown$seconds), format_duration, character(1))
  )
  c(
    "  design observed (serial-equivalent):",
    sprintf(
      "  total compute:  %s",
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
