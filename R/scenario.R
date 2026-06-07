#' Define a Simulation Scenario
#'
#' Constructs a purely declarative `ssdsims_scenario` object: the root of
#' the targets-based pipeline (see `TARGETS-DESIGN.md` section 1). The object stores
#' only declarative fields - a scalar `seed`, the replicate count `nsim`, the
#' sample sizes `nrow`, the dataset *names*, and the `fit` and `hc` argument
#' grids. It performs **no** random-number generation, **no** task expansion,
#' and has **no** dependency on `targets`.
#'
#' Input data is forwarded through [ssd_data()] for validation (a numeric
#' `Conc` column is required) and retained on the scenario (as `$data`) so a
#' local run ([ssd_run_scenario_baseline()]) can sample it directly. The dataset
#' *names* (`$datasets`) are what the targets/cluster path hashes; the validated
#' tibbles ride on the scenario and are isolated by name via [scenario_dataset()],
#' so the hash need not carry the data frames.
#'
#' # Dataset input
#'
#' The preferred form is an [ssd_data()] collection, which owns validation and
#' naming: `ssd_define_scenario(ssd_data(boron = ccme_boron, cadmium =
#' ccme_cadmium), ...)`. For convenience, bare data frame input is also
#' accepted in four forms (routed through the same `Conc` validation):
#'
#' 1. A single data frame, name derived from the argument expression:
#'    `ssd_define_scenario(ssddata::ccme_boron, ...)` gives `"ccme_boron"`.
#' 2. A single data frame with an explicit `name=`:
#'    `ssd_define_scenario(ssddata::ccme_boron, name = "boron", ...)`.
#' 3. A named list, names taken from the list:
#'    `ssd_define_scenario(list(boron = ccme_boron, cadmium = ccme_cadmium), ...)`.
#' 4. An unnamed list, names derived per element:
#'    `ssd_define_scenario(list(ccme_boron, ccme_cadmium), ...)`.
#'
#' Supplying both a named list and `name=` is an error.
#'
#' # `ci`
#'
#' `ci` is a scalar flag (not a cross-join axis): the point estimate `est` is
#' invariant to `ci` - it is computed analytically from the fit, independent of
#' the bootstrap and RNG - so a single `ci = TRUE` run is a strict superset of
#' `ci = FALSE` (same `est`, plus the `se`/`lcl`/`ucl` columns). The choice is
#' scenario-wide either/or: `ci = FALSE` for cheap, bootstrap-free point
#' estimates, or `ci = TRUE` for estimates plus confidence intervals. When
#' `ci = FALSE`, the bootstrap-only knobs `nboot`, `ci_method`, and `parametric`
#' are meaningless; passing any of them in that case is an error, so set
#' `ci = TRUE` to enable bootstrap, or omit the knobs.
#'
#' # `dists` and `est_method`
#'
#' `dists` and `est_method` are **simulation settings**, not cross-join axes -
#' they are absent from `task_axes("fit")`/`task_axes("hc")`, so they never
#' multiply tasks or enter the per-task RNG primer. `dists` is the *fit*-level
#' setting: the whole character vector is handed to one `ssd_fit_dists()` call
#' per fit task (fanning out per distribution would dissolve the model averaging
#' that defines a fit). `est_method` is an *hc*-level setting: every requested
#' method is summarised from each hc task's **single** bootstrap sample set
#' rather than re-bootstrapping per method (the CI is est_method-invariant and
#' the point `est` is analytical), so a vector `est_method` yields one row per
#' method within a task without fanning out into separate tasks.
#'
#' @inheritParams ssdtools::ssd_fit_dists
#' @inheritParams ssdtools::ssd_hc
#' @inheritParams params
#' @param data An [ssd_data()] collection (preferred), or - for convenience -
#'   a single data frame or a (named or unnamed) list of data frames. Bare
#'   inputs are validated via the same `Conc` contract as [ssd_data()].
#' @param name An optional dataset name for the single-data-frame form,
#'   overriding the derived name. Must not be combined with a named list or an
#'   [ssd_data()] collection.
#' @param seed A scalar whole number; the scenario's RNG root. Required -
#'   changing it fully re-roots the scenario's random-number draws.
#' @param min_pmix The `min_pmix` function(s), referenced **by name**. Supply
#'   either a character vector of names, or a function (or list of functions)
#'   with a single argument that inputs the number of rows of data and returns
#'   a proportion between 0 and 0.5 - in which case the name is derived from the
#'   argument expression (e.g. `ssdtools::ssd_min_pmix` gives `"ssd_min_pmix"`),
#'   mirroring dataset name derivation. The name is what the task path hashes;
#'   the resolved single-argument function is additionally materialised on the
#'   scenario (keyed by name) for execution and isolated via
#'   [scenario_min_pmix()]. A name-string is resolved to a function at
#'   construction (from `ssdtools` or the caller's environment), failing fast if
#'   it cannot be resolved to a single-argument function.
#' @param range_shape1 A list of numeric vectors of length two of the lower and
#'   upper bounds for the shape1 parameter.
#' @param range_shape2 A list of numeric vectors of length two of the lower and
#'   upper bounds for the shape2 parameter.
#' @param samples A logical scalar (default `FALSE`): retain the bootstrap draws
#'   in the hc result's `samples` list-column (passed to [ssdtools::ssd_hc()]).
#'   This is **output retention only** - it does not change the estimates or the
#'   per-task RNG, so it is not a grid or task axis (a single `TRUE` is a superset
#'   of `FALSE`). Changing it re-runs the hc step (the discarded draws must be
#'   re-bootstrapped) but yields byte-identical estimates; retained samples can
#'   be large (`nboot` draws per dist per task), so it is off by default.
#' @param partition_by An optional, possibly-partial named list keyed by step
#'   (`sample`/`fit`/`hc`) of character vectors naming the Hive **path** axes for
#'   that step (one shard per path cell; the inner complement rides as Parquet
#'   columns). Each entry must be unique, non-missing, and a subset of that
#'   step's axis vocabulary: `sample` = `dataset`, `sim`, `replace`; `fit` adds
#'   `nrow`, `rescale`, `computable`, `at_boundary_ok`, `min_pmix`,
#'   `range_shape1`, `range_shape2`; `hc` adds `nboot`, `ci_method`,
#'   `parametric` (`ci` and `est_method` are hc simulation settings, not axes;
#'   `dists` is the fit-level simulation setting).
#'   `"nrow"` is rejected only for `sample` (the
#'   shared draw carries no `nrow` axis; the `fit` step truncates it inline), and
#'   is a valid path axis for `fit`/`hc`. Steps partition **independently** -
#'   there is no cross-step constraint; a step may be finer or coarser than its
#'   neighbour on a shared axis (the m:n parent-shard relationship is resolved at
#'   the read layer). Steps left unnamed take their documented defaults (`sample
#'   = c("dataset", "sim", "replace")`, `fit = c("dataset", "sim", "nrow",
#'   "rescale")`, `hc = c("dataset", "sim")`; these supersede `TARGETS-DESIGN.md`
#'   section 5's pre-fold table). The split is orthogonal to the per-task RNG
#'   primer, so changing it shifts file paths only, never results.
#' @param bundle An optional, possibly-partial named list keyed by step, the
#'   per-step **complement** of `partition_by`: it names the **inner** axes to
#'   keep together within a shard, and the stored path axes become
#'   `setdiff(task_axes(step), bundle[[step]])`. `partition_by` and `bundle` are
#'   complementary per-step entry points - at most one may name a given step (a
#'   step in **both** is an error), but they may be mixed across steps and either
#'   may be partial. Use `partition_by` when you want few path axes, `bundle`
#'   when you want fine sharding and only a few inner axes. Both normalise into
#'   the single stored `partition_by` path list.
#' @param upload An optional upload specification (a list), or `NULL` for no
#'   upload.
#' @param ... Unused; must be empty.
#' @return An S3 object of class `ssdsims_scenario`.
#' @export
#' @examples
#' ssd_define_scenario(ssddata::ccme_boron, nsim = 100L, seed = 42L, nrow = c(5L, 10L))
ssd_define_scenario <- function(
  data,
  nsim,
  seed,
  ...,
  name = NULL,
  nrow = 6L,
  replace = FALSE,
  rescale = FALSE,
  computable = FALSE,
  at_boundary_ok = TRUE,
  min_pmix = list(ssdtools::ssd_min_pmix),
  range_shape1 = list(c(0.05, 20)),
  range_shape2 = list(c(0.05, 20)),
  nboot = 1000,
  ci_method = "weighted_samples",
  parametric = TRUE,
  dists = ssdtools::ssd_dists_bcanz(),
  est_method = "multi",
  proportion = 0.05,
  ci = FALSE,
  samples = FALSE,
  partition_by = NULL,
  bundle = NULL,
  upload = NULL
) {
  data_expr <- rlang::enexpr(data)
  min_pmix_expr <- rlang::enexpr(min_pmix)
  user_env <- rlang::caller_env()
  call <- environment()
  chk::chk_unused(...)

  # --- scalar / vector knob validation ----------------------------------
  if (missing(seed)) {
    chk::abort_chk(
      "`seed` must be supplied (a scalar whole number); ",
      "it is the scenario's RNG root.",
      call = call
    )
  }
  chk::chk_whole_number(seed)

  if (missing(nsim)) {
    chk::abort_chk(
      "`nsim` must be supplied (a count of the number of simulations).",
      call = call
    )
  }
  chk::chk_whole_number(nsim)
  chk::chk_gt(nsim)

  chk::chk_whole_numeric(nrow)
  chk::chk_not_any_na(nrow)
  chk::chk_unique(nrow)
  chk::chk_range(nrow, c(5, 1000))
  chk::chk_length(nrow, upper = Inf)

  chk::chk_logical(replace)
  chk::chk_not_any_na(replace)
  chk::chk_unique(replace)
  chk::chk_length(replace, upper = 2L)

  chk::chk_null_or(name, vld = chk::vld_string)
  chk::chk_null_or(upload, vld = chk::vld_list)
  # `partition_by`/`bundle` are validated and normalised below.

  # --- fit-grid validation (mirrors ssd_fit_dists_sims) ------------------
  chk::chk_character(dists)
  chk::chk_not_any_na(dists)
  chk::chk_unique(dists)

  chk::chk_logical(rescale)
  chk::chk_not_any_na(rescale)
  chk::chk_unique(rescale)
  chk::chk_length(rescale, upper = 2L)

  chk::chk_logical(computable)
  chk::chk_not_any_na(computable)
  chk::chk_unique(computable)
  chk::chk_length(computable, upper = 2L)

  chk::chk_logical(at_boundary_ok)
  chk::chk_not_any_na(at_boundary_ok)
  chk::chk_unique(at_boundary_ok)
  chk::chk_length(at_boundary_ok, upper = 2L)

  # `min_pmix` is stored by name (derived below); validated in the helper.

  chk::chk_list(range_shape1)
  chk::chk_length(range_shape1, upper = Inf)
  chk::chk_all(range_shape1, chk::chk_double)
  chk::chk_all(range_shape1, chk::chk_length, upper = 2L)
  chk::chk_unique(range_shape1)

  chk::chk_list(range_shape2)
  chk::chk_length(range_shape2, upper = Inf)
  chk::chk_all(range_shape2, chk::chk_double)
  chk::chk_all(range_shape2, chk::chk_length, upper = 2L)
  chk::chk_unique(range_shape2)

  # --- hc-grid validation (mirrors ssd_hc_sims) --------------------------
  chk::chk_numeric(proportion)
  chk::chk_not_any_na(proportion)
  chk::chk_range(proportion, c(0, 1))
  chk::chk_unique(proportion)

  # `ci` is a scalar flag (not a grid/task axis): the point estimate is
  # invariant to `ci`, so `ci = TRUE` is a superset of `ci = FALSE` and the
  # choice is scenario-wide either/or.
  chk::chk_flag(ci)

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

  # `samples` is output-retention only (scalar, not a grid axis or task axis).
  chk::chk_flag(samples)

  # --- ci = FALSE rejects bootstrap-only knobs ---------------------------
  if (isFALSE(ci)) {
    passed <- c(
      if (!missing(nboot)) "nboot",
      if (!missing(ci_method)) "ci_method",
      if (!missing(parametric)) "parametric"
    )
    if (length(passed)) {
      chk::abort_chk(
        "Bootstrap-only knob",
        if (length(passed) > 1L) "s" else "",
        " (",
        chk::cc(passed, conj = " and "),
        ") cannot be set when `ci = FALSE`. ",
        "Set `ci = TRUE` to enable bootstrap, or omit the knob",
        if (length(passed) > 1L) "s" else "",
        ".",
        call = call
      )
    }
  }

  # --- datasets: validated and retained as an ssd_data() collection ------
  datasets <- scenario_datasets(data, name, data_expr, call = call)

  # --- min_pmix: names for hashing + functions materialised for execution -
  min_pmix_spec <- scenario_min_pmix_materialise(
    min_pmix,
    min_pmix_expr,
    env = user_env,
    call = call
  )

  partition_by <- validate_partition_by(partition_by, bundle, call = call)

  structure(
    list(
      seed = as.integer(seed),
      nsim = as.integer(nsim),
      datasets = names(datasets),
      data = datasets,
      nrow = as.integer(nrow),
      replace = replace,
      fit = list(
        dists = dists,
        rescale = rescale,
        computable = computable,
        at_boundary_ok = at_boundary_ok,
        min_pmix = min_pmix_spec$names,
        range_shape1 = range_shape1,
        range_shape2 = range_shape2
      ),
      min_pmix_fns = min_pmix_spec$fns,
      hc = list(
        nboot = nboot,
        ci_method = ci_method,
        parametric = parametric,
        est_method = est_method,
        proportion = proportion,
        ci = ci,
        samples = samples
      ),
      partition_by = partition_by,
      upload = upload
    ),
    class = "ssdsims_scenario"
  )
}

#' Documented per-step `partition_by` defaults.
#'
#' Three-step defaults (`sample`/`fit`/`hc`), superseding `TARGETS-DESIGN.md`
#' section 5's pre-fold table (whose `data` key predates the `sample` split and
#' the `data`-into-`fit` fold). `nrow` shards at the `fit` level: each
#' truncation size is its own fit shard, while the shared draw upstream is
#' de-duplicated at the `sample` step.
#' @noRd
scenario_default_partition_by <- function() {
  list(
    sample = c("dataset", "sim", "replace"),
    fit = c("dataset", "sim", "nrow", "rescale"),
    hc = c("dataset", "sim")
  )
}

#' Validate `partition_by`/`bundle` and normalise to a complete `partition_by`.
#'
#' `partition_by` (path axes) and `bundle` (inner axes - the path complement)
#' are complementary, optional, possibly-partial named lists keyed by step
#' (`sample`/`fit`/`hc`). Each named entry is validated against that step's
#' `task_axes()` vocabulary (character, unique, non-`NA`, a subset); `"nrow"` is
#' rejected under `partition_by$sample` with a bespoke message (the shared draw
#' carries no `nrow`; `fit` truncates it inline) and falls under the generic
#' unknown-axis error for `bundle$sample`. A step named in **both** arguments
#' aborts. Validation is **per-step only** - there is no cross-step
#' parent-consistency check (steps partition independently; the m:n parent-shard
#' relationship is resolved at the read layer). Returns the normalised, complete
#' three-step `partition_by` path list: a `bundle[[step]]` becomes its path
#' complement `setdiff(task_axes(step), bundle[[step]])`, and steps named in
#' neither argument take their documented default. Aborts in the context of
#' `call` (the user-facing function). Plain loops (not `purrr::walk`) keep
#' internal frames out of the error header (error-call-origin rule).
#' @noRd
validate_partition_by <- function(
  partition_by = NULL,
  bundle = NULL,
  call = rlang::caller_env()
) {
  validate_axis_list(partition_by, "partition_by", call = call)
  validate_axis_list(bundle, "bundle", call = call)

  both <- intersect(names(partition_by), names(bundle))
  if (length(both)) {
    chk::abort_chk(
      "Step",
      if (length(both) > 1L) "s" else "",
      " ",
      chk::cc(encodeString(both, quote = "\""), conj = " and "),
      " named in both `partition_by` and `bundle`; ",
      "give each step its path axes (`partition_by`) or its inner axes ",
      "(`bundle`), not both.",
      call = call
    )
  }

  out <- scenario_default_partition_by()
  for (step in names(partition_by)) {
    out[[step]] <- partition_by[[step]]
  }
  for (step in names(bundle)) {
    out[[step]] <- setdiff(task_axes(step), bundle[[step]])
  }
  out
}

#' Validate one of the `partition_by`/`bundle` axis lists (or `NULL`).
#'
#' `arg` names the argument for messages. Each named entry must name a real step
#' and be a character vector that is unique, non-`NA`, and a subset of that
#' step's `task_axes()`. `"nrow"` under `partition_by$sample` gets a bespoke
#' message.
#' @noRd
validate_axis_list <- function(lst, arg, call = rlang::caller_env()) {
  if (is.null(lst)) {
    return(invisible(NULL))
  }
  steps <- c("sample", "fit", "hc")
  if (!is.list(lst) || is.null(names(lst)) || !all(nzchar(names(lst)))) {
    chk::abort_chk(
      "`",
      arg,
      "` must be a named list keyed by step (`sample`, `fit`, `hc`).",
      call = call
    )
  }
  bad_steps <- setdiff(names(lst), steps)
  if (length(bad_steps)) {
    chk::abort_chk(
      "`",
      arg,
      "` names unknown step",
      if (length(bad_steps) > 1L) "s " else " ",
      chk::cc(encodeString(bad_steps, quote = "\""), conj = " and "),
      "; valid steps are `sample`, `fit`, and `hc`.",
      call = call
    )
  }
  for (step in names(lst)) {
    axes <- lst[[step]]
    if (!is.character(axes)) {
      chk::abort_chk(
        "`",
        arg,
        "$",
        step,
        "` must be a character vector.",
        call = call
      )
    }
    if (anyNA(axes)) {
      chk::abort_chk(
        "`",
        arg,
        "$",
        step,
        "` must not contain missing values.",
        call = call
      )
    }
    if (anyDuplicated(axes)) {
      chk::abort_chk(
        "`",
        arg,
        "$",
        step,
        "` must not contain duplicate axis names.",
        call = call
      )
    }
    if (arg == "partition_by" && step == "sample" && "nrow" %in% axes) {
      chk::abort_chk(
        "`partition_by$sample` must not include \"nrow\": the `sample` step ",
        "is the shared draw and carries no `nrow` axis (every `nrow` ",
        "truncates the same draw inside the `fit` step).",
        call = call
      )
    }
    unknown <- setdiff(axes, task_axes(step))
    if (length(unknown)) {
      chk::abort_chk(
        "`",
        arg,
        "$",
        step,
        "` names unknown ",
        if (length(unknown) > 1L) "axes " else "axis ",
        chk::cc(encodeString(unknown, quote = "\""), conj = " and "),
        "; valid axes for the `",
        step,
        "` step are ",
        chk::cc(encodeString(task_axes(step), quote = "\""), conj = " and "),
        ".",
        call = call
      )
    }
  }
  invisible(NULL)
}

#' Path-vs-inner axis split for a scenario step.
#'
#' Returns `list(path = ..., inner = ...)` for `step`: the `path` axes are the
#' scenario's `partition_by[[step]]` (the Hive directory levels - one shard per
#' path cell, so the shard count is `prod(lengths)` of the path-axis value sets,
#' and the `<step>_id` `path_key()` keys on them), and the `inner` axes are the
#' lazy complement `setdiff(task_axes(step), path)` (carried as Parquet columns
#' within each shard). This is the consumer hook for `task-tables` and
#' `hive-partitioning`; storing only the path axes (and computing the inner
#' complement) keeps `partition_by` the single source of truth. The split is
#' orthogonal to the per-task primer (which hashes all of `task_axes(step)`), so
#' it never changes results - only on-disk layout.
#' @noRd
scenario_partition_axes <- function(scenario, step) {
  path <- scenario$partition_by[[step]]
  list(path = path, inner = setdiff(task_axes(step), path))
}

#' Validate and retain the constructor's `data` argument.
#'
#' Accepts an `ssd_data()` collection (returned as-is), a single data frame, or
#' a list of data frames, and returns a validated, named `ssd_data()` collection
#' (an `ssdsims_data` object). The scenario retains this for local runs; the
#' targets/cluster path uses only `names()` of it.
#' @noRd
scenario_datasets <- function(
  data,
  name,
  data_expr,
  call = rlang::caller_env()
) {
  if (inherits(data, "ssdsims_data")) {
    if (!is.null(name)) {
      chk::abort_chk(
        "`name` must not be supplied when `data` is an `ssd_data()` collection.",
        call = call
      )
    }
    return(data)
  }

  if (is.data.frame(data)) {
    if (is.null(name)) {
      name <- expr_to_name(data_expr)
      if (is.null(name)) {
        chk::abort_chk(
          "Unable to derive a dataset name from the data argument; ",
          "supply an explicit `name=` or use `ssd_data()`.",
          call = call
        )
      }
    }
    tibble <- ssd_data_validate(data, name = name, call = call)
    return(structure(
      rlang::set_names(list(tibble), name),
      class = "ssdsims_data"
    ))
  }

  if (is.list(data)) {
    list_names <- names(data)
    is_named <- !is.null(list_names) && all(nzchar(list_names))
    if (is_named) {
      if (!is.null(name)) {
        chk::abort_chk(
          "`name` must not be supplied with a named list of datasets.",
          call = call
        )
      }
      nms <- list_names
    } else {
      if (!is.null(name)) {
        chk::abort_chk(
          "`name` applies only to a single data frame; ",
          "use a named list or `ssd_data()` for multiple datasets.",
          call = call
        )
      }
      nms <- list_expr_names(data_expr, label = "dataset", call = call)
    }
    if (anyDuplicated(nms)) {
      chk::abort_chk("Dataset names must be unique.", call = call)
    }
    # Loop (not `purrr::map2`) so a validation error surfaces as
    # `ssd_define_scenario()`, not a `purrr` frame (error-call-origin rule).
    tibbles <- vector("list", length(data))
    for (i in seq_along(data)) {
      tibbles[[i]] <- ssd_data_validate(data[[i]], name = nms[[i]], call = call)
    }
    return(structure(rlang::set_names(tibbles, nms), class = "ssdsims_data"))
  }

  chk::abort_chk(
    "`data` must be an `ssd_data()` collection, a data frame, ",
    "or a list of data frames.",
    call = call
  )
}

#' Derive a single dataset name from a captured argument expression.
#'
#' Returns `NULL` when no meaningful name can be derived (e.g. a literal
#' `data.frame(...)` call).
#' @noRd
expr_to_name <- function(expr) {
  if (rlang::is_symbol(expr)) {
    return(rlang::as_string(expr))
  }
  if (rlang::is_call(expr, c("::", ":::"))) {
    return(rlang::as_string(expr[[3]]))
  }
  NULL
}

#' Derive per-element names from a captured `list(...)` expression.
#' @noRd
list_expr_names <- function(
  list_expr,
  label = "dataset",
  call = rlang::caller_env()
) {
  if (!rlang::is_call(list_expr, "list")) {
    chk::abort_chk(
      "Unable to derive ",
      label,
      " names from the list argument; ",
      "supply a named list (e.g. `list(boron = ...)`).",
      call = call
    )
  }
  elems <- rlang::call_args(list_expr)
  nms <- vapply(
    elems,
    function(e) expr_to_name(e) %||% NA_character_,
    character(1),
    USE.NAMES = FALSE
  )
  if (anyNA(nms)) {
    chk::abort_chk(
      "Unable to derive a name for every ",
      label,
      " in the list; ",
      "supply a named list (e.g. `list(boron = ...)`).",
      call = call
    )
  }
  nms
}

#' Materialise `min_pmix` names *and* functions from the value and captured
#' argument expression.
#'
#' Returns `list(names = <character>, fns = <named list of functions>)`. The
#' names are what the task path hashes (mirroring dataset name derivation); the
#' functions ride on the scenario for execution, isolated later by name via
#' [scenario_min_pmix()]. Accepts:
#'
#' * a character vector of names - each resolved to a single-argument function
#'   at construction (from `ssdtools` or `env`), failing fast if unresolvable;
#' * a function - validated, its name derived by symbol capture, kept under
#'   that name;
#' * a list of functions - each validated, names taken from the list or derived
#'   per element, kept under those names.
#'
#' The names never carry function values into a hash (`task_axes("fit")` keys on
#' the name only).
#' @noRd
scenario_min_pmix_materialise <- function(
  min_pmix,
  min_pmix_expr,
  env = rlang::caller_env(),
  call = rlang::caller_env()
) {
  if (is.character(min_pmix)) {
    if (anyNA(min_pmix)) {
      chk::abort_chk("`min_pmix` names must not be missing.", call = call)
    }
    if (anyDuplicated(min_pmix)) {
      chk::abort_chk("`min_pmix` names must be unique.", call = call)
    }
    # Plain loop (not `lapply`/`purrr::map`) so an unresolvable-name abort
    # surfaces from `ssd_define_scenario()`, not an internal map frame
    # (error-call-origin rule).
    fns <- vector("list", length(min_pmix))
    for (i in seq_along(min_pmix)) {
      fns[[i]] <- resolve_min_pmix_name(min_pmix[[i]], env = env, call = call)
    }
    return(list(names = min_pmix, fns = rlang::set_names(fns, min_pmix)))
  }

  if (is.function(min_pmix)) {
    check_min_pmix_function(min_pmix, call = call)
    nm <- expr_to_name(min_pmix_expr)
    if (is.null(nm)) {
      chk::abort_chk(
        "Unable to derive a name for `min_pmix`; ",
        "supply it by name (a character vector).",
        call = call
      )
    }
    return(list(names = nm, fns = rlang::set_names(list(min_pmix), nm)))
  }

  if (is.list(min_pmix)) {
    for (entry in min_pmix) {
      check_min_pmix_function(entry, call = call)
    }
    list_names <- names(min_pmix)
    if (!is.null(list_names) && all(nzchar(list_names))) {
      nms <- list_names
    } else {
      nms <- list_expr_names(min_pmix_expr, label = "min_pmix", call = call)
    }
    if (anyDuplicated(nms)) {
      chk::abort_chk("`min_pmix` names must be unique.", call = call)
    }
    return(list(names = nms, fns = rlang::set_names(min_pmix, nms)))
  }

  chk::abort_chk(
    "`min_pmix` must be a character vector of names, ",
    "or a function or list of functions.",
    call = call
  )
}

#' Resolve a `min_pmix` name to a single-argument function at construction.
#'
#' Looks the name up in `ssdtools` first, then `env` (the caller's environment,
#' searched inheritably). Aborts in the context of `call` when the name cannot
#' be resolved to a single-argument function.
#' @noRd
resolve_min_pmix_name <- function(name, env, call = rlang::caller_env()) {
  out <- rlang::env_get(rlang::ns_env("ssdtools"), name, default = NULL)
  if (!rlang::is_function(out)) {
    out <- rlang::env_get(env, name, default = NULL, inherit = TRUE)
  }
  if (!rlang::is_function(out) || length(formals(out)) != 1L) {
    chk::abort_chk(
      "Unable to resolve `min_pmix` name ",
      encodeString(name, quote = "\""),
      " to a single-argument function.",
      call = call
    )
  }
  out
}

#' Assert a `min_pmix` entry is a single-argument function (in the context of
#' `call`).
#' @noRd
check_min_pmix_function <- function(f, call = rlang::caller_env()) {
  if (!is.function(f) || length(formals(f)) != 1L) {
    chk::abort_chk(
      "Each `min_pmix` function must take a single argument ",
      "(the number of rows).",
      call = call
    )
  }
}

#' @export
#' @noRd
print.ssdsims_scenario <- function(x, ...) {
  cat("<ssdsims_scenario>\n")
  cat("  seed:     ", x$seed, "\n", sep = "")
  cat("  nsim:     ", x$nsim, "\n", sep = "")
  cat("  datasets: ", paste(x$datasets, collapse = ", "), "\n", sep = "")
  cat("  nrow:     ", paste(x$nrow, collapse = ", "), "\n", sep = "")
  cat("  replace:  ", paste(x$replace, collapse = ", "), "\n", sep = "")
  cat("  fit grid:\n")
  print_grid(x$fit, "fit")
  cat("  hc grid:\n")
  print_grid(x$hc, "hc")
  cat("  partition_by:\n")
  for (step in c("sample", "fit", "hc")) {
    cat(
      "    ",
      step,
      ": ",
      paste(scenario_partition_axes(x, step)$path, collapse = ", "),
      "\n",
      sep = ""
    )
  }
  cat("  bundle:\n")
  for (step in c("sample", "fit", "hc")) {
    cat(
      "    ",
      step,
      ": ",
      paste(scenario_partition_axes(x, step)$inner, collapse = ", "),
      "\n",
      sep = ""
    )
  }
  invisible(x)
}

#' Print a step's argument grid by role: cross-join axes first, then the
#' simulation settings (the knobs absent from `task_axes(step)`), each marked
#' `(setting)`. Within each role group the stored order is preserved, so the
#' fit grid renders `dists` after its axes and the hc grid renders
#' `est_method`/`proportion`/`ci`/`samples` after `nboot`/`ci_method`/`parametric`.
#' @noRd
print_grid <- function(grid, step) {
  nms <- names(grid)
  is_setting <- !nms %in% task_axes(step)
  for (i in c(which(!is_setting), which(is_setting))) {
    marker <- if (is_setting[i]) " (setting)" else ""
    cat(
      "    ",
      nms[i],
      ": ",
      fmt_grid_value(grid[[nms[i]]]),
      marker,
      "\n",
      sep = ""
    )
  }
}

#' Format a grid value without deparsing function bodies (snapshot-stable).
#' @noRd
fmt_grid_value <- function(value) {
  if (is.list(value)) {
    parts <- vapply(
      value,
      function(e) {
        if (is.function(e)) {
          "<fn>"
        } else {
          paste(as.character(e), collapse = ", ")
        }
      },
      character(1)
    )
    return(paste0("{", paste(parts, collapse = "; "), "}"))
  }
  paste(as.character(value), collapse = ", ")
}
