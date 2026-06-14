# `ssd_design()`: a named collection of scenarios run as one pipeline.
#
# A **design** (GLOSSARY.md *Design terms*) is the union of several regular
# per-scenario grids into one possibly-non-regular (ragged) task set: the primary
# use is exploring finer detail over a *subset* of the axes without exploding the
# full cross-product. The collection owns naming (mirroring `ssd_scenario_data()`)
# and a name->value **consistency contract** that is the safety underpinning the
# design factory's naked cell addressing (`ssd_design_targets()`): because shards
# are addressed by cell (not by an opaque per-scenario key), the same name must
# mean the same thing across members, or two members could write different bytes
# to one cell. Construction is RNG-free.

#' Assemble and Validate a Design of Scenarios
#'
#' Collects one or more [ssd_define_scenario()] scenarios into a validated, named
#' collection - the **design** (design-of-experiments sense: the set of
#' conditions to run), the union of regular per-scenario grids into one
#' possibly-non-regular design. The design is turned into a single `targets`
#' pipeline by [ssd_design_targets()].
#'
#' Names are taken from the argument names where supplied, otherwise derived from
#' the argument expression by symbol capture (e.g. a variable `base` becomes
#' `"base"`), mirroring [ssd_scenario_data()]. Each name is a **scenario name**
#' within the design, used only as the `scenario` identity column in the combined
#' summary and the per-scenario summary target-name suffix - never in a shard
#' path, a shard target name, the per-task primer, or any result value. Names must
#' be unique, non-empty, and safe to serve as a target-name suffix (start with a
#' letter; letters, digits, and underscore only). A design of **one** scenario is
#' valid and uniformly shaped - the recommended starting point for a study that
#' may grow (see the *migration* vignette).
#'
#' @section Consistency contract:
#' Because [ssd_design_targets()] addresses shards by cell and shares a cell
#' across members (computing it once), the same **name** must denote the same
#' value across members, or two members could disagree on a shared cell's bytes.
#' `ssd_design()` therefore validates across members that the same `dataset` name
#' binds identical data, the same `min_pmix` name binds an identical function, the
#' same `distset` name binds identical members, and that `partition_by` is
#' identical - aborting with an informative error otherwise. The `seed` may vary
#' across members (it becomes a `seed=` results level); members sharing a `seed`
#' share their coincident cells (common random numbers).
#'
#' @param ... One or more `ssdsims_scenario` objects from
#'   [ssd_define_scenario()], optionally named.
#' @return An `ssdsims_design` object: a named list of `ssdsims_scenario` objects.
#' @seealso [ssd_define_scenario()], [ssd_design_targets()].
#' @export
#' @examples
#' data <- ssd_scenario_data(ssddata::ccme_boron)
#' coarse <- ssd_define_scenario(data, nsim = 2L, seed = 42L, nrow = c(5L, 10L))
#' dense <- ssd_define_scenario(data, nsim = 2L, seed = 42L, nrow = c(6L, 7L, 8L))
#' ssd_design(coarse, dense)
ssd_design <- function(...) {
  inputs <- rlang::list2(...)
  exprs <- rlang::enexprs(...)
  call <- environment()
  if (length(inputs) == 0L) {
    chk::abort_chk(
      "`ssd_design()` requires at least one scenario.",
      call = call
    )
  }
  arg_nms <- names(inputs)
  if (is.null(arg_nms)) {
    arg_nms <- rep("", length(inputs))
  }
  nms <- character(length(inputs))
  # Plain loop (not `purrr`) so validation errors surface as `ssd_design()`, not
  # a map frame (error-call-origin rule).
  for (i in seq_along(inputs)) {
    if (!inherits(inputs[[i]], "ssdsims_scenario")) {
      chk::abort_chk(
        "Each element must be an `ssdsims_scenario` from ",
        "`ssd_define_scenario()`; element ",
        i,
        " is not.",
        call = call
      )
    }
    nm <- arg_nms[[i]]
    if (!nzchar(nm)) {
      nm <- expr_to_name(exprs[[i]])
      if (is.null(nm)) {
        chk::abort_chk(
          "Unable to derive a name for scenario ",
          i,
          "; supply a name (e.g. `ssd_design(base = ...)`).",
          call = call
        )
      }
    }
    nms[[i]] <- nm
  }
  validate_design_names(nms, call = call)
  design <- rlang::set_names(inputs, nms)
  validate_design_consistency(design, call = call)
  structure(design, class = "ssdsims_design")
}

# Names must be unique, non-empty/non-`NA`, and safe to serve as a target-name
# suffix and (downstream) a `scenario` identity value: start with a letter, then
# letters, digits, and underscore only (the conservative intersection of valid R
# symbol fragments and path-safe strings).
validate_design_names <- function(nms, call = rlang::caller_env()) {
  if (anyNA(nms) || !all(nzchar(nms))) {
    chk::abort_chk("Scenario names must be non-empty.", call = call)
  }
  safe <- grepl("^[A-Za-z][A-Za-z0-9_]*$", nms)
  if (!all(safe)) {
    chk::abort_chk(
      "Scenario names must start with a letter and contain only letters, ",
      "digits, and underscore; offending: ",
      chk::cc(nms[!safe], conj = " and "),
      ".",
      call = call
    )
  }
  if (anyDuplicated(nms)) {
    chk::abort_chk("Scenario names must be unique.", call = call)
  }
  invisible(nms)
}

# The name->value consistency contract: a name shared across members must bind an
# identical value, and `partition_by` must be uniform - the safety that lets the
# design factory address shards by cell (rather than by an opaque per-scenario
# key) and share a cell across members. A plain nested loop keeps internal frames
# out of the error header (error-call-origin rule).
validate_design_consistency <- function(design, call = rlang::caller_env()) {
  first <- design[[1L]]
  for (i in seq_along(design)[-1L]) {
    s <- design[[i]]
    if (!identical(s$partition_by, first$partition_by)) {
      chk::abort_chk(
        "All scenarios in a design must share the same `partition_by` ",
        "(scenario ",
        encodeString(names(design)[[i]], quote = "\""),
        " differs).",
        call = call
      )
    }
  }
  check_binding(design, function(s) s$data, "dataset", call = call)
  check_binding(design, function(s) s$min_pmix_fns, "min_pmix", call = call)
  check_binding(design, function(s) s$hc$distsets, "distset", call = call)
  invisible(design)
}

# Across members, a name appearing in `accessor(scenario)` (a named list keyed by
# `dataset`/`min_pmix`/`distset` name) must bind an identical value everywhere it
# appears. Records the first binding per name and aborts on the first divergence.
check_binding <- function(design, accessor, what, call = rlang::caller_env()) {
  seen <- list()
  for (i in seq_along(design)) {
    bindings <- accessor(design[[i]])
    for (nm in names(bindings)) {
      if (is.null(seen[[nm]])) {
        seen[nm] <- list(bindings[[nm]])
      } else if (!identical(seen[[nm]], bindings[[nm]])) {
        chk::abort_chk(
          "The ",
          what,
          " name ",
          encodeString(nm, quote = "\""),
          " binds different values across scenarios; a name shared across a ",
          "design must denote the same value.",
          call = call
        )
      }
    }
  }
  invisible(NULL)
}

#' @export
#' @noRd
print.ssdsims_design <- function(x, ...) {
  cat("<ssdsims_design>\n")
  cat("  scenarios: ", length(x), "\n", sep = "")
  for (nm in names(x)) {
    cat("    ", nm, " (seed ", x[[nm]]$seed, ")\n", sep = "")
  }
  invisible(x)
}
