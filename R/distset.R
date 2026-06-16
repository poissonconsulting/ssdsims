# Distribution sets: the validated, by-value collection `dists` accepts
# (TARGETS-DESIGN.md `dists-scenario-setting`, refined by `distset-hc-axis`). A
# distribution *set* is the pool of distributions model-averaged together to
# form one SSD (one `est`); a *collection* is a named list of such sets. The
# constructor owns naming (from `...`) and member validation by value - the
# `ssd_scenario_data()` precedent - so `ssd_define_scenario()` does no
# expression-archaeology to name sets. The set *name* (not its members) is what
# enters the `distset=` Hive path segment and the per-task primer; members ride
# on the scenario for execution (isolated by `scenario_distset()`) and never
# enter a hash.

#' Assemble One or More Distribution Sets
#'
#' Constructs a validated, named `ssdsims_distset` collection - the single entry
#' point `ssd_define_scenario(dists = ...)` accepts. A distribution **set** is the
#' pool of distributions model-averaged together to form one SSD (one `est`); a
#' **collection** is a named list of such sets, each supplied as a `...` argument
#' whose name labels the set.
#'
#' The fit step fits the **union** of every set's members once (the single
#' model-averaged superset every pool is a subset of); the hc step then subsets
#' that one union fit down to each set's members and re-averages, so several pools
#' share one fit rather than re-fitting (`distset` is an hc-level axis - see
#' [ssd_scenario_hc_tasks()]). The set **name** is what hashes onto the hc task
#' path (mirroring the by-name treatment of `min_pmix` and datasets); the member
#' vectors are carried for execution only and never enter a task hash.
#'
#' Set names are taken from the `...` argument names and must be unique,
#' non-missing, and filesystem-safe (each becomes a `distset=<name>` Hive path
#' segment). Each set must be a non-empty, unique, non-`NA` character vector whose
#' members are a subset of [ssdtools::ssd_dists_all()].
#'
#' @param ... One or more named character vectors, each a distribution set (its
#'   name labels the pool, its values are the distribution names averaged
#'   together).
#' @return An `ssdsims_distset` object: a validated, named list of
#'   distribution-name character vectors.
#' @seealso [ssd_define_scenario()], [scenario_distset()].
#' @export
#' @examples
#' ssd_distset(BCANZ = ssdtools::ssd_dists_bcanz())
#' ssd_distset(
#'   BCANZ = ssdtools::ssd_dists_bcanz(),
#'   Iwasaki = c("burrIII3", "gamma", "llogis", "lnorm", "weibull"),
#'   lnorm = "lnorm"
#' )
ssd_distset <- function(...) {
  call <- environment()
  sets <- list(...)

  if (!length(sets)) {
    chk::abort_chk(
      "`ssd_distset()` must be given at least one distribution set.",
      call = call
    )
  }

  nms <- names(sets)
  if (is.null(nms) || !all(nzchar(nms))) {
    chk::abort_chk(
      "Every distribution set must be named (e.g. ",
      "`ssd_distset(BCANZ = ssdtools::ssd_dists_bcanz())`).",
      call = call
    )
  }
  if (anyNA(nms)) {
    chk::abort_chk("Distribution-set names must not be missing.", call = call)
  }
  if (anyDuplicated(nms)) {
    chk::abort_chk("Distribution-set names must be unique.", call = call)
  }
  # Each name becomes a `distset=<name>` Hive path segment, so it must be
  # filesystem-safe (no path separators or other awkward characters).
  bad_names <- nms[!grepl("^[A-Za-z0-9._-]+$", nms)]
  if (length(bad_names)) {
    chk::abort_chk(
      "Distribution-set name",
      if (length(bad_names) > 1L) "s " else " ",
      chk::cc(encodeString(bad_names, quote = "\""), conj = " and "),
      " must be filesystem-safe (letters, digits, `.`, `_`, or `-` only); ",
      "each becomes a `distset=<name>` path segment.",
      call = call
    )
  }

  all_dists <- ssdtools::ssd_dists_all()
  # Plain loop (not `purrr`/`lapply`) so a per-set abort surfaces from
  # `ssd_distset()`, not an internal map frame (error-call-origin rule).
  for (nm in nms) {
    set <- sets[[nm]]
    if (!is.character(set) || !length(set)) {
      chk::abort_chk(
        "Distribution set ",
        encodeString(nm, quote = "\""),
        " must be a non-empty character vector of distribution names.",
        call = call
      )
    }
    if (anyNA(set)) {
      chk::abort_chk(
        "Distribution set ",
        encodeString(nm, quote = "\""),
        " must not contain missing values.",
        call = call
      )
    }
    if (anyDuplicated(set)) {
      chk::abort_chk(
        "Distribution set ",
        encodeString(nm, quote = "\""),
        " must not contain duplicate distribution names.",
        call = call
      )
    }
    unknown <- setdiff(set, all_dists)
    if (length(unknown)) {
      chk::abort_chk(
        "Distribution set ",
        encodeString(nm, quote = "\""),
        " names unknown distribution",
        if (length(unknown) > 1L) "s " else " ",
        chk::cc(encodeString(unknown, quote = "\""), conj = " and "),
        "; members must be a subset of `ssdtools::ssd_dists_all()`.",
        call = call
      )
    }
  }

  structure(sets, class = "ssdsims_distset")
}

#' @export
#' @noRd
print.ssdsims_distset <- function(x, ...) {
  cat("<ssdsims_distset>\n")
  for (nm in names(x)) {
    cat("  ", nm, ": ", paste(x[[nm]], collapse = ", "), "\n", sep = "")
  }
  invisible(x)
}
