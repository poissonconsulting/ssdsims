# Scenario accessors: isolate an already-materialised value from a scenario by
# name (TARGETS-DESIGN.md section 1.1). Datasets are materialised inline by
# `ssd_define_scenario()` (validated via `ssd_scenario_data()`); `min_pmix` functions are
# materialised on the scenario keyed by name at construction. These accessors do
# the one job a shard body needs: pull a value out by name. They perform no
# registration, persistence, or re-validation - the values arrive ready to use.
# The per-task hash keys on the *name* (`task_axes()`), never on these values, so
# carrying functions/tibbles on the scenario does not affect task identity.

#' Isolate a Materialised Dataset from a Scenario by Name
#'
#' Returns the validated, materialised dataset tibble stored on `scenario` under
#' `name`. The dataset was validated (a numeric `Conc` column) and materialised
#' at construction by [ssd_define_scenario()], so this accessor performs no
#' registration, persistence, or re-validation - it just isolates the value a
#' shard body fits. Aborts with an informative error when `name` is not one of
#' the scenario's datasets.
#'
#' Names - not values - drive task hashing (`TARGETS-DESIGN.md` section 1.1):
#' the task path carries the dataset *name* and this accessor resolves it back to
#' the tibble at run time, so the tibble never enters a task identity.
#'
#' @param scenario An `ssdsims_scenario` from [ssd_define_scenario()].
#' @param name A scalar string naming one of the scenario's datasets.
#' @return The materialised dataset tibble stored under `name`.
#' @seealso [scenario_min_pmix()] for the `min_pmix` counterpart.
#' @export
#' @examples
#' data <- ssd_scenario_data(ssddata::ccme_boron)
#' scenario <- ssd_define_scenario(data, nsim = 1L, seed = 42L)
#' scenario_dataset(scenario, "ccme_boron")
scenario_dataset <- function(scenario, name) {
  chk::chk_s3_class(scenario, "ssdsims_scenario")
  chk::chk_string(name)
  known <- names(scenario$data)
  if (!name %in% known) {
    chk::abort_chk(
      "Unknown dataset ",
      encodeString(name, quote = "\""),
      "; scenario datasets are ",
      chk::cc(known, conj = " and "),
      "."
    )
  }
  scenario$data[[name]]
}

#' Isolate a Materialised `min_pmix` Function from a Scenario by Name
#'
#' Returns the single-argument `min_pmix` function materialised on `scenario`
#' under `name`. [ssd_define_scenario()] resolves each `min_pmix` reference to a
#' function once, at construction (so a cluster worker needs no shared
#' interactive environment), and stores it keyed by name; this accessor isolates
#' it. Aborts with an informative error when `name` is not one of the scenario's
#' `min_pmix` names.
#'
#' Names - not function values - drive task hashing (`TARGETS-DESIGN.md` section
#' 1.1): the fit-task path carries the `min_pmix` *name*, and this accessor
#' resolves it back to the function at run time, so the function value never
#' enters a task identity (no byte-stability concern from byte-compilation or
#' captured environments).
#'
#' @inheritParams scenario_dataset
#' @param name A scalar string naming one of the scenario's `min_pmix` entries.
#' @return The single-argument `min_pmix` function stored under `name`.
#' @seealso [scenario_dataset()] for the dataset counterpart.
#' @export
#' @examples
#' data <- ssd_scenario_data(ssddata::ccme_boron)
#' scenario <- ssd_define_scenario(data, nsim = 1L, seed = 42L)
#' scenario_min_pmix(scenario, "ssd_min_pmix")
scenario_min_pmix <- function(scenario, name) {
  chk::chk_s3_class(scenario, "ssdsims_scenario")
  chk::chk_string(name)
  known <- names(scenario$min_pmix_fns)
  if (!name %in% known) {
    chk::abort_chk(
      "Unknown `min_pmix` name ",
      encodeString(name, quote = "\""),
      "; scenario `min_pmix` names are ",
      chk::cc(known, conj = " and "),
      "."
    )
  }
  scenario$min_pmix_fns[[name]]
}

#' Isolate a Distribution Set from a Scenario by Name
#'
#' Returns the member character vector of the distribution set stored on
#' `scenario` under `name` (from `scenario$hc$distsets`). [ssd_define_scenario()]
#' validates each set at construction (via [ssd_distset()]), so this accessor
#' performs no registration, persistence, or re-validation - it just isolates the
#' members the hc runner subsets the parent union fit by. Aborts with an
#' informative error when `name` is not one of the scenario's distribution-set
#' names.
#'
#' Names - not members - drive task hashing (`TARGETS-DESIGN.md` section 1.1):
#' the hc-task path carries the `distset` *name*, and this accessor resolves it
#' back to the member vector at run time, so the members never enter a task
#' identity (the same by-name pattern as `min_pmix` and datasets).
#'
#' @inheritParams scenario_dataset
#' @param name A scalar string naming one of the scenario's distribution sets.
#' @return The member character vector of the distribution set stored under
#'   `name`.
#' @seealso [ssd_distset()], [scenario_min_pmix()].
#' @export
#' @examples
#' data <- ssd_scenario_data(ssddata::ccme_boron)
#' scenario <- ssd_define_scenario(
#'   data,
#'   nsim = 1L,
#'   seed = 42L,
#'   dists = ssd_distset(BCANZ = ssdtools::ssd_dists_bcanz())
#' )
#' scenario_distset(scenario, "BCANZ")
scenario_distset <- function(scenario, name) {
  chk::chk_s3_class(scenario, "ssdsims_scenario")
  chk::chk_string(name)
  known <- names(scenario$hc$distsets)
  if (!name %in% known) {
    chk::abort_chk(
      "Unknown distribution set ",
      encodeString(name, quote = "\""),
      "; scenario distribution sets are ",
      chk::cc(known, conj = " and "),
      "."
    )
  }
  scenario$hc$distsets[[name]]
}
