# Shard grouping: turn a step's per-task table into a per-shard table - one row
# per `partition_by` path cell, with a `tasks` list-column of that cell's task
# rows (TARGETS-DESIGN.md section 5/section 6). This is the `values` a
# `tarchetypes::tar_map()` consumes to mint one named target per shard (static
# branching, section 6). Grouping is RNG-free: a task's `(seed, primer)` is
# attached here (a pure hash via `task_primers()`, not a draw), so the
# `task-lists` "no seed/primer on the bare derivations" contract is preserved
# while putting the primer where the shard body consumes it. Each task's
# `(seed, primer)` is identical to the baseline runner's, so sharding is a pure
# re-layout that never changes results.
#
# The split between path axes (Hive directory levels, one shard per cell) and
# inner axes (Parquet columns within a shard) comes from `partition-by`'s single
# source of truth, `scenario_partition_axes(scenario, step)`; this code does not
# duplicate it. The task rows in `tasks` carry every axis value (and the
# `<step>_id`/`<parent>_id` keys), so a runner computes its own and its parent's
# partition path with the existing `path_key()` - no extra path columns needed.

#' Group sample Tasks into Shards
#'
#' Groups the scenario's `sample` task table ([ssd_scenario_sample_tasks()]) by
#' its `partition_by$sample` **path** axes into one row per shard: each shard row
#' carries the path-axis columns (the `tar_map` target-name suffix and Hive path)
#' and a `tasks` list-column of that shard's task rows, each decorated with
#' `seed = scenario$seed` and its per-task `primer` ([task_primer()] over
#' `task_axes("sample")`). The decoration is RNG-free; the bare task table keeps
#' its no-`(seed, primer)` contract.
#'
#' @param scenario An `ssdsims_scenario` from [ssd_define_scenario()].
#' @return A tibble with one row per `sample` shard: the path-axis columns and a
#'   `tasks` list-column. Suitable as `tarchetypes::tar_map(values = )`.
#' @seealso [ssd_run_sample_step()], [ssd_scenario_fit_shards()].
#' @export
#' @examples
#' scenario <- ssd_define_scenario(ssddata::ccme_boron, nsim = 2L, seed = 42L)
#' ssd_scenario_sample_shards(scenario)
ssd_scenario_sample_shards <- function(scenario) {
  scenario_shards(scenario, "sample")
}

#' Group fit Tasks into Shards
#'
#' As [ssd_scenario_sample_shards()] for the `fit` step: groups
#' [ssd_scenario_fit_tasks()] by `partition_by$fit`. Each task row in `tasks`
#' carries its parent `sample` path-axis values and `sample_id`, so the runner
#' opens the matching `sample` shard by partition path.
#'
#' @inheritParams ssd_scenario_sample_shards
#' @return A tibble with one row per `fit` shard (path-axis columns + a `tasks`
#'   list-column).
#' @seealso [ssd_run_fit_step()].
#' @export
#' @examples
#' scenario <- ssd_define_scenario(
#'   ssddata::ccme_boron,
#'   nsim = 2L,
#'   seed = 42L,
#'   rescale = c(FALSE, TRUE)
#' )
#' ssd_scenario_fit_shards(scenario)
ssd_scenario_fit_shards <- function(scenario) {
  scenario_shards(scenario, "fit")
}

#' Group hc Tasks into Shards
#'
#' As [ssd_scenario_sample_shards()] for the `hc` step: groups
#' [ssd_scenario_hc_tasks()] by `partition_by$hc`. Each task row in `tasks`
#' carries its parent `fit` path-axis values and `fit_id`, so the runner opens
#' the matching `fit` shard by partition path.
#'
#' @inheritParams ssd_scenario_sample_shards
#' @return A tibble with one row per `hc` shard (path-axis columns + a `tasks`
#'   list-column).
#' @seealso [ssd_run_hc_step()].
#' @export
#' @examples
#' scenario <- ssd_define_scenario(
#'   ssddata::ccme_boron,
#'   nsim = 2L,
#'   seed = 42L,
#'   ci = c(FALSE, TRUE)
#' )
#' ssd_scenario_hc_shards(scenario)
ssd_scenario_hc_shards <- function(scenario) {
  scenario_shards(scenario, "hc")
}

# Shared body: derive the step's task table, decorate each row with
# `(seed, primer)` (RNG-free), and split into one shard row per `partition_by`
# path cell. The path-axis columns lead (the `tar_map` `names` / Hive path); the
# `tasks` list-column holds the *full* task rows (path axes included) so a runner
# has every axis value and the `<step>_id`/`<parent>_id` keys it needs.
scenario_shards <- function(scenario, step) {
  chk::chk_s3_class(scenario, "ssdsims_scenario")
  tbl <- tibble::as_tibble(ssd_scenario_tasks(scenario, step))
  tbl$seed <- scenario$seed
  tbl$primer <- task_primers(tbl, step)
  path_axes <- scenario_partition_axes(scenario, step)$path
  grouped <- dplyr::group_by(tbl, dplyr::across(dplyr::all_of(path_axes)))
  shards <- dplyr::group_keys(grouped)
  shards$tasks <- dplyr::group_split(grouped, .keep = TRUE)
  tibble::as_tibble(shards)
}
