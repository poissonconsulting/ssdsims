#' Expand a Scenario into its Task Grid
#'
#' Returns the fully-expanded tibble of tasks that
#' [`ssd_run_scenario2()`] iterates over. Each row is a single
#' (sim, stream, ...) task with its own L'Ecuyer-CMRG substream states.
#' Exposed for `targets`/`tarchetypes` pipelines that partition tasks
#' into jobs (one Parquet per job).
#'
#' @param scenario An `ssdsims_scenario` object built by
#' [`ssd_sim_data2()`].
#' @return A tibble with one row per task.
#' @export
#' @examples
#' tasks <- ssd_scenario_tasks(ssd_sim_data2(ssddata::ccme_boron, nsim = 2))
#' nrow(tasks)
ssd_scenario_tasks <- function(scenario) {
  chk::chk_is(scenario, "ssdsims_scenario")
  build_tasks(scenario)
}

#' Run a Job (Subset of Tasks)
#'
#' Runs one or more tasks (rows from [`ssd_scenario_tasks()`]) using the
#' shared parameters in `scenario`, and returns a tibble of results with
#' `data`, `fits` and `hc` list-columns. Designed to be called from
#' inside a `targets` target so the result can be written to one Parquet
#' file per job.
#'
#' @param tasks A subset of the tibble returned by
#' [`ssd_scenario_tasks()`]. May be one or many rows.
#' @param scenario The `ssdsims_scenario` object the `tasks` were
#' derived from.
#' @return A tibble matching the shape produced by
#' [`ssd_run_scenario2()`] for the supplied rows.
#' @export
#' @examples
#' scenario <- ssd_sim_data2(ssddata::ccme_boron, nsim = 2)
#' tasks <- ssd_scenario_tasks(scenario)
#' ssd_run_job(tasks[1, , drop = FALSE], scenario)
ssd_run_job <- function(tasks, scenario) {
  chk::chk_is(scenario, "ssdsims_scenario")
  chk::chk_data(tasks)

  if (!nrow(tasks)) {
    tasks$data <- list()
    tasks$fits <- list()
    tasks$hc <- list()
    return(drop_internal_cols(tasks))
  }

  results <- purrr::map(
    seq_len(nrow(tasks)),
    \(i) run_task(tasks[i, , drop = FALSE], scenario)
  )

  tasks$data <- purrr::map(results, "data")
  tasks$fits <- purrr::map(results, "fits")
  tasks$hc <- purrr::map(results, "hc")

  drop_internal_cols(tasks)
}

#' Persist a Job Result to Parquet
#'
#' Writes a tibble produced by [`ssd_run_job()`] (or
#' [`ssd_run_scenario2()`]) to Parquet. Columns that Arrow cannot
#' represent natively are serialized to qs2 binary blobs (held in
#' sibling `*_qs2` raw-vector columns); the original columns are
#' dropped from the file and reconstructed on read. By default the
#' fits, min_pmix and args list-columns are qs2-blobbed; native list
#' types (data, hc tibbles, range_shape numeric vectors) are kept as
#' Arrow nested structures.
#'
#' @param job A job tibble (output of [`ssd_run_job()`] or
#' [`ssd_run_scenario2()`]).
#' @param path Destination Parquet file path.
#' @return `path`, invisibly.
#' @seealso [`ssd_read_job_parquet()`].
#' @export
ssd_write_job_parquet <- function(job, path) {
  chk::chk_data(job)
  chk::chk_string(path)
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required to write Parquet output.")
  }
  if (!requireNamespace("qs2", quietly = TRUE)) {
    stop("Package 'qs2' is required to write qs2-encoded columns.")
  }
  for (col in qs2_columns(job)) {
    job[[paste0(col, "_qs2")]] <- lapply(job[[col]], qs2::qs_serialize)
    job[[col]] <- NULL
  }
  arrow::write_parquet(job, path)
  invisible(path)
}

#' Read a Job Result from Parquet
#'
#' Reads a Parquet file produced by [`ssd_write_job_parquet()`].
#' qs2-encoded columns are decoded back to their original R objects
#' when `decode = TRUE` (the default).
#'
#' @param path Parquet file path.
#' @param decode Whether to decode `*_qs2` columns back to native R
#' objects. Set to `FALSE` to inspect just the metadata.
#' @return A tibble matching the shape of the input to
#' [`ssd_write_job_parquet()`].
#' @seealso [`ssd_write_job_parquet()`].
#' @export
ssd_read_job_parquet <- function(path, decode = TRUE) {
  chk::chk_string(path)
  chk::chk_flag(decode)
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required to read Parquet output.")
  }
  job <- dplyr::as_tibble(arrow::read_parquet(path))
  if (decode) {
    if (!requireNamespace("qs2", quietly = TRUE)) {
      stop("Package 'qs2' is required to decode qs2-encoded columns.")
    }
    blob_cols <- grep("_qs2$", names(job), value = TRUE)
    for (bc in blob_cols) {
      decoded <- lapply(job[[bc]], function(raw) qs2::qs_deserialize(raw))
      job[[sub("_qs2$", "", bc)]] <- decoded
      job[[bc]] <- NULL
    }
  }
  job
}

qs2_columns <- function(job) {
  intersect(c("fits", "min_pmix", "args"), names(job))
}
