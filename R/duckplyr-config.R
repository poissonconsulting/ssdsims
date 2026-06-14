# Pipeline-scoped duckplyr/DuckDB configuration (the `duckplyr-config`
# change). duckplyr's managed DuckDB instance configures itself from the
# MACHINE (all cores, ~80% of node RAM), not the JOB: on a one-CPU/few-GB
# cluster worker that oversubscribes the CPU allocation and overruns the
# memory cgroup (an OOM kill, not a clean R error). The pipeline entry points
# (the per-shard step runners, `ssd_summarise()`, and the single-core
# `ssd_run_scenario_shards()`) therefore open a `local_duckplyr_config()`
# scope: a single thread, an explicit memory limit, and silenced duckplyr
# fallback telemetry - all restored when the scope exits, so a user's own
# duckplyr session settings are untouched outside the pipeline.

# The DuckDB settings a config scope touches, read back via
# `duckplyr::read_sql_duckdb()` (the snapshot the deferred restore replays).
# `threads` arrives numeric and `preserve_insertion_order` boolean;
# `memory_limit` arrives as the engine's RENDERED string (e.g. "953.6 MiB"),
# which is lossy - see `duckplyr_memory_limit_restore()` for the
# round-trip-exact restore value.
duckplyr_current_settings <- function() {
  settings <- tibble::as_tibble(dplyr::collect(duckplyr::read_sql_duckdb(
    paste0(
      "SELECT current_setting('threads') AS threads, ",
      "current_setting('memory_limit') AS memory_limit, ",
      "current_setting('preserve_insertion_order') AS preserve_insertion_order"
    )
  )))
  list(
    threads = as.integer(settings$threads[[1L]]),
    memory_limit = settings$memory_limit[[1L]],
    preserve_insertion_order = isTRUE(as.logical(settings$preserve_insertion_order[[
      1L
    ]]))
  )
}

# A `SET memory_limit` value (exact bytes, the `B` suffix) that renders back
# to exactly the snapshot string `rendered`. DuckDB reports `memory_limit`
# rounded DOWN to one decimal of a binary unit ("1GB" reads back as
# "953.6 MiB"), and it floors at every 1024 hop - so naively re-`SET`ting the
# reported string drifts downward one notch per snapshot/restore cycle
# ("286.1 MiB" -> "286.0 MiB" -> ...). Choosing the smallest byte count that
# still reports as `rendered` - ceiling at the unit one BELOW the displayed
# one - makes restore idempotent: the reported setting round-trips exactly
# (verified across B/KiB/MiB/GiB/TiB renderings; the true byte value is
# within one displayed decimal of the original, which the lossy getter
# cannot recover).
duckplyr_memory_limit_restore <- function(rendered) {
  m <- regmatches(rendered, regexec("^([0-9.]+) ?([A-Za-z]+)$", rendered))[[1L]]
  units <- c(B = 1, KiB = 1024, MiB = 1024^2, GiB = 1024^3, TiB = 1024^4)
  if (length(m) != 3L || !m[[3L]] %in% names(units)) {
    # An unrecognised rendering (future engine): fall back to replaying the
    # string itself - possibly one notch lossy, never failing the restore.
    return(rendered)
  }
  mult <- units[[m[[3L]]]]
  value <- as.numeric(m[[2L]])
  bytes <- if (mult == 1) {
    ceiling(value)
  } else {
    ceiling(value * 1024) * (mult / 1024)
  }
  sprintf("%.0fB", bytes)
}

# The `SSDSIMS_DUCKDB_THREADS` knob: a positive whole number, default 1
# (the worker has one CPU; `exploration/RESULTS.md` shows a single thread is
# no slower - 2x faster, even, on the large nested shard). Aborts in the
# context of `call` (the user-facing runner) on a malformed value.
duckplyr_env_threads <- function(call = rlang::caller_env()) {
  value <- Sys.getenv("SSDSIMS_DUCKDB_THREADS", unset = "")
  if (!nzchar(value)) {
    return(1L)
  }
  threads <- suppressWarnings(as.integer(value))
  if (is.na(threads) || threads < 1L || threads != as.numeric(value)) {
    chk::abort_chk(
      "`SSDSIMS_DUCKDB_THREADS` must be a positive whole number, not ",
      encodeString(value, quote = "\""),
      ".",
      call = call
    )
  }
  threads
}

# The `SSDSIMS_DUCKDB_MEMORY_LIMIT` knob, default "1GB". The value is passed
# through to DuckDB's `SET memory_limit`, whose parser is the authority - a
# malformed value surfaces DuckDB's own error rather than being silently
# ignored.
duckplyr_env_memory_limit <- function() {
  value <- Sys.getenv("SSDSIMS_DUCKDB_MEMORY_LIMIT", unset = "")
  if (!nzchar(value)) "1GB" else value
}

#' Pipeline-Scoped duckplyr/DuckDB Configuration
#'
#' Configures duckplyr's managed DuckDB connection for pipeline work for the
#' duration of the calling frame, restoring the prior configuration when
#' `.local_envir` exits (the withr convention; compare [local_dqrng_backend()]).
#' Within the scope:
#'
#' * DuckDB `threads` is set from `SSDSIMS_DUCKDB_THREADS` (default **1** -
#'   matching the one-CPU worker the cluster template requests; the
#'   `duckplyr-config` change's `exploration/` measurements show a single
#'   thread is no slower on the pipeline's writes).
#' * DuckDB `memory_limit` is set from `SSDSIMS_DUCKDB_MEMORY_LIMIT` (default
#'   **1GB** - never the engine's machine-derived 80% of node RAM, so a write
#'   that outgrows the worker fails as a loud, catchable R error instead of a
#'   scheduler cgroup kill).
#' * duckplyr's fallback telemetry is silenced (`DUCKPLYR_FALLBACK_COLLECT=0`,
#'   `DUCKPLYR_FALLBACK_AUTOUPLOAD=0`, scoped env vars), so pipeline work
#'   neither writes fallback logs nor seeds duckplyr's interactive attach-time
#'   banner.
#' * DuckDB `preserve_insertion_order` is set to `false`, so the full-summary
#'   write may pass `ROW_GROUP_SIZE_BYTES` (the engine refuses it while
#'   preserving order, and its Binder check consults only the GLOBAL setting -
#'   the per-copy `PRESERVE_ORDER` option cannot substitute; see the
#'   `duckplyr-config` change's
#'   `exploration/experiment-preserve-order-copy-option.R`). Under the default
#'   single thread there is one producer, so writes were observed in input
#'   order and byte-identical across runs regardless; with
#'   `SSDSIMS_DUCKDB_THREADS` raised above one, row order within written
#'   files is no longer guaranteed (values are unaffected).
#'
#' **Raising the memory limit**: set the env var - on a cluster via the
#' controller's `script_lines` (e.g. `export SSDSIMS_DUCKDB_MEMORY_LIMIT=3GB`
#' beside `memory_gigabytes_per_cpu = 4`), interactively via
#' `Sys.setenv(SSDSIMS_DUCKDB_MEMORY_LIMIT = "3GB")`. **Implications**: keep
#' headroom for R's own footprint within the scheduler allocation (R holds a
#' shard's draws while DuckDB ingests them, so budget roughly half the job
#' for DuckDB), and a higher limit only moves the *shard*-payload floor -
#' writing a shard whose `samples` list-column holds `P` bytes of draws needs
#' `memory_limit` of about `5 * P`, because the engine demands the list
#' column's child array as a single allocation it can neither spill nor
#' stream. The full summary's floor instead follows `ssd_summarise()`'s
#' `samples_row_group_bytes` budget and never needs a higher limit. Evidence:
#' the `duckplyr-config` change's `exploration/RESULTS.md`.
#'
#' The settings are connection-global while the scope is open: any other
#' duckplyr work the same process does mid-scope sees one thread and the
#' capped memory. On workers the process is the pipeline; interactively the
#' scope is per-call and restores on exit. Nested scopes are safe - each
#' snapshot/restore layer replays the one above it.
#'
#' @inheritParams withr::local_seed
#' @return `NULL`, invisibly (called for its scoped side effects).
#' @noRd
local_duckplyr_config <- function(.local_envir = parent.frame()) {
  chk::chk_environment(.local_envir)
  threads <- duckplyr_env_threads(call = .local_envir)
  memory_limit <- duckplyr_env_memory_limit()
  prior <- duckplyr_current_settings()
  restore_limit <- duckplyr_memory_limit_restore(prior$memory_limit)
  # Register the restore BEFORE applying the new settings, so a rejected
  # memory value (DuckDB's parser is the authority) cannot leave the threads
  # setting changed with no restore scheduled.
  withr::defer(
    {
      duckplyr::db_exec(paste0("SET threads TO ", prior$threads))
      duckplyr::db_exec(paste0("SET memory_limit = '", restore_limit, "'"))
      duckplyr::db_exec(paste0(
        "SET preserve_insertion_order = ",
        if (prior$preserve_insertion_order) "true" else "false"
      ))
    },
    envir = .local_envir
  )
  duckplyr::db_exec(paste0("SET threads TO ", threads))
  duckplyr::db_exec(paste0("SET memory_limit = '", memory_limit, "'"))
  duckplyr::db_exec("SET preserve_insertion_order = false")
  withr::local_envvar(
    DUCKPLYR_FALLBACK_COLLECT = "0",
    DUCKPLYR_FALLBACK_AUTOUPLOAD = "0",
    .local_envir = .local_envir
  )
  invisible(NULL)
}
