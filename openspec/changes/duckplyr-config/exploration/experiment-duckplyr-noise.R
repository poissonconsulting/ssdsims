# Experiment: what noise does duckplyr emit on the pipeline's code paths, and
# which switches silence it?
#
# Context (`duckplyr-message`, ROADMAP.md Next): "Turn off noise from
# duckplyr." Reading duckplyr 1.2.1's source pins three candidate sources:
#   - per-call fallback messages from `rel_try()`: emitted ONLY when
#     `DUCKPLYR_FALLBACK_INFO=TRUE` (opt-in, so silent by default);
#   - local telemetry collection (`tel_collect()`): ON by default
#     (`DUCKPLYR_FALLBACK_COLLECT` unset), silently writing fallback logs;
#   - the attach-time nudge (`fallback_autoupload()` via `.onAttach`): when
#     fallback logs EXIST and `DUCKPLYR_FALLBACK_AUTOUPLOAD` is unset, every
#     `library(duckplyr)` prints a multi-line "fallback events can be
#     collected and uploaded..." banner, plus the "Overwriting dplyr methods"
#     methods_overwrite() message.
#
# Each scenario runs in fresh R processes under a scrubbed HOME:
#   phase 1 (namespace-only, like ssdsims/workers): the pipeline's real
#     surface - shard write with a nested `samples` list-column, glob
#     read-back, projection, summary rewrite, collect - plus a verb that
#     forces a dplyr fallback (seeding telemetry logs);
#   phase 2 (attach, like an interactive analysis session): `library(duckplyr)`
#     with startup messages captured.
# The `silenced` scenario repeats both with the candidate de-noising env vars.
# Run with:
#   Rscript openspec/changes/duckplyr-config/experiment-duckplyr-noise.R --matrix

args <- commandArgs(trailingOnly = TRUE)

if ("--matrix" %in% args) {
  script <- sub(
    "^--file=",
    "",
    grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[[1L]]
  )
  scenarios <- list(
    default = character(),
    silenced = c(
      "DUCKPLYR_FALLBACK_COLLECT=0",
      "DUCKPLYR_FALLBACK_AUTOUPLOAD=0"
    )
  )
  for (name in names(scenarios)) {
    cat("==== scenario:", name, "====\n")
    # A scrubbed HOME so fallback logs / the once-per-user autoupload answer
    # cannot leak in from a previous run (stored under rappdirs' user dir).
    home <- tempfile("exp-home-")
    dir.create(home)
    env <- c(
      scenarios[[name]],
      paste0("HOME=", home),
      paste0("XDG_DATA_HOME=", file.path(home, ".local", "share"))
    )
    cat("-- phase 1: namespace-only pipeline surface --\n")
    system2("Rscript", script, env = c(env, "EXP_SCENARIO=TRUE"))
    cat("-- phase 2: library(duckplyr) attach --\n")
    system2("Rscript", script, env = c(env, "EXP_ATTACH=TRUE"))
  }
  quit(save = "no")
}

if (nzchar(Sys.getenv("EXP_ATTACH"))) {
  msgs <- character()
  withCallingHandlers(
    packageStartupMessage = function(cnd) {
      msgs <<- c(msgs, conditionMessage(cnd))
      invokeRestart("muffleMessage")
    },
    library(duckplyr)
  )
  if (length(msgs) == 0L) {
    cat("no startup messages\n")
  } else {
    cat("[startup]", gsub("\n", "\n[startup] ", paste(msgs, collapse = "\n")))
    cat("\n")
  }
  quit(save = "no")
}

stopifnot(nzchar(Sys.getenv("EXP_SCENARIO")))

conditions <- list()
record <- function(cnd) {
  conditions[[length(conditions) + 1L]] <<- cnd
  if (inherits(cnd, "message")) {
    invokeRestart("muffleMessage")
  } else {
    invokeRestart("muffleWarning")
  }
}

withCallingHandlers(
  message = record,
  warning = record,
  {
    df <- tibble::tibble(
      hc_id = sprintf("hc-%03d", 1:20),
      est = as.double(1:20),
      samples = lapply(1:20, function(i) as.double(seq_len(1000)) + i)
    )
    dir <- tempfile("noise-")
    path <- file.path(dir, "hc", "sim=1", "part.parquet")
    dir.create(dirname(path), recursive = TRUE)
    # write (ssd_write_parquet body)
    duckplyr::compute_parquet(duckplyr::as_duckdb_tibble(df), path)
    # glob read + projection + summary rewrite (ssd_summarise body)
    shards <- duckplyr::read_parquet_duckdb(
      file.path(dir, "hc", "**", "part.parquet"),
      options = list(hive_partitioning = FALSE)
    )
    compact <- dplyr::select(shards, -dplyr::any_of(c("dists", "samples")))
    duckplyr::compute_parquet(compact, file.path(dir, "summary.parquet"))
    # plain collect into R (ssd_read_parquet body)
    invisible(tibble::as_tibble(dplyr::collect(shards)))
    # verbs duckplyr cannot translate, to force the dplyr-fallback path (and
    # thereby seed telemetry logs for the attach phase)
    tbl <- duckplyr::as_duckdb_tibble(data.frame(x = 1:10, g = rep(1:2, 5)))
    invisible(dplyr::collect(dplyr::filter(
      dplyr::group_by(tbl, g),
      x > min(x) # grouped filter: not translatable, falls back
    )))
    invisible(dplyr::slice_sample(tbl, n = 2))
  }
)

if (length(conditions) == 0L) {
  cat("no conditions emitted\n")
} else {
  for (cnd in conditions) {
    cat(sprintf(
      "[%s] class=%s : %s\n",
      if (inherits(cnd, "message")) "message" else "warning",
      paste(setdiff(class(cnd), c("condition")), collapse = ","),
      trimws(conditionMessage(cnd))
    ))
  }
}
