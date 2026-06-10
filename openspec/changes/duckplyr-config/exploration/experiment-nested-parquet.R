# Experiment: can duckplyr write a Parquet file with a nested list column
# containing 50k doubles - and at what memory/thread cost?
#
# Context (`duckplyr-config`, ROADMAP.md Next): cluster workers should run
# duckplyr with reduced main memory and a single thread. The suspected culprit
# for blow-ups is the nested `samples` list-column an hc shard carries when
# `scenario$hc$samples = TRUE` (`nboot` bootstrap draws per dist per task row,
# written through `ssd_write_parquet()` = `as_duckdb_tibble()` |>
# `compute_parquet()`).
#
# One case per fresh R process (a clean DuckDB instance per setting), driven by
# environment variables:
#   EXP_ROWS    number of rows in the shard-like tibble
#   EXP_DOUBLES doubles per `samples` cell
#   EXP_THREADS DuckDB `threads` ('' = leave default)
#   EXP_MEMLIM  DuckDB `memory_limit` ('' = leave default)
#   EXP_NAMED   'TRUE' to name the doubles in each cell (the targets-runner
#               FIXME: shards unname `samples` before writing)
#
# Each run prints one parseable RESULT line:
#   RESULT ok=<TRUE|FALSE> rows=. doubles=. threads=. memlim=. named=.
#     write_s=. file_mb=. rss_peak_mb=. roundtrip=<identical|MISMATCH|skipped>
#     error=<message>
#
# Run the full matrix with:
#   Rscript openspec/changes/duckplyr-config/experiment-nested-parquet.R --matrix

args <- commandArgs(trailingOnly = TRUE)

if ("--matrix" %in% args) {
  script <- sub(
    "^--file=",
    "",
    grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[[1L]]
  )
  cases <- expand.grid(
    rows = c(1L, 100L, 1000L),
    doubles = c(50000L),
    threads = c("", "1"),
    memlim = c("", "1GB", "100MB"),
    named = "FALSE",
    stringsAsFactors = FALSE
  )
  # The single-cell question asked verbatim, plus the named-cell FIXME probe.
  cases <- rbind(
    cases,
    data.frame(
      rows = c(1L, 10L),
      doubles = 50000L,
      threads = "",
      memlim = "",
      named = c("FALSE", "TRUE")
    )
  )
  for (i in seq_len(nrow(cases))) {
    env <- c(
      paste0("EXP_ROWS=", cases$rows[i]),
      paste0("EXP_DOUBLES=", cases$doubles[i]),
      paste0("EXP_THREADS=", cases$threads[i]),
      paste0("EXP_MEMLIM=", cases$memlim[i]),
      paste0("EXP_NAMED=", cases$named[i])
    )
    system2("Rscript", script, env = env)
  }
  quit(save = "no")
}

rows <- as.integer(Sys.getenv("EXP_ROWS", "1"))
doubles <- as.integer(Sys.getenv("EXP_DOUBLES", "50000"))
threads <- Sys.getenv("EXP_THREADS", "")
memlim <- Sys.getenv("EXP_MEMLIM", "")
named <- isTRUE(as.logical(Sys.getenv("EXP_NAMED", "FALSE")))

rss_peak_mb <- function() {
  status <- readLines("/proc/self/status")
  hwm <- grep("^VmHWM:", status, value = TRUE)
  round(as.numeric(gsub("[^0-9]", "", hwm)) / 1024)
}

suppressMessages(library(duckplyr))

if (nzchar(threads)) {
  duckplyr::db_exec(paste0("SET threads TO ", threads))
}
if (nzchar(memlim)) {
  duckplyr::db_exec(paste0("SET memory_limit = '", memlim, "'"))
}

# A shard-shaped tibble: ids + estimate columns + the nested `samples` draws.
cell <- function(i) {
  x <- as.double(seq_len(doubles)) + i
  if (named) {
    names(x) <- paste0("d", seq_len(doubles))
  }
  x
}
df <- tibble::tibble(
  hc_id = sprintf("hc-%05d", seq_len(rows)),
  proportion = 0.05,
  est = as.double(seq_len(rows)),
  samples = lapply(seq_len(rows), cell)
)

path <- tempfile(fileext = ".parquet")
t0 <- proc.time()[["elapsed"]]
res <- tryCatch(
  {
    # The exact `ssd_write_parquet()` body (R/targets-runner.R).
    duckplyr::compute_parquet(duckplyr::as_duckdb_tibble(df), path)
    TRUE
  },
  error = function(e) conditionMessage(e)
)
write_s <- round(proc.time()[["elapsed"]] - t0, 2)

ok <- isTRUE(res)
file_mb <- if (ok) round(file.size(path) / 2^20, 1) else NA
roundtrip <- "skipped"
if (ok) {
  back <- tibble::as_tibble(dplyr::collect(duckplyr::read_parquet_duckdb(
    path,
    options = list(hive_partitioning = FALSE)
  )))
  roundtrip <- if (identical(back$samples, lapply(df$samples, unname))) {
    "identical"
  } else {
    "MISMATCH"
  }
}

cat(sprintf(
  paste0(
    "RESULT ok=%s rows=%d doubles=%d threads=%s memlim=%s named=%s ",
    "write_s=%.2f file_mb=%s rss_peak_mb=%d roundtrip=%s%s\n"
  ),
  ok,
  rows,
  doubles,
  if (nzchar(threads)) threads else "default",
  if (nzchar(memlim)) memlim else "default",
  named,
  write_s,
  file_mb,
  rss_peak_mb(),
  roundtrip,
  if (ok) "" else paste0(" error=", encodeString(res, quote = "\""))
))
