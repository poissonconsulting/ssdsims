# Experiment: minimum memory to UNION >2048 one-row nested Parquets into one
# summary Parquet (the `summary-samples.parquet` / `path_with_samples` path).
#
# Insight (review): DuckDB's Parquet writer has a MINIMUM row-group size of
# 2048 rows. With 50k doubles per `samples` cell, one minimum row group is
# 2048 x 50000 x 8 = ~819 MB of child data that must be buffered - which is
# why `ROW_GROUP_SIZE 2048` changed nothing in
# `experiment-nested-mitigations.R` (it was already the floor). Shard writes
# stay small (a shard holds few rows), so the pinch point is the FULL SUMMARY
# fan-in: `ssd_summarise(path_with_samples =)` unions every hc shard and
# rewrites the `samples` column, crossing the 2048-row boundary.
#
# This experiment reproduces that path verbatim: a corpus of N one-row Parquet
# files (each `samples` = 50k doubles), read via the same
# `read_parquet_duckdb(glob, hive_partitioning = FALSE)` |> `compute_parquet()`
# pipeline `ssd_summarise()` uses (data never enters R), under a bisected
# `memory_limit` (threads = 1), plus cells probing the row-group floor:
#   - row_group_size 100 (does the writer honour < 2048? check actual
#     metadata),
#   - row_group_size_bytes 100MB (+ preserve_insertion_order = false, which
#     DuckDB requires for it to take effect),
#   - the compact-summary control (samples projected OUT) at a tiny limit.
#
# RESULT lines:
#   RESULT ok=. n_files=. memlim=. rowgroup=. rgbytes=. order=. compact=.
#     write_s=. file_mb=. rss_peak_mb=. row_groups=<actual row-group row
#     counts of the output> error=.
# Run with:
#   Rscript openspec/changes/duckplyr-config/exploration/experiment-summary-union.R --matrix

args <- commandArgs(trailingOnly = TRUE)
corpus_dir <- file.path(tempdir(), "ssdsims-summary-union-corpus")
corpus_env <- Sys.getenv("EXP_CORPUS", corpus_dir)

if ("--matrix" %in% args) {
  script <- sub(
    "^--file=",
    "",
    grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[[1L]]
  )
  # Persistent corpus shared by all cases (built once, ~0.2 MB per file).
  corpus <- file.path(dirname(tempdir()), "ssdsims-summary-union-corpus")
  system2(
    "Rscript",
    script,
    env = c(paste0("EXP_CORPUS=", corpus), "EXP_BUILD=4100")
  )
  cases <- rbind(
    # 1. bisect the memory floor for the full-summary union (n just over and
    #    well over one minimum row group)
    expand.grid(
      n = c(2100L, 4100L),
      memlim = c("500MB", "800MB", "1GB", "1.5GB", "2GB", "3GB"),
      rowgroup = "",
      rgbytes = "",
      order = "",
      compact = "FALSE",
      stringsAsFactors = FALSE
    ),
    # 2. row-group floor probes on the harder case
    data.frame(
      n = 4100L,
      memlim = c("1GB", "1GB", "500MB"),
      rowgroup = c("100", "", ""),
      rgbytes = c("", "100MB", "100MB"),
      order = c("", "false", "false"),
      compact = "FALSE"
    ),
    # 3. compact-summary control: samples projected out at the DuckDB level
    data.frame(
      n = 4100L,
      memlim = "100MB",
      rowgroup = "",
      rgbytes = "",
      order = "",
      compact = "TRUE"
    )
  )
  for (i in seq_len(nrow(cases))) {
    env <- c(
      paste0("EXP_CORPUS=", corpus),
      paste0("EXP_N=", cases$n[i]),
      paste0("EXP_MEMLIM=", cases$memlim[i]),
      paste0("EXP_ROWGROUP=", cases$rowgroup[i]),
      paste0("EXP_RGBYTES=", cases$rgbytes[i]),
      paste0("EXP_ORDER=", cases$order[i]),
      paste0("EXP_COMPACT=", cases$compact[i])
    )
    system2("Rscript", script, env = env)
  }
  quit(save = "no")
}

doubles <- 50000L

# ---- corpus build: N one-row Parquets, each `samples` = 50k doubles --------
build <- Sys.getenv("EXP_BUILD", "")
if (nzchar(build)) {
  n <- as.integer(build)
  suppressMessages(library(duckplyr))
  duckplyr::db_exec("SET threads TO 1")
  existing <- length(list.files(corpus_env, pattern = "[.]parquet$"))
  if (existing >= n) {
    cat("corpus ready:", existing, "files\n")
    quit(save = "no")
  }
  dir.create(corpus_env, recursive = TRUE, showWarnings = FALSE)
  t0 <- proc.time()[["elapsed"]]
  for (i in seq_len(n)) {
    df <- tibble::tibble(
      hc_id = sprintf("hc-%05d", i),
      est = as.double(i),
      samples = list(as.double(seq_len(doubles)) + i)
    )
    duckplyr::compute_parquet(
      duckplyr::as_duckdb_tibble(df),
      file.path(corpus_env, sprintf("part-%05d.parquet", i))
    )
  }
  cat(
    "corpus built:",
    n,
    "files in",
    round(proc.time()[["elapsed"]] - t0),
    "s\n"
  )
  quit(save = "no")
}

# ---- one union case --------------------------------------------------------
n <- as.integer(Sys.getenv("EXP_N", "2100"))
memlim <- Sys.getenv("EXP_MEMLIM", "1GB")
rowgroup <- Sys.getenv("EXP_ROWGROUP", "")
rgbytes <- Sys.getenv("EXP_RGBYTES", "")
order <- Sys.getenv("EXP_ORDER", "")
compact <- isTRUE(as.logical(Sys.getenv("EXP_COMPACT", "FALSE")))

rss_peak_mb <- function() {
  status <- readLines("/proc/self/status")
  hwm <- grep("^VmHWM:", status, value = TRUE)
  round(as.numeric(gsub("[^0-9]", "", hwm)) / 1024)
}

suppressMessages(library(duckplyr))
duckplyr::db_exec("SET threads TO 1")
duckplyr::db_exec(paste0("SET memory_limit = '", memlim, "'"))
if (nzchar(order)) {
  duckplyr::db_exec("SET preserve_insertion_order = false")
}

files <- head(
  list.files(corpus_env, pattern = "[.]parquet$", full.names = TRUE),
  n
)
stopifnot(length(files) == n)

copy_options <- c(
  if (nzchar(rowgroup)) list(row_group_size = as.integer(rowgroup)),
  if (nzchar(rgbytes)) list(row_group_size_bytes = rgbytes)
)
if (length(copy_options) == 0L) {
  copy_options <- NULL
}

out <- tempfile(fileext = ".parquet")
t0 <- proc.time()[["elapsed"]]
res <- tryCatch(
  {
    # The ssd_summarise() body: lazy glob read, optional projection, write -
    # the rows never materialise in R.
    tbl <- duckplyr::read_parquet_duckdb(
      files,
      options = list(hive_partitioning = FALSE)
    )
    if (compact) {
      tbl <- dplyr::select(tbl, -dplyr::any_of(c("dists", "samples")))
    }
    duckplyr::compute_parquet(tbl, out, options = copy_options)
    TRUE
  },
  error = function(e) conditionMessage(e)
)
write_s <- round(proc.time()[["elapsed"]] - t0, 2)

ok <- isTRUE(res)
file_mb <- if (ok) round(file.size(out) / 2^20, 1) else NA
row_groups <- if (ok) {
  meta <- dplyr::collect(duckplyr::read_sql_duckdb(paste0(
    "SELECT row_group_id, max(row_group_num_rows) AS rows ",
    "FROM parquet_metadata('",
    out,
    "') GROUP BY row_group_id ORDER BY row_group_id"
  )))
  paste(meta$rows, collapse = "+")
} else {
  "NA"
}

cat(sprintf(
  paste0(
    "RESULT ok=%s n_files=%d memlim=%s rowgroup=%s rgbytes=%s order=%s ",
    "compact=%s write_s=%.2f file_mb=%s rss_peak_mb=%d row_groups=%s%s\n"
  ),
  ok,
  n,
  memlim,
  if (nzchar(rowgroup)) rowgroup else "default",
  if (nzchar(rgbytes)) rgbytes else "default",
  if (nzchar(order)) "false" else "default",
  compact,
  write_s,
  file_mb,
  rss_peak_mb(),
  row_groups,
  if (ok) {
    ""
  } else {
    paste0(" error=", encodeString(substr(res, 1, 150), quote = "\""))
  }
))
