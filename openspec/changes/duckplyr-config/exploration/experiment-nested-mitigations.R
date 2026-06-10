# Experiment: which DuckDB settings let the FAILING nested-write case pass?
#
# `experiment-nested-parquet.R` established that one 50k-double `samples` cell
# is harmless, but a shard of 1000 such rows (50M doubles, ~400MB of payload)
# peaks at ~2.3GB RSS and OOMs under `memory_limit='1GB'`. DuckDB's own error
# names the candidate mitigations; this matrix tests them on that failing case:
#   - SET preserve_insertion_order = false
#   - SET temp_directory (allow spilling; duckplyr's default in-memory DB has
#     none)
#   - COPY ... (ROW_GROUP_SIZE <n>) via `compute_parquet(options =)` (the
#     default 122 880-row groups are absurd when one row nests 50k doubles)
# each alone and combined, at memory_limit 1GB and 500MB, threads 1.
#
# RESULT lines as in experiment-nested-parquet.R. Run with:
#   Rscript openspec/changes/duckplyr-config/experiment-nested-mitigations.R --matrix

args <- commandArgs(trailingOnly = TRUE)

if ("--matrix" %in% args) {
  script <- sub(
    "^--file=",
    "",
    grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[[1L]]
  )
  cases <- expand.grid(
    memlim = c("1GB", "500MB"),
    order = c("", "false"),
    spill = c("", "yes"),
    rowgroup = c("", "2048"),
    stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(cases))) {
    env <- c(
      "EXP_ROWS=1000",
      "EXP_DOUBLES=50000",
      paste0("EXP_MEMLIM=", cases$memlim[i]),
      paste0("EXP_ORDER=", cases$order[i]),
      paste0("EXP_SPILL=", cases$spill[i]),
      paste0("EXP_ROWGROUP=", cases$rowgroup[i])
    )
    system2("Rscript", script, env = env)
  }
  quit(save = "no")
}

rows <- as.integer(Sys.getenv("EXP_ROWS", "1000"))
doubles <- as.integer(Sys.getenv("EXP_DOUBLES", "50000"))
memlim <- Sys.getenv("EXP_MEMLIM", "1GB")
order <- Sys.getenv("EXP_ORDER", "")
spill <- Sys.getenv("EXP_SPILL", "")
rowgroup <- Sys.getenv("EXP_ROWGROUP", "")

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
if (nzchar(spill)) {
  duckplyr::db_exec(paste0(
    "SET temp_directory = '",
    tempfile("duckdb-spill-"),
    "'"
  ))
}
copy_options <- if (nzchar(rowgroup)) {
  list(row_group_size = as.integer(rowgroup))
} else {
  NULL
}

df <- tibble::tibble(
  hc_id = sprintf("hc-%05d", seq_len(rows)),
  proportion = 0.05,
  est = as.double(seq_len(rows)),
  samples = lapply(seq_len(rows), function(i) as.double(seq_len(doubles)) + i)
)

path <- tempfile(fileext = ".parquet")
t0 <- proc.time()[["elapsed"]]
res <- tryCatch(
  {
    duckplyr::compute_parquet(
      duckplyr::as_duckdb_tibble(df),
      path,
      options = copy_options
    )
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
  back <- back[order(back$hc_id), ] # insertion order may be relaxed
  roundtrip <- if (identical(back$samples, df$samples)) {
    "identical"
  } else {
    "MISMATCH"
  }
}

cat(sprintf(
  paste0(
    "RESULT ok=%s rows=%d doubles=%d memlim=%s order=%s spill=%s rowgroup=%s ",
    "write_s=%.2f file_mb=%s rss_peak_mb=%d roundtrip=%s%s\n"
  ),
  ok,
  rows,
  doubles,
  memlim,
  if (nzchar(order)) "false" else "default",
  if (nzchar(spill)) "yes" else "no",
  if (nzchar(rowgroup)) rowgroup else "default",
  write_s,
  file_mb,
  rss_peak_mb(),
  roundtrip,
  if (ok) {
    ""
  } else {
    paste0(" error=", encodeString(substr(res, 1, 200), quote = "\""))
  }
))
