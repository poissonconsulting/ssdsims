# Experiment: is NESTING the culprit, or just the payload size?
#
# `experiment-nested-mitigations.R` showed that none of DuckDB's recommended
# knobs (preserve_insertion_order, temp_directory spill, ROW_GROUP_SIZE) move
# the OOM point of the nested write at all, and that the failing allocation is
# exactly `rows * doubles * 8` bytes - the LIST column's entire child array as
# ONE buffer. Three questions follow:
#
#   1. floor: what memory_limit does the 1000 x 50k nested shard need?
#   2. flat:  does the SAME payload as a flat long table (50M rows of
#      (hc_id, draw)) write under limits where the nested form OOMs?
#   3. stage: does the nested OOM happen in the Parquet WRITER, or already at
#      df->DuckDB ingestion (`compute()` into a table, no Parquet)?
#
# RESULT lines as before, plus shape= and stage=. Run with:
#   Rscript openspec/changes/duckplyr-config/experiment-nested-vs-flat.R --matrix

args <- commandArgs(trailingOnly = TRUE)

if ("--matrix" %in% args) {
  script <- sub(
    "^--file=",
    "",
    grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[[1L]]
  )
  cases <- rbind(
    # 1. the nested floor
    data.frame(
      shape = "nested",
      stage = "parquet",
      memlim = c("1.2GB", "1.5GB", "2GB")
    ),
    # 2. the same 50M-double payload, flat
    data.frame(
      shape = "flat",
      stage = "parquet",
      memlim = c("1GB", "500MB", "100MB")
    ),
    # 3. nested, but materialise to a DuckDB table instead of Parquet
    data.frame(
      shape = "nested",
      stage = "table",
      memlim = c("1GB", "500MB")
    )
  )
  for (i in seq_len(nrow(cases))) {
    env <- c(
      paste0("EXP_SHAPE=", cases$shape[i]),
      paste0("EXP_STAGE=", cases$stage[i]),
      paste0("EXP_MEMLIM=", cases$memlim[i])
    )
    system2("Rscript", script, env = env)
  }
  quit(save = "no")
}

shape <- Sys.getenv("EXP_SHAPE", "nested")
stage <- Sys.getenv("EXP_STAGE", "parquet")
memlim <- Sys.getenv("EXP_MEMLIM", "1GB")
rows <- 1000L
doubles <- 50000L

rss_peak_mb <- function() {
  status <- readLines("/proc/self/status")
  hwm <- grep("^VmHWM:", status, value = TRUE)
  round(as.numeric(gsub("[^0-9]", "", hwm)) / 1024)
}

suppressMessages(library(duckplyr))
duckplyr::db_exec("SET threads TO 1")
duckplyr::db_exec(paste0("SET memory_limit = '", memlim, "'"))

df <- if (shape == "nested") {
  tibble::tibble(
    hc_id = sprintf("hc-%05d", seq_len(rows)),
    samples = lapply(seq_len(rows), function(i) as.double(seq_len(doubles)) + i)
  )
} else {
  tibble::tibble(
    hc_id = rep(sprintf("hc-%05d", seq_len(rows)), each = doubles),
    draw = as.double(seq_len(rows * doubles))
  )
}

path <- tempfile(fileext = ".parquet")
t0 <- proc.time()[["elapsed"]]
res <- tryCatch(
  {
    tbl <- duckplyr::as_duckdb_tibble(df)
    if (stage == "parquet") {
      duckplyr::compute_parquet(tbl, path)
    } else {
      dplyr::compute(tbl)
    }
    TRUE
  },
  error = function(e) conditionMessage(e)
)
write_s <- round(proc.time()[["elapsed"]] - t0, 2)

ok <- isTRUE(res)
file_mb <- if (ok && stage == "parquet") {
  round(file.size(path) / 2^20, 1)
} else {
  NA
}

cat(sprintf(
  paste0(
    "RESULT ok=%s shape=%s stage=%s rows=%d doubles=%d memlim=%s ",
    "write_s=%.2f file_mb=%s rss_peak_mb=%d%s\n"
  ),
  ok,
  shape,
  stage,
  rows,
  doubles,
  memlim,
  write_s,
  file_mb,
  rss_peak_mb(),
  if (ok) {
    ""
  } else {
    paste0(" error=", encodeString(substr(res, 1, 150), quote = "\""))
  }
))
