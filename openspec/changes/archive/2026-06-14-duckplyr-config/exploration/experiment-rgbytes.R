# Experiment: is ROW_GROUP_SIZE_BYTES the right configuration option for the
# full-summary write?
#
# `experiment-summary-union.R` showed both writer configuration options
# rescue the >2048-row nested union: a fixed `ROW_GROUP_SIZE`
# (order-preserving) and `ROW_GROUP_SIZE_BYTES` (payload-aware, but documented
# as requiring `preserve_insertion_order = false`). Review preference: accept
# nondeterministic order; the bytes configuration option is attractive because
# it yields LARGE row groups when `samples` cells are small and small groups
# when they are huge - no scenario-derived arithmetic. Open questions, one per
# block:
#
#   A. requirement: is the bytes configuration option really inert with
#      insertion order preserved (the documented constraint), or does it work
#      anyway?
#   B. determinism in practice: with threads = 1 and
#      preserve_insertion_order = false, do repeated runs produce identical
#      bytes, and is the input row order preserved anyway?
#   C. adaptivity: with SMALL cells (1000 doubles, the `nboot = 1000`
#      default), does the bytes cap produce correspondingly large row groups?
#   D. floor: how low can `memory_limit` go for the 4100 x 50k union under
#      the bytes cap (and a smaller 32MB cap)?
#
# Corpora (built once, reused):
#   big:   4100 one-row files, one 50k-double `samples` cell each
#          (shared with experiment-summary-union.R)
#   small: 30 files x 1000 rows, 1000-double cells (30000 rows, ~240MB)
# Run with:
#   Rscript openspec/changes/duckplyr-config/exploration/experiment-rgbytes.R --matrix

args <- commandArgs(trailingOnly = TRUE)
big_corpus <- file.path(dirname(tempdir()), "ssdsims-summary-union-corpus")
small_corpus <- file.path(dirname(tempdir()), "ssdsims-rgbytes-small-corpus")

if ("--matrix" %in% args) {
  script <- sub(
    "^--file=",
    "",
    grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[[1L]]
  )
  system2("Rscript", script, env = "EXP_BUILD=TRUE")
  cases <- rbind(
    # A. does the bytes configuration option need
    #    preserve_insertion_order = false?
    data.frame(
      corpus = "big",
      memlim = "1GB",
      rgbytes = "100MB",
      order = "",
      mode = "single"
    ),
    # B. determinism + order with order=false, threads=1 (two writes, hashed)
    data.frame(
      corpus = "big",
      memlim = "1GB",
      rgbytes = "100MB",
      order = "false",
      mode = "determinism"
    ),
    # C. adaptivity on small cells, both order modes
    data.frame(
      corpus = "small",
      memlim = "500MB",
      rgbytes = c("100MB", "100MB"),
      order = c("false", ""),
      mode = "single"
    ),
    # D. the floor under the bytes cap
    data.frame(
      corpus = "big",
      memlim = c("250MB", "150MB", "150MB"),
      rgbytes = c("100MB", "100MB", "32MB"),
      order = "false",
      mode = "single"
    )
  )
  for (i in seq_len(nrow(cases))) {
    env <- c(
      paste0("EXP_CORPUS_KIND=", cases$corpus[i]),
      paste0("EXP_MEMLIM=", cases$memlim[i]),
      paste0("EXP_RGBYTES=", cases$rgbytes[i]),
      paste0("EXP_ORDER=", cases$order[i]),
      paste0("EXP_MODE=", cases$mode[i])
    )
    system2("Rscript", script, env = env)
  }
  quit(save = "no")
}

rss_peak_mb <- function() {
  status <- readLines("/proc/self/status")
  hwm <- grep("^VmHWM:", status, value = TRUE)
  round(as.numeric(gsub("[^0-9]", "", hwm)) / 1024)
}

# ---- corpora ---------------------------------------------------------------
if (nzchar(Sys.getenv("EXP_BUILD"))) {
  suppressMessages(library(duckplyr))
  duckplyr::db_exec("SET threads TO 1")
  n_big <- length(list.files(big_corpus, pattern = "[.]parquet$"))
  if (n_big < 4100L) {
    dir.create(big_corpus, recursive = TRUE, showWarnings = FALSE)
    for (i in seq_len(4100L)) {
      df <- tibble::tibble(
        hc_id = sprintf("hc-%05d", i),
        est = as.double(i),
        samples = list(as.double(seq_len(50000L)) + i)
      )
      duckplyr::compute_parquet(
        duckplyr::as_duckdb_tibble(df),
        file.path(big_corpus, sprintf("part-%05d.parquet", i))
      )
    }
  }
  n_small <- length(list.files(small_corpus, pattern = "[.]parquet$"))
  if (n_small < 30L) {
    dir.create(small_corpus, recursive = TRUE, showWarnings = FALSE)
    for (f in seq_len(30L)) {
      ids <- (f - 1L) * 1000L + seq_len(1000L)
      df <- tibble::tibble(
        hc_id = sprintf("hc-%05d", ids),
        est = as.double(ids),
        samples = lapply(ids, function(i) as.double(seq_len(1000L)) + i)
      )
      duckplyr::compute_parquet(
        duckplyr::as_duckdb_tibble(df),
        file.path(small_corpus, sprintf("part-%05d.parquet", f))
      )
    }
  }
  cat("corpora ready\n")
  quit(save = "no")
}

# ---- one case --------------------------------------------------------------
kind <- Sys.getenv("EXP_CORPUS_KIND", "big")
memlim <- Sys.getenv("EXP_MEMLIM", "1GB")
rgbytes <- Sys.getenv("EXP_RGBYTES", "100MB")
order <- Sys.getenv("EXP_ORDER", "")
mode <- Sys.getenv("EXP_MODE", "single")

suppressMessages(library(duckplyr))
duckplyr::db_exec("SET threads TO 1")
duckplyr::db_exec(paste0("SET memory_limit = '", memlim, "'"))
if (nzchar(order)) {
  duckplyr::db_exec("SET preserve_insertion_order = false")
}

corpus <- if (kind == "big") big_corpus else small_corpus
files <- list.files(corpus, pattern = "[.]parquet$", full.names = TRUE)

write_once <- function(out) {
  tbl <- duckplyr::read_parquet_duckdb(
    files,
    options = list(hive_partitioning = FALSE)
  )
  duckplyr::compute_parquet(
    tbl,
    out,
    options = list(row_group_size_bytes = rgbytes)
  )
}

summarise_groups <- function(out) {
  meta <- dplyr::collect(duckplyr::read_sql_duckdb(paste0(
    "SELECT row_group_id, max(row_group_num_rows) AS rows ",
    "FROM parquet_metadata('",
    out,
    "') GROUP BY row_group_id ORDER BY row_group_id"
  )))
  rows <- meta$rows
  if (length(rows) > 6L) {
    paste0(length(rows), "groups[", paste(rows[1:3], collapse = "+"), "...]")
  } else {
    paste(rows, collapse = "+")
  }
}

# Is the output's hc_id sequence the input (file-name) order?
order_preserved <- function(out) {
  ids <- dplyr::collect(duckplyr::read_sql_duckdb(paste0(
    "SELECT hc_id FROM read_parquet('",
    out,
    "')"
  )))$hc_id
  identical(ids, sort(ids))
}

out1 <- tempfile(fileext = ".parquet")
t0 <- proc.time()[["elapsed"]]
res <- tryCatch(
  {
    write_once(out1)
    TRUE
  },
  error = function(e) conditionMessage(e)
)
write_s <- round(proc.time()[["elapsed"]] - t0, 2)

ok <- isTRUE(res)
extra <- ""
if (ok && mode == "determinism") {
  out2 <- tempfile(fileext = ".parquet")
  write_once(out2)
  h1 <- unname(tools::md5sum(out1))
  h2 <- unname(tools::md5sum(out2))
  extra <- sprintf(
    " bytes_identical=%s order_preserved=%s",
    identical(h1, h2),
    order_preserved(out1)
  )
} else if (ok) {
  extra <- sprintf(" order_preserved=%s", order_preserved(out1))
}

cat(sprintf(
  paste0(
    "RESULT ok=%s corpus=%s memlim=%s rgbytes=%s order=%s mode=%s ",
    "write_s=%.2f file_mb=%s rss_peak_mb=%d row_groups=%s%s%s\n"
  ),
  ok,
  kind,
  memlim,
  rgbytes,
  if (nzchar(order)) "false" else "default",
  mode,
  write_s,
  if (ok) round(file.size(out1) / 2^20, 1) else NA,
  rss_peak_mb(),
  if (ok) summarise_groups(out1) else "NA",
  extra,
  if (ok) {
    ""
  } else {
    paste0(" error=", encodeString(substr(res, 1, 150), quote = "\""))
  }
))
