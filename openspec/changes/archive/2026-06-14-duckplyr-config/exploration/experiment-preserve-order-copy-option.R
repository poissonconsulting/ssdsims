# Experiment: can `preserve_insertion_order` ride on `compute_parquet()`'s
# COPY options instead of a global SET?
#
# Review question on the implementation: the scoped
# `SET preserve_insertion_order = false` around the full-summary write would
# be unnecessary if the relaxation could be passed per write, as a COPY
# option in `compute_parquet(options = )` next to `row_group_size_bytes`.
# This probe answers that on duckdb 1.5.2 (threads = 1 throughout):
#
#   1. `preserve_insertion_order` as a COPY option - recognised?
#   2. `PRESERVE_ORDER` (the actual COPY option name) = FALSE, alone
#   3. `PRESERVE_ORDER = FALSE` + `ROW_GROUP_SIZE_BYTES` (the combination the
#      full summary needs)
#   4. global `preserve_insertion_order = false` + `PRESERVE_ORDER = TRUE` +
#      `ROW_GROUP_SIZE_BYTES` (which setting does the Binder check consult?)
#   5. global false + `ROW_GROUP_SIZE_BYTES` only (the implemented path)
#
# Run with:
#   Rscript openspec/changes/duckplyr-config/exploration/experiment-preserve-order-copy-option.R

suppressMessages(library(duckplyr))
duckplyr::db_exec("SET threads TO 1")

df <- tibble::tibble(
  id = seq_len(5000L),
  samples = lapply(seq_len(5000L), function(i) as.double(seq_len(10L)))
)
src <- tempfile(fileext = ".parquet")
duckplyr::compute_parquet(duckplyr::as_duckdb_tibble(df), src)

groups <- function(path) {
  rows <- dplyr::collect(duckplyr::read_sql_duckdb(paste0(
    "SELECT max(row_group_num_rows) AS rows FROM parquet_metadata('",
    path,
    "') GROUP BY row_group_id"
  )))$rows
  paste(rows, collapse = "+")
}

try_copy <- function(label, opts) {
  out <- tempfile(fileext = ".parquet")
  result <- tryCatch(
    {
      duckplyr::compute_parquet(
        duckplyr::read_parquet_duckdb(src),
        out,
        options = opts
      )
      paste("OK groups:", groups(out))
    },
    error = function(e) paste("ERR:", substr(conditionMessage(e), 1, 100))
  )
  cat("RESULT", label, "->", result, "\n")
}

try_copy(
  "copy-option preserve_insertion_order=FALSE",
  list(preserve_insertion_order = FALSE)
)
try_copy("copy-option preserve_order=FALSE alone", list(preserve_order = FALSE))
try_copy(
  "copy-option preserve_order=FALSE + rgbytes=100KB",
  list(preserve_order = FALSE, row_group_size_bytes = "100KB")
)
duckplyr::db_exec("SET preserve_insertion_order = false")
try_copy(
  "global false + copy-option preserve_order=TRUE + rgbytes=100KB",
  list(preserve_order = TRUE, row_group_size_bytes = "100KB")
)
try_copy(
  "global false + rgbytes=100KB only",
  list(row_group_size_bytes = "100KB")
)
