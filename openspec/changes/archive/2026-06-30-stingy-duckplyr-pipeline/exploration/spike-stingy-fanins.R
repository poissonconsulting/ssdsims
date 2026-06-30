# Spike: does any never-collected fan-in chain secretly fall back to dplyr?
#
# Method: apply `prudence = "stingy"` to all three fan-in reads
# (ssd_summarise, ssd_summarise_member, ssd_summarise_design) and make the
# write seam preserve a frame's prudence instead of re-wrapping it lavish,
# then run the NOT_CRAN fan-in suites. A hidden fallback would need to
# materialise columns, which a stingy frame forbids -> a loud error.
#
# Result (duckplyr 1.2.1.9901, duckdb 1.5.4):
#   test-design-targets.R   pass=25 fail=0 skip=0   (member + design unions)
#   test-run-scenario.R     pass=15 fail=0 skip=0   (ssd_summarise fan-in)
#   test-cost-analysis.R    pass=13 fail=0 skip=0
#   full suite: same 6 pre-existing (unrelated) failures, ZERO new ones.
# Conclusion: select(any_of) / filter(%in%) / constant mutate / union_all /
# compute_parquet all execute in DuckDB. No dd$ rescue needed in the fan-ins.
#
# The edits the spike applied (reverted afterwards; this is the record):
#   - read_parquet_duckdb(..., prudence = "stingy") in ssd_summarise(),
#     ssd_summarise_member(), ssd_summarise_design()
#   - ssd_write_parquet(): pass an existing duckplyr_df through to
#     compute_parquet() unchanged; wrap a plain tibble as prudence = "stingy".

# --- standalone mechanic checks (no ssdsims needed) -------------------------
suppressMessages(library(duckplyr))
Sys.setenv(DUCKPLYR_FALLBACK_COLLECT = 0)

# dd$epoch duration math is sub-second-exact (matches difftime(units="secs"))
t0 <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
res <- duckdb_tibble(.start = t0, .end = t0 + 1.25) |>
  dplyr::mutate(seconds = dd$epoch(.end) - dd$epoch(.start)) |>
  dplyr::collect()
stopifnot(isTRUE(all.equal(res$seconds, 1.25)))

# stingy never auto-materialises; collect() is the explicit escape
s <- duckdb_tibble(g = c("a", "a", "b"), x = 1:3, .prudence = "stingy")
stopifnot(inherits(try(nrow(s), silent = TRUE), "try-error")) # errors (good)
stopifnot(nrow(dplyr::collect(dplyr::filter(s, x %in% c(1L, 3L)))) == 2L)

# --- full reproduction (run from the package root) --------------------------
# Apply the edits above, then:
#   R CMD INSTALL --no-docs --no-multiarch .
#   NOT_CRAN=true Rscript -e '
#     Sys.setenv(NOT_CRAN="true"); library(testthat); library(ssdsims)
#     for (f in c("test-design-targets.R","test-run-scenario.R","test-cost-analysis.R")) {
#       d <- as.data.frame(test_file(file.path("tests/testthat", f), reporter="silent"))
#       cat(sprintf("%-26s pass=%d fail=%d skip=%d\n", f, sum(d$passed), sum(d$failed), sum(d$skipped)))
#     }'
