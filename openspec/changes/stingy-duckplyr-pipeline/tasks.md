## 1. Write seam preserves prudence

- [x] 1.1 In `R/targets-runner.R`, change `ssd_write_parquet()` so an existing
  `duckplyr_df` is passed to `duckplyr::compute_parquet()` unchanged
  (preserving its prudence) and a plain data frame is wrapped with
  `duckplyr::as_duckdb_tibble(df, prudence = "stingy")`; update the function's
  comment to explain why the lavish re-wrap is removed.

## 2. Stingy on the internal pipeline reads

- [x] 2.1 `ssd_summarise()` (`R/targets-runner.R`): add `prudence = "stingy"`
  to its `read_parquet_duckdb()` call.
- [x] 2.2 `ssd_summarise_member()` (`R/design-targets.R`): add
  `prudence = "stingy"` to its `read_parquet_duckdb()` call.
- [x] 2.3 `ssd_summarise_design()` (`R/design-targets.R`): add
  `prudence = "stingy"` to its `read_parquet_duckdb()` call.
- [x] 2.4 `ssd_read_parquet()` (`R/targets-runner.R`): add
  `prudence = "stingy"` to its `read_parquet_duckdb()` call, keeping the
  explicit `collect()`, and reshape the nested
  `as_tibble(collect(read_parquet_duckdb(...)))` into a native `|>` chain.

## 3. In-engine duration math (cost-analysis)

- [x] 3.1 `read_step_timings()` (`R/cost-analysis.R`): read with
  `prudence = "stingy"`; compute `seconds` in DuckDB via
  `dplyr::mutate(seconds = dd$epoch(.end) - dd$epoch(.start))` before the
  explicit `collect()`; reshape the nested
  `as_tibble(collect(distinct(select(...))))` calls into `|>` chains.
- [x] 3.2 `design_fast_hc()` (`R/cost-analysis.R`): same — stingy read,
  in-engine `dd$epoch` duration, `|>` reshape of the nested calls.
- [x] 3.3 Ensure `dd` resolves cleanly under `R CMD check` (confirm the
  `@autoglobal`/`globalVariables` machinery covers `dd`, or register it), so
  no "undefined global" NOTE appears.

## 4. Upload read-back prudence (stingy default + lavish opt-out)

- [x] 4.1 `ssd_open_uploaded()` (generic + Azure method, `R/upload.R`): add a
  `prudence = "stingy"` argument threaded into `read_parquet_duckdb()`.
- [x] 4.2 `ssd_summarise_uploaded()` (generic + Azure method, `R/upload.R`):
  add a `prudence = "stingy"` argument threaded into `read_parquet_duckdb()`.
- [x] 4.3 Update the roxygen for both generics: document `prudence`, the
  stingy default, the `"lavish"` opt-out, and that `collect()`/
  `compute_parquet()` are unaffected.

## 5. Optional pipe drive-by

- [x] 5.1 `duckplyr_current_settings()` (`R/duckplyr-config.R`): reshape the
  nested `as_tibble(collect(read_sql_duckdb(...)))` into a `|>` chain (no
  behaviour change — its read is already stingy internally).

## 6. Tests

- [x] 6.1 Add a test asserting the fan-in reads are stingy and that a stingy
  fan-in frame is written without being downgraded (e.g. a frame returned by a
  fan-in carries `prudent_duckplyr_df`; the summary write succeeds).
- [x] 6.2 Add a test for `ssd_write_parquet()`: a plain tibble is wrapped
  stingy and a passed-in `duckplyr_df` keeps its prudence.
- [x] 6.3 Add a test that the cost-analysis `seconds` computed via `dd$epoch`
  equal the previous `difftime(units = "secs")` result (sub-second exact).
- [x] 6.4 Add a test for the upload generics: the default returns a stingy
  table (implicit access errors; `collect()` works) and
  `prudence = "lavish"` restores auto-materialisation.
- [x] 6.5 Run the `NOT_CRAN=true` fan-in suites
  (`test-design-targets`, `test-run-scenario`, `test-cost-analysis`,
  `test-upload`) and confirm no new failures beyond the known baseline.

## 7. Finalise

- [x] 7.1 Format with `air` and run `devtools::document()` for the new
  `prudence` arguments.
- [x] 7.2 `NEWS.md` is built by `fledge` from the squash-merge PR title, so no
  manual edit is needed; the PR title is a valid Conventional Commit
  (`feat: ...`).
