## 1. `ssd_summarise()` second output

- [x] 1.1 Add a trailing `path_with_samples = NULL` argument to `ssd_summarise()`
  (`R/targets-runner.R`); keep the compact `path` write (the
  `select(-any_of(c("dists", "samples")))` projection collected into R) unchanged.
- [x] 1.2 When `path_with_samples` is non-`NULL`, also write the full hc union to
  `path_with_samples` via a lazy DuckDB read→write
  (`compute_parquet(read_parquet_duckdb(glob, hive_partitioning = FALSE), path_with_samples)`),
  retaining `dists`/`samples` and never collecting the draws into R.
- [x] 1.3 Return `path` when `path_with_samples` is `NULL`, else the vector
  `c(path, path_with_samples)` (the `format = "file"` contract).
- [x] 1.4 Update the roxygen for `ssd_summarise()` (`@param path_with_samples`, `@return`,
  and a `@details`/`@examples` note describing the two outputs and the
  no-R-materialise guarantee for both).

## 2. Pipeline wiring

- [x] 2.1 In `ssd_scenario_targets()`, compute
  `summary_full_path <- file.path(root, "summary-samples.parquet")` and pass
  `path_with_samples` to the `summary` target's `ssd_summarise()` call **iff**
  `scenario$hc$samples` is `TRUE`.
- [x] 2.2 Ensure the `summary` target returns the path vector when both files are
  written (verify `format = "file"` tracks both), and is byte-for-byte unchanged
  when `samples = FALSE`.

## 3. Docs and templates

- [x] 3.1 Update `vignettes/sharded-pipeline.qmd` to describe the compact vs. full
  summary and the `samples = TRUE` trigger.
- [x] 3.2 Update `inst/targets-templates/` run scripts/READMEs that read or report
  the summary (note the second file appears under `samples = TRUE`).
- [x] 3.3 Reflect the two-output contract in `GLOSSARY.md`/`TARGETS-DESIGN.md`
  where the summary is described, if needed.

## 4. Tests

- [x] 4.1 `samples = TRUE`: `ssd_summarise(..., path_with_samples = ...)` writes both
  files; assert the full file's `samples` column is populated (and `dists`
  present), the compact omits both, and `est`/`lcl`/`ucl` are identical across
  the two files.
- [x] 4.2 `path_with_samples = NULL`: only the compact file is written (no second file),
  and the return value is the single path.
- [x] 4.3 Pipeline: a `samples = TRUE` scenario's `summary` target tracks both
  paths; a `samples = FALSE` scenario tracks only `summary.parquet`.

## 5. Validation

- [x] 5.1 Format with `air` and run `openspec validate dual-summary-outputs --strict`.
- [x] 5.2 Run `R CMD check`/`devtools::test()` for the touched runner and confirm
  the roadmap (§12) bullet matches the shipped behaviour before archiving.
