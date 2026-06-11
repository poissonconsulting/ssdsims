## 1. Option

- [x] 1.1 Add scalar `samples = FALSE` to `ssd_define_scenario()`; validate with `chk::chk_flag()`; store at `scenario$hc$samples`; document `@param samples`; render in `print.ssdsims_scenario()`
- [x] 1.2 Thread `samples` through `hc_data_task()` / `hc_data_task_primer()` to `ssd_hc(..., samples = samples)` in both ci branches; pass `samples = scenario$hc$samples` from the baseline runner and `ssd_run_hc_step()`
- [x] 1.3 Keep `samples` off `task_axes("hc")` and the primer (output-only; estimates and RNG unchanged)

## 2. Summary

- [x] 2.1 `ssd_summarize()` projects out the `dists`/`samples` list-columns (DuckDB projection) so the analysis-ready summary stays tidy; the draws remain in the per-shard hc Parquet

## 3. Tests and checks

- [x] 3.1 `samples` defaults FALSE and is stored; rejects a non-flag (snapshot error)
- [x] 3.2 Baseline runner: `samples = TRUE` retains draws while estimates stay byte-identical to `samples = FALSE`
- [x] 3.3 Run `devtools::document()`, `air format .`; update `man/` and the `print` snapshot
