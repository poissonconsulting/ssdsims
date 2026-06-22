## 1. Prerequisites and inventory

- [x] 1.1 Confirm the replacement public surface is shipped (`ssd_define_scenario()` / `ssd_scenario_data()` / `ssd_gen()` / `ssd_run_scenario_baseline()` / `ssd_run_scenario_shards()`); spot-check that `ssd_gen()` covers each generator input the monolith accepted (`fitdists`, `tmbfit`, function, function-name string) so retirement leaves no capability gap.
- [x] 1.2 Repo-wide search recording every reference to the removal targets (`ssd_run_scenario`, `ssd_sim_data`, `ssd_fit_dists_sims`, `ssd_hc_sims`, `run_scenario`, `slice_sample_state`, `do_call_seed`, `fit_dists_state`, `fit_dists_seed`, `hc_state`, `hc_seed`, `*_lecuyer_cmrg_*`, `get_lecuyer_cmrg_stream_state(s)`, `parallel::nextRNGStream`/`nextRNGSubStream`) across `R/`, `tests/`, `vignettes/`, `scripts/`, `man/`, `GLOSSARY.md`, `TARGETS-DESIGN.md` — this is the gate for §6.

## 2. Drop the monolith runners (callers → callees)

- [x] 2.1 Delete `R/run-scenario.R` (the `ssd_run_scenario()` generic + five S3 methods).
- [x] 2.2 Delete `R/simulate-data.R` (the `ssd_sim_data()` generic + five S3 methods).
- [x] 2.3 Delete `R/fit-dists-sims.R` (`ssd_fit_dists_sims()`).
- [x] 2.4 Delete `R/hc-sims.R` (`ssd_hc_sims()`).
- [x] 2.5 Remove the private `run_scenario()` chainer from `R/internal.R`.

## 3. Remove the L'Ecuyer-CMRG RNG backend

- [x] 3.1 Remove the six `*_state`/`*_seed` shims from `R/internal.R`: `slice_sample_state()`, `do_call_seed()`, `fit_dists_state()`, `fit_dists_seed()`, `hc_state()`, `hc_seed()`.
- [x] 3.2 If `R/internal.R` retains no live symbol after 2.5 + 3.1, delete the file (and any Collate entry); otherwise leave only the surviving symbols.
- [x] 3.3 Delete `R/lecuyer-cmrg-seed.R` in full (exported `local_lecuyer_cmrg_seed()` / `with_lecuyer_cmrg_seed()` / `local_lecuyer_cmrg_state()` / `with_lecuyer_cmrg_state()` and the internal `get_lecuyer_cmrg_state()` / `get_lecuyer_cmrg_stream_state(s)()` / state helpers).

## 4. NAMESPACE, man, and tests

- [x] 4.1 Run `devtools::document()` to regenerate `NAMESPACE` (drops the `ssd_run_scenario`/`ssd_sim_data` `S3method()` lines and the eight removed `export()`s) and to delete the orphaned `man/*.Rd` pages.
- [x] 4.2 Delete the monolith-only test suites: `tests/testthat/test-run-scenario.R`, `test-simulate-data.R`, `test-fit-dists-sims.R`, `test-hc-sims.R`, `test-lecuyer-cmrg-seed.R`.
- [x] 4.3 Scan the remaining test suites for incidental use of a removed helper (e.g. `with_lecuyer_cmrg_seed()` in setup) and rewrite onto `local_dqrng_backend()` / `local_dqrng_state()`, or drop if monolith-only.

## 5. Scripts and documentation

- [x] 5.1 Remove the monolith-only reference/exploration scripts: `scripts/example.R`, `example2.R`, `example-expanded.R`, `example-expanded-grids.R`, `example-expanded-grids-independent.R`, `experiment-substream-restart.R`, `reprex-trace.R`, `reprex-trace_reprex.md`.
- [x] 5.2 Strip residual L'Ecuyer prose from `GLOSSARY.md` (the `_state`/`state =` misnomer notes), the vignettes, and `TARGETS-DESIGN.md` cross-references; keep the dqrng / primer terminology.
- [x] 5.3 Re-point any vignette / docs prose that references the removed functions at the canonical surface (`ssd_define_scenario()` + `ssd_run_scenario_baseline()` / `ssd_run_scenario_shards()`); leave the comprehensive README/vignette overhaul to the `readme` change.
- [x] 5.4 Add a `NEWS.md` entry flagging the BREAKING removal of `ssd_run_scenario()`, `ssd_sim_data()`, `ssd_fit_dists_sims()`, `ssd_hc_sims()`, and the four `*_lecuyer_cmrg_*` helpers, pointing at `ssd_run_scenario_baseline()` / `ssd_run_scenario_shards()`. `NEWS.md` carries a "contributors should not edit this file" banner and is `fledge`-built from the squash-merge PR title, so the BREAKING entry ships via a BREAKING-flagged Conventional Commit PR title rather than a hand edit of the generated file.

## 6. Verify and finalize

- [x] 6.1 Re-run the §1.2 symbol search: zero references to any removed symbol remain in `R/` or in kept tests/scripts/docs.
- [x] 6.2 `air format` the changed files.
- [x] 6.3 `devtools::test()` passes (937+ PASS; the only failures are 3 `test-task-shards.R` targets-pipeline tests that pass in isolation and fail only under full-suite parallel `crew`/`tar_make` execution with a worker-side `tar_map`-not-found error — environmental, and untouched by this change). `R CMD check` is clean on every structural check this change affects: *dependencies in R code* (no unused-`parallel`/`methods` import NOTE), *Rd cross-references*, *missing documentation entries*, *code/documentation mismatches*, *S3 generic/method consistency*, and *R code for possible problems* (no missing symbols) all OK. The remaining check WARNINGs/NOTE are the absent `quarto` VignetteBuilder in the sandbox, not this change.
- [x] 6.4 `openspec validate cleanup-lecuyer --strict` passes.
