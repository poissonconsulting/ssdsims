# azure-summary — Tasks

## 1. Vectorise the ship generic (`R/upload.R`)

- [x] 1.1 Relax `ssd_upload_shard()`'s contract to a character vector of one or more local paths: `ssd_upload_shard.ssdsims_upload_dryrun()` validates with `chk::chk_character(path)` (non-empty), records one `ssdsims_upload_skip` message per element, and returns `path` unchanged
- [x] 1.2 Vectorise `ssd_upload_shard.ssdsims_upload_azure_blob()`: resolve credentials and build the container endpoint **once**, then ship each element with `AzureStor::upload_blob()` over `upload_blob_key()` in a plain `for` loop (error-call-origin convention), returning `path` unchanged
- [x] 1.3 Update the `ssd_upload_shard()` roxygen (`@param path` is now "the local Parquet path(s)"; mention the `upload_summary` caller and the per-call credential resolution)

## 2. Summary read-back steps (`R/upload.R`)

- [x] 2.1 Extend `azure_glob()` so `step = "summary_samples"` maps to the single-blob tail `summary-samples.parquet` (alongside the existing `summary` → `summary.parquet`)
- [x] 2.2 Add `"summary_samples"` to the `rlang::arg_match0()` domains in `ssd_open_uploaded.ssdsims_upload_azure_blob()` and `ssd_summarise_uploaded.ssdsims_upload_azure_blob()`
- [x] 2.3 In `ssd_summarise_uploaded.ssdsims_upload_azure_blob()`, abort (in the user-facing call's context, before any network/extension work) when `step == "summary"` and `drop_samples` is `FALSE`, with a message pointing at `step = "summary_samples"`
- [x] 2.4 Update the `ssd_open_uploaded()`/`ssd_summarise_uploaded()` roxygen: document the two summary steps, that `summary-samples.parquet` exists iff the scenario set `samples = TRUE`, and the new abort

## 3. The `upload_summary` target (`R/targets-runner.R`)

- [x] 3.1 In `ssd_scenario_targets()`, when `upload` is non-`NULL`, build `upload_summary` via `targets::tar_target_raw("upload_summary", rlang::expr(ssd_upload_shard(summary, !!upload)), format = "file", error = "null", cue = cue)` and append it to the returned target list; with `upload = NULL` the list is unchanged
- [x] 3.2 Update the factory's "Uploading shards to cloud storage" roxygen section and the surrounding comments to cover the summary pairing (content-hash skip, both summary files, `error = "null"` isolation)

## 4. Tests (`tests/testthat/test-upload.R`)

- [x] 4.1 Vectorised ship generic: a dry-run `ssd_upload_shard(c(a, b), ...)` records one skip per file and returns the vector unchanged; a scalar call behaves as before; Azure with absent credentials still aborts before any shipping
- [x] 4.2 Glob/step domains: `azure_glob(upload, "summary_samples")` (bare and prefixed) resolves to `az://<container>[/<prefix>]/summary-samples.parquet`; `ssd_open_uploaded()`/`ssd_summarise_uploaded()` accept `"summary_samples"` and still reject an unknown step
- [x] 4.3 The impossible combination: `ssd_summarise_uploaded(upload, "summary", drop_samples = FALSE)` aborts naming `step = "summary_samples"` (snapshot), before resolving credentials
- [x] 4.4 Factory DAG: `upload = NULL` emits no `upload_summary` target; a dry-run (and mocked Azure) destination emits exactly one, `format = "file"` and `error = "null"`
- [x] 4.5 End-to-end dry-run `tar_make()` (following the existing pipeline-run test pattern): `upload_summary` completes recording skips for `summary.parquet` — and for `summary-samples.parquet` when the scenario sets `samples = TRUE` — and a re-driven `tar_make()` skips `upload_summary` (content-hash, nothing re-shipped)
- [x] 4.6 Byte-identical results: extend (or confirm) the existing three-mode comparison so the summary file bytes are identical across `upload = NULL`, dry-run, and mocked Azure runs

## 5. Docs and polish

- [x] 5.1 Update `vignettes/cloud-upload.qmd`: note in the live dry-run section that the summary ships too (the `upload_summary` node), add described (non-evaluated) `ssd_open_uploaded(upload, "summary")` / `ssd_summarise_uploaded(upload, "summary")` / `step = "summary_samples"` read-back one-liners, and extend the callout (summary re-ships only when its bytes change; `summary-samples.parquet` exists iff `samples = TRUE`)
- [x] 5.2 `devtools::document()` to regenerate `man/`, then `air format .`
- [x] 5.3 Run `devtools::test(filter = "^upload")` and the targets-runner tests (`devtools::test(filter = "^task-shards|^run-scenario")` as applicable), then a full `devtools::test()`
