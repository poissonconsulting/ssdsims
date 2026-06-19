# azure-summary

## Why

The targets pipeline ships every step shard to Azure, and the read-back path already advertises `ssd_open_uploaded(upload, "summary")` over `az://<container>[/<prefix>]/summary.parquet` — but **nothing ever uploads the summary**: `ssd_scenario_targets()` mints `upload_<step>` targets only inside the per-shard `tar_map`s, so the `summary` fan-in's outputs (`summary.parquet` and, since `dual-summary-outputs`, the optional `summary-samples.parquet`) stay local. The documented `step = "summary"` read-back is a dangling pointer to a blob that is never written, and the with-samples summary has no read-back glob at all. The `azure-summary` roadmap item closes this: conveniently access the summary Parquet files from Azure.

## What Changes

- **`upload_summary` target.** When `upload` is non-`NULL`, `ssd_scenario_targets()` pairs the `summary` target with an `upload_summary` target (`format = "file"`, `error = "null"`, honouring `cue`) that ships the summary Parquet file(s) via `ssd_upload_shard()` and returns the local path(s). Like the per-shard upload targets it is content-hashed: an unchanged summary is not re-uploaded. With `upload = NULL` the DAG is unchanged.
- **`ssd_upload_shard()` accepts a vector of paths.** The `summary` target's value is `path` or `c(path, path_with_samples)`, so the ship generic relaxes from one string to a character vector of one or more local paths — each file is shipped (Azure) or skip-recorded (dry-run) and the vector is returned unchanged, keeping the `format = "file"` contract. Scalar behaviour is unchanged.
- **`summary_samples` read-back step.** `ssd_open_uploaded()` and `ssd_summarise_uploaded()` (and the internal `azure_glob()`) gain `"summary_samples"` in their `step` domain, mapping to the single `<container>[/<prefix>]/summary-samples.parquet` blob — the uploaded full summary that retains the `dists`/`samples` list-columns.
- **Fail loud on the impossible combination.** `ssd_summarise_uploaded(upload, step = "summary", drop_samples = FALSE)` aborts pointing at `step = "summary_samples"`: the compact summary blob physically lacks the `dists`/`samples` columns, so silently returning a sample-less table would be a quiet no-op of exactly the kind the upload path forbids.
- **Docs.** The cloud-upload vignette gains a short "the summary ships too" passage (the `upload_summary` node in the dry-run DAG, plus the described Azure read-back one-liners); help pages updated.

## Capabilities

### New Capabilities

None — this completes the existing cloud-upload capability.

### Modified Capabilities

- `cloud-upload`:
  - The per-shard ship requirement (`ssd_upload_shard()`) widens from "one shard Parquet" to "one or more local Parquet files" (a character vector of paths, returned unchanged).
  - The content-hashed upload-targets requirement gains the `upload_summary` target pairing the `summary` fan-in (both summary files, content-hash skip, `error = "null"`).
  - The in-place read-back and summarise requirements gain the `"summary"`/`"summary_samples"` step mapping to the single summary blobs, and the loud abort for `step = "summary", drop_samples = FALSE`.
  - The vignette requirement gains the summary upload/read-back demonstration.

## Impact

- **Code**: `R/targets-runner.R` (`ssd_scenario_targets()` mints `upload_summary`), `R/upload.R` (`ssd_upload_shard()` methods vectorised; `azure_glob()`/`step` domains gain `summary_samples`; the `summary`+`drop_samples = FALSE` abort).
- **Tests**: `tests/testthat/test-upload.R` (factory DAG with/without upload, vectorised ship generic, glob/step-domain coverage, the new abort, dry-run end-to-end skip of the summary paths).
- **Docs**: `man/` regenerated; `vignettes/cloud-upload.qmd`; `openspec/specs/cloud-upload/spec.md` via this change's delta.
- **No breaking change**: `upload = NULL` DAGs, scalar `ssd_upload_shard()` calls, and all existing step values behave as before. The same `SSDSIMS_AZURE_*` credential contract covers the new paths; no new dependencies.
