# azure-summary — Design

## Context

`ssd_scenario_targets()` (in `R/targets-runner.R`) builds three per-shard `tar_map`s (sample/fit/hc) plus a single `summary` fan-in target. When `upload` is non-`NULL`, `step_map()` pairs each step shard with an `upload_<step>` target (`format = "file"`, `error = "null"`) shipping the shard's local path via `ssd_upload_shard(path, upload)` — but the `summary` target gets no such pairing, so `summary.parquet` (and the optional `summary-samples.parquet` from `dual-summary-outputs`, written iff `scenario$hc$samples` is `TRUE`) never reaches the destination. Meanwhile the read-back machinery in `R/upload.R` already half-expects it: `azure_glob(upload, "summary")` resolves to `az://<container>[/<prefix>]/summary.parquet` and `ssd_open_uploaded()`/`ssd_summarise_uploaded()` accept `step = "summary"` — a blob nothing writes — and there is no glob at all for the with-samples file.

Two facts the implementation leans on:

- The `summary` target's value already satisfies the `format = "file"` contract as **a character vector**: `ssd_summarise()` returns `path` or `c(path, path_with_samples)`.
- `upload_blob_key()` strips everything up to the last `layout=<hash>/` segment, so `<root>/layout=<hash>/summary.parquet` keys to `summary.parquet` — exactly the tail `azure_glob(upload, "summary")` already reads. The blob layout needs no new convention; the with-samples key `summary-samples.parquet` falls out the same way.

## Goals / Non-Goals

**Goals:**

- Ship the summary Parquet file(s) to the upload destination as a content-hashed target, in the same opt-in, fail-loud style as the per-shard uploads.
- Make every uploaded summary file conveniently readable in place: `step = "summary"` (compact) and `step = "summary_samples"` (full, with `dists`/`samples`).
- Keep the dry-run/`NULL` modes and the byte-identical-results guarantee intact.

**Non-Goals:**

- No download helper — read-back stays in place via DuckDB's `azure` extension, like the shard path.
- No new backend, no upload-hash/corruption detection (still deferred with `manifest-revival`), no change to the credential contract.
- No change to `ssd_summarise()` itself or the local summary layout.

## Decisions

### D1 — A single `upload_summary` target outside the `tar_map`s

When `upload` is non-`NULL`, append one `tar_target_raw("upload_summary", expr(ssd_upload_shard(summary, !!upload)), format = "file", error = "null", cue = cue)` to the factory's returned list. Depending on the `summary` **symbol** (not the paths) gives `targets` the dependency edge and the content-hash skip for free: the summary target is itself `format = "file"`, so `upload_summary` re-runs only when the summary bytes change, mirroring the per-shard behaviour.

*Alternative considered*: two separate targets (`upload_summary`, `upload_summary_samples`) splitting the vector. Rejected — the with-samples path exists only conditionally, the split buys no extra failure isolation worth the DAG complexity (both files come from the same fan-in and rewrite together), and `error = "null"` already isolates the pair from the compute DAG.

### D2 — Vectorise `ssd_upload_shard()` over `path`

Relax the generic's contract from one string to a **character vector of one or more local paths**: `chk::chk_character(path)` + non-empty, ship each element in order, return `path` unchanged. The Azure method loops over the elements (a plain `for` loop, per the error-call-origin convention) calling `AzureStor::upload_blob()` per file with `upload_blob_key()` as today; the dry-run method records one `ssdsims_upload_skip` message per element. Credentials are resolved once per call, not per element.

*Alternative considered*: keep the generic scalar and iterate inside the target command (`for (p in summary) ssd_upload_shard(p, upload)`). Rejected — it bloats the generated `_targets.R` expression, re-resolves credentials per file, and every future multi-file caller would re-implement the loop. The spec wording changes from "one shard" to "the file(s) at `path`"; all existing scalar call sites are unaffected.

### D3 — `summary_samples` as an explicit `step` value

Extend the `step` domain of `ssd_open_uploaded()` and `ssd_summarise_uploaded()` (Azure methods' `rlang::arg_match0()`) and of `azure_glob()` to `c("sample", "fit", "hc", "summary", "summary_samples")`, with `summary_samples` mapping to the single-blob tail `summary-samples.parquet`. An explicit step name keeps the step→glob mapping one-to-one and works for `ssd_open_uploaded()`, which has no `drop_samples` argument to overload.

*Alternative considered*: overloading `ssd_summarise_uploaded(upload, "summary", drop_samples = FALSE)` to silently redirect to the with-samples blob. Rejected — it muddles "which blob am I reading" with "which columns am I projecting", and leaves `ssd_open_uploaded()` without any route to the file.

### D4 — `step = "summary", drop_samples = FALSE` aborts

In `ssd_summarise_uploaded.ssdsims_upload_azure_blob()`, that combination aborts with a message pointing at `step = "summary_samples"`. The compact blob physically lacks the `dists`/`samples` columns, so the existing `any_of()` projection would make `drop_samples = FALSE` a **silent no-op** — the exact failure mode the upload path is designed to forbid. The converse (`step = "summary_samples", drop_samples = TRUE`, the default) stays a legitimate projection, not an error: `drop_samples` keeps its uniform meaning (project the heavy columns away) everywhere it can be honoured.

### D5 — Reading a never-written `summary-samples.parquet` stays a backend error

When the scenario ran with `samples = FALSE` no with-samples file exists, and `step = "summary_samples"` fails inside DuckDB with a not-found error on the named blob. That is acceptable fail-loud behaviour for a single-blob read (the message names the missing blob); the front end cannot know remotely whether the file should exist without a network round-trip, which `azure_glob()` deliberately never does.

## Risks / Trade-offs

- [The summary uploads even when some hc shards failed (`error = "null"` survivors-union)] → Same semantics as the local summary: it is the union of what landed, and a re-driven `tar_make()` rewrites and re-uploads it once the shards heal. Documented in the vignette callout.
- [Vectorised `ssd_upload_shard()` is a (compatible) contract widening; a partial multi-file failure aborts mid-vector] → Under `error = "null"` the whole `upload_summary` branch nulls and a re-drive re-ships both files; uploads are idempotent puts, so the half-shipped state is harmless.
- [`summary-samples.parquet` read-back error surfaces as a DuckDB message, not a curated one] → Acceptable per D5; the vignette states the file exists iff the scenario set `samples = TRUE`.
- [Existing buckets uploaded before this change have shard layers but no summary blob] → `step = "summary"` then fails loud on the missing blob; re-driving the pipeline with the new package version ships it (the summary target's file hash is unchanged but `upload_summary` is a *new* target, so it runs).

## Migration Plan

Pure addition: no exported signature breaks (`path` widens, `step` domain widens, one new target name). No data migration; existing destinations gain the summary blob on the next `tar_make()`. Rollback is reverting the package version — local results and uploaded shards are untouched.

## Open Questions

None — the blob layout, credential contract, and naming all follow the existing cloud-upload conventions.
