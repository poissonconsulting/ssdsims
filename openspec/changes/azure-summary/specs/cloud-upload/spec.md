# cloud-upload Delta Specification (azure-summary)

## ADDED Requirements

### Requirement: The combined summary is uploaded alongside the shards
When `upload` is non-`NULL`, the targets factory SHALL pair the `summary` fan-in target with an `upload_summary` target (`format = "file"`, `error = "null"`, honouring the factory's `cue`) that ships the summary Parquet file(s) — `summary.parquet` and, when the scenario retains the bootstrap draws, `summary-samples.parquet` — to the destination via `ssd_upload_shard()`, taking the `summary` target's value (the local path or paths) as its input and returning it unchanged. Because the upload target depends on the summary target's content hash, an unchanged summary SHALL NOT be re-uploaded on a re-driven `tar_make()`. The uploaded blobs SHALL land at `<container>[/<prefix>]/summary.parquet` and `<container>[/<prefix>]/summary-samples.parquet` (the path below the layout-keyed results root), so the read-back globs address them without a new layout convention. With `upload = NULL` the pipeline SHALL contain no `upload_summary` target; with `upload = ssd_upload_dryrun()` the target SHALL be present as a no-op that reaches no network; the per-task and summary result bytes SHALL be identical across the three `upload` modes.

#### Scenario: A non-NULL upload pairs the summary with an upload target
- **WHEN** `ssd_scenario_targets(scenario, upload = ssd_upload_dryrun())` (or an Azure destination) is called
- **THEN** the returned target list SHALL contain exactly one `upload_summary` target, `format = "file"` and `error = "null"`, whose command ships the `summary` target's path(s) via `ssd_upload_shard()`

#### Scenario: NULL adds no summary upload node
- **WHEN** the factory is called with `upload = NULL`
- **THEN** the returned target list SHALL contain no `upload_summary` target

#### Scenario: Both summary files ship when the scenario keeps samples
- **WHEN** a scenario with `samples = TRUE` is run with a non-`NULL` `upload` and the pipeline completes
- **THEN** the `upload_summary` target SHALL ship both `summary.parquet` and `summary-samples.parquet` (the dry-run destination recording one skip per file) and return both local paths

#### Scenario: An unchanged summary is not re-uploaded
- **WHEN** a pipeline with a non-`NULL` `upload` is `tar_make()`'d, then re-driven with no shard content changed
- **THEN** the `upload_summary` target SHALL NOT re-run, so the summary is not re-shipped

### Requirement: The vignette demonstrates the summary upload and read-back
The cloud-upload vignette SHALL state that the summary Parquet file(s) ship alongside the shards (the `upload_summary` target appearing in its live dry-run DAG) and SHALL show — as described, non-evaluated chunks — reading the uploaded summary back in place with `ssd_open_uploaded(upload, "summary")` / `ssd_summarise_uploaded(upload, "summary")`, noting that `step = "summary_samples"` addresses the full summary and exists only when the scenario set `samples = TRUE`.

#### Scenario: Vignette covers the summary path
- **WHEN** a reader follows the cloud-upload vignette
- **THEN** it SHALL show the `upload_summary` target in the dry-run pipeline and the `step = "summary"` / `step = "summary_samples"` read-back one-liners for an Azure destination

## MODIFIED Requirements

### Requirement: A per-shard ship generic dispatches on the destination, failing loud on absent credentials
The package SHALL provide `ssd_upload_shard(path, upload)`, a generic dispatching on the upload object's class, that ships the local Parquet file(s) at `path` — a character vector of one or more local paths (one element per file; the per-shard targets pass one, the `upload_summary` target may pass two) — to the destination. For an Azure destination it SHALL upload each file at `path` to `<url>/<container>[/<prefix>]/<key>` where the key is the file's path below the layout-keyed results root (a shard's `<step>/<partition-path>/part.parquet`, the summary's `summary.parquet`/`summary-samples.parquet`), resolving the credentials **once per call**, and SHALL return the local `path` vector unchanged (so the target stays `format = "file"`); when the required credentials are **absent** it SHALL abort with a loud error (not a silent no-op), so that intent to skip the network is only ever expressed by passing `ssd_upload_dryrun()`. For a dry-run destination it SHALL perform no network I/O, record a skip per file, and return the local `path` unchanged. A failed Azure upload SHALL surface as an error on that call (which, under the pipeline's per-target `error = "null"`, leaves the local file(s) on disk and the rest uploading, so a re-driven run retries only the failed uploads).

#### Scenario: Azure ships the shard and returns the local path
- **WHEN** `ssd_upload_shard(path, ssd_upload_azure(...))` is called with a single path and valid credentials
- **THEN** it SHALL put the shard's Parquet at the destination's Hive path and return the local `path`

#### Scenario: A vector of paths ships every file and returns the vector
- **WHEN** `ssd_upload_shard(c(path, path_with_samples), upload)` is called
- **THEN** it SHALL ship (or, for a dry run, record a skip for) each file in order and return the input vector unchanged

#### Scenario: Azure with absent credentials fails loud
- **WHEN** `ssd_upload_shard(path, ssd_upload_azure(...))` is called with the required credentials absent
- **THEN** it SHALL abort with an error, NOT return the path as a silent no-op

#### Scenario: Dry-run records a skip and reaches no network
- **WHEN** `ssd_upload_shard(path, ssd_upload_dryrun())` is called
- **THEN** it SHALL perform no network I/O, record that each file was skipped, and return the local `path`

### Requirement: Uploaded results are readable in place via a destination-dispatched generic
The package SHALL provide `ssd_open_uploaded(upload, step)`, a generic dispatching on the upload object's class, that opens the **uploaded** results for querying — so that right after an upload a user can read the data back and confirm it landed. For an Azure destination it SHALL return a lazy `duckplyr`/DuckDB table over the Hive glob `<container>[/<prefix>]/<step>/**/part.parquet` at the destination (honouring the destination's optional `prefix` subdirectory), read **in place** via DuckDB's `azure` extension (predicate pushdown straight against blob storage, **no download** of the Parquet). It SHALL resolve the **same** front-end secret as the write path (one of the `SSDSIMS_AZURE_*` secret environment variables) and **remap** it — together with the account name derived from `url` — into a DuckDB `azure` secret (`CREATE SECRET`) for the backend read — one credential source, translated, never a second source — and SHALL abort with a loud error (naming the missing requirement) when the `azure` extension or a required credential is absent. `step` SHALL select one of the step layers (`sample`/`fit`/`hc`) or one of the uploaded combined summaries: `"summary"` SHALL read the single blob `<container>[/<prefix>]/summary.parquet` (the compact summary) and `"summary_samples"` the single blob `<container>[/<prefix>]/summary-samples.parquet` (the full summary retaining the `dists`/`samples` list-columns, present only when the scenario set `samples = TRUE`). For a dry-run destination, `ssd_open_uploaded()` SHALL abort with an informative error stating that a dry run uploads nothing and that the local shards should be read directly — it SHALL NOT silently return an empty or local table. The result of `ssd_open_uploaded()` SHALL be composable with `dplyr` verbs (e.g. `dplyr::count()`) so a one-line round-trip check verifies the upload.

#### Scenario: Azure results read in place without downloading
- **WHEN** `ssd_open_uploaded(ssd_upload_azure(...), step = "hc")` is called with valid credentials
- **THEN** it SHALL return a lazy table over `<container>/hc/**/part.parquet` read in place via the DuckDB `azure` extension, without downloading the Parquet, composable with `dplyr` verbs

#### Scenario: The uploaded summaries are addressable steps
- **WHEN** `ssd_open_uploaded(upload, step = "summary")` or `ssd_open_uploaded(upload, step = "summary_samples")` is called on an Azure destination
- **THEN** it SHALL read the single blob `<container>[/<prefix>]/summary.parquet` or `<container>[/<prefix>]/summary-samples.parquet` respectively, in place, as a lazy table

#### Scenario: A round-trip check verifies a just-uploaded shard
- **WHEN** a shard is uploaded and then `ssd_open_uploaded(upload, step)` is collected (or counted)
- **THEN** the rows read back from the destination SHALL match the rows in the local shard, so the upload is verifiable in one line

#### Scenario: Missing extension or credentials fails loud
- **WHEN** `ssd_open_uploaded(ssd_upload_azure(...), ...)` is called with the DuckDB `azure` extension or a required credential absent
- **THEN** it SHALL abort with an error naming the missing requirement, not return an empty or partial table

#### Scenario: Dry-run has nothing to read back
- **WHEN** `ssd_open_uploaded(ssd_upload_dryrun(), ...)` is called
- **THEN** it SHALL abort stating that a dry run uploads nothing and that the local shards should be read directly

### Requirement: Uploaded results are summarisable in place via a destination-dispatched generic
The package SHALL provide `ssd_summarise_uploaded(upload, step = "hc", drop_samples = TRUE)`, a generic dispatching on the upload object's class — the cloud counterpart of `ssd_summarise()` — that fans a step's **uploaded** shards into a single **lazy** `duckplyr`/DuckDB table read **in place** (no download). For an Azure destination it SHALL read the `<container>[/<prefix>]/<step>/**/part.parquet` Hive glob via DuckDB's `azure` extension — or, for `step = "summary"` / `step = "summary_samples"`, the single uploaded summary blob (`summary.parquet` / `summary-samples.parquet`) — resolving the **same** front-end secret as the write path and remapping it (with the account derived from `url`) into a DuckDB `azure` secret (as `ssd_open_uploaded()` does), and SHALL return the union as a **lazy duckplyr table** (not collected, so the read and projection stay in DuckDB and the result composes with `dplyr`). The `drop_samples` flag SHALL default to `TRUE`, projecting away the heavy `dists`/`samples` list-columns (mirroring `ssd_summarise()`); when `FALSE` it SHALL retain them (so the in-flight bootstrap `samples` are available). Because the compact summary blob physically lacks those columns, `step = "summary"` with `drop_samples = FALSE` SHALL abort with an informative error pointing at `step = "summary_samples"` rather than silently returning a table without them. The default method (an unknown destination) and the dry-run method SHALL abort with an informative error rather than returning an empty or local table.

#### Scenario: Azure summary unioned in place, samples projected away by default
- **WHEN** `ssd_summarise_uploaded(ssd_upload_azure(...), step = "hc")` is called with valid credentials
- **THEN** it SHALL return a lazy `duckplyr` table unioning `<container>[/<prefix>]/hc/**/part.parquet` read in place via the DuckDB `azure` extension, with the `dists`/`samples` list-columns projected away

#### Scenario: drop_samples = FALSE keeps the in-flight samples
- **WHEN** `ssd_summarise_uploaded(upload, step, drop_samples = FALSE)` is called for a step whose shards carry the list-columns
- **THEN** the returned tibble SHALL retain the `samples` (and `dists`) list-columns

#### Scenario: The uploaded full summary is summarisable in place
- **WHEN** `ssd_summarise_uploaded(upload, step = "summary_samples", drop_samples = FALSE)` is called on an Azure destination
- **THEN** it SHALL return a lazy table over the single `<container>[/<prefix>]/summary-samples.parquet` blob with the `dists`/`samples` list-columns retained

#### Scenario: The compact summary cannot pretend to carry samples
- **WHEN** `ssd_summarise_uploaded(upload, step = "summary", drop_samples = FALSE)` is called
- **THEN** it SHALL abort with an informative error pointing at `step = "summary_samples"`, not silently return a table lacking the list-columns

#### Scenario: Dry-run has nothing to summarise
- **WHEN** `ssd_summarise_uploaded(ssd_upload_dryrun(), ...)` is called
- **THEN** it SHALL abort stating that a dry run uploads nothing and that the local shards should be summarised directly with `ssd_summarise()`
