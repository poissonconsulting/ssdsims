## ADDED Requirements

### Requirement: Typed, self-validating upload destination objects
The package SHALL represent an upload destination as a classed S3 object rather than an untyped list. It SHALL provide `ssd_upload_azure(url, container, ..., prefix = NULL, domain = "blob.core.windows.net")` returning an object of class `c("ssdsims_upload_azure_blob", "ssdsims_upload")` and `ssd_upload_dryrun()` returning an object of class `c("ssdsims_upload_dryrun", "ssdsims_upload")`. The constructors SHALL validate their arguments at construction time — `ssd_upload_azure()` SHALL require `url` and `container` to be non-empty strings and abort, in the context of the user-facing call, with an informative error naming the offending argument when they are not. `ssd_upload_azure()` SHALL accept an optional `prefix` — a subdirectory (blob-name prefix) **within** the container under which the shards are written — that defaults to `NULL` (the container root); it SHALL sit after `...` so it (and `domain`) MUST be passed by name (`rlang::check_dots_empty()` aborting on a positional or misspelled argument). When supplied `prefix` SHALL be a non-empty string (validated as such, aborting otherwise), SHALL have leading and trailing slashes trimmed, and SHALL collapse to no prefix when it trims to empty. The storage **account name** SHALL be derived from `url` — the host label before `.<domain>` (so `https://acct.blob.core.windows.net` with the default `domain` yields account `"acct"`) — and construction SHALL abort when `url`'s host does not end with `.<domain>`; `domain` SHALL default to `"blob.core.windows.net"` and be overridable for a sovereign/non-public cloud. The account SHALL therefore **not** be a required environment variable. An upload object SHALL be a plain, serialisable value carrying only the destination (e.g. `url`, `container`, `prefix`, `domain`, and the derived `account`) and SHALL NOT carry credentials, secrets, open connections, or environments, so it travels unchanged to `crew` workers and through `targets`.

#### Scenario: Azure object constructed and classed
- **WHEN** `ssd_upload_azure(url = "https://acct.blob.core.windows.net", container = "ssdsims-results")` is called
- **THEN** it SHALL return an object inheriting `"ssdsims_upload_azure_blob"` and `"ssdsims_upload"` that stores the `url` and `container` (and a `NULL` `prefix`) and carries no credentials

#### Scenario: An optional subdirectory prefix is stored and normalised
- **WHEN** `ssd_upload_azure(url, container, prefix = "/study-2026/run-3/")` is called
- **THEN** it SHALL store the prefix with surrounding slashes trimmed (`"study-2026/run-3"`), so the shards write under `<container>/<prefix>/<step>/...` and `ssd_open_uploaded()` reads them back from the same prefixed location; a `prefix` that trims to empty SHALL be treated as no prefix

#### Scenario: Dry-run object constructed and classed
- **WHEN** `ssd_upload_dryrun()` is called
- **THEN** it SHALL return an object inheriting `"ssdsims_upload_dryrun"` and `"ssdsims_upload"` that reaches no network

#### Scenario: Construction validates the destination
- **WHEN** `ssd_upload_azure()` is called with a missing or non-string `url` or `container`, or a non-`NULL` non-string `prefix`
- **THEN** it SHALL abort at construction time with an informative error naming the offending argument, rather than deferring the failure to upload time

#### Scenario: prefix must be passed by name
- **WHEN** `ssd_upload_azure()` is called with a positional third argument or a misspelled named argument
- **THEN** `rlang::check_dots_empty()` SHALL abort with an informative error, so `prefix` is only ever supplied by name

### Requirement: A front-door probe verifies credentials and connectivity, failing loud
The package SHALL provide `ssd_test_upload(upload)`, a generic dispatching on the upload object's class, that confirms before any compute whether the destination is reachable and the credentials are in the right place. For an Azure destination it SHALL resolve the **secret** from the environment (one of `SSDSIMS_AZURE_STORAGE_KEY`, `SSDSIMS_AZURE_STORAGE_SAS`, or a service-principal combination — the storage account name comes from `url`, not the environment) and, when no secret is present, abort with a **loud error naming the missing variable**; when credentials are present it SHALL perform a minimal round-trip (list the container, then write and delete a small marker blob), returning silently on success and aborting with the backend's diagnostic on failure. For a dry-run destination it SHALL succeed trivially without reaching any network. The probe SHALL be runnable interactively as a one-liner so a user can check their wiring at the prompt before `tar_make()`.

#### Scenario: Azure probe names a missing credential
- **WHEN** `ssd_test_upload(ssd_upload_azure(...))` is called with a required `SSDSIMS_AZURE_*` environment variable unset
- **THEN** it SHALL abort with an error that names the missing variable, rather than succeeding silently or failing later on a worker

#### Scenario: Azure probe round-trips when wired
- **WHEN** `ssd_test_upload(ssd_upload_azure(...))` is called with valid credentials present
- **THEN** it SHALL list the container and write then delete a marker blob, and return silently on success

#### Scenario: Dry-run probe is trivially OK
- **WHEN** `ssd_test_upload(ssd_upload_dryrun())` is called
- **THEN** it SHALL return successfully without resolving credentials or reaching any network

### Requirement: A per-shard ship generic dispatches on the destination, failing loud on absent credentials
The package SHALL provide `ssd_upload_shard(path, upload)`, a generic dispatching on the upload object's class, that ships one shard Parquet to the destination. For an Azure destination it SHALL upload the file at `path` to `<url>/<container>[/<prefix>]/<step>/<partition-path>/part.parquet` (honouring the destination's optional `prefix` subdirectory) and return the local `path` (so the target stays `format = "file"`); when the required credentials are **absent** it SHALL abort with a loud error (not a silent no-op), so that intent to skip the network is only ever expressed by passing `ssd_upload_dryrun()`. For a dry-run destination it SHALL perform no network I/O, record a skip, and return the local `path`. A failed Azure upload SHALL surface as an error on that call (which, under the pipeline's per-shard `error = "null"`, leaves the local shard on disk and the rest uploading, so a re-driven run retries only the failed uploads).

#### Scenario: Azure ships the shard and returns the local path
- **WHEN** `ssd_upload_shard(path, ssd_upload_azure(...))` is called with valid credentials
- **THEN** it SHALL put the shard's Parquet at the destination's Hive path and return the local `path`

#### Scenario: Azure with absent credentials fails loud
- **WHEN** `ssd_upload_shard(path, ssd_upload_azure(...))` is called with the required credentials absent
- **THEN** it SHALL abort with an error, NOT return the path as a silent no-op

#### Scenario: Dry-run records a skip and reaches no network
- **WHEN** `ssd_upload_shard(path, ssd_upload_dryrun())` is called
- **THEN** it SHALL perform no network I/O, record that the shard was skipped, and return the local `path`

### Requirement: Upload is an opt-in runner concern with NULL and dry-run modes
The cloud-upload behaviour SHALL be selected by the runner's `upload` argument (see the `task-shards` factory), not by an `ssdsims_scenario` field, and SHALL support three modes. With `upload = NULL` (the default) the pipeline SHALL contain **no** `upload_<step>` targets — the upload feature is absent from the DAG. With `upload = ssd_upload_dryrun()` the pipeline SHALL contain the `upload_<step>` targets but each SHALL be a no-op that reaches no network, so the DAG shape is exercised offline and in CI without credentials. With `upload = ssd_upload_azure(...)` the pipeline SHALL contain the `upload_<step>` targets that ship to Azure. The same scenario run with different `upload` values SHALL produce byte-identical per-task results; only the presence and behaviour of the `upload_<step>` targets SHALL differ.

#### Scenario: NULL adds no upload nodes
- **WHEN** the factory is called with `upload = NULL`
- **THEN** the returned target list SHALL contain no `upload_<step>` targets

#### Scenario: Dry-run adds no-op upload nodes
- **WHEN** the factory is called with `upload = ssd_upload_dryrun()` and the pipeline is run
- **THEN** the `upload_<step>` targets SHALL be present and SHALL complete without reaching any network

#### Scenario: Upload mode does not change results
- **WHEN** the same scenario is run with `upload = NULL`, with `ssd_upload_dryrun()`, and with `ssd_upload_azure(...)`
- **THEN** the per-task result rows SHALL be byte-identical across the three runs

### Requirement: Per-shard upload targets are content-hashed and record the cloud sha256
When `upload` is non-`NULL`, the factory SHALL pair each step shard target with an `upload_<step>` target in the same `tar_map`, with `format = "file"` and `error = "null"`, taking the shard's local path as input. Because the upload target depends on the shard's content hash, an unchanged shard SHALL NOT be re-uploaded on a re-driven `tar_make()`, and a partial extension SHALL upload only the new or rewritten shards. The upload SHALL record the cloud copy's sha256 in the manifest (paired with the local sha256), so a transfer corruption is detectable as a mismatch; the local shard SHALL remain on disk so `targets`' `format = "file"` tracking of the compute step is unaffected.

#### Scenario: Unchanged shards are not re-uploaded
- **WHEN** a pipeline with a non-`NULL` `upload` is `tar_make()`'d, then re-driven with no shard content changed
- **THEN** no `upload_<step>` target SHALL re-run, so nothing is re-uploaded

#### Scenario: Only changed shards re-upload
- **WHEN** a scenario is extended so that only some shards are new or rewritten, and `tar_make()` is re-driven
- **THEN** only the `upload_<step>` targets for the new or rewritten shards SHALL run

#### Scenario: The cloud sha256 is recorded
- **WHEN** a shard is uploaded
- **THEN** the cloud copy's sha256 SHALL be recorded in the manifest alongside the local sha256, so a mismatch flags a corrupted transfer

### Requirement: Uploaded results are readable in place via a destination-dispatched generic
The package SHALL provide `ssd_open_uploaded(upload, step)`, a generic dispatching on the upload object's class, that opens the **uploaded** results for querying — so that right after an upload a user can read the data back and confirm it landed. For an Azure destination it SHALL return a lazy `duckplyr`/DuckDB table over the Hive glob `<container>[/<prefix>]/<step>/**/part.parquet` at the destination (honouring the destination's optional `prefix` subdirectory), read **in place** via DuckDB's `azure` extension (predicate pushdown straight against blob storage, **no download** of the Parquet). It SHALL resolve the **same** front-end secret as the write path (one of the `SSDSIMS_AZURE_*` secret environment variables) and **remap** it — together with the account name derived from `url` — into a DuckDB `azure` secret (`CREATE SECRET`) for the backend read — one credential source, translated, never a second source — and SHALL abort with a loud error (naming the missing requirement) when the `azure` extension or a required credential is absent. `step` SHALL select one of the step layers (`sample`/`fit`/`hc`), and the package SHALL offer a path to the uploaded `summary` as well. For a dry-run destination, `ssd_open_uploaded()` SHALL abort with an informative error stating that a dry run uploads nothing and that the local shards should be read directly — it SHALL NOT silently return an empty or local table. The result of `ssd_open_uploaded()` SHALL be composable with `dplyr` verbs (e.g. `dplyr::count()`) so a one-line round-trip check verifies the upload.

#### Scenario: Azure results read in place without downloading
- **WHEN** `ssd_open_uploaded(ssd_upload_azure(...), step = "hc")` is called with valid credentials
- **THEN** it SHALL return a lazy table over `<container>/hc/**/part.parquet` read in place via the DuckDB `azure` extension, without downloading the Parquet, composable with `dplyr` verbs

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
The package SHALL provide `ssd_summarise_uploaded(upload, step = "hc", drop_samples = TRUE)`, a generic dispatching on the upload object's class — the cloud counterpart of `ssd_summarise()` — that fans a step's **uploaded** shards into a single **lazy** `duckplyr`/DuckDB table read **in place** (no download). For an Azure destination it SHALL read the `<container>[/<prefix>]/<step>/**/part.parquet` Hive glob via DuckDB's `azure` extension, resolving the **same** front-end secret as the write path and remapping it (with the account derived from `url`) into a DuckDB `azure` secret (as `ssd_open_uploaded()` does), and SHALL return the union as a **lazy duckplyr table** (not collected, so the read and projection stay in DuckDB and the result composes with `dplyr`). The `drop_samples` flag SHALL default to `TRUE`, projecting away the heavy `dists`/`samples` list-columns (mirroring `ssd_summarise()`); when `FALSE` it SHALL retain them (so the in-flight bootstrap `samples` are available). The default method (an unknown destination) and the dry-run method SHALL abort with an informative error rather than returning an empty or local table.

#### Scenario: Azure summary unioned in place, samples projected away by default
- **WHEN** `ssd_summarise_uploaded(ssd_upload_azure(...), step = "hc")` is called with valid credentials
- **THEN** it SHALL return a lazy `duckplyr` table unioning `<container>[/<prefix>]/hc/**/part.parquet` read in place via the DuckDB `azure` extension, with the `dists`/`samples` list-columns projected away

#### Scenario: drop_samples = FALSE keeps the in-flight samples
- **WHEN** `ssd_summarise_uploaded(upload, step, drop_samples = FALSE)` is called
- **THEN** the returned tibble SHALL retain the `samples` (and `dists`) list-columns

#### Scenario: Dry-run has nothing to summarise
- **WHEN** `ssd_summarise_uploaded(ssd_upload_dryrun(), ...)` is called
- **THEN** it SHALL abort stating that a dry run uploads nothing and that the local shards should be summarised directly with `ssd_summarise()`

### Requirement: The backend set is extensible by a documented constructor-plus-methods contract
The package SHALL document, on the `ssd_upload_shard()` generic's help page, the contract for adding a new destination backend (e.g. S3, GCS): provide a constructor returning an object of class `c("ssdsims_upload_<backend>", "ssdsims_upload")` (validating its destination at construction) and implement the generic methods — `ssd_upload_shard()`, `ssd_test_upload()`, `ssd_open_uploaded()`, and `ssd_summarise_uploaded()` — for it. The package SHALL NOT ship speculative backends beyond Azure and dry-run; the dispatch SHALL be open so a new backend is added without editing the existing methods.

#### Scenario: Extension contract is documented
- **WHEN** a developer reads `?ssd_upload_shard`
- **THEN** it SHALL state that a new backend is added by writing a constructor returning an `ssdsims_upload_<backend>`/`ssdsims_upload` object (with construction-time validation) and implementing the generics (`ssd_upload_shard()`, `ssd_test_upload()`, `ssd_open_uploaded()`, `ssd_summarise_uploaded()`), with no edit to existing methods

#### Scenario: No speculative backends are shipped
- **WHEN** the package's exported upload constructors are enumerated
- **THEN** only `ssd_upload_azure()` and `ssd_upload_dryrun()` SHALL be present

### Requirement: A vignette demonstrates local upload and the cluster/Azure extension
The package SHALL ship a vignette ("Uploading Shards to Cloud Storage") that chains after the sharded-pipeline and cluster-pipeline vignettes and demonstrates the upload hook. Its **live** (evaluated) chunks SHALL run the pipeline locally with `ssd_upload_dryrun()` so the vignette builds with **no network access and no credentials** (guarded by `requireNamespace()` like the sibling vignettes), exercising `ssd_test_upload()` and the no-op `upload_<step>` targets. The vignette SHALL then show — as **described, non-evaluated** chunks — how to retarget the same `ssd_scenario_targets(scenario, ..., upload = ssd_upload_azure(...))` call to a real Azure destination on a cluster, **and how to read the results back with `ssd_open_uploaded(upload, step)` immediately after the upload to verify it landed** (an in-place query and a one-line `dplyr::count()` round-trip). It SHALL call out what to pay attention to: credentials must be present on the **workers** (not only the login/submit node), `ssd_test_upload()` SHALL be run as a preflight, a missing credential **fails loud** (per the credential requirement above), unchanged shards are **not re-uploaded** (the content-hash skip), and the read-back is **in place** (no download). The sharded-pipeline and cluster-pipeline vignettes SHALL gain a forward link to this vignette, so the three form a chain (define → shard → cluster → upload).

#### Scenario: Vignette builds offline with no credentials
- **WHEN** the vignette is rendered in an environment with no cloud credentials and no network
- **THEN** its live chunks SHALL complete using `ssd_upload_dryrun()` (skipping gracefully if the fitting/storage suggested packages are absent), and SHALL NOT require any `SSDSIMS_AZURE_*` variable

#### Scenario: Vignette documents the cluster extension and its caveats
- **WHEN** a reader follows the vignette past the local run
- **THEN** it SHALL show the same factory call retargeted to `ssd_upload_azure(...)` and SHALL state that credentials must reach the workers, that `ssd_test_upload()` is the preflight, that a missing credential fails loud, and that unchanged shards are not re-uploaded

#### Scenario: The vignette chain is cross-linked
- **WHEN** the `sharded-pipeline` and `cluster-pipeline` vignettes are read
- **THEN** each SHALL link forward to the "Uploading Shards to Cloud Storage" vignette, and that vignette SHALL link back to both
