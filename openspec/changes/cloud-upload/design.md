## Context

`TARGETS-DESIGN.md` §6.1 sketches a cloud-upload hook and §13 reserves a `cloud-upload` node, but neither is implemented. The sketch has three weaknesses this change addresses:

1. **The destination is an untyped list** (`scenario$upload = list(backend = "azure_blob", url, container)`), validated only as "a list or NULL" (`R/scenario.R:177`). A magic `backend` string, no autocomplete, and validation deferred to upload time — i.e. to a `crew` worker, mid-run.
2. **Missing credentials silently dry-run.** §6.1 says `ssd_upload_shard()` "checks for credentials and, when absent, returns the local path as a no-op." A run can go green having shipped nothing.
3. **The destination lives on the scenario** (`R/scenario.R:168`, `:314`; arg at `:108`,`:138`), even though every other scenario field is purely declarative compute and the single-core runner deliberately performs no upload (`R/shard-runner.R:39`).

The `ssd_scenario_targets()` factory (`R/targets-runner.R:625`, owned by the `task-shards` capability) already takes `root` as a runner argument and mints the `tar_map` step shards this change pairs upload targets onto. `scenario_results_dir()` (`R/targets-runner.R:74`) hashes `partition_by` layout, explicitly **not** `root` — establishing the precedent that a *destination* is not part of scenario identity.

## Goals / Non-Goals

**Goals:**

- Typed, self-validating destination objects: `ssd_upload_azure(url, container)` and `ssd_upload_dryrun()`, classed `c("ssdsims_upload_<backend>", "ssdsims_upload")`.
- Three class-dispatched generics — construction-time validation, `ssd_test_upload(upload)` (the front-door creds/connectivity probe), `ssd_upload_shard(path, upload)` (ship one shard) — with Azure and dry-run methods.
- Fail-loud credentials: Azure with absent creds aborts; skipping the network is expressed only by `ssd_upload_dryrun()`.
- Relocate the destination from the scenario to `ssd_scenario_targets(scenario, ..., root, upload, cue)`, with `check_dots_empty()` forcing named args, supporting both `upload = NULL` (no upload nodes) and `ssd_upload_dryrun()` (no-op nodes).
- Per-shard `upload_<step>` targets (`format = "file"`, `error = "null"`), content-hashed, recording the cloud sha256 in the manifest.
- A read-back tool, `ssd_open_uploaded(upload, step)`, that opens the uploaded results in place (DuckDB `azure` extension, no download) so an upload can be verified immediately; a generic dispatching on the same destination object.
- A documented "constructor + three generics" extension contract for future S3/GCS.
- A vignette ("Uploading Shards to Cloud Storage") that runs locally with `ssd_upload_dryrun()` and documents the cluster/Azure extension, chained after the shard and cluster vignettes.

**Non-Goals:**

- Shipping S3/GCS or any backend beyond Azure and dry-run (the dispatch is open; backends are added later).
- A full analysis layer (joins across step layers, summary dashboards, plotting). `ssd_open_uploaded()` provides the *reader* — a lazy `duckplyr` handle on the uploaded shards — and a verification round-trip; everything built *on top* of that handle is out of scope.
- Owning credentials. Auth stays external (env vars / service principal); the upload object never carries secrets.
- Changing the per-task compute, the shard layout, or the single-core/baseline runners (they remain upload-free by design).

## Decisions

### Decision: destination objects are plain, classed S3 lists — not R6, not connections

`ssd_upload_azure()`/`ssd_upload_dryrun()` return `structure(list(url=, container=), class = c("ssdsims_upload_azure_blob", "ssdsims_upload"))`. This matches house style (`ssdsims_scenario` at `R/scenario.R:286`, `ssdsims_data` at `:533`/`:569`) and, critically, stays a **plain serialisable value** so it rides on the `upload` argument to `crew` workers and through `targets` without an open handle or environment. *Alternative considered:* an R6 object holding a live client — rejected: not cleanly serialisable across workers, and it would tempt carrying credentials in-object.

### Decision: fail loud on missing credentials; dry-run is explicit, not a fallback

This departs from §6.1's "absent creds → silent no-op." A silent skip lets a pipeline succeed having uploaded nothing — discovered only when the container turns up empty days later. Instead: `ssd_upload_azure(...)` with absent creds **errors** (at probe time via `ssd_test_upload()`, and as a backstop at `ssd_upload_shard()` time on a worker, where `error = "null"` keeps the local shard and lets the rest proceed). Intent to skip the network is expressed *only* by passing `ssd_upload_dryrun()`. This makes "no upload" a visible decision rather than an accident of the environment. *Alternative considered:* keep the silent fallback for "runs offline unchanged" — rejected; `ssd_upload_dryrun()` gives that property explicitly and honestly.

### Decision: `ssd_test_upload()` is the front-door probe and the user's "are my creds right?" check

A user's real question is "are my credentials in the right place?" `ssd_test_upload(upload)` answers it as a one-liner at the prompt: for Azure it resolves env creds (loud error naming the missing `AZURE_*` var), then does a marker-blob round-trip; for dry-run it is trivially OK. The pipeline calls it once at the start so an auth/network failure aborts before any compute. Its whole purpose is to move the failure from *deep in the DAG on a worker* to *the interactive prompt before `tar_make()`*. *Alternative considered:* fold the check into `ssd_upload_shard()` only — rejected; that surfaces the failure late and per-shard, after compute has burned.

### Decision: `upload` is a runner argument, the remote-destination sibling of `root`

Move `upload` off the scenario onto `ssd_scenario_targets(scenario, ..., root, upload, cue)`. Two facts force this: the single-core runner has **no** upload by design (`R/shard-runner.R:39`), so upload is execution-mode-bound, not scenario-intrinsic; and `scenario_results_dir()` hashes layout, not `root`, so a *destination* is already established as non-identity. Two pipelines shipping the same scenario to different containers are the *same scenario*. The destination gradient becomes: baseline → scenario only; single-core → scenario + `root`; pipeline → scenario + `root` + `upload`. The `upload_<step>` targets are minted inside the factory — exactly where the argument now lives. *Alternative considered:* keep `upload` on the scenario so the scenario file is a self-contained record of "where results live" — rejected: it conflicts with the single-core runner's no-upload contract and with `root` being a runner concern; provenance of "what was shipped" is recorded by the `manifest` from the `upload_<step>` *targets*, not from the scenario.

### Decision: `...` + `check_dots_empty()` to force named arguments

`ssd_scenario_targets()` grows from two to three optional knobs (`root`, `upload`, `cue`). Placing `...` immediately after `scenario` and calling `rlang::check_dots_empty()` forces all three to be passed by name, so a positional `ssd_scenario_targets(scenario, my_upload)` or a misspelled `uplaod =` aborts loudly instead of silently binding to the wrong parameter. This is cheap insurance now that destinations are easy to confuse with roots (both are path-like). *Alternative considered:* leave them positional — rejected; `root` and `upload` are both string/path-shaped and trivially transposable.

### Decision: `upload = NULL` vs `ssd_upload_dryrun()` are distinct and both supported

`NULL` (default) means the `upload_<step>` nodes **do not exist** in the DAG — the cleanest graph for the common non-uploader. `ssd_upload_dryrun()` means the nodes **exist and no-op** — so the package's own tests and CI exercise the full upload DAG shape (target wiring, `tar_map` pairing, manifest recording) without credentials or network. Keeping both lets production stay clean while the feature stays testable offline. *Alternative considered:* a single default — rejected; one default cannot serve both "no feature" and "feature exercised offline."

### Decision: upload is a per-shard `format = "file"` target, not an inline side effect

Following §6.1, the `upload_<step>` target takes the shard's local path (`format = "file"`) so `targets` re-runs it only when the shard's content hash changes — a re-driven `tar_make()` that rebuilt nothing uploads nothing; a partial extension uploads only new/rewritten shards. `error = "null"` isolates a per-shard upload failure. The local shard stays on disk (compute-step tracking unaffected); the cloud copy is an additional artefact whose sha256 the manifest records beside the local sha256.

### Decision: `ssd_open_uploaded()` reads the upload back in place — a fourth generic on the destination

"Upload then immediately verify" is the natural round-trip, so the §6.1 in-place read story is pulled into scope as a fourth generic on the same upload object: `ssd_open_uploaded(upload, step)`. The destination object already holds the URL/container, so it is the right thing to dispatch on — symmetric with `ssd_upload_shard()`/`ssd_test_upload()`. The Azure method returns a **lazy** `duckplyr`/DuckDB table over the `<container>/<step>/**/part.parquet` Hive glob, read **in place** via DuckDB's `azure` extension (predicate pushdown straight against blob storage, no download) — exactly the §6.1 "read the shards back in place" property, and the same Hive layout that makes local reads work. Because it is lazy and `dplyr`-composable, a one-line `ssd_open_uploaded(upload, step) |> dplyr::count()` is the immediate post-upload smoke test, and a row-for-row compare against the local shard verifies the transfer (complementing the manifest's local-vs-cloud sha256). The dry-run method **errors** ("a dry run uploads nothing; read the local shards directly") rather than silently returning a local or empty table, so the read path is as fail-loud as the write path. **Credentials are single-sourced:** the read path resolves the **same** front-end `AZURE_*` credentials as the write path and **remaps** them into a DuckDB `azure` secret (`CREATE SECRET`) for the backend — the user configures one credential source, and the package translates it for DuckDB rather than asking for a second. The `azure` extension or a missing credential fails loud naming the missing requirement. *Alternatives considered:* a separate `ssd_read_results(url, container, step)` free function (rejected — duplicates the destination already captured in the object, and loses class dispatch); downloading the blobs to a temp dir then reading locally (rejected — defeats the in-place predicate-pushdown story and is slow for large result sets).

### Decision: the Azure backend is built on `AzureStor` + `AzureRMR`

Decided elsewhere: the Azure method uses `AzureStor` for the blob operations (container endpoint, `upload_blob()`/`list_blobs()`/`delete_blob()` — the put/list/delete behind `ssd_upload_shard()` and `ssd_test_upload()`'s marker round-trip) and `AzureRMR` for the authentication layer (AAD tokens / service principal), complementing the simpler `AZURE_STORAGE_ACCOUNT` + `AZURE_STORAGE_KEY` path. Both are pure-R, are widely used (the `Azure*` family), and stay in `Suggests` so the package builds and the dry-run/`NULL` paths run without them, guarded by `rlang::check_installed(c("AzureStor", "AzureRMR"))` in the Azure methods. *Alternatives considered:* a raw `httr2` REST client (rejected — re-implements auth/retry that `AzureStor` already provides) or the `paws`-style generated SDK (no first-class Azure equivalent in R). The off-cluster read-back uses DuckDB's `azure` extension, separate from these write-path clients.

### Decision: the vignette runs locally with dry-run and *describes* the cluster/Azure path

The new vignette mirrors the convention already set by its siblings: the `sharded-pipeline` vignette runs locally and shows the `targets`/cluster pipeline "but does not run it," and the `cluster-pipeline` vignette's scheduler chunks are not evaluated (no SLURM at build time). The upload vignette does the same for the network: its **live** chunks run `ssd_scenario_targets(..., upload = ssd_upload_dryrun())` on a local root, so the vignette builds in CI with **no credentials and no network** (guarded by `requireNamespace()` exactly like `sharded-pipeline.qmd`), exercising `ssd_test_upload()` and the no-op `upload_<step>` targets end to end. The Azure destination and the cluster wiring are shown as **described, non-evaluated** chunks. This is the cleanest place the dry-run object pays off — it is what makes a network-free vignette possible while still walking every node of the upload DAG. *Alternative considered:* a vignette that talks to a live Azure account — rejected; it cannot run in CI, contradicts the "builds offline" property, and would leak/require credentials.

The vignette's cluster section is deliberately a *delta on the existing cluster vignette*, not a re-teaching of `crew`/SLURM: it adds the one line (`upload = ssd_upload_azure(...)` in the cluster template's `_targets.R`) and the operational caveats that only matter once a real backend is involved — credentials must reach the **workers** (set via the controller's `script_lines`/module loads or the scheduler's env propagation, not just the login node where `ssd_test_upload()` is easy to run interactively); the Azure client and DuckDB `azure` extension must be installed on the workers (the ManyLinux binary path the cluster vignette already covers); a missing credential **fails loud** on the shard's upload branch (`error = "null"` keeps the rest shipping); and the content-hash skip means a re-driven run re-uploads only changed shards. It forward-links from `sharded-pipeline.qmd` and `cluster-pipeline.qmd` so the four-vignette chain reads define → shard → cluster → upload.

## Risks / Trade-offs

- **No Azure in CI.** A real round-trip needs credentials CI lacks → mitigated: `ssd_upload_dryrun()` exercises the full DAG shape and the generics' wiring offline; the live Azure round-trip is a documented manual/lab validation step. The Azure method is thin (put/list/delete) over `AzureStor`.
- **BREAKING signature changes.** Removing `scenario$upload` and reordering `ssd_scenario_targets()` breaks any caller using the §6.1 sketch → mitigated: the feature is not yet released as code (the sketch lives only in `TARGETS-DESIGN.md`); the change is a clean cutover, and `check_dots_empty()` turns silently-wrong positional calls into loud errors.
- **Coordination with `dists-simulation-setting`.** That in-progress change's "arguments grouped by role" delta lists `upload` as the last constructor argument → mitigated: whichever change lands second drops the `upload` reference from that ordering (recorded in the proposal's Impact). No code conflict, only a spec reconciliation.
- **Suggested-dependency drift.** `AzureStor`/`AzureRMR` and the DuckDB `azure` extension are in `Suggests`; absence must degrade cleanly → mitigated: `rlang::check_installed(c("AzureStor", "AzureRMR"))` guards the Azure methods, and the dry-run/NULL paths need none of them, so the package builds, checks, and runs the local templates without them.

## Open Questions

- **Credential schema.** The client is decided (`AzureStor` + `AzureRMR`); what remains is which auth mode(s) `ssd_test_upload.ssdsims_upload_azure_blob()` resolves and in what precedence — `AZURE_STORAGE_ACCOUNT` + `AZURE_STORAGE_KEY` (account key, via `AzureStor`), a SAS token, or an AAD service principal (via `AzureRMR`). The spec only requires that a missing required variable is named loudly; the precise env-var set is pinned during implementation.
- **Manifest coupling.** The precise field where the cloud sha256 lands depends on the `manifest` change's record shape; this change records "local + cloud sha256 per shard" and defers the exact column to `manifest`.

(*Resolved:* read-back auth — `ssd_open_uploaded()` reuses the same front-end `AZURE_*` credentials as the write path and remaps them into a DuckDB `azure` secret for the backend; see the `ssd_open_uploaded()` decision above. The only remaining auth detail is the write-path mode/precedence in the "Credential schema" question.)
