## 1. Upload objects and generics (`R/upload.R`)

- [ ] 1.1 Add `AzureStor`, `AzureRMR`, and the DuckDB `azure` extension to `DESCRIPTION` `Suggests`; document that the upload path is opt-in
- [ ] 1.2 Implement `ssd_upload_azure(url, container)` returning `structure(list(url, container), class = c("ssdsims_upload_azure_blob", "ssdsims_upload"))` with `chk`-style construction-time validation of `url`/`container` (non-empty strings), aborting in the user-facing call context
- [ ] 1.3 Implement `ssd_upload_dryrun()` returning `structure(list(), class = c("ssdsims_upload_dryrun", "ssdsims_upload"))`
- [ ] 1.4 Define the `ssd_test_upload(upload)` generic (`UseMethod`) and `ssd_upload_shard(path, upload)` generic; add a default method that aborts on an unknown class
- [ ] 1.5 Implement `ssd_test_upload.ssdsims_upload_azure_blob()` over `AzureStor`/`AzureRMR`: `rlang::check_installed(c("AzureStor", "AzureRMR"))`, resolve env credentials (account-key, SAS, or AAD service principal), abort with a loud error **naming the missing `AZURE_*` variable**, then list container + write/delete a marker blob
- [ ] 1.6 Implement `ssd_test_upload.ssdsims_upload_dryrun()` as a trivial success (no credential resolution, no network)
- [ ] 1.7 Implement `ssd_upload_shard.ssdsims_upload_azure_blob(path, upload)` via `AzureStor::upload_blob()`: put the Parquet at `<url>/<container>/<step>/<partition-path>/part.parquet`, return local `path`; abort loudly when credentials are absent (no silent no-op)
- [ ] 1.8 Implement `ssd_upload_shard.ssdsims_upload_dryrun(path, upload)`: no network I/O, record a skip, return local `path`
- [ ] 1.9 Compute and surface the cloud copy's sha256 from `ssd_upload_shard()` for the manifest to record alongside the local sha256
- [ ] 1.10 Roxygen-document all constructors and generics; on `?ssd_upload_shard` document the extension contract (constructor + three methods, no edit to existing methods); `@export` the public functions and run `devtools::document()` to update `NAMESPACE`/`man/`

## 2. Remove `upload` from the scenario (`R/scenario.R`)

- [ ] 2.1 Drop the `upload` argument and its `@param` from `ssd_define_scenario()`
- [ ] 2.2 Remove the `upload` field from the `structure(...)` scenario constructor and the `chk::chk_null_or(upload, ...)` validation
- [ ] 2.3 Remove any `upload` rendering from `print.ssdsims_scenario()`
- [ ] 2.4 Update scenario tests and examples that reference `scenario$upload`

## 3. Wire `upload` into the factory (`R/targets-runner.R`)

- [ ] 3.1 Change `ssd_scenario_targets()` to `function(scenario, ..., root = scenario_results_dir(scenario), upload = NULL, cue = NULL)` and call `rlang::check_dots_empty()` first
- [ ] 3.2 Validate `upload` as `NULL` or an `ssdsims_upload` object; keep `chk::chk_string(root)`
- [ ] 3.3 When `upload` is non-`NULL`, emit a paired `upload_<step>` target per shard in each step's `tar_map` (`format = "file"`, `error = "null"`), taking the shard's local path as input and calling `ssd_upload_shard(path, upload)`
- [ ] 3.4 When `upload = NULL`, emit no `upload_<step>` targets (the default, clean DAG)
- [ ] 3.5 Have the pipeline call `ssd_test_upload(upload)` once up front (non-`NULL` upload) so an auth/network failure aborts before compute
- [ ] 3.6 Update `@param`/`@examples` and the shipped `_targets.R` templates to pass `upload = ...` by name

## 4. Tests

- [ ] 4.1 Unit-test constructors: classes set, fields stored, `ssd_upload_azure()` validation aborts on bad `url`/`container`, objects carry no credentials
- [ ] 4.2 Test `ssd_test_upload()`: dry-run trivially OK; Azure with a missing `AZURE_*` var aborts naming the variable (mock/skip the live round-trip)
- [ ] 4.3 Test `ssd_upload_shard()`: dry-run records a skip and reaches no network; Azure with absent creds aborts loudly (no silent path return)
- [ ] 4.4 Test the factory: `upload = NULL` yields no `upload_<step>` targets; `ssd_upload_dryrun()` yields one paired target per shard; `check_dots_empty()` rejects positional/misspelled args
- [ ] 4.5 Test that per-task results are byte-identical across `upload = NULL`, `ssd_upload_dryrun()`, and (mocked) Azure runs
- [ ] 4.6 Test content-hash skip: a re-driven dry-run pipeline with unchanged shards re-runs no `upload_<step>` target; a partial extension runs only the new shards' upload targets

## 5. Vignette ("Uploading Shards to Cloud Storage", `vignettes/cloud-upload.qmd`)

- [ ] 5.1 Create `vignettes/cloud-upload.qmd` with the gerund title, `quarto::html` engine, and the `requireNamespace()`-guarded `eval` setup mirroring `sharded-pipeline.qmd` (skip gracefully without the fitting/storage suggests)
- [ ] 5.2 Write the intro that chains off the shard + cluster vignettes (link back to `sharded-pipeline.html` and `cluster-pipeline.html`) and states the model: upload is a runner argument (sibling of `root`), live chunks use `ssd_upload_dryrun()` so the build needs no network or credentials
- [ ] 5.3 Live (evaluated) section — local run: build a scenario, call `ssd_scenario_targets(scenario, root = <tmp>, upload = ssd_upload_dryrun())`, show `ssd_test_upload(ssd_upload_dryrun())` succeeding and the no-op `upload_<step>` targets in the graph; contrast `upload = NULL` (no upload nodes)
- [ ] 5.4 Described (non-evaluated) section — Azure on a cluster: show the same factory call with `upload = ssd_upload_azure(url, container)` dropped into the cluster template's `_targets.R`, and `ssd_test_upload()` as an interactive preflight
- [ ] 5.5 "What to pay attention to" callout: credentials must reach the **workers** (controller `script_lines`/module loads or scheduler env propagation, not just the login node); the Azure client + DuckDB `azure` extension must be installed on workers (ManyLinux path); a missing credential **fails loud** (`error = "null"` keeps the rest shipping); unchanged shards are not re-uploaded (content-hash skip)
- [ ] 5.6 Add forward links to the new vignette from `vignettes/sharded-pipeline.qmd` and `vignettes/cluster-pipeline.qmd`
- [ ] 5.7 Render the vignette offline (no `AZURE_*` set) to confirm it builds with no network/credentials

## 6. Docs and design sync

- [ ] 6.1 Update `TARGETS-DESIGN.md` §6.1 (destination on the runner, fail-loud creds, explicit `ssd_upload_dryrun()`) and mark §13's `cloud-upload` node realised
- [ ] 6.2 Reconcile with `dists-simulation-setting`: drop the `upload` reference from the "arguments grouped by role" ordering (whichever change lands second)
- [ ] 6.3 Add a `NEWS.md` entry for the new upload API and the BREAKING removal of `scenario$upload`
- [ ] 6.4 Run `air format`, `devtools::document()`, `devtools::check()`, and `pkgdown` reference + articles build; ensure no cloud credentials are needed for the default test/check/vignette run
