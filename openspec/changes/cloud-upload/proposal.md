## Why

The data/fit/hc shards must be readable **from outside the cluster** — laptops, dashboards, downstream R/Python — so the pipeline needs to ship each shard to an object store (`TARGETS-DESIGN.md` §6.1, §13 reserves the `cloud-upload` node). Today the destination is sketched as an untyped `scenario$upload` list with a magic `backend` string and a *silent* dry-run when credentials are missing — a footgun that lets a run go green having uploaded nothing. This change replaces that with typed, self-validating upload objects, a fail-loud credential contract, and a one-line front-door probe, and relocates the destination to where it is actually consumed.

## What Changes

- **New upload objects.** `ssd_upload_azure(url, container)` and `ssd_upload_dryrun()` return validated S3 objects (class `c("ssdsims_upload_<backend>", "ssdsims_upload")`); construction validates `url`/`container`.
- **Three generics dispatch on the object's class:**
  - `ssd_upload_shard(path, upload)` — ships one shard (Azure puts the blob and returns the local path; dryrun records a skip).
  - `ssd_test_upload(upload)` — the front-door connectivity + credentials probe (Azure: resolve env creds with a **loud error naming the missing `AZURE_*` var**, then list container + write/delete a marker blob; dryrun: trivially OK).
- **BREAKING — fail loud on missing credentials.** Azure with absent credentials is now a **hard error**, not a silent dry-run no-op (a departure from `TARGETS-DESIGN.md` §6.1). Intent to skip the network must be declared explicitly via `ssd_upload_dryrun()`.
- **BREAKING — `upload` moves off the scenario.** Drop the `upload` argument from `ssd_define_scenario()` and the `upload` field from the `ssdsims_scenario` object. The destination is execution-bound, not part of the scenario's declarative identity (the single-core runner deliberately has no upload; `scenario_results_dir()` hashes layout, not `root`).
- **`upload` becomes a runner argument**, the remote-destination sibling of `root`: `ssd_scenario_targets(scenario, ..., root, upload, cue)`. `...` + `rlang::check_dots_empty()` force `root`/`upload`/`cue` to be passed by name.
- **Two no-upload modes, both supported:** `upload = NULL` (default — **no** `upload_<step>` nodes in the DAG, for non-uploaders) and `upload = ssd_upload_dryrun()` (`upload_<step>` nodes present but no-op, to exercise the DAG shape offline/in CI).
- **Per-shard upload targets.** When `upload` is non-`NULL`, the factory pairs each step shard with an `upload_<step>` target (`format = "file"`, `error = "null"`) via the same `tar_map`, content-hashed so unchanged shards never re-upload; the upload's cloud sha256 is recorded in the manifest.
- **Documented extension contract** on `?ssd_upload_shard`: to add S3/GCS, write a constructor + the three methods. No speculative backends are added now.
- **New vignette** ("Uploading Shards to Cloud Storage", `vignettes/cloud-upload.qmd`) chained after the shard and cluster vignettes: it runs the upload **locally** (live chunks use `ssd_upload_dryrun()` so the build needs no network or credentials), then shows how to **extend the same factory call to a real Azure destination on a cluster** and what to watch for — credentials on the *workers* (not just the login node), the `ssd_test_upload()` preflight, the fail-loud behaviour, and the content-hash re-upload skip. Forward links are added from `sharded-pipeline.qmd` and `cluster-pipeline.qmd`.

## Capabilities

### New Capabilities
- `cloud-upload`: typed upload destination objects (`ssd_upload_azure()`, `ssd_upload_dryrun()`), the three class-dispatched generics (`ssd_upload_shard()`, `ssd_test_upload()`, plus construction-time validation), the fail-loud credential contract, the per-shard `upload_<step>` targets paired with each step shard, and the documented backend-extension contract.

### Modified Capabilities
- `scenario-definition`: remove `upload` from the `ssd_define_scenario()` signature and from the declarative `ssdsims_scenario` field set (the "Upload defaults to none" behaviour is removed; the destination now lives on the runner).
- `task-shards`: `ssd_scenario_targets()` gains a trailing `...` with `rlang::check_dots_empty()` and an `upload` argument (sibling of `root`); when `upload` is non-`NULL` the factory emits a paired `upload_<step>` target per shard.

## Impact

- **New code**: `R/upload.R` (the constructors, generics, Azure + dryrun methods, `ssd_test_upload()`), exported in `NAMESPACE`, with `man/` docs.
- **New docs**: `vignettes/cloud-upload.qmd` (local dry-run run + cluster/Azure extension guide), cross-linked with the `sharded-pipeline` and `cluster-pipeline` vignettes (which gain a forward link).
- **Changed code**: `R/scenario.R` (drop `upload` arg/field/validation/print), `R/targets-runner.R` (`ssd_scenario_targets()` signature + `upload_<step>` target wiring).
- **Dependencies**: `AzureStor` (blob put/list/delete) and `AzureRMR` (the AAD / service-principal auth layer) — decided elsewhere — plus DuckDB's `azure` extension for in-place read-back, all added to `Suggests` (the upload path is opt-in; the package builds and tests without cloud creds via `ssd_upload_dryrun()`). Auth stays external (`AZURE_STORAGE_ACCOUNT`, `AZURE_STORAGE_KEY`, or a service principal) — the upload object carries only the destination, never secrets.
- **Design docs**: `TARGETS-DESIGN.md` §6.1 is updated (destination on the runner, fail-loud creds, explicit dryrun) and §13's `cloud-upload` node is realised.
- **Depends on**: `task-shards` (the `ssd_scenario_targets()` factory) and `scenario-definition` (the `upload`-field removal). **Pairs with** `manifest` (the cloud sha256 is recorded there).
- **Coordination**: the in-progress `dists-simulation-setting` change lists `upload` as the last constructor argument in its "arguments grouped by role" delta. Whichever change lands second SHALL reconcile — dropping the `upload` reference from that ordering — so the constructor signature stays consistent.
