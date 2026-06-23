# cloud-upload: the storage account is derived from the url (+ domain)

    Code
      ssd_upload_azure("https://acct.example.com", "c")
    Condition
      Error in `ssd_upload_azure()`:
      ! `url` ("https://acct.example.com") must be an Azure Blob endpoint of the form `https://<account>.blob.core.windows.net` so the storage account name can be derived; pass a matching `domain` for a non-public cloud.

# cloud-upload: ssd_upload_azure() forces prefix to be passed by name

    Code
      ssd_upload_azure("https://acct", "c", "study-2026")
    Condition
      Error in `ssd_upload_azure()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = "study-2026"
      i Did you forget to name an argument?

---

    Code
      ssd_upload_azure("https://acct", "c", prefx = "study-2026")
    Condition
      Error in `ssd_upload_azure()`:
      ! `...` must be empty.
      x Problematic argument:
      * prefx = "study-2026"

# cloud-upload: ssd_upload_azure() validates its destination

    Code
      ssd_upload_azure(url = "", container = "c")
    Condition
      Error in `ssd_upload_azure()`:
      ! `url` must be a non-empty string.

---

    Code
      ssd_upload_azure(url = "https://acct", container = 1L)
    Condition
      Error in `ssd_upload_azure()`:
      ! `container` must be a non-empty string.

---

    Code
      ssd_upload_azure(url = "https://acct.blob.core.windows.net", container = "c",
        prefix = 1L)
    Condition
      Error in `ssd_upload_azure()`:
      ! `prefix` must be `NULL` or a non-empty string (a subdirectory within the container).

# cloud-upload: ssd_test_upload() Azure names the missing credential

    Code
      ssd_test_upload(upload)
    Condition
      Error:
      ! Azure credentials are incomplete: no authentication secret was found. Set `SSDSIMS_AZURE_STORAGE_KEY` (account-key auth), or `SSDSIMS_AZURE_STORAGE_SAS` (SAS auth), or the service-principal trio `SSDSIMS_AZURE_TENANT_ID`/`SSDSIMS_AZURE_CLIENT_ID`/`SSDSIMS_AZURE_CLIENT_SECRET` (missing: 'SSDSIMS_AZURE_TENANT_ID', 'SSDSIMS_AZURE_CLIENT_ID' and 'SSDSIMS_AZURE_CLIENT_SECRET'). The storage account name is taken from the destination `url`, not the environment.

# cloud-upload: no secret set names the auth options

    Code
      resolve_azure_credentials()
    Condition
      Error:
      ! Azure credentials are incomplete: no authentication secret was found. Set `SSDSIMS_AZURE_STORAGE_KEY` (account-key auth), or `SSDSIMS_AZURE_STORAGE_SAS` (SAS auth), or the service-principal trio `SSDSIMS_AZURE_TENANT_ID`/`SSDSIMS_AZURE_CLIENT_ID`/`SSDSIMS_AZURE_CLIENT_SECRET` (missing: 'SSDSIMS_AZURE_TENANT_ID', 'SSDSIMS_AZURE_CLIENT_ID' and 'SSDSIMS_AZURE_CLIENT_SECRET'). The storage account name is taken from the destination `url`, not the environment.

# cloud-upload: ssd_upload_shard() validates path as a character vector

    Code
      ssd_upload_shard(character(0L), ssd_upload_dryrun())
    Condition
      Error in `ssd_upload_shard.ssdsims_upload_dryrun()`:
      ! `path` must not be empty (zero length).

---

    Code
      ssd_upload_shard(1L, ssd_upload_dryrun())
    Condition
      Error in `ssd_upload_shard.ssdsims_upload_dryrun()`:
      ! `path` must be character.

# cloud-upload: Azure ssd_upload_shard() with absent credentials fails loud

    Code
      ssd_upload_shard(path, upload)
    Condition
      Error:
      ! Azure credentials are incomplete: no authentication secret was found. Set `SSDSIMS_AZURE_STORAGE_KEY` (account-key auth), or `SSDSIMS_AZURE_STORAGE_SAS` (SAS auth), or the service-principal trio `SSDSIMS_AZURE_TENANT_ID`/`SSDSIMS_AZURE_CLIENT_ID`/`SSDSIMS_AZURE_CLIENT_SECRET` (missing: 'SSDSIMS_AZURE_TENANT_ID', 'SSDSIMS_AZURE_CLIENT_ID' and 'SSDSIMS_AZURE_CLIENT_SECRET'). The storage account name is taken from the destination `url`, not the environment.

# cloud-upload: unknown upload object aborts on every generic

    Code
      ssd_test_upload(list())
    Condition
      Error:
      ! `upload` must be an upload destination from `ssd_upload_azure()` or `ssd_upload_dryrun()`, not an object of class "list".

---

    Code
      ssd_upload_shard("x", list())
    Condition
      Error:
      ! `upload` must be an upload destination from `ssd_upload_azure()` or `ssd_upload_dryrun()`, not an object of class "list".

---

    Code
      ssd_open_uploaded(list(), "hc")
    Condition
      Error:
      ! `upload` must be an upload destination from `ssd_upload_azure()` or `ssd_upload_dryrun()`, not an object of class "list".

# cloud-upload: ssd_open_uploaded() on a dry run has nothing to read back

    Code
      ssd_open_uploaded(ssd_upload_dryrun(), "hc")
    Condition
      Error:
      ! A dry-run upload ships nothing, so there is nothing to read back. Read the local shards directly (e.g. with `ssd_summarise()` or `duckplyr::read_parquet_duckdb()` under the results root).

# cloud-upload: ssd_summarise_uploaded() on a dry run has nothing to summarise

    Code
      ssd_summarise_uploaded(ssd_upload_dryrun())
    Condition
      Error:
      ! A dry-run upload ships nothing, so there is nothing to summarise. Summarise the local shards directly with `ssd_summarise()`.

# cloud-upload: ssd_summarise_uploaded() aborts for an unknown destination

    Code
      ssd_summarise_uploaded(list())
    Condition
      Error:
      ! `upload` must be an upload destination from `ssd_upload_azure()` or `ssd_upload_dryrun()`, not an object of class "list".

# cloud-upload: ssd_summarise_uploaded() validates step and drop_samples first

    Code
      ssd_summarise_uploaded(upload, step = "nope")
    Condition
      Error in `ssd_summarise_uploaded()`:
      ! `step` must be one of "sample", "fit", "hc", "summary", or "summary_samples", not "nope".

---

    Code
      ssd_summarise_uploaded(upload, drop_samples = "yes")
    Condition
      Error in `ssd_summarise_uploaded.ssdsims_upload_azure_blob()`:
      ! `drop_samples` must be a flag (TRUE or FALSE).

# cloud-upload: the compact summary cannot pretend to carry samples

    Code
      ssd_summarise_uploaded(upload, step = "summary", drop_samples = FALSE)
    Condition
      Error:
      ! The uploaded compact summary (`step = "summary"`) does not carry the `dists`/`samples` list-columns, so `drop_samples = FALSE` cannot be honoured. Read the uploaded full summary with `step = "summary_samples"` instead (shipped only when the scenario set `samples = TRUE`).

# cloud-upload: root, upload, and cue must be passed by name

    Code
      ssd_scenario_targets(scenario, "results")
    Condition
      Error in `ssd_scenario_targets()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = "results"
      i Did you forget to name an argument?

---

    Code
      ssd_scenario_targets(scenario, uplaod = ssd_upload_dryrun())
    Condition
      Error in `ssd_scenario_targets()`:
      ! `...` must be empty.
      x Problematic argument:
      * uplaod = ssd_upload_dryrun()

# cloud-upload: a non-upload object aborts in the factory

    Code
      ssd_scenario_targets(scenario, upload = list())
    Condition
      Error in `ssd_scenario_targets()`:
      ! `upload` must be `NULL` or an upload destination from `ssd_upload_azure()` or `ssd_upload_dryrun()`.

# cloud-upload: the dots force prudence/drop_samples to be passed by name

    Code
      ssd_open_uploaded(upload, "hc", "stingy")
    Condition
      Error in `ssd_open_uploaded()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = "stingy"
      i Did you forget to name an argument?

---

    Code
      ssd_summarise_uploaded(upload, "hc", TRUE)
    Condition
      Error in `ssd_summarise_uploaded()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = TRUE
      i Did you forget to name an argument?

