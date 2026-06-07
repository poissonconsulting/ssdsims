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
      ssd_upload_azure(url = "https://acct", container = "c", prefix = 1L)
    Condition
      Error in `ssd_upload_azure()`:
      ! `prefix` must be `NULL` or a non-empty string (a subdirectory within the container).

# cloud-upload: ssd_test_upload() Azure names the missing credential

    Code
      ssd_test_upload(upload)
    Condition
      Error:
      ! Azure credentials are incomplete: the environment variable `SSDSIMS_AZURE_STORAGE_ACCOUNT` is not set. Set it (the storage account name) together with one of `SSDSIMS_AZURE_STORAGE_KEY`, `SSDSIMS_AZURE_STORAGE_SAS`, or the service-principal trio `SSDSIMS_AZURE_TENANT_ID`/`SSDSIMS_AZURE_CLIENT_ID`/`SSDSIMS_AZURE_CLIENT_SECRET`.

# cloud-upload: an account with no secret names the auth options

    Code
      resolve_azure_credentials()
    Condition
      Error:
      ! Azure credentials are incomplete: `SSDSIMS_AZURE_STORAGE_ACCOUNT` is set, but no authentication secret was found. Set `SSDSIMS_AZURE_STORAGE_KEY` (account-key auth), or `SSDSIMS_AZURE_STORAGE_SAS` (SAS auth), or the service-principal trio (missing: 'SSDSIMS_AZURE_TENANT_ID', 'SSDSIMS_AZURE_CLIENT_ID' and 'SSDSIMS_AZURE_CLIENT_SECRET').

# cloud-upload: Azure ssd_upload_shard() with absent credentials fails loud

    Code
      ssd_upload_shard(path, upload)
    Condition
      Error:
      ! Azure credentials are incomplete: the environment variable `SSDSIMS_AZURE_STORAGE_ACCOUNT` is not set. Set it (the storage account name) together with one of `SSDSIMS_AZURE_STORAGE_KEY`, `SSDSIMS_AZURE_STORAGE_SAS`, or the service-principal trio `SSDSIMS_AZURE_TENANT_ID`/`SSDSIMS_AZURE_CLIENT_ID`/`SSDSIMS_AZURE_CLIENT_SECRET`.

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
      ! A dry-run upload ships nothing, so there is nothing to read back. Read the local shards directly (e.g. with `ssd_summarize()` or `duckplyr::read_parquet_duckdb()` under the results root).

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

