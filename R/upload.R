# Typed, self-validating upload destinations and the class-dispatched generics
# that ship, probe, and read back shards (TARGETS-DESIGN.md section 6.1). An
# upload object is a plain, serialisable value carrying only the destination
# (never credentials), so it rides on the `upload` argument to `crew` workers
# and through `targets` unchanged. Three (plus construction) generics dispatch
# on its class: `ssd_test_upload()` (the front-door creds/connectivity probe),
# `ssd_upload_shard()` (ship one shard), and `ssd_open_uploaded()` (read the
# uploaded results back in place). Azure with absent credentials is a hard
# error, never a silent no-op; the only way to skip the network is to pass
# `ssd_upload_dryrun()`.

#' Upload Destinations for a Scenario's Shards
#'
#' Typed, self-validating destination objects for the targets pipeline's
#' per-shard upload (`TARGETS-DESIGN.md` section 6.1). Pass one to
#' [ssd_scenario_targets()]'s `upload` argument (the remote-destination sibling
#' of `root`) to pair each step shard with an `upload_<step>` target.
#'
#' `ssd_upload_azure()` describes an Azure Blob Storage container;
#' `ssd_upload_dryrun()` is a no-op destination that reaches no network, so the
#' upload DAG shape can be exercised offline and in CI without credentials. Both
#' return a plain, serialisable S3 object of class
#' `c("ssdsims_upload_<backend>", "ssdsims_upload")` that carries **only the
#' destination** - never credentials, open connections, or environments - so it
#' travels unchanged to `crew` workers and through `targets`.
#'
#' Credentials stay **external** to the object: the Azure methods
#' ([ssd_test_upload()], [ssd_upload_shard()], [ssd_open_uploaded()]) resolve
#' them from the environment at call time (`SSDSIMS_AZURE_STORAGE_ACCOUNT` plus one of
#' `SSDSIMS_AZURE_STORAGE_KEY`, `SSDSIMS_AZURE_STORAGE_SAS`, or the service-principal trio
#' `SSDSIMS_AZURE_TENANT_ID`/`SSDSIMS_AZURE_CLIENT_ID`/`SSDSIMS_AZURE_CLIENT_SECRET`), and abort with a
#' loud error naming the missing variable when a required one is absent.
#'
#' @param url The Azure Blob Storage account endpoint, e.g.
#'   `"https://<account>.blob.core.windows.net"` (a non-empty string).
#' @param container The blob container name (a non-empty string).
#' @param ... Unused; must be empty. Its presence forces `prefix` to be passed
#'   **by name** (`rlang::check_dots_empty()` aborts on a positional or
#'   misspelled argument).
#' @param prefix An optional subdirectory (blob-name prefix) **within** the
#'   container under which the shards are written, e.g. `"study-2026/run-3"`, or
#'   `NULL` (default) to write at the container root. Leading/trailing slashes
#'   are trimmed. With a prefix the shards land at
#'   `<container>/<prefix>/<step>/<partition-path>/part.parquet` and
#'   [ssd_open_uploaded()] reads them back from the same prefixed glob, so one
#'   container can hold several independent result sets.
#' @return An S3 object of class `c("ssdsims_upload_azure_blob",
#'   "ssdsims_upload")` (for `ssd_upload_azure()`) or
#'   `c("ssdsims_upload_dryrun", "ssdsims_upload")` (for `ssd_upload_dryrun()`).
#' @seealso [ssd_test_upload()], [ssd_upload_shard()], [ssd_open_uploaded()],
#'   [ssd_scenario_targets()].
#' @export
#' @examples
#' ssd_upload_azure("https://acct.blob.core.windows.net", "ssdsims-results")
#' ssd_upload_azure(
#'   "https://acct.blob.core.windows.net",
#'   "ssdsims-results",
#'   prefix = "study-2026/run-3"
#' )
#' ssd_upload_dryrun()
ssd_upload_azure <- function(url, container, ..., prefix = NULL) {
  call <- environment()
  rlang::check_dots_empty()
  if (!chk::vld_string(url) || !nzchar(url)) {
    chk::abort_chk("`url` must be a non-empty string.", call = call)
  }
  if (!chk::vld_string(container) || !nzchar(container)) {
    chk::abort_chk("`container` must be a non-empty string.", call = call)
  }
  if (!is.null(prefix)) {
    if (!chk::vld_string(prefix) || !nzchar(prefix)) {
      chk::abort_chk(
        "`prefix` must be `NULL` or a non-empty string (a subdirectory ",
        "within the container).",
        call = call
      )
    }
    prefix <- normalise_blob_prefix(prefix)
    if (!nzchar(prefix)) {
      prefix <- NULL
    }
  }
  structure(
    list(url = url, container = container, prefix = prefix),
    class = c("ssdsims_upload_azure_blob", "ssdsims_upload")
  )
}

#' @rdname ssd_upload_azure
#' @export
ssd_upload_dryrun <- function() {
  structure(list(), class = c("ssdsims_upload_dryrun", "ssdsims_upload"))
}

# ---- generics --------------------------------------------------------------

#' Probe an Upload Destination's Credentials and Connectivity
#'
#' The front-door check, dispatched on the upload object's class, that confirms
#' **before any compute** whether the destination is reachable and the
#' credentials are in the right place (`TARGETS-DESIGN.md` section 6.1). Run it
#' as a one-liner at the prompt before `tar_make()`, or let
#' [ssd_scenario_targets()] run it once up front for you.
#'
#' For an Azure destination it resolves the credentials from the environment
#' and, when a required variable is **absent**, aborts with a loud error
#' **naming the missing variable** (rather than failing later on a worker); when
#' they are present it lists the container and writes then deletes a small
#' marker blob, returning invisibly on success and aborting with the backend's
#' diagnostic on failure. For a dry-run destination it succeeds trivially
#' without resolving credentials or reaching any network.
#'
#' @param upload An upload destination from [ssd_upload_azure()] or
#'   [ssd_upload_dryrun()].
#' @return `NULL`, invisibly (called for its side effect: the probe).
#' @seealso [ssd_upload_shard()], [ssd_open_uploaded()], [ssd_upload_azure()].
#' @export
#' @examples
#' ssd_test_upload(ssd_upload_dryrun())
ssd_test_upload <- function(upload) {
  UseMethod("ssd_test_upload")
}

#' Ship One Shard to an Upload Destination
#'
#' A generic, dispatched on the upload object's class, that ships one shard
#' Parquet to the destination and returns the **local** `path` (so the paired
#' `upload_<step>` target stays `format = "file"`). For an Azure destination it
#' uploads the file at `path` to
#' `<url>/<container>[/<prefix>]/<step>/<partition-path>/part.parquet` (the
#' optional `prefix` subdirectory from [ssd_upload_azure()]);
#' when the required credentials are **absent** it aborts with a loud error -
#' never a silent no-op - so intent to skip the network is only ever expressed
#' by passing [ssd_upload_dryrun()]. For a dry-run destination it performs no
#' network I/O, records a skip, and returns the local `path`.
#'
#' @section Adding a backend:
#' The destination set is open and extended by a **constructor-plus-methods**
#' contract - no edit to the existing methods. To add S3, GCS, or another
#' backend:
#'
#' 1. Write a constructor returning an object of class
#'    `c("ssdsims_upload_<backend>", "ssdsims_upload")` that validates its
#'    destination at construction (as [ssd_upload_azure()] validates its `url`
#'    and `container`) and carries no credentials.
#' 2. Implement the three generic methods for that class:
#'    `ssd_upload_shard()` (ship one shard, return the local path),
#'    [ssd_test_upload()] (the credentials/connectivity probe, failing loud on a
#'    missing credential), and [ssd_open_uploaded()] (read the uploaded results
#'    back in place).
#'
#' The package ships only the Azure and dry-run backends; no speculative
#' backends are added.
#'
#' @param path The local shard Parquet path (the `<step>_step` target's value).
#' @inheritParams ssd_test_upload
#' @return The local `path` (a string). For an Azure upload the returned value
#'   carries the cloud copy's sha256 as a `"cloud_sha256"` attribute, which the
#'   manifest can record alongside the local sha256 so a corrupted transfer
#'   shows up as a mismatch.
#' @seealso [ssd_test_upload()], [ssd_open_uploaded()], [ssd_scenario_targets()].
#' @export
#' @examples
#' path <- tempfile(fileext = ".parquet")
#' file.create(path)
#' ssd_upload_shard(path, ssd_upload_dryrun())
ssd_upload_shard <- function(path, upload) {
  UseMethod("ssd_upload_shard", upload)
}

#' Open Uploaded Results for Querying, In Place
#'
#' A generic, dispatched on the upload object's class, that opens the
#' **uploaded** results so a user can read them back and confirm they landed
#' right after an upload (`TARGETS-DESIGN.md` section 6.1). For an Azure
#' destination it returns a **lazy** `duckplyr`/DuckDB table over the Hive glob
#' `<container>[/<prefix>]/<step>/**/part.parquet` (honouring the destination's
#' optional `prefix` subdirectory), read **in place** via DuckDB's `azure`
#' extension (predicate pushdown straight against blob storage - **no
#' download**), composable with `dplyr` verbs so a one-line
#' `ssd_open_uploaded(upload, step) |> dplyr::count()` is the immediate
#' post-upload smoke test. It resolves the **same** front-end `SSDSIMS_AZURE_*`
#' credentials as the write path and remaps them into a DuckDB `azure` secret
#' for the backend read, aborting (naming the missing requirement) when the
#' `azure` extension or a required credential is absent. For a dry-run
#' destination it aborts: a dry run uploads nothing, so the local shards should
#' be read directly.
#'
#' @inheritParams ssd_test_upload
#' @param step One of `"sample"`, `"fit"`, `"hc"` (the step layer to read), or
#'   `"summary"` (the uploaded combined summary).
#' @return A lazy, `dplyr`-composable table over the uploaded results.
#' @seealso [ssd_upload_shard()], [ssd_test_upload()].
#' @export
#' @examples
#' \dontrun{
#' upload <- ssd_upload_azure("https://acct.blob.core.windows.net", "results")
#' ssd_open_uploaded(upload, "hc") |> dplyr::count()
#' }
ssd_open_uploaded <- function(upload, step) {
  UseMethod("ssd_open_uploaded")
}

# ---- default methods (unknown class) ---------------------------------------

#' @export
ssd_test_upload.default <- function(upload) {
  abort_unknown_upload(upload, call = rlang::caller_env())
}

#' @export
ssd_upload_shard.default <- function(path, upload) {
  abort_unknown_upload(upload, call = rlang::caller_env())
}

#' @export
ssd_open_uploaded.default <- function(upload, step) {
  abort_unknown_upload(upload, call = rlang::caller_env())
}

# ---- dry-run methods -------------------------------------------------------

#' @export
ssd_test_upload.ssdsims_upload_dryrun <- function(upload) {
  invisible(NULL)
}

#' @export
ssd_upload_shard.ssdsims_upload_dryrun <- function(path, upload) {
  chk::chk_string(path)
  rlang::inform(
    paste0("Dry-run upload: skipped ", encodeString(path, quote = "\""), "."),
    class = "ssdsims_upload_skip"
  )
  path
}

#' @export
ssd_open_uploaded.ssdsims_upload_dryrun <- function(upload, step) {
  chk::abort_chk(
    "A dry-run upload ships nothing, so there is nothing to read back. ",
    "Read the local shards directly (e.g. with `ssd_summarise()` or ",
    "`duckplyr::read_parquet_duckdb()` under the results root).",
    call = rlang::caller_env()
  )
}

# ---- Azure Blob methods ----------------------------------------------------

#' @export
ssd_test_upload.ssdsims_upload_azure_blob <- function(upload) {
  azure_check_installed()
  creds <- resolve_azure_credentials(call = rlang::caller_env())
  container <- azure_container_endpoint(upload, creds)
  AzureStor::list_blobs(container)
  marker <- azure_blob_dest(
    upload,
    paste0("ssdsims-marker-", as.integer(Sys.time()), ".txt")
  )
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp), add = TRUE)
  writeLines("ssdsims upload probe", tmp)
  AzureStor::upload_blob(container, src = tmp, dest = marker)
  AzureStor::delete_blob(container, marker, confirm = FALSE)
  invisible(NULL)
}

#' @export
ssd_upload_shard.ssdsims_upload_azure_blob <- function(path, upload) {
  chk::chk_string(path)
  azure_check_installed()
  creds <- resolve_azure_credentials(call = rlang::caller_env())
  container <- azure_container_endpoint(upload, creds)
  AzureStor::upload_blob(
    container,
    src = path,
    dest = azure_blob_dest(upload, upload_blob_key(path))
  )
  out <- path
  attr(out, "cloud_sha256") <- ssd_file_sha256(path)
  out
}

#' @export
ssd_open_uploaded.ssdsims_upload_azure_blob <- function(upload, step) {
  step <- rlang::arg_match0(step, c("sample", "fit", "hc", "summary"))
  rlang::check_installed("duckplyr")
  creds <- resolve_azure_credentials(call = rlang::caller_env())
  azure_load_duckdb_extension(creds, call = rlang::caller_env())
  duckplyr::read_parquet_duckdb(
    azure_glob(upload, step),
    options = list(hive_partitioning = FALSE)
  )
}

# ---- internals -------------------------------------------------------------

# Abort for an `upload` that is not an `ssdsims_upload` object (the default
# methods). Raised in the context of `call` (the user-facing generic call).
abort_unknown_upload <- function(upload, call = rlang::caller_env()) {
  chk::abort_chk(
    "`upload` must be an upload destination from `ssd_upload_azure()` or ",
    "`ssd_upload_dryrun()`, not an object of class ",
    encodeString(class(upload)[[1L]], quote = "\""),
    ".",
    call = call
  )
}

# Mockable wrapper over the Azure Suggests check, so tests can exercise the
# credential-resolution / round-trip path without `AzureStor`/`AzureRMR`
# installed (test-suite AGENTS: wrap, do not use `local_mocked_bindings()`'s
# `.package`).
azure_check_installed <- function() {
  rlang::check_installed(c("AzureStor", "AzureRMR"))
}

# Resolve the external Azure credentials from the environment, in precedence:
# account key, then SAS, then an AAD service principal - each alongside
# `SSDSIMS_AZURE_STORAGE_ACCOUNT`. Returns a `list(mode, account, ...)`; aborts in the
# context of `call` with a loud error naming the missing variable when no
# complete credential set is present, so the failure surfaces at the prompt (or
# the pipeline's up-front probe) rather than deep in the DAG on a worker.
resolve_azure_credentials <- function(call = rlang::caller_env()) {
  env <- function(name) {
    value <- Sys.getenv(name, unset = NA_character_)
    if (is.na(value) || !nzchar(value)) NULL else value
  }
  account <- env("SSDSIMS_AZURE_STORAGE_ACCOUNT")
  if (is.null(account)) {
    chk::abort_chk(
      "Azure credentials are incomplete: the environment variable ",
      "`SSDSIMS_AZURE_STORAGE_ACCOUNT` is not set. Set it (the storage account name) ",
      "together with one of `SSDSIMS_AZURE_STORAGE_KEY`, `SSDSIMS_AZURE_STORAGE_SAS`, or the ",
      "service-principal trio `SSDSIMS_AZURE_TENANT_ID`/`SSDSIMS_AZURE_CLIENT_ID`/",
      "`SSDSIMS_AZURE_CLIENT_SECRET`.",
      call = call
    )
  }
  key <- env("SSDSIMS_AZURE_STORAGE_KEY")
  if (!is.null(key)) {
    return(list(mode = "key", account = account, key = key))
  }
  sas <- env("SSDSIMS_AZURE_STORAGE_SAS")
  if (!is.null(sas)) {
    return(list(mode = "sas", account = account, sas = sas))
  }
  tenant <- env("SSDSIMS_AZURE_TENANT_ID")
  client <- env("SSDSIMS_AZURE_CLIENT_ID")
  secret <- env("SSDSIMS_AZURE_CLIENT_SECRET")
  if (!is.null(tenant) && !is.null(client) && !is.null(secret)) {
    return(list(
      mode = "service_principal",
      account = account,
      tenant = tenant,
      client = client,
      secret = secret
    ))
  }
  missing <- c(
    if (is.null(tenant)) "SSDSIMS_AZURE_TENANT_ID",
    if (is.null(client)) "SSDSIMS_AZURE_CLIENT_ID",
    if (is.null(secret)) "SSDSIMS_AZURE_CLIENT_SECRET"
  )
  chk::abort_chk(
    "Azure credentials are incomplete: `SSDSIMS_AZURE_STORAGE_ACCOUNT` is set, but no ",
    "authentication secret was found. Set `SSDSIMS_AZURE_STORAGE_KEY` (account-key ",
    "auth), or `SSDSIMS_AZURE_STORAGE_SAS` (SAS auth), or the service-principal trio ",
    "(missing: ",
    chk::cc(missing, conj = " and "),
    ").",
    call = call
  )
}

# Build an `AzureStor` container endpoint from the destination object and the
# resolved credentials. Pure over `AzureStor`/`AzureRMR`; the auth mode selects
# the endpoint's credential (account key, SAS, or an AAD service-principal
# token).
azure_container_endpoint <- function(upload, creds) {
  endpoint <- switch(
    creds$mode,
    key = AzureStor::blob_endpoint(upload$url, key = creds$key),
    sas = AzureStor::blob_endpoint(upload$url, sas = creds$sas),
    service_principal = AzureStor::blob_endpoint(
      upload$url,
      token = AzureRMR::get_azure_token(
        resource = "https://storage.azure.com/",
        tenant = creds$tenant,
        app = creds$client,
        password = creds$secret,
        use_cache = FALSE
      )
    )
  )
  AzureStor::storage_container(endpoint, upload$container)
}

# The shard's blob key within the container: the portion of its local Parquet
# path after the `layout=<hash>/` results-root segment, i.e.
# `<step>/<partition-path>/part.parquet`. This is the same Hive layout the local
# tree uses, so `ssd_open_uploaded()`'s `<step>/**/part.parquet` glob addresses
# the uploaded shards. A path without a `layout=` segment (an ad-hoc root) falls
# back to its full normalised form.
upload_blob_key <- function(path) {
  parts <- strsplit(path, "/", fixed = TRUE)[[1L]]
  layout <- grep("^layout=", parts)
  if (length(layout)) {
    start <- layout[[length(layout)]] + 1L
    parts <- parts[seq.int(start, length(parts))]
  }
  paste(parts, collapse = "/")
}

# Trim leading/trailing slashes from a container subdirectory prefix so it joins
# cleanly into a blob key (`"/a/b/"` -> `"a/b"`).
normalise_blob_prefix <- function(prefix) {
  gsub("^/+|/+$", "", prefix)
}

# Prepend the destination's optional subdirectory `prefix` to a blob `key`
# (within the container), so a prefixed destination writes/reads under
# `<prefix>/<key>` and an unprefixed one writes/reads at the container root.
azure_blob_dest <- function(upload, key) {
  if (is.null(upload$prefix)) key else paste0(upload$prefix, "/", key)
}

# The DuckDB `azure` glob for a step layer:
# `az://<container>[/<prefix>]/<step>/**/part.parquet` (or the single
# `[<prefix>/]summary.parquet` for the combined summary), addressing the uploaded
# shards in place via the `azure` extension.
azure_glob <- function(upload, step) {
  tail <- if (step == "summary") {
    "summary.parquet"
  } else {
    sprintf("%s/**/part.parquet", step)
  }
  sprintf("az://%s/%s", upload$container, azure_blob_dest(upload, tail))
}

# `CREATE SECRET` SQL that remaps the front-end Azure credentials into a DuckDB
# `azure` secret for the in-place read (one credential source, translated for
# the backend - never a second source).
azure_duckdb_secret_sql <- function(creds) {
  body <- switch(
    creds$mode,
    key = sprintf(
      "TYPE azure, CONNECTION_STRING 'AccountName=%s;AccountKey=%s'",
      creds$account,
      creds$key
    ),
    sas = sprintf(
      "TYPE azure, CONNECTION_STRING 'AccountName=%s;SharedAccessSignature=%s'",
      creds$account,
      creds$sas
    ),
    service_principal = sprintf(
      paste0(
        "TYPE azure, PROVIDER service_principal, TENANT_ID '%s', ",
        "CLIENT_ID '%s', CLIENT_SECRET '%s', ACCOUNT_NAME '%s'"
      ),
      creds$tenant,
      creds$client,
      creds$secret,
      creds$account
    )
  )
  sprintf("CREATE OR REPLACE SECRET ssdsims_azure (%s)", body)
}

# Load DuckDB's `azure` extension on `duckplyr`'s managed connection and create
# the `azure` secret from the resolved credentials. Aborts in the context of
# `call`, naming the `azure` extension, when it cannot be installed/loaded (e.g.
# offline workers without it pre-installed) - fail loud, never a partial table.
azure_load_duckdb_extension <- function(creds, call = rlang::caller_env()) {
  ok <- tryCatch(
    {
      duckplyr::db_exec("INSTALL azure")
      duckplyr::db_exec("LOAD azure")
      TRUE
    },
    error = function(e) e
  )
  if (!isTRUE(ok)) {
    chk::abort_chk(
      "Could not load DuckDB's `azure` extension (needed to read uploaded ",
      "shards in place): ",
      conditionMessage(ok),
      ". Install it on this machine (and on the cluster workers) with ",
      "`INSTALL azure;` on a DuckDB connection with network access.",
      call = call
    )
  }
  duckplyr::db_exec(azure_duckdb_secret_sql(creds))
  invisible(NULL)
}
