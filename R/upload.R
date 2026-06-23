# Typed, self-validating upload destinations and the class-dispatched generics
# that ship, probe, and read back shards (TARGETS-DESIGN.md section 6.1). An
# upload object is a plain, serialisable value carrying only the destination
# (never credentials), so it rides on the `upload` argument to `crew` workers
# and through `targets` unchanged. Four (plus construction) generics dispatch
# on its class: `ssd_test_upload()` (the front-door creds/connectivity probe),
# `ssd_upload_shard()` (ship one shard), `ssd_open_uploaded()` (read the
# uploaded results back in place), and `ssd_summarise_uploaded()` (the in-place
# fan-in summary, the cloud `ssd_summarise()`). Azure with absent credentials is
# a hard error, never a silent no-op; the only way to skip the network is to
# pass `ssd_upload_dryrun()`.

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
#' ([ssd_test_upload()], [ssd_upload_shard()], [ssd_open_uploaded()],
#' [ssd_summarise_uploaded()]) resolve the **secret** from the environment at
#' call time - one of `SSDSIMS_AZURE_STORAGE_KEY`, `SSDSIMS_AZURE_STORAGE_SAS`,
#' or the service-principal trio
#' `SSDSIMS_AZURE_TENANT_ID`/`SSDSIMS_AZURE_CLIENT_ID`/`SSDSIMS_AZURE_CLIENT_SECRET`
#' - and abort with a loud error naming the missing variable when none is
#' present. The storage **account name** is derived from `url` (see `domain`),
#' so it is **not** an environment variable.
#'
#' @param url The Azure Blob Storage account endpoint, e.g.
#'   `"https://<account>.blob.core.windows.net"` (a non-empty string). The
#'   storage **account name** is derived from this endpoint's leading host label
#'   (so it need not be repeated in the environment); see `domain`.
#' @param container The blob container name (a non-empty string).
#' @param ... Unused; must be empty. Its presence forces `prefix`/`domain` to be
#'   passed **by name** (`rlang::check_dots_empty()` aborts on a positional or
#'   misspelled argument).
#' @param prefix An optional subdirectory (blob-name prefix) **within** the
#'   container under which the shards are written, e.g. `"study-2026/run-3"`, or
#'   `NULL` (default) to write at the container root. Leading/trailing slashes
#'   are trimmed. With a prefix the shards land at
#'   `<container>/<prefix>/<step>/<partition-path>/part.parquet` and
#'   [ssd_open_uploaded()] reads them back from the same prefixed glob, so one
#'   container can hold several independent result sets.
#' @param domain The storage endpoint domain suffix (default
#'   `"blob.core.windows.net"`). The storage account name is the part of `url`'s
#'   host *before* `.<domain>` — so `https://acct.blob.core.windows.net` yields
#'   account `"acct"`. Override it for a sovereign/non-public cloud (e.g.
#'   `"blob.core.usgovcloudapi.net"`). `url` must end with `.<domain>` or
#'   construction aborts. The derived account is what the read-back path
#'   ([ssd_open_uploaded()], [ssd_summarise_uploaded()]) hands to DuckDB's
#'   `azure` secret, so **no** account environment variable is needed.
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
ssd_upload_azure <- function(
  url,
  container,
  ...,
  prefix = NULL,
  domain = "blob.core.windows.net"
) {
  call <- environment()
  rlang::check_dots_empty()
  if (!chk::vld_string(url) || !nzchar(url)) {
    chk::abort_chk("`url` must be a non-empty string.", call = call)
  }
  if (!chk::vld_string(container) || !nzchar(container)) {
    chk::abort_chk("`container` must be a non-empty string.", call = call)
  }
  if (!chk::vld_string(domain) || !nzchar(domain)) {
    chk::abort_chk("`domain` must be a non-empty string.", call = call)
  }
  account <- azure_account_from_url(url, domain, call = call)
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
    list(
      url = url,
      container = container,
      prefix = prefix,
      domain = domain,
      account = account
    ),
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
#' as a one-liner at the prompt before `tar_make()` - it is the user's explicit
#' preflight. [ssd_scenario_targets()] deliberately does **not** call it (the
#' factory does no network I/O, so sourcing `_targets.R` stays side-effect
#' free); a missing credential still fails loud per-shard at
#' [ssd_upload_shard()] time as a backstop.
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

#' Ship Shard (or Summary) Parquet Files to an Upload Destination
#'
#' A generic, dispatched on the upload object's class, that ships the local
#' Parquet file(s) at `path` to the destination and returns the **local**
#' `path` unchanged (so the paired upload target stays `format = "file"`).
#' The per-shard `upload_<step>` targets pass one path; the `upload_summary`
#' target passes the summary Parquet path(s) - `summary.parquet` plus, when the
#' scenario retains the bootstrap draws, `summary-samples.parquet`. For an
#' Azure destination it resolves the credentials **once per call** and uploads
#' each file to `<url>/<container>[/<prefix>]/<key>`, where the key is the
#' file's path below the layout-keyed results root (a shard's
#' `<step>/<partition-path>/part.parquet`, the summary's `summary.parquet` /
#' `summary-samples.parquet`; the optional `prefix` subdirectory comes from
#' [ssd_upload_azure()]); when the required credentials are **absent** it
#' aborts with a loud error - never a silent no-op - so intent to skip the
#' network is only ever expressed by passing [ssd_upload_dryrun()]. For a
#' dry-run destination it performs no network I/O, records a skip per file,
#' and returns the local `path`.
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
#' 2. Implement the generic methods for that class:
#'    `ssd_upload_shard()` (ship one shard, return the local path),
#'    [ssd_test_upload()] (the credentials/connectivity probe, failing loud on a
#'    missing credential), [ssd_open_uploaded()] (read the uploaded results
#'    back in place), and [ssd_summarise_uploaded()] (the in-place fan-in
#'    summary).
#'
#' The package ships only the Azure and dry-run backends; no speculative
#' backends are added.
#'
#' @param path The local Parquet path(s) - a character vector of one or more
#'   files (a `<step>_step` target's single shard path, or the `summary`
#'   target's path(s)).
#' @inheritParams ssd_test_upload
#' @return The local `path` (a character vector), unchanged, so the paired
#'   upload target stays `format = "file"`.
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
#' optional `prefix` subdirectory) - or, for the combined summaries, the single
#' blob `summary.parquet` (`step = "summary"`) / `summary-samples.parquet`
#' (`step = "summary_samples"`, shipped only when the scenario set
#' `samples = TRUE`) - read **in place** via DuckDB's `azure`
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
#' @param step One of `"sample"`, `"fit"`, `"hc"` (the step layer to read),
#'   `"summary"` (the uploaded compact summary), or `"summary_samples"` (the
#'   uploaded full summary retaining the `dists`/`samples` list-columns,
#'   shipped only when the scenario set `samples = TRUE`).
#' @param ... Unused; must be empty. Its presence forces `prudence` to be
#'   passed **by name** (`rlang::check_dots_empty()` aborts on a positional or
#'   named extra arg).
#' @param prudence The duckplyr prudence of the returned table (default
#'   `"stingy"`): `"stingy"` keeps it lazy and composable but makes an implicit
#'   materialisation (e.g. `nrow()`/`$`) against the remote glob error rather
#'   than triggering an unbounded download/scan; `"lavish"` restores automatic
#'   materialisation on first access. `dplyr::collect()` and
#'   `duckplyr::compute_parquet()` work under either.
#' @return A lazy, `dplyr`-composable table over the uploaded results.
#' @seealso [ssd_upload_shard()], [ssd_test_upload()].
#' @export
#' @examples
#' \dontrun{
#' upload <- ssd_upload_azure("https://acct.blob.core.windows.net", "results")
#' ssd_open_uploaded(upload, "hc") |> dplyr::count()
#' }
ssd_open_uploaded <- function(upload, step, ..., prudence = "stingy") {
  UseMethod("ssd_open_uploaded")
}

#' Summarise Uploaded Results, In Place (the cloud `ssd_summarise()`)
#'
#' The cloud counterpart of [ssd_summarise()]: a generic, dispatched on the
#' upload object's class, that fans a step's **uploaded** shards into a single
#' **lazy** `duckplyr` table read **in place** (no download). For an Azure
#' destination it reads the `<container>[/<prefix>]/<step>/**/part.parquet` Hive
#' glob - or, for the combined summaries, the single blob `summary.parquet`
#' (`step = "summary"`) / `summary-samples.parquet` (`step = "summary_samples"`,
#' shipped only when the scenario set `samples = TRUE`) - via DuckDB's `azure`
#' extension - resolving the **same** front-end
#' secret as the write path and remapping it (with the account derived from
#' `url`) into a DuckDB `azure` secret - and returns the union as a lazy
#' `duckplyr` tibble (not collected, so the read and projection stay in DuckDB).
#' By default it projects away the heavy `dists`/`samples` list-columns (the
#' analysis-ready summary, mirroring [ssd_summarise()]); pass
#' `drop_samples = FALSE` to keep them when the in-flight bootstrap `samples`
#' are needed. Because the uploaded compact summary physically lacks those
#' columns, `step = "summary"` with `drop_samples = FALSE` aborts pointing at
#' `step = "summary_samples"` rather than silently returning a sample-less
#' table. The default method
#' (an unknown destination) and the dry-run method both abort.
#'
#' @inheritParams ssd_open_uploaded
#' @param ... Unused; must be empty. Its presence forces `drop_samples` and
#'   `prudence` to be passed **by name** (`rlang::check_dots_empty()` aborts on
#'   a positional or named extra arg).
#' @param drop_samples Flag (default `TRUE`): project away the heavy
#'   `dists`/`samples` list-columns for the analysis-ready summary. Pass `FALSE`
#'   to keep them (e.g. when the in-flight bootstrap `samples` are needed).
#' @return A **lazy** `duckplyr`/DuckDB tibble over the unioned, uploaded `step`
#'   layer (not collected), composable with `dplyr` verbs - `dplyr::collect()`
#'   it (or write it with `duckplyr::compute_parquet()`) when you need the rows
#'   in R.
#' @seealso [ssd_open_uploaded()], [ssd_summarise()], [ssd_upload_shard()].
#' @export
#' @examples
#' \dontrun{
#' upload <- ssd_upload_azure("https://acct.blob.core.windows.net", "results")
#' ssd_summarise_uploaded(upload, "hc")
#' ssd_summarise_uploaded(upload, "hc", drop_samples = FALSE) # keep samples
#' }
ssd_summarise_uploaded <- function(
  upload,
  step = "hc",
  ...,
  drop_samples = TRUE,
  prudence = "stingy"
) {
  UseMethod("ssd_summarise_uploaded")
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
ssd_open_uploaded.default <- function(upload, step, ..., prudence = "stingy") {
  abort_unknown_upload(upload, call = rlang::caller_env())
}

#' @export
ssd_summarise_uploaded.default <- function(
  upload,
  step = "hc",
  ...,
  drop_samples = TRUE,
  prudence = "stingy"
) {
  abort_unknown_upload(upload, call = rlang::caller_env())
}

# ---- dry-run methods -------------------------------------------------------

#' @export
ssd_test_upload.ssdsims_upload_dryrun <- function(upload) {
  invisible(NULL)
}

#' @export
ssd_upload_shard.ssdsims_upload_dryrun <- function(path, upload) {
  chk::chk_character(path)
  chk::chk_not_any_na(path)
  chk::chk_not_empty(path)
  for (p in path) {
    rlang::inform(
      paste0("Dry-run upload: skipped ", encodeString(p, quote = "\""), "."),
      class = "ssdsims_upload_skip"
    )
  }
  path
}

#' @export
ssd_open_uploaded.ssdsims_upload_dryrun <- function(
  upload,
  step,
  ...,
  prudence = "stingy"
) {
  chk::abort_chk(
    "A dry-run upload ships nothing, so there is nothing to read back. ",
    "Read the local shards directly (e.g. with `ssd_summarise()` or ",
    "`duckplyr::read_parquet_duckdb()` under the results root).",
    call = rlang::caller_env()
  )
}

#' @export
ssd_summarise_uploaded.ssdsims_upload_dryrun <- function(
  upload,
  step = "hc",
  ...,
  drop_samples = TRUE,
  prudence = "stingy"
) {
  chk::abort_chk(
    "A dry-run upload ships nothing, so there is nothing to summarise. ",
    "Summarise the local shards directly with `ssd_summarise()`.",
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
  chk::chk_character(path)
  chk::chk_not_any_na(path)
  chk::chk_not_empty(path)
  azure_check_installed()
  creds <- resolve_azure_credentials(call = rlang::caller_env())
  container <- azure_container_endpoint(upload, creds)
  # A plain loop (not purrr) keeps this frame out of an upload error's header
  # (error-call-origin); credentials and the endpoint are resolved once per
  # call, not per file.
  for (p in path) {
    AzureStor::upload_blob(
      container,
      src = p,
      dest = azure_blob_dest(upload, upload_blob_key(p))
    )
  }
  path
}

#' @export
ssd_open_uploaded.ssdsims_upload_azure_blob <- function(
  upload,
  step,
  ...,
  prudence = "stingy"
) {
  rlang::check_dots_empty()
  step <- rlang::arg_match0(
    step,
    c("sample", "fit", "hc", "summary", "summary_samples")
  )
  rlang::check_installed("duckplyr")
  creds <- resolve_azure_credentials(call = rlang::caller_env())
  azure_load_duckdb_extension(creds, upload$account, call = rlang::caller_env())
  duckplyr::read_parquet_duckdb(
    azure_glob(upload, step),
    options = list(hive_partitioning = FALSE),
    prudence = prudence
  )
}

#' @export
ssd_summarise_uploaded.ssdsims_upload_azure_blob <- function(
  upload,
  step = "hc",
  ...,
  drop_samples = TRUE,
  prudence = "stingy"
) {
  rlang::check_dots_empty()
  step <- rlang::arg_match0(
    step,
    c("sample", "fit", "hc", "summary", "summary_samples")
  )
  chk::chk_flag(drop_samples)
  # The uploaded compact summary physically lacks the `dists`/`samples`
  # list-columns, so honouring `drop_samples = FALSE` against it is impossible
  # - abort (before any credential/extension work) rather than silently
  # returning a sample-less table.
  if (step == "summary" && !drop_samples) {
    chk::abort_chk(
      "The uploaded compact summary (`step = \"summary\"`) does not carry the ",
      "`dists`/`samples` list-columns, so `drop_samples = FALSE` cannot be ",
      "honoured. Read the uploaded full summary with ",
      "`step = \"summary_samples\"` instead (shipped only when the scenario ",
      "set `samples = TRUE`).",
      call = rlang::caller_env()
    )
  }
  rlang::check_installed("duckplyr")
  # Fetch the SAME credentials used for uploading and remap them into a DuckDB
  # `azure` secret (shared with `ssd_upload_shard()`/`ssd_open_uploaded()` - one
  # credential source, translated for the backend, never a second source). The
  # account name comes from the destination's `url`, not the environment.
  creds <- resolve_azure_credentials(call = rlang::caller_env())
  azure_load_duckdb_extension(creds, upload$account, call = rlang::caller_env())

  # Read the step's Hive glob via the `azure` extension (predicate pushdown, no
  # download) and optionally project away the heavy `dists`/`samples`
  # list-columns (the analysis-ready summary, mirroring `ssd_summarise()`). The
  # result stays a **lazy duckplyr tibble** (not collected), so it composes with
  # `dplyr` verbs and the read/projection run inside DuckDB.
  tbl <- duckplyr::read_parquet_duckdb(
    azure_glob(upload, step),
    options = list(hive_partitioning = FALSE),
    prudence = prudence
  )
  if (drop_samples) {
    tbl <- dplyr::select(tbl, -dplyr::any_of(c("dists", "samples")))
  }
  tbl
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

# Derive the storage account name from the endpoint `url`'s leading host label
# (the part before `.<domain>`), e.g. `https://acct.blob.core.windows.net` ->
# `"acct"`. The account is intrinsic to the endpoint, so it need not be repeated
# in the environment. Aborts in the context of `call` when `url` does not end
# with `.<domain>` (so the account would be ambiguous) - pass a matching
# `domain` for a sovereign/non-public cloud.
azure_account_from_url <- function(url, domain, call = rlang::caller_env()) {
  host <- sub("^[a-z]+://", "", url, ignore.case = TRUE)
  host <- sub("[:/?].*$", "", host)
  suffix <- paste0(".", domain)
  if (!endsWith(host, suffix) || nchar(host) <= nchar(suffix)) {
    chk::abort_chk(
      "`url` (",
      encodeString(url, quote = "\""),
      ") must be an Azure Blob endpoint of the form ",
      "`https://<account>.",
      domain,
      "` so the storage account name can be derived; pass a matching `domain` ",
      "for a non-public cloud.",
      call = call
    )
  }
  substr(host, 1L, nchar(host) - nchar(suffix))
}

# Resolve the external Azure **secret** from the environment, in precedence:
# account key, then SAS, then an AAD service principal. Returns a
# `list(mode, ...)` carrying only the secret material - the storage account name
# is derived from the destination `url` (see `azure_account_from_url()`), not the
# environment. Aborts in the context of `call` with a loud error naming the
# missing variable when no complete secret is present, so the failure surfaces at
# the prompt (the user's `ssd_test_upload()` preflight) or, as a backstop, on the
# shard's upload branch - rather than silently.
resolve_azure_credentials <- function(call = rlang::caller_env()) {
  env <- function(name) {
    value <- Sys.getenv(name, unset = NA_character_)
    if (is.na(value) || !nzchar(value)) NULL else value
  }
  key <- env("SSDSIMS_AZURE_STORAGE_KEY")
  if (!is.null(key)) {
    return(list(mode = "key", key = key))
  }
  sas <- env("SSDSIMS_AZURE_STORAGE_SAS")
  if (!is.null(sas)) {
    return(list(mode = "sas", sas = sas))
  }
  tenant <- env("SSDSIMS_AZURE_TENANT_ID")
  client <- env("SSDSIMS_AZURE_CLIENT_ID")
  secret <- env("SSDSIMS_AZURE_CLIENT_SECRET")
  if (!is.null(tenant) && !is.null(client) && !is.null(secret)) {
    return(list(
      mode = "service_principal",
      tenant = tenant,
      client = client,
      secret = secret
    ))
  }
  missing_sp <- c(
    if (is.null(tenant)) "SSDSIMS_AZURE_TENANT_ID",
    if (is.null(client)) "SSDSIMS_AZURE_CLIENT_ID",
    if (is.null(secret)) "SSDSIMS_AZURE_CLIENT_SECRET"
  )
  chk::abort_chk(
    "Azure credentials are incomplete: no authentication secret was found. ",
    "Set `SSDSIMS_AZURE_STORAGE_KEY` (account-key auth), or ",
    "`SSDSIMS_AZURE_STORAGE_SAS` (SAS auth), or the service-principal trio ",
    "`SSDSIMS_AZURE_TENANT_ID`/`SSDSIMS_AZURE_CLIENT_ID`/`SSDSIMS_AZURE_CLIENT_SECRET` ",
    "(missing: ",
    chk::cc(missing_sp, conj = " and "),
    "). The storage account name is taken from the destination `url`, not the ",
    "environment.",
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
# `[<prefix>/]summary.parquet` / `[<prefix>/]summary-samples.parquet` for the
# combined summaries), addressing the uploaded shards in place via the `azure`
# extension.
azure_glob <- function(upload, step) {
  tail <- switch(
    step,
    summary = "summary.parquet",
    summary_samples = "summary-samples.parquet",
    sprintf("%s/**/part.parquet", step)
  )
  sprintf("az://%s/%s", upload$container, azure_blob_dest(upload, tail))
}

# `CREATE SECRET` SQL that remaps the front-end Azure secret into a DuckDB
# `azure` secret for the in-place read (one credential source, translated for
# the backend - never a second source). `account` is the storage account name
# derived from the destination `url`.
azure_duckdb_secret_sql <- function(creds, account) {
  body <- switch(
    creds$mode,
    key = sprintf(
      "TYPE azure, CONNECTION_STRING 'AccountName=%s;AccountKey=%s'",
      account,
      creds$key
    ),
    sas = sprintf(
      "TYPE azure, CONNECTION_STRING 'AccountName=%s;SharedAccessSignature=%s'",
      account,
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
      account
    )
  )
  sprintf("CREATE OR REPLACE SECRET ssdsims_azure (%s)", body)
}

# Load DuckDB's `azure` extension on `duckplyr`'s managed connection and create
# the `azure` secret from the resolved credentials and the `account` (derived
# from the destination `url`). Aborts in the context of `call`, naming the
# `azure` extension, when it cannot be installed/loaded (e.g. offline workers
# without it pre-installed) - fail loud, never a partial table.
azure_load_duckdb_extension <- function(
  creds,
  account,
  call = rlang::caller_env()
) {
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
  duckplyr::db_exec(azure_duckdb_secret_sql(creds, account))
  invisible(NULL)
}
