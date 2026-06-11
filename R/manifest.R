# Per-scenario manifest (TARGETS-DESIGN.md section 8.5/section 9). A small JSON
# sidecar to the results directory recording the scenario's declarative fields,
# the complete session info captured at write time (the bit-stability contract,
# section 9), and `completed_shards` - each completed shard's partition path and
# the sha256 of its Parquet, recorded at write time or hashed on disk.
#
# The manifest splits into a stable *head* (scenario fields + session info,
# written once at pipeline init) and an accreting *tail* (`completed_shards`,
# assembled from the shards on disk after a run). The recorder writes one
# sidecar per shard - one writer per file, never mutating a shared manifest - so
# parallel shard targets do not race; the assembler unions those sidecars (and
# hashes shards without one) into the head. `manifest` owns the document format,
# the reader, and the assembler; it computes no Parquet paths of its own (the
# shard directory is supplied by the caller, composed from `path_key()` and the
# step's result root).

#' Write a Per-Scenario Manifest
#'
#' Writes `<dir>/manifest.json`, a small JSON sidecar recording the scenario's
#' declarative fields - `seed`, `datasets`, `min_pmix`, `fit`, `hc`, and
#' `partition_by` - together with the **complete session info** captured at
#' write time (R version, platform, and every attached/loaded package version,
#' via [sessioninfo::session_info()]), serialised as a `session_info` block (a
#' `platform` list plus a `name -> version` package map). The three
#' bit-stability-critical versions (`r_version`, `dqrng_version`,
#' `ssdtools_version`) are also surfaced as a flat convenience subset for the
#' reproducibility contract (`TARGETS-DESIGN.md` section 8.5/section 9). This is
#' the manifest's stable *head*; [ssd_assemble_manifest()] adds the accreting
#' `completed_shards` tail after a run.
#'
#' The session info is **descriptive**: the manifest records the toolchain a
#' result set was produced under, so a re-run's drift is diagnosable rather than
#' guessable. It does not *enforce* a version on read - a mismatch is a signal
#' for the replay/verify layer, not an abort here. Falls back to
#' [utils::sessionInfo()] when `sessioninfo` is unavailable.
#'
#' @inheritParams scenario_dataset
#' @param dir The results directory to write `manifest.json` into (created if it
#'   does not exist).
#' @return The path to the written `manifest.json`, invisibly.
#' @seealso [ssd_read_manifest()], [ssd_record_shard()],
#'   [ssd_assemble_manifest()].
#' @keywords internal
ssd_write_manifest <- function(scenario, dir) {
  chk::chk_s3_class(scenario, "ssdsims_scenario")
  chk::chk_string(dir)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, "manifest.json")
  jsonlite::write_json(
    manifest_head(scenario),
    path,
    auto_unbox = TRUE,
    pretty = TRUE
  )
  invisible(path)
}

#' Read a Per-Scenario Manifest
#'
#' Reads `<dir>/manifest.json` back into an R list, round-tripping the
#' declarative fields and session-info block written by [ssd_write_manifest()]
#' (and the `completed_shards` tail added by [ssd_assemble_manifest()]) without
#' lossy coercion: `seed`/`nboot` are restored as whole numbers and logical
#' scenario options (`rescale`, `parametric`, ...) as logicals.
#'
#' @inheritParams ssd_write_manifest
#' @return The manifest as an R list, with the scalar/vector option types of the
#'   written scenario restored.
#' @seealso [ssd_write_manifest()].
#' @keywords internal
ssd_read_manifest <- function(dir) {
  chk::chk_string(dir)
  path <- file.path(dir, "manifest.json")
  if (!file.exists(path)) {
    chk::abort_chk(
      "No manifest found at ",
      encodeString(path, quote = "\""),
      "."
    )
  }
  manifest <- restore_atomic(jsonlite::read_json(path, simplifyVector = FALSE))
  manifest$seed <- as.integer(manifest$seed)
  if (!is.null(manifest$hc$nboot)) {
    manifest$hc$nboot <- as.integer(manifest$hc$nboot)
  }
  manifest
}

#' Record a Completed Shard's sha256 Alongside its Parquet
#'
#' On a shard's successful write, records that shard's sha256 in a per-shard
#' sidecar (`<dir>/meta.json`) next to the shard's Parquet rather than mutating a
#' shared manifest, so parallel shard targets do not race (`TARGETS-DESIGN.md`
#' section 8.5). This captures the **trusted-as-produced** sha256 - which
#' post-hoc hashing of possibly-touched files cannot. [ssd_assemble_manifest()]
#' prefers a sidecar where present and falls back to hashing.
#'
#' The shard directory is **supplied by the caller** (composed from the row's
#' Hive partition path and the step's result root); the manifest computes no
#' Parquet paths itself.
#'
#' @param dir The shard's directory - where the shard's Parquet (and its
#'   sidecar) live. Created if it does not exist.
#' @param partition_key The shard's partition path, e.g.
#'   `"fit/dataset=boron/sim=1/rescale=FALSE"`, recorded in the sidecar.
#' @param sha256 The shard Parquet's sha256, as produced (e.g. from the shared
#'   `ssd_file_sha256()` internal).
#' @return The path to the written sidecar, invisibly.
#' @seealso [ssd_assemble_manifest()], [ssd_write_manifest()].
#' @keywords internal
ssd_record_shard <- function(dir, partition_key, sha256) {
  chk::chk_string(dir)
  chk::chk_string(partition_key)
  chk::chk_string(sha256)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  record <- list(partition_key = partition_key, sha256 = sha256)
  path <- file.path(dir, "meta.json")
  jsonlite::write_json(record, path, auto_unbox = TRUE, pretty = TRUE)
  invisible(path)
}

#' Assemble `completed_shards` from the Shards on Disk
#'
#' Builds the manifest's `completed_shards` map - shard partition path to
#' `{ sha256 }` - from the shard Parquets present under `dir`, so the manifest
#' reflects the set of shards whose Parquet exists (`TARGETS-DESIGN.md` section
#' 8.5). A shard's per-shard sidecar ([ssd_record_shard()]) is preferred where
#' present (carrying the trusted-as-produced sha256); otherwise the shard's
#' Parquet is hashed directly. Assembly therefore needs **no** hook into the
#' pipeline runner - the shards are the truth. A shard whose Parquet is absent
#' is absent from `completed_shards`.
#'
#' The assembled tail is merged into the manifest head at `<dir>/manifest.json`
#' (written by [ssd_write_manifest()]) and written back, so [ssd_read_manifest()]
#' returns head and tail together.
#'
#' @inheritParams ssd_read_manifest
#' @return The merged manifest (head plus `completed_shards`) as an R list,
#'   invisibly.
#' @seealso [ssd_record_shard()], [ssd_write_manifest()].
#' @keywords internal
ssd_assemble_manifest <- function(dir) {
  chk::chk_string(dir)
  completed_shards <- assemble_completed_shards(dir)

  path <- file.path(dir, "manifest.json")
  manifest <- if (file.exists(path)) {
    jsonlite::read_json(path, simplifyVector = FALSE)
  } else {
    list()
  }
  manifest$completed_shards <- completed_shards
  jsonlite::write_json(manifest, path, auto_unbox = TRUE, pretty = TRUE)
  invisible(restore_atomic(manifest))
}

# ---- internals -------------------------------------------------------------

# The manifest's stable head: the scenario's declarative fields, the flat
# bit-stability-critical version subset (section 9), and the complete session
# info captured at write time.
manifest_head <- function(scenario) {
  list(
    seed = scenario$seed,
    datasets = scenario$datasets,
    min_pmix = scenario$fit$min_pmix,
    fit = scenario$fit,
    hc = scenario$hc,
    partition_by = scenario$partition_by,
    r_version = as.character(getRversion()),
    dqrng_version = as.character(utils::packageVersion("dqrng")),
    ssdtools_version = as.character(utils::packageVersion("ssdtools")),
    session_info = manifest_session_info()
  )
}

# Complete session info as a serialisable block: a `platform` list plus a
# `name -> version` package map. Uses `sessioninfo::session_info()` (the
# poissonconsulting house tool); falls back to `utils::sessionInfo()` when
# `sessioninfo` is unavailable so a manifest can always record the toolchain.
manifest_session_info <- function() {
  if (rlang::is_installed("sessioninfo")) {
    info <- sessioninfo::session_info(info = c("platform", "packages"))
    platform <- as.list(unclass(info$platform))
    packages <- as.data.frame(info$packages)
    versions <- packages$loadedversion
    missing <- is.na(versions)
    versions[missing] <- packages$ondiskversion[missing]
    return(list(
      platform = platform,
      packages = as.list(rlang::set_names(
        as.character(versions),
        packages$package
      ))
    ))
  }
  info <- utils::sessionInfo()
  loaded <- c(info$otherPkgs, info$loadedOnly)
  list(
    platform = list(
      version = info$R.version$version.string,
      os = info$running,
      system = info$platform
    ),
    packages = purrr::map(loaded, \(pkg) pkg$Version)
  )
}

# Walk the results tree under `dir`, building `completed_shards` keyed by each
# shard Parquet's partition path (its directory relative to `dir`). A shard's
# sidecar is preferred where present; otherwise the Parquet is hashed. Keys come
# straight from `list.files(full.names = FALSE)` - always forward-slash relative
# paths - so the key is platform-stable (on Windows `dir` may carry backslashes
# while `list.files()` returns forward slashes, so stripping `dir` as a string
# prefix would not match and would leak the absolute path into the key).
assemble_completed_shards <- function(dir) {
  rel_parquets <- list.files(
    dir,
    pattern = "^part\\.parquet$",
    recursive = TRUE,
    full.names = FALSE
  )
  completed_shards <- list()
  for (rel_parquet in rel_parquets) {
    key <- dirname(rel_parquet)
    shard_dir <- file.path(dir, key)
    parquet <- file.path(dir, rel_parquet)
    completed_shards[[key]] <- shard_entry(shard_dir, parquet)
  }
  completed_shards
}

# One `completed_shards` entry: a sidecar's recorded sha when present, else the
# Parquet's sha256 hashed on disk.
shard_entry <- function(shard_dir, parquet) {
  sidecar <- file.path(shard_dir, "meta.json")
  if (file.exists(sidecar)) {
    record <- jsonlite::read_json(sidecar, simplifyVector = FALSE)
    return(list(sha256 = record$sha256))
  }
  list(sha256 = ssd_file_sha256(parquet))
}

# Restore atomic vectors flattened to JSON arrays (read back as unnamed lists by
# `simplifyVector = FALSE`) so the round-trip is lossless. Recurse into every
# list; collapse a non-empty, unnamed list of length-1 atomics back to an atomic
# vector. Named lists (session info, `completed_shards`, the per-shard entries)
# and lists of longer vectors (e.g. `range_shape1`) are preserved as-is.
restore_atomic <- function(x) {
  if (!is.list(x)) {
    return(x)
  }
  x <- purrr::map(x, restore_atomic)
  collapsible <- length(x) > 0L &&
    is.null(names(x)) &&
    all(purrr::map_lgl(x, \(e) is.atomic(e) && length(e) == 1L))
  if (collapsible) {
    return(unlist(x, use.names = FALSE))
  }
  x
}
