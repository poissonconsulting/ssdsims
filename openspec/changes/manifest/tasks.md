## 1. Dependencies and shared helpers

- [x] 1.1 Add `jsonlite`, `digest`, and `sessioninfo` to `DESCRIPTION` `Imports`; add the `ssd_file_sha256(path)` internal in a shared utils file
- [x] 1.2 Ensure the shared `ssd_file_sha256(path)` internal exists (`digest::digest(file = path, algo = "sha256")`)

## 2. Manifest head: writer and reader

- [x] 2.1 Add `ssd_write_manifest(scenario, dir)` in `R/manifest.R`: write `<dir>/manifest.json` with `seed`, `datasets`, `min_pmix`, `fit`, `hc`, `partition_by` and the complete session info captured at write time via `sessioninfo::session_info()` (serialised as a `session_info` block: `platform` + a `name -> version` package map), surfacing `r_version`/`dqrng_version`/`ssdtools_version` as a flat subset; fall back to `utils::sessionInfo()` if `sessioninfo` is unavailable (`jsonlite::write_json(auto_unbox = TRUE, pretty = TRUE)`)
- [x] 2.2 Add `ssd_read_manifest(dir)`: read with `simplifyVector = FALSE` and restore whole-number/logical knob types so the round-trip is lossless

## 3. Per-shard recording

- [x] 3.1 Add `ssd_record_shard(dir, partition_key, sha256)`: write a per-shard sidecar (`<shard-dir>/meta.json`) next to the shard's Parquet — one writer per file, no shared-manifest mutation. `manifest` records one trusted-as-produced sha256 per shard and nothing cloud-specific (`cloud-upload` ships the sidecar and verifies by re-hash; see design)

## 4. Assembly

- [x] 4.1 Add the assembler: scan the results tree for shard Parquets and build `completed_shards` (`partition path -> { sha256 }`) — use a shard's per-shard sidecar when present, else hash the Parquet directly (no dependency on the `task-tables` runner); merge into the manifest head
- [x] 4.2 A shard whose Parquet is absent SHALL be absent from `completed_shards`

## 5. Docs and reference

- [x] 5.1 Roxygen documenting the §8.5 field set, the complete-session-info reproducibility contract (§9), and the sidecar-plus-assembly design
- [x] 5.2 Add a `manifest` reference group to `_pkgdown.yml`

## 6. Tests and checks

- [x] 6.1 `tests/testthat/test-manifest.R`: write/read round-trip preserves declarative fields and the session-info block (incl. the `r`/`dqrng`/`ssdtools` subset), with `seed`/`nboot` whole numbers and logical knobs intact
- [x] 6.2 `ssd_record_shard()` writes a per-shard `meta.json` sidecar; concurrent records do not collide (distinct sidecars)
- [x] 6.3 Assembler builds `completed_shards` by hashing shards on disk; prefers a sidecar's recorded sha when present; a shard with no Parquet is absent
- [x] 6.4 Run `devtools::document()`, `air format .`, and `devtools::check()`; update `NAMESPACE`/`man/`
