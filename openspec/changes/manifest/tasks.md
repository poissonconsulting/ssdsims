## 1. Dependencies and shared helpers

- [ ] 1.1 Add `jsonlite`, `digest`, and `sessioninfo` to `DESCRIPTION` `Imports` (`digest` / `ssd_file_sha256()` are shared with `registry`; introduce in a shared utils file if not already present)
- [ ] 1.2 Ensure the shared `ssd_file_sha256(path)` internal exists (`digest::digest(file = path, algo = "sha256")`)

## 2. Manifest head: writer and reader

- [ ] 2.1 Add `ssd_write_manifest(scenario, dir)` in `R/manifest.R`: write `<dir>/manifest.json` with `seed`, `datasets`, `min_pmix`, `fit`, `hc`, `partition_by` and the complete session info captured at write time via `sessioninfo::session_info()` (serialised as a `session_info` block: `platform` + a `name -> version` package map), surfacing `r_version`/`dqrng_version`/`ssdtools_version` as a flat subset; fall back to `utils::sessionInfo()` if `sessioninfo` is unavailable (`jsonlite::write_json(auto_unbox = TRUE, pretty = TRUE)`)
- [ ] 2.2 Add `ssd_read_manifest(dir)`: read with `simplifyVector = FALSE` and restore whole-number/logical knob types so the round-trip is lossless

## 3. Per-shard recording

- [ ] 3.1 Add `ssd_record_shard(dir, partition_key, sha256, cloud_sha256 = NULL)`: write a per-shard sidecar (e.g. `<shard-dir>/.sha256.json`) next to the shard's Parquet — one writer per file, no shared-manifest mutation
- [ ] 3.2 Record `cloud_sha256` alongside the local sha256 when supplied (§6.1)

## 4. Assembly

- [ ] 4.1 Add the assembler: scan the results tree for shard Parquets and build `completed_shards` (`partition path -> { sha256, cloud_sha256? }`) — use a shard's per-shard sidecar when present, else hash the Parquet directly (no dependency on the `task-tables` runner); merge into the manifest head
- [ ] 4.2 A shard whose Parquet is absent SHALL be absent from `completed_shards`

## 5. Docs and reference

- [ ] 5.1 Roxygen documenting the §8.5 field set, the complete-session-info reproducibility contract (§9), and the sidecar-plus-assembly design
- [ ] 5.2 Add a `manifest` reference group to `_pkgdown.yml`

## 6. Tests and checks

- [ ] 6.1 `tests/testthat/test-manifest.R`: write/read round-trip preserves declarative fields and the session-info block (incl. the `r`/`dqrng`/`ssdtools` subset), with `seed`/`nboot` whole numbers and logical knobs intact
- [ ] 6.2 `ssd_record_shard()` writes a per-shard sidecar; cloud sha256 recorded when supplied; concurrent records do not collide (distinct sidecars)
- [ ] 6.3 Assembler builds `completed_shards` by hashing shards on disk; prefers a sidecar's recorded sha (and `cloud_sha256`) when present; a shard with no Parquet is absent
- [ ] 6.4 Run `devtools::document()`, `air format .`, and `devtools::check()`; update `NAMESPACE`/`man/`
