## 1. Dependencies and shared helpers

- [ ] 1.1 Add `jsonlite` and `digest` to `DESCRIPTION` `Imports` (`digest` / `ssd_file_sha256()` are shared with `registry`; introduce in a shared utils file if not already present)
- [ ] 1.2 Ensure the shared `ssd_file_sha256(path)` internal exists (`digest::digest(file = path, algo = "sha256")`)

## 2. Manifest head: writer and reader

- [ ] 2.1 Add `ssd_write_manifest(scenario, dir)` in `R/manifest.R`: write `<dir>/manifest.json` with `seed`, `datasets`, `min_pmix`, `fit`, `hc`, `partition_by` and the `r_version`/`dqrng_version`/`ssdtools_version` pins captured at write time (`jsonlite::write_json(auto_unbox = TRUE, pretty = TRUE)`)
- [ ] 2.2 Add `ssd_read_manifest(dir)`: read with `simplifyVector = FALSE` and restore whole-number/logical knob types so the round-trip is lossless

## 3. Per-shard recording

- [ ] 3.1 Add `ssd_record_shard(dir, partition_key, sha256, cloud_sha256 = NULL)`: write a per-shard sidecar (e.g. `<shard-dir>/.sha256.json`) next to the shard's Parquet — one writer per file, no shared-manifest mutation
- [ ] 3.2 Record `cloud_sha256` alongside the local sha256 when supplied (§6.1)

## 4. Assembly

- [ ] 4.1 Add the assembler: scan the results tree for per-shard sidecars and union them into `completed_shards` (`partition path -> { sha256, cloud_sha256? }`); merge into the manifest head
- [ ] 4.2 Shards without a sidecar SHALL be absent from `completed_shards`

## 5. Docs and reference

- [ ] 5.1 Roxygen documenting the §8.5 field set, the version-pin reproducibility contract (§9), and the sidecar-plus-assembly design
- [ ] 5.2 Add a `manifest` reference group to `_pkgdown.yml`

## 6. Tests and checks

- [ ] 6.1 `tests/testthat/test-manifest.R`: write/read round-trip preserves declarative fields and version pins, with `seed`/`nboot` whole numbers and logical knobs intact
- [ ] 6.2 `ssd_record_shard()` writes a per-shard sidecar; cloud sha256 recorded when supplied; concurrent records do not collide (distinct sidecars)
- [ ] 6.3 Assembler unions sidecars into `completed_shards`; an unrecorded shard is absent
- [ ] 6.4 Run `devtools::document()`, `air format .`, and `devtools::check()`; update `NAMESPACE`/`man/`
