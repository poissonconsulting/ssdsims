## Why

The reproducibility and completeness contracts in `TARGETS-DESIGN.md` §8.5/§9 rest on a per-scenario **manifest**: a small JSON sidecar recording the scenario's declarative fields, the pinned `r`/`dqrng`/`ssdtools` versions that guarantee bit-stability across re-runs (§9), and `completed_shards` — each completed shard's partition path plus the sha256 recorded at write time. That sha256 set is what lets the lightweight replay recipe verify a locally-regenerated upstream against the cluster's *actual* bytes before re-running a failing task (§7), and what `shard-completeness-assert` reads to record expected-vs-actual per shard (§6.2/§8.4). No manifest exists today, so none of those downstream guarantees can be built.

## What Changes

- Add `ssd_write_manifest(scenario, dir)` — writes `<dir>/manifest.json` with the §8.5 declarative field set (`seed`, `datasets`, `min_pmix`, `fit`, `hc`, `partition_by`) and the pinned `r_version` / `dqrng_version` / `ssdtools_version` captured at write time.
- Add `ssd_read_manifest(dir)` — reads the manifest back to an R list (round-trip).
- Add a per-shard recorder `ssd_record_shard(dir, partition_key, sha256, ...)` — on a shard's successful write, records that shard's sha256 (and its cloud-copy sha256 when `upload` is set, §6.1) **alongside the shard's Parquet** as a small sidecar, avoiding a write race between parallel shard targets.
- Add a manifest assembler that unions the per-shard sidecars into the manifest's `completed_shards` map (`partition path -> { sha256, cloud_sha256? }`).

## Capabilities

### New Capabilities
- `manifest`: a per-scenario JSON manifest (writer/reader) carrying the §8.5 declarative fields and version pins, plus per-shard sha256 recording and assembly into `completed_shards`.

### Modified Capabilities
<!-- None: this is a new, self-contained sidecar; no existing spec's requirements change. -->

## Impact

- **New code**: `R/manifest.R` (`ssd_write_manifest()`, `ssd_read_manifest()`, `ssd_record_shard()`, the assembler); reuse of the shared `ssd_file_sha256()` internal. Tests in `tests/testthat/test-manifest.R`.
- **APIs**: New exports for the writer/reader/recorder. Roxygen/`man/` and a `_pkgdown.yml` reference group.
- **Dependencies**: adds `jsonlite` (manifest JSON) and `digest` (file sha256) to `Imports`. (`digest` and the `ssd_file_sha256()` helper are shared with `registry`; whichever lands first introduces them.)
- **On-disk layout**: introduces `<results>/manifest.json` and per-shard sha256 sidecars next to each shard Parquet (§8.5).
- **Downstream**: unblocks `task-tables` (§12) — the per-shard step bodies call `ssd_record_shard()` on success and the pipeline assembles `completed_shards`. Feeds `shard-completeness-assert` (expected-vs-actual per shard, §6.2/§8.4) and `replay-helper` (verify upstream by sha256 before replay, §7). Independent of `registry` within the targets-plumbing subgraph; both feed `task-tables`.
