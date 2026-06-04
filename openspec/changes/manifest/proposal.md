## Why

The reproducibility and completeness contracts in `TARGETS-DESIGN.md` §8.5/§9 rest on a per-scenario **manifest**: a small JSON sidecar recording the scenario's declarative fields, the **complete session info** (R version, platform, and every attached/loaded package version) that guarantees bit-stability across re-runs (§9), and `completed_shards` — each completed shard's partition path plus the sha256 recorded at write time. That sha256 set is what lets the lightweight replay recipe verify a locally-regenerated upstream against the cluster's *actual* bytes before re-running a failing task (§7), and what `shard-completeness-assert` reads to record expected-vs-actual per shard (§6.2/§8.4). No manifest exists today, so none of those downstream guarantees can be built.

## What Changes

- Add `ssd_write_manifest(scenario, dir)` — writes `<dir>/manifest.json` with the §8.5 declarative field set (`seed`, `datasets`, `min_pmix`, `fit`, `hc`, `partition_by`) and the **complete session info** captured at write time via `sessioninfo::session_info()` (platform + all package versions), with `r`/`dqrng`/`ssdtools` called out as the bit-stability-critical subset (§9).
- Add `ssd_read_manifest(dir)` — reads the manifest back to an R list (round-trip).
- Add a per-shard recorder `ssd_record_shard(dir, partition_key, sha256, ...)` — records a shard's trusted-as-produced sha256 (and its cloud-copy sha256 when `upload` is set, §6.1) **alongside the shard's Parquet** as a small sidecar, one writer per file (no race). This is the *at-write-time* path wired in by the downstream consumers that need the trusted sha (`replay-helper` / `cloud-upload`), **not** the happy-path `task-tables` runner; the baseline assembler instead hashes the shards on disk. The shard directory is **supplied by the caller** (composed from the existing `path_key()` and the step's result root); `manifest` defines no Parquet paths of its own.
- Add a manifest assembler that unions the per-shard sidecars into the manifest's `completed_shards` map (`partition path -> { sha256, cloud_sha256? }`).

## Capabilities

### New Capabilities
- `manifest`: a per-scenario JSON manifest (writer/reader) carrying the §8.5 declarative fields and version pins, plus per-shard sha256 recording and assembly into `completed_shards`.

### Modified Capabilities
<!-- None: this is a new, self-contained sidecar; no existing spec's requirements change. -->

## Impact

- **New code**: `R/manifest.R` (`ssd_write_manifest()`, `ssd_read_manifest()`, `ssd_record_shard()`, the assembler); reuse of the shared `ssd_file_sha256()` internal. Tests in `tests/testthat/test-manifest.R`.
- **APIs**: New exports for the writer/reader/recorder. Roxygen/`man/` and a `_pkgdown.yml` reference group.
- **Dependencies**: adds `jsonlite` (manifest JSON), `digest` (file sha256), and `sessioninfo` (complete session info) to `Imports`. (`digest` and the `ssd_file_sha256()` helper are introduced by this change — they are no longer needed by `scenario-accessors`, which dropped dataset persistence.)
- **On-disk layout**: introduces `<results>/manifest.json` and per-shard sha256 sidecars next to each shard Parquet (§8.5).
- **Dependencies (direction)**: depends only on the *scenario* (`ssd-define-scenario`) for the head; it is **not** a prerequisite of `task-tables` and `task-tables` reads nothing from it (avoiding a dependency inversion — see design and `task-tables`). The `completed_shards` half is logically *downstream* of `task-tables` (the shards must exist to be hashed).
- **Downstream**: feeds `replay-helper` (verify upstream against `completed_shards` sha256 before replay, §7) and `shard-completeness-assert` (record expected-vs-actual per shard, §6.2/§8.4); `cloud-upload` records the cloud-copy sha256 (§6.1). TARGETS-DESIGN.md §12 DAG: `define → manifest`, `manifest → replay-helper`, `manifest → shard-completeness-assert`.
- **When to land it**: any time before the first consumer (`replay-helper` / `shard-completeness-assert`); *operationally*, before the first expensive cluster run whose results you intend to trust/reproduce/replay (session info and the trusted-as-produced sha cannot be reconstructed afterwards). Not needed for the local/toy pipeline.
