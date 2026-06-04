## ADDED Requirements

### Requirement: Write a per-scenario manifest
The package SHALL provide `ssd_write_manifest(scenario, dir)` that writes a JSON manifest (`<dir>/manifest.json`) recording the scenario's declarative fields — `seed`, `datasets`, `min_pmix`, `fit`, `hc`, and `partition_by` — together with the complete session info captured at write time (R version, platform, and all attached/loaded package versions, e.g. via `sessioninfo::session_info()`), with `r_version`, `dqrng_version`, and `ssdtools_version` also surfaced as a flat bit-stability-critical subset (`TARGETS-DESIGN.md` §8.5/§9). The manifest SHALL be a small JSON sidecar, not a Parquet file.

#### Scenario: Manifest records the scenario's declarative fields
- **WHEN** `ssd_write_manifest(scenario, dir)` is called
- **THEN** `<dir>/manifest.json` SHALL exist and SHALL contain the scenario's `seed`, `datasets`, `min_pmix`, `fit`, `hc`, and `partition_by` values

#### Scenario: Manifest records complete session info
- **WHEN** a manifest is written
- **THEN** it SHALL record the complete session info present at write time (R version, platform, and all attached/loaded package versions), and SHALL surface `r_version`, `dqrng_version`, and `ssdtools_version` as a flat convenience subset

### Requirement: Read a manifest back
The package SHALL provide `ssd_read_manifest(dir)` that reads `<dir>/manifest.json` into an R list, round-tripping the declarative fields and session info written by `ssd_write_manifest()` without lossy coercion of integer or logical knobs.

#### Scenario: Round-trip preserves manifest contents
- **WHEN** a manifest is written for a scenario and then read back
- **THEN** the read fields SHALL equal the written values, with `seed`/`nboot` preserved as whole numbers and logical knobs preserved as logicals

### Requirement: Record a completed shard's sha256 alongside its Parquet
The package SHALL provide `ssd_record_shard(dir, partition_key, sha256, ...)` that, on a shard's successful write, records that shard's sha256 in a per-shard sidecar next to the shard's Parquet rather than mutating a shared manifest, so parallel shard targets do not race (`TARGETS-DESIGN.md` §8.5). The shard directory SHALL be supplied by the caller; the manifest SHALL NOT compute Parquet paths itself. When a cloud-copy sha256 is supplied (`upload` set, §6.1) it SHALL be recorded alongside the local sha256.

#### Scenario: A successful shard records its sha256 in a sidecar
- **WHEN** `ssd_record_shard(dir, "fit/dataset=boron/sim=1/rescale=FALSE", sha)` is called after the shard's Parquet is written
- **THEN** a per-shard sidecar SHALL be written next to that shard's Parquet recording `sha` for the shard's partition path

#### Scenario: Cloud sha256 is recorded when supplied
- **WHEN** `ssd_record_shard()` is called with a cloud-copy sha256
- **THEN** the sidecar SHALL record both the local and cloud sha256 for the shard

#### Scenario: Concurrent shard records do not collide
- **WHEN** several shards record their sha256 concurrently
- **THEN** each SHALL write its own sidecar and no record SHALL overwrite another

### Requirement: Assemble completed_shards from the shards on disk
The package SHALL provide a manifest assembler that builds the manifest's `completed_shards` map — partition path to `{ sha256, cloud_sha256? }` — from the shards present under a results tree, so the manifest reflects the set of shards whose Parquet exists (`TARGETS-DESIGN.md` §8.5). The assembler SHALL use a shard's per-shard sidecar when present (carrying the trusted-as-produced sha256, and `cloud_sha256` when uploaded) and SHALL otherwise hash the shard's Parquet directly, so assembly does NOT require the pipeline runner to have recorded anything.

#### Scenario: Assembly hashes shards that have no sidecar
- **WHEN** shard Parquets exist under the results tree with no per-shard sidecars and the assembler is run
- **THEN** the manifest's `completed_shards` SHALL contain one entry per shard Parquet, keyed by partition path, each carrying the sha256 of that Parquet

#### Scenario: Assembly prefers a shard's recorded sidecar
- **WHEN** a shard has a per-shard sidecar (e.g. recorded at write time, with a `cloud_sha256`) and the assembler is run
- **THEN** that shard's `completed_shards` entry SHALL carry the sidecar's recorded sha256 (and `cloud_sha256` when present) rather than a re-hash

#### Scenario: A shard whose Parquet is absent is absent from completed_shards
- **WHEN** a shard's Parquet was not written and the assembler runs
- **THEN** that shard's partition path SHALL NOT appear in `completed_shards`
