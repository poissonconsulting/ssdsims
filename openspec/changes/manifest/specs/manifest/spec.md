## ADDED Requirements

### Requirement: Write a per-scenario manifest
The package SHALL provide `ssd_write_manifest(scenario, dir)` that writes a JSON manifest (`<dir>/manifest.json`) recording the scenario's declarative fields — `seed`, `datasets`, `min_pmix`, `fit`, `hc`, and `partition_by` — together with pinned `r_version`, `dqrng_version`, and `ssdtools_version` captured at write time (`TARGETS-DESIGN.md` §8.5/§9). The manifest SHALL be a small JSON sidecar, not a Parquet file.

#### Scenario: Manifest records the scenario's declarative fields
- **WHEN** `ssd_write_manifest(scenario, dir)` is called
- **THEN** `<dir>/manifest.json` SHALL exist and SHALL contain the scenario's `seed`, `datasets`, `min_pmix`, `fit`, `hc`, and `partition_by` values

#### Scenario: Manifest pins the toolchain versions
- **WHEN** a manifest is written
- **THEN** it SHALL record `r_version`, `dqrng_version`, and `ssdtools_version` as the versions present at write time

### Requirement: Read a manifest back
The package SHALL provide `ssd_read_manifest(dir)` that reads `<dir>/manifest.json` into an R list, round-tripping the declarative fields and version pins written by `ssd_write_manifest()` without lossy coercion of integer or logical knobs.

#### Scenario: Round-trip preserves manifest contents
- **WHEN** a manifest is written for a scenario and then read back
- **THEN** the read fields SHALL equal the written values, with `seed`/`nboot` preserved as whole numbers and logical knobs preserved as logicals

### Requirement: Record a completed shard's sha256 alongside its Parquet
The package SHALL provide `ssd_record_shard(dir, partition_key, sha256, ...)` that, on a shard's successful write, records that shard's sha256 in a per-shard sidecar next to the shard's Parquet rather than mutating a shared manifest, so parallel shard targets do not race (`TARGETS-DESIGN.md` §8.5). When a cloud-copy sha256 is supplied (`upload` set, §6.1) it SHALL be recorded alongside the local sha256.

#### Scenario: A successful shard records its sha256 in a sidecar
- **WHEN** `ssd_record_shard(dir, "fit/dataset=boron/sim=1/rescale=FALSE", sha)` is called after the shard's Parquet is written
- **THEN** a per-shard sidecar SHALL be written next to that shard's Parquet recording `sha` for the shard's partition path

#### Scenario: Cloud sha256 is recorded when supplied
- **WHEN** `ssd_record_shard()` is called with a cloud-copy sha256
- **THEN** the sidecar SHALL record both the local and cloud sha256 for the shard

#### Scenario: Concurrent shard records do not collide
- **WHEN** several shards record their sha256 concurrently
- **THEN** each SHALL write its own sidecar and no record SHALL overwrite another

### Requirement: Assemble completed_shards into the manifest
The package SHALL provide a manifest assembler that unions the per-shard sidecars under a results tree into the manifest's `completed_shards` map — partition path to `{ sha256, cloud_sha256? }` — so the manifest reflects the set of shards whose Parquet exists and is trusted (`TARGETS-DESIGN.md` §8.5).

#### Scenario: Assembly unions the per-shard records
- **WHEN** several shards have recorded their sidecars and the assembler is run against the results tree
- **THEN** the manifest's `completed_shards` SHALL contain one entry per recorded shard, keyed by partition path, each carrying that shard's recorded sha256 (and `cloud_sha256` when present)

#### Scenario: A shard with no record is absent from completed_shards
- **WHEN** a shard's Parquet was not written (no sidecar) and the assembler runs
- **THEN** that shard's partition path SHALL NOT appear in `completed_shards`
