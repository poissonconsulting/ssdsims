# Record a Completed Shard's sha256 Alongside its Parquet

On a shard's successful write, records that shard's sha256 in a
per-shard sidecar (`<dir>/meta.json`) next to the shard's Parquet rather
than mutating a shared manifest, so parallel shard targets do not race
(`TARGETS-DESIGN.md` section 8.5). This captures the
**trusted-as-produced** sha256 - which post-hoc hashing of
possibly-touched files cannot.
[`ssd_assemble_manifest()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_assemble_manifest.md)
prefers a sidecar where present and falls back to hashing.

## Usage

``` r
ssd_record_shard(dir, partition_key, sha256)
```

## Arguments

- dir:

  The shard's directory - where the shard's Parquet (and its sidecar)
  live. Created if it does not exist.

- partition_key:

  The shard's partition path, e.g.
  `"fit/dataset=boron/sim=1/rescale=FALSE"`, recorded in the sidecar.

- sha256:

  The shard Parquet's sha256, as produced (e.g. from the shared
  `ssd_file_sha256()` internal).

## Value

The path to the written sidecar, invisibly.

## Details

The shard directory is **supplied by the caller** (composed from the
row's Hive partition path and the step's result root); the manifest
computes no Parquet paths itself.

## See also

[`ssd_assemble_manifest()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_assemble_manifest.md),
[`ssd_write_manifest()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_write_manifest.md).
