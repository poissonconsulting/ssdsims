# Assemble `completed_shards` from the Shards on Disk

Builds the manifest's `completed_shards` map - shard partition path to
`{ sha256 }` - from the shard Parquets present under `dir`, so the
manifest reflects the set of shards whose Parquet exists
(`TARGETS-DESIGN.md` section 8.5). A shard's per-shard sidecar
([`ssd_record_shard()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_record_shard.md))
is preferred where present (carrying the trusted-as-produced sha256);
otherwise the shard's Parquet is hashed directly. Assembly therefore
needs **no** hook into the pipeline runner - the shards are the truth. A
shard whose Parquet is absent is absent from `completed_shards`.

## Usage

``` r
ssd_assemble_manifest(dir)
```

## Arguments

- dir:

  The results directory to write `manifest.json` into (created if it
  does not exist).

## Value

The merged manifest (head plus `completed_shards`) as an R list,
invisibly.

## Details

The assembled tail is merged into the manifest head at
`<dir>/manifest.json` (written by
[`ssd_write_manifest()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_write_manifest.md))
and written back, so
[`ssd_read_manifest()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_read_manifest.md)
returns head and tail together.

## See also

[`ssd_record_shard()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_record_shard.md),
[`ssd_write_manifest()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_write_manifest.md).

## Examples

``` r
scenario <- ssd_define_scenario(ssddata::ccme_boron, nsim = 1L, seed = 42L)
dir <- withr::local_tempdir()
ssd_write_manifest(scenario, dir)
ssd_assemble_manifest(dir)$completed_shards
#> list()
```
