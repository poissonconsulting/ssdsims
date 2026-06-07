# Write a Per-Scenario Manifest

Writes `<dir>/manifest.json`, a small JSON sidecar recording the
scenario's declarative fields - `seed`, `datasets`, `min_pmix`, `fit`,
`hc`, and `partition_by` - together with the **complete session info**
captured at write time (R version, platform, and every attached/loaded
package version, via
[`sessioninfo::session_info()`](https://sessioninfo.r-lib.org/reference/session_info.html)),
serialised as a `session_info` block (a `platform` list plus a
`name -> version` package map). The three bit-stability-critical
versions (`r_version`, `dqrng_version`, `ssdtools_version`) are also
surfaced as a flat convenience subset for the reproducibility contract
(`TARGETS-DESIGN.md` section 8.5/section 9). This is the manifest's
stable *head*;
[`ssd_assemble_manifest()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_assemble_manifest.md)
adds the accreting `completed_shards` tail after a run.

## Usage

``` r
ssd_write_manifest(scenario, dir)
```

## Arguments

- scenario:

  An `ssdsims_scenario` from
  [`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md).

- dir:

  The results directory to write `manifest.json` into (created if it
  does not exist).

## Value

The path to the written `manifest.json`, invisibly.

## Details

The session info is **descriptive**: the manifest records the toolchain
a result set was produced under, so a re-run's drift is diagnosable
rather than guessable. It does not *enforce* a version on read - a
mismatch is a signal for the replay/verify layer, not an abort here.
Falls back to
[`utils::sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html) when
`sessioninfo` is unavailable.

## See also

[`ssd_read_manifest()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_read_manifest.md),
[`ssd_record_shard()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_record_shard.md),
[`ssd_assemble_manifest()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_assemble_manifest.md).

## Examples

``` r
scenario <- ssd_define_scenario(ssddata::ccme_boron, nsim = 1L, seed = 42L)
dir <- withr::local_tempdir()
ssd_write_manifest(scenario, dir)
ssd_read_manifest(dir)$seed
#> [1] 42
```
