# Summarise One Design Member from the Shared hc Shards

The per-scenario fan-in used by
[`ssd_design_targets()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_design_targets.md):
reads the (shared) hc shards under `dir_hc`, filters to the member's hc
task identities (`hc_ids`), projects out the `dists`/`samples`
list-columns at the DuckDB level, and writes the member's compact
summary at `path`. Filtering by `hc_id` selects exactly the member's
rows from the shards it shares with other members of the design. The
read, filter, projection, and write all happen inside DuckDB (never
collecting into R), mirroring
[`ssd_summarise()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_summarise.md).

## Usage

``` r
ssd_summarise_member(dir_hc, hc_ids, path)
```

## Arguments

- dir_hc:

  The (shared) `hc` results root of the member's seed group.

- hc_ids:

  The member's hc task identities (`hc_id`s) to keep.

- path:

  The output Parquet path for the member's compact summary (the
  `format = "file"` contract).

## Value

`path`.

## See also

[`ssd_design_targets()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_design_targets.md),
[`ssd_summarise_design()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_summarise_design.md),
[`ssd_summarise()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_summarise.md).
