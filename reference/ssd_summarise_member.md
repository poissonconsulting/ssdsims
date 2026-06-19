# Summarise One Design Member from the Shared hc Shards

The per-scenario fan-in used by
[`ssd_design_targets()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_design_targets.md):
reads the (shared) hc shards under `dir_hc`, filters to the member's
**serving** hc task identities (`hc_ids`) - its own cells, plus the
coincident `ci = TRUE` cell serving each of its `ci = FALSE` cells under
[`ssd_design_targets()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_design_targets.md)'s
`ci` routing - projects out the `dists`/`samples` list-columns at the
DuckDB level, and writes the member's compact summary at `path`. When
the design aggregates differing readouts into a shared cell,
`proportion`/`est_method` narrow the maximal computed set back to the
member's requested readout rows. The read, filter, projection, and write
all happen inside DuckDB (never collecting into R), mirroring
[`ssd_summarise()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_summarise.md).

## Usage

``` r
ssd_summarise_member(
  dir_hc,
  hc_ids,
  path,
  proportion = NULL,
  est_method = NULL
)
```

## Arguments

- dir_hc:

  The (shared) `hc` results root of the member's seed group.

- hc_ids:

  The member's serving hc task identities (`hc_id`s) to keep.

- path:

  The output Parquet path for the member's compact summary (the
  `format = "file"` contract).

- proportion:

  Optional `proportion` values to keep (the member's requested
  proportions when the design aggregates a wider union into the shared
  cell); `NULL` (the default) keeps every proportion.

- est_method:

  Optional `est_method` values to keep (the member's requested methods
  when the design aggregates a wider union); `NULL` (the default) keeps
  every method.

## Value

`path`.

## See also

[`ssd_design_targets()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_design_targets.md),
[`ssd_summarise_design()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_summarise_design.md),
[`ssd_summarise()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_summarise.md).
