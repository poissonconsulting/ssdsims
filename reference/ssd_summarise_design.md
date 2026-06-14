# Combine Per-scenario Summaries into One Design Summary

The design pipeline's fan-in: unions the named per-scenario compact
summary Parquets into one combined summary, tagging each row with a
`scenario` identity column equal to the member's name within the design.
The union is performed at the DuckDB level (each file read lazily via
`duckplyr`, tagged, and `union_all`-ed straight back out), so no
per-scenario summary is collected into R. Per-scenario files that did
not land are skipped, so the combined summary unions the surviving
members (the keep-going property of
[`ssd_summarise()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_summarise.md)).

## Usage

``` r
ssd_summarise_design(summaries, path)
```

## Arguments

- summaries:

  A **named** character vector of per-scenario compact summary Parquet
  paths; the names are the scenario names within the design.

- path:

  The output Parquet path for the combined design summary (the
  `format = "file"` contract).

## Value

`path` (the `format = "file"` contract).

## See also

[`ssd_design_targets()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_design_targets.md),
[`ssd_summarise()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_summarise.md).
