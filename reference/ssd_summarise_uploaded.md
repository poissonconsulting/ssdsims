# Summarise Uploaded Results, In Place (the cloud `ssd_summarise()`)

The cloud counterpart of
[`ssd_summarise()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_summarise.md):
a generic, dispatched on the upload object's class, that fans a step's
**uploaded** shards into a single **lazy** `duckplyr` table read **in
place** (no download). For an Azure destination it reads the
`<container>[/<prefix>]/<step>/**/part.parquet` Hive glob - or, for the
combined summaries, the single blob `summary.parquet`
(`step = "summary"`) / `summary-samples.parquet`
(`step = "summary_samples"`, shipped only when the scenario set
`samples = TRUE`) - via DuckDB's `azure` extension - resolving the
**same** front-end secret as the write path and remapping it (with the
account derived from `url`) into a DuckDB `azure` secret - and returns
the union as a lazy `duckplyr` tibble (not collected, so the read and
projection stay in DuckDB). By default it projects away the heavy
`dists`/`samples` list-columns (the analysis-ready summary, mirroring
[`ssd_summarise()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_summarise.md));
pass `drop_samples = FALSE` to keep them when the in-flight bootstrap
`samples` are needed. Because the uploaded compact summary physically
lacks those columns, `step = "summary"` with `drop_samples = FALSE`
aborts pointing at `step = "summary_samples"` rather than silently
returning a sample-less table. The default method (an unknown
destination) and the dry-run method both abort.

## Usage

``` r
ssd_summarise_uploaded(upload, step = "hc", drop_samples = TRUE)
```

## Arguments

- upload:

  An upload destination from
  [`ssd_upload_azure()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_azure.md)
  or
  [`ssd_upload_dryrun()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_azure.md).

- step:

  One of `"sample"`, `"fit"`, `"hc"` (the step layer to read),
  `"summary"` (the uploaded compact summary), or `"summary_samples"`
  (the uploaded full summary retaining the `dists`/`samples`
  list-columns, shipped only when the scenario set `samples = TRUE`).

- drop_samples:

  Flag (default `TRUE`): project away the heavy `dists`/`samples`
  list-columns for the analysis-ready summary. Pass `FALSE` to keep them
  (e.g. when the in-flight bootstrap `samples` are needed).

## Value

A **lazy** `duckplyr`/DuckDB tibble over the unioned, uploaded `step`
layer (not collected), composable with `dplyr` verbs -
[`dplyr::collect()`](https://dplyr.tidyverse.org/reference/compute.html)
it (or write it with
[`duckplyr::compute_parquet()`](https://duckplyr.tidyverse.org/reference/compute_parquet.html))
when you need the rows in R.

## See also

[`ssd_open_uploaded()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_open_uploaded.md),
[`ssd_summarise()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_summarise.md),
[`ssd_upload_shard()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_shard.md).

## Examples

``` r
if (FALSE) { # \dontrun{
upload <- ssd_upload_azure("https://acct.blob.core.windows.net", "results")
ssd_summarise_uploaded(upload, "hc")
ssd_summarise_uploaded(upload, "hc", drop_samples = FALSE) # keep samples
} # }
```
