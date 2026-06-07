# Open Uploaded Results for Querying, In Place

A generic, dispatched on the upload object's class, that opens the
**uploaded** results so a user can read them back and confirm they
landed right after an upload (`TARGETS-DESIGN.md` section 6.1). For an
Azure destination it returns a **lazy** `duckplyr`/DuckDB table over the
Hive glob `<container>[/<prefix>]/<step>/**/part.parquet` (honouring the
destination's optional `prefix` subdirectory), read **in place** via
DuckDB's `azure` extension (predicate pushdown straight against blob
storage - **no download**), composable with `dplyr` verbs so a one-line
`ssd_open_uploaded(upload, step) |> dplyr::count()` is the immediate
post-upload smoke test. It resolves the **same** front-end
`SSDSIMS_AZURE_*` credentials as the write path and remaps them into a
DuckDB `azure` secret for the backend read, aborting (naming the missing
requirement) when the `azure` extension or a required credential is
absent. For a dry-run destination it aborts: a dry run uploads nothing,
so the local shards should be read directly.

## Usage

``` r
ssd_open_uploaded(upload, step)
```

## Arguments

- upload:

  An upload destination from
  [`ssd_upload_azure()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_azure.md)
  or
  [`ssd_upload_dryrun()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_azure.md).

- step:

  One of `"sample"`, `"fit"`, `"hc"` (the step layer to read), or
  `"summary"` (the uploaded combined summary).

## Value

A lazy, `dplyr`-composable table over the uploaded results.

## See also

[`ssd_upload_shard()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_shard.md),
[`ssd_test_upload()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_test_upload.md).

## Examples

``` r
if (FALSE) { # \dontrun{
upload <- ssd_upload_azure("https://acct.blob.core.windows.net", "results")
ssd_open_uploaded(upload, "hc") |> dplyr::count()
} # }
```
