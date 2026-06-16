# Ship One Shard to an Upload Destination

A generic, dispatched on the upload object's class, that ships one shard
Parquet to the destination and returns the **local** `path` (so the
paired `upload_<step>` target stays `format = "file"`). For an Azure
destination it uploads the file at `path` to
`<url>/<container>[/<prefix>]/<step>/<partition-path>/part.parquet` (the
optional `prefix` subdirectory from
[`ssd_upload_azure()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_azure.md));
when the required credentials are **absent** it aborts with a loud
error - never a silent no-op - so intent to skip the network is only
ever expressed by passing
[`ssd_upload_dryrun()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_azure.md).
For a dry-run destination it performs no network I/O, records a skip,
and returns the local `path`.

## Usage

``` r
ssd_upload_shard(path, upload)
```

## Arguments

- path:

  The local shard Parquet path (the `<step>_step` target's value).

- upload:

  An upload destination from
  [`ssd_upload_azure()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_azure.md)
  or
  [`ssd_upload_dryrun()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_azure.md).

## Value

The local `path` (a string), so the `upload_<step>` target stays
`format = "file"`.

## Adding a backend

The destination set is open and extended by a
**constructor-plus-methods** contract - no edit to the existing methods.
To add S3, GCS, or another backend:

1.  Write a constructor returning an object of class
    `c("ssdsims_upload_<backend>", "ssdsims_upload")` that validates its
    destination at construction (as
    [`ssd_upload_azure()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_azure.md)
    validates its `url` and `container`) and carries no credentials.

2.  Implement the generic methods for that class: `ssd_upload_shard()`
    (ship one shard, return the local path),
    [`ssd_test_upload()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_test_upload.md)
    (the credentials/connectivity probe, failing loud on a missing
    credential),
    [`ssd_open_uploaded()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_open_uploaded.md)
    (read the uploaded results back in place), and
    [`ssd_summarise_uploaded()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_summarise_uploaded.md)
    (the in-place fan-in summary).

The package ships only the Azure and dry-run backends; no speculative
backends are added.

## See also

[`ssd_test_upload()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_test_upload.md),
[`ssd_open_uploaded()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_open_uploaded.md),
[`ssd_scenario_targets()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_targets.md).

## Examples

``` r
path <- tempfile(fileext = ".parquet")
file.create(path)
#> [1] TRUE
ssd_upload_shard(path, ssd_upload_dryrun())
#> Dry-run upload: skipped "/tmp/RtmpAk4aun/file3c582041fac4.parquet".
#> [1] "/tmp/RtmpAk4aun/file3c582041fac4.parquet"
```
