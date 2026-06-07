# Upload Destinations for a Scenario's Shards

Typed, self-validating destination objects for the targets pipeline's
per-shard upload (`TARGETS-DESIGN.md` section 6.1). Pass one to
[`ssd_scenario_targets()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_targets.md)'s
`upload` argument (the remote-destination sibling of `root`) to pair
each step shard with an `upload_<step>` target.

## Usage

``` r
ssd_upload_azure(
  url,
  container,
  ...,
  prefix = NULL,
  domain = "blob.core.windows.net"
)

ssd_upload_dryrun()
```

## Arguments

- url:

  The Azure Blob Storage account endpoint, e.g.
  `"https://<account>.blob.core.windows.net"` (a non-empty string). The
  storage **account name** is derived from this endpoint's leading host
  label (so it need not be repeated in the environment); see `domain`.

- container:

  The blob container name (a non-empty string).

- ...:

  Unused; must be empty. Its presence forces `prefix`/`domain` to be
  passed **by name**
  ([`rlang::check_dots_empty()`](https://rlang.r-lib.org/reference/check_dots_empty.html)
  aborts on a positional or misspelled argument).

- prefix:

  An optional subdirectory (blob-name prefix) **within** the container
  under which the shards are written, e.g. `"study-2026/run-3"`, or
  `NULL` (default) to write at the container root. Leading/trailing
  slashes are trimmed. With a prefix the shards land at
  `<container>/<prefix>/<step>/<partition-path>/part.parquet` and
  [`ssd_open_uploaded()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_open_uploaded.md)
  reads them back from the same prefixed glob, so one container can hold
  several independent result sets.

- domain:

  The storage endpoint domain suffix (default
  `"blob.core.windows.net"`). The storage account name is the part of
  `url`'s host *before* `.<domain>` — so
  `https://acct.blob.core.windows.net` yields account `"acct"`. Override
  it for a sovereign/non-public cloud (e.g.
  `"blob.core.usgovcloudapi.net"`). `url` must end with `.<domain>` or
  construction aborts. The derived account is what the read-back path
  ([`ssd_open_uploaded()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_open_uploaded.md),
  [`ssd_summarise_uploaded()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_summarise_uploaded.md))
  hands to DuckDB's `azure` secret, so **no** account environment
  variable is needed.

## Value

An S3 object of class `c("ssdsims_upload_azure_blob", "ssdsims_upload")`
(for `ssd_upload_azure()`) or
`c("ssdsims_upload_dryrun", "ssdsims_upload")` (for
`ssd_upload_dryrun()`).

## Details

`ssd_upload_azure()` describes an Azure Blob Storage container;
`ssd_upload_dryrun()` is a no-op destination that reaches no network, so
the upload DAG shape can be exercised offline and in CI without
credentials. Both return a plain, serialisable S3 object of class
`c("ssdsims_upload_<backend>", "ssdsims_upload")` that carries **only
the destination** - never credentials, open connections, or
environments - so it travels unchanged to `crew` workers and through
`targets`.

Credentials stay **external** to the object: the Azure methods
([`ssd_test_upload()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_test_upload.md),
[`ssd_upload_shard()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_shard.md),
[`ssd_open_uploaded()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_open_uploaded.md),
[`ssd_summarise_uploaded()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_summarise_uploaded.md))
resolve the **secret** from the environment at call time - one of
`SSDSIMS_AZURE_STORAGE_KEY`, `SSDSIMS_AZURE_STORAGE_SAS`, or the
service-principal trio
`SSDSIMS_AZURE_TENANT_ID`/`SSDSIMS_AZURE_CLIENT_ID`/`SSDSIMS_AZURE_CLIENT_SECRET`

- and abort with a loud error naming the missing variable when none is
  present. The storage **account name** is derived from `url` (see
  `domain`), so it is **not** an environment variable.

## See also

[`ssd_test_upload()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_test_upload.md),
[`ssd_upload_shard()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_shard.md),
[`ssd_open_uploaded()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_open_uploaded.md),
[`ssd_scenario_targets()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_targets.md).

## Examples

``` r
ssd_upload_azure("https://acct.blob.core.windows.net", "ssdsims-results")
#> $url
#> [1] "https://acct.blob.core.windows.net"
#> 
#> $container
#> [1] "ssdsims-results"
#> 
#> $prefix
#> NULL
#> 
#> $domain
#> [1] "blob.core.windows.net"
#> 
#> $account
#> [1] "acct"
#> 
#> attr(,"class")
#> [1] "ssdsims_upload_azure_blob" "ssdsims_upload"           
ssd_upload_azure(
  "https://acct.blob.core.windows.net",
  "ssdsims-results",
  prefix = "study-2026/run-3"
)
#> $url
#> [1] "https://acct.blob.core.windows.net"
#> 
#> $container
#> [1] "ssdsims-results"
#> 
#> $prefix
#> [1] "study-2026/run-3"
#> 
#> $domain
#> [1] "blob.core.windows.net"
#> 
#> $account
#> [1] "acct"
#> 
#> attr(,"class")
#> [1] "ssdsims_upload_azure_blob" "ssdsims_upload"           
ssd_upload_dryrun()
#> list()
#> attr(,"class")
#> [1] "ssdsims_upload_dryrun" "ssdsims_upload"       
```
