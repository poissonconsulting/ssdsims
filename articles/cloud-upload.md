# Uploading Shards to Cloud Storage

``` r

library(ssdsims)
```

The [“Running a Sharded
Pipeline”](https://poissonconsulting.github.io/ssdsims/articles/sharded-pipeline.md)
vignette materialises each step as Hive-partitioned Parquet shards, and
the [“Running on a SLURM
Cluster”](https://poissonconsulting.github.io/ssdsims/articles/cluster-pipeline.md)
vignette runs that same pipeline on a cluster. This vignette covers the
last link: shipping each shard to an **object store** as it is produced,
so the results are readable **from outside the cluster** — analysis
notebooks, dashboards, downstream R/Python (`TARGETS-DESIGN.md` §6.1).

The model is simple: **upload is a runner argument**, the
remote-destination sibling of `root`. You pass an `upload` object to the
[`ssd_scenario_targets()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_targets.md)
factory and it pairs each step shard with an `upload_<step>` target.
There are three modes:

- `upload = NULL` (the default) — **no** upload targets; the clean DAG.
- `upload = ssd_upload_dryrun()` — upload targets that **no-op** (reach
  no network), so the DAG shape can be exercised offline and in CI.
- `upload = ssd_upload_azure(url, container)` — upload targets that ship
  to Azure Blob Storage.

The per-task `sample`/`fit`/`hc` results are **byte-identical** across
all three — only the presence and behaviour of the `upload_<step>`
targets differ. This vignette’s **live chunks use
[`ssd_upload_dryrun()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_azure.md)**,
so the build needs no network and no credentials; the Azure path is
shown as described, non-evaluated chunks.

## The destination objects

[`ssd_upload_azure()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_azure.md)
and
[`ssd_upload_dryrun()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_azure.md)
return plain, classed S3 objects that carry **only the destination** —
never credentials, connections, or environments — so they travel
unchanged to `crew` workers and through `targets`:

``` r

dryrun <- ssd_upload_dryrun()
azure <- ssd_upload_azure(
  url = "https://acct.blob.core.windows.net",
  container = "ssdsims-results"
)
class(azure)
#> [1] "ssdsims_upload_azure_blob" "ssdsims_upload"
unclass(azure)
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
```

Credentials stay **external**: the Azure methods resolve the **secret**
from the environment at call time — one of `SSDSIMS_AZURE_STORAGE_KEY`,
`SSDSIMS_AZURE_STORAGE_SAS`, or the service-principal trio
(`SSDSIMS_AZURE_TENANT_ID`/`SSDSIMS_AZURE_CLIENT_ID`/`SSDSIMS_AZURE_CLIENT_SECRET`)
— and the object itself holds no secrets. The storage **account name**
is derived from `url` (the `acct` in
`https://acct.blob.core.windows.net`), so there is **no** account
environment variable; for a sovereign cloud, set `domain` (e.g.
`domain = "blob.core.usgovcloudapi.net"`).

To write under a **subdirectory** of the container (so one container can
hold several independent result sets), pass `prefix`:

``` r

ssd_upload_azure(
  url = "https://acct.blob.core.windows.net",
  container = "ssdsims-results",
  prefix = "study-2026/run-3"
)$prefix
#> [1] "study-2026/run-3"
```

The shards then land at
`<container>/<prefix>/<step>/<partition>/part.parquet`, and
[`ssd_open_uploaded()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_open_uploaded.md)
reads them back from the same prefixed location.

## Run it locally with a dry run

The dry-run probe is trivially OK — no credentials, no network:

``` r

ssd_test_upload(ssd_upload_dryrun())
```

Build a scenario and hand it to the factory with
`upload = ssd_upload_dryrun()`. You run
[`ssd_test_upload()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_test_upload.md)
yourself as a preflight (above); the factory does no network I/O of its
own — it just pairs each step shard with a no-op `upload_<step>` target:

``` r

data <- ssd_scenario_data(ssddata::ccme_boron)
scenario <- ssd_define_scenario(
  data,
  nsim = 2L,
  seed = 42L,
  nrow = 6L,
  dists = ssd_distset(lnorm = "lnorm"))
root <- tempfile("results-")
targets_dry <- ssd_scenario_targets(scenario, root = root, upload = dryrun)
```

The upload targets are present in the DAG, one paired with each shard:

``` r

target_names <- function(x) {
  if (inherits(x, "tar_target")) {
    return(x$settings$name)
  }
  if (is.list(x)) {
    return(unlist(lapply(x, target_names), use.names = FALSE))
  }
  character(0)
}
names_dry <- target_names(targets_dry)
grep("^upload_", names_dry, value = TRUE)
#> [1] "upload_sample_42_ccme_boron_1_TRUE" "upload_sample_42_ccme_boron_2_TRUE"
#> [3] "upload_fit_42_ccme_boron_1_6_FALSE" "upload_fit_42_ccme_boron_2_6_FALSE"
#> [5] "upload_hc_42_ccme_boron_1"          "upload_hc_42_ccme_boron_2"
```

Contrast `upload = NULL` — the default — which emits **no** upload nodes
at all:

``` r

targets_null <- ssd_scenario_targets(scenario, root = root)
grep("^upload_", target_names(targets_null), value = TRUE)
#> character(0)
```

A no-op upload is exactly a no-op: it reaches no network and returns the
shard’s local path unchanged. Materialise one shard locally, then “ship”
it with the dry-run destination:

``` r

run <- ssd_run_scenario_shards(scenario)
shard <- list.files(
  file.path(run$dir, "hc"),
  pattern = "part.parquet",
  recursive = TRUE,
  full.names = TRUE
)[1]
identical(ssd_upload_shard(shard, dryrun), shard)
#> Dry-run upload: skipped
#> "/tmp/RtmpU2P4e5/ssdsims-shards-41eb4a162245/hc/dataset=ccme_boron/sim=1/part.parquet".
#> [1] TRUE
```

That is the whole upload DAG — the probe, the paired `upload_<step>`
targets, and the per-shard ship — exercised end to end with no network
and no credentials.

## Extend the same call to Azure on a cluster

To ship to a real Azure container, swap
[`ssd_upload_dryrun()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_azure.md)
for `ssd_upload_azure(url, container)` in the **cluster** template’s
`_targets.R` ([“Running on a SLURM
Cluster”](https://poissonconsulting.github.io/ssdsims/articles/cluster-pipeline.md)).
It is the same factory call — one line changes:

``` r

ssd_scenario_targets(
  scenario,
  upload = ssd_upload_azure(
    url = "https://<account>.blob.core.windows.net",
    container = "ssdsims-results"
  )
)
```

Run the credentials/connectivity probe as an interactive **preflight**
before
[`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html)
— it lists the container and writes then deletes a marker blob, aborting
loudly (naming the missing `SSDSIMS_AZURE_*` variable) if your wiring is
wrong:

``` r

Sys.setenv(SSDSIMS_AZURE_STORAGE_KEY = "<key>") # account comes from the url
ssd_test_upload(ssd_upload_azure("https://<account>.blob.core.windows.net", "ssdsims-results"))
# silent on success; aborts naming the missing variable otherwise
```

## Verify the upload, in place

Right after an upload, read the results **back in place** to confirm
they landed — no download. `ssd_open_uploaded(upload, step)` returns a
lazy `duckplyr`/DuckDB table over the
`<container>/<step>/**/part.parquet` Hive glob, read straight against
blob storage via DuckDB’s `azure` extension (it remaps your
`SSDSIMS_AZURE_*` credentials into a DuckDB `azure` secret for you).
Because it is lazy and `dplyr`-composable, a one-line `count()` is the
immediate smoke test:

``` r

upload <- ssd_upload_azure("https://<account>.blob.core.windows.net", "ssdsims-results")
ssd_open_uploaded(upload, step = "hc") |>
  dplyr::count()
```

A row-for-row compare of `ssd_open_uploaded(upload, step)` against the
local shard verifies the transfer. (A dry-run destination has nothing to
read back: `ssd_open_uploaded(ssd_upload_dryrun(), ...)` aborts and
points you at the local shards.)

For the analysis-ready table,
[`ssd_summarise_uploaded()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_summarise_uploaded.md)
is the cloud counterpart of
[`ssd_summarise()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_summarise.md):
it unions a step’s uploaded shards in place and collects them, dropping
the heavy `dists`/`samples` list-columns by default. Pass
`drop_samples = FALSE` when you need the in-flight bootstrap `samples`:

``` r

ssd_summarise_uploaded(upload, step = "hc") # analysis-ready summary tibble
ssd_summarise_uploaded(upload, step = "hc", drop_samples = FALSE) # keep samples
```

## What to pay attention to

> **Important**
>
> - **Credentials must reach the *workers*.**
>   [`ssd_test_upload()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_test_upload.md)
>   is easy to run interactively on the login node, but the shards
>   upload **on the compute nodes**. Set `SSDSIMS_AZURE_*` so the
>   workers see them — via the controller’s `script_lines`/module loads,
>   or your scheduler’s environment propagation — not just on the login
>   node.
> - **The Azure client and the DuckDB `azure` extension must be on the
>   workers.** Install `AzureStor`/`AzureRMR` (and, for the read-back,
>   DuckDB’s `azure` extension) on the workers, alongside `ssdsims` —
>   the same ManyLinux binary path the cluster vignette covers.
> - **A missing credential fails loud.** Azure with absent credentials
>   **aborts** — it is never a silent no-op. Intent to skip the network
>   is expressed only by
>   [`ssd_upload_dryrun()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_azure.md).
>   Under the pipeline’s per-shard `error = "null"`, a failed upload
>   isolates to its own branch and the rest keep shipping, so a
>   re-driven run retries only the failed uploads.
> - **Unchanged shards are not re-uploaded.** Each `upload_<step>`
>   target takes the shard’s path as a `format = "file"` input, so
>   `targets` re-uploads a shard only when its content hash changes — a
>   re-driven
>   [`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html)
>   that rebuilt nothing uploads nothing; a partial extension uploads
>   only the new shards.
> - **The read-back is in place.**
>   [`ssd_open_uploaded()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_open_uploaded.md)
>   predicate-pushes straight against blob storage — it does **not**
>   download the Parquet.

## See also

- [“Running a Sharded
  Pipeline”](https://poissonconsulting.github.io/ssdsims/articles/sharded-pipeline.md)
  — materialising the shards this vignette uploads.
- [“Running on a SLURM
  Cluster”](https://poissonconsulting.github.io/ssdsims/articles/cluster-pipeline.md)
  — the cluster template whose `_targets.R` gains the
  `upload = ssd_upload_azure(...)` line.
- `TARGETS-DESIGN.md` §6.1 (the cloud-upload hook).
- [`?ssd_upload_azure`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_azure.md),
  [`?ssd_test_upload`](https://poissonconsulting.github.io/ssdsims/reference/ssd_test_upload.md),
  [`?ssd_upload_shard`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_shard.md),
  [`?ssd_open_uploaded`](https://poissonconsulting.github.io/ssdsims/reference/ssd_open_uploaded.md).
  \`\`\`
