# Probe an Upload Destination's Credentials and Connectivity

The front-door check, dispatched on the upload object's class, that
confirms **before any compute** whether the destination is reachable and
the credentials are in the right place (`TARGETS-DESIGN.md` section
6.1). Run it as a one-liner at the prompt before
[`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html),
or let
[`ssd_scenario_targets()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_targets.md)
run it once up front for you.

## Usage

``` r
ssd_test_upload(upload)
```

## Arguments

- upload:

  An upload destination from
  [`ssd_upload_azure()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_azure.md)
  or
  [`ssd_upload_dryrun()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_azure.md).

## Value

`NULL`, invisibly (called for its side effect: the probe).

## Details

For an Azure destination it resolves the credentials from the
environment and, when a required variable is **absent**, aborts with a
loud error **naming the missing variable** (rather than failing later on
a worker); when they are present it lists the container and writes then
deletes a small marker blob, returning invisibly on success and aborting
with the backend's diagnostic on failure. For a dry-run destination it
succeeds trivially without resolving credentials or reaching any
network.

## See also

[`ssd_upload_shard()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_shard.md),
[`ssd_open_uploaded()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_open_uploaded.md),
[`ssd_upload_azure()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_azure.md).

## Examples

``` r
ssd_test_upload(ssd_upload_dryrun())
```
