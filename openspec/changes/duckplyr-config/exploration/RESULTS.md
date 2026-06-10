# Experiment results (captured)

Environment: duckplyr 1.2.1, duckdb (R) 1.5.2, R 4.5.3, Linux x86_64,
4 cores / 15.6 GB RAM (DuckDB defaults observed on this box: `threads = 4`,
`memory_limit = 12.5 GiB`). Each RESULT line is one fresh R process.
`rss_peak_mb` is the process's `VmHWM` (includes R's own copy of the data).
Re-run any table with `Rscript <script> --matrix`.

## experiment-nested-parquet.R

Shard-shaped tibble, `samples` list-column with 50 000 doubles per cell,
written via the `ssd_write_parquet()` body
(`as_duckdb_tibble()` |> `compute_parquet()`).

```
RESULT ok=TRUE  rows=1    doubles=50000 threads=default memlim=default named=FALSE write_s=0.18  file_mb=0.2   rss_peak_mb=160  roundtrip=identical
RESULT ok=TRUE  rows=100  doubles=50000 threads=default memlim=default named=FALSE write_s=2.50  file_mb=18.1  rss_peak_mb=389  roundtrip=identical
RESULT ok=TRUE  rows=1000 doubles=50000 threads=default memlim=default named=FALSE write_s=23.62 file_mb=181   rss_peak_mb=2355 roundtrip=identical
RESULT ok=TRUE  rows=1    doubles=50000 threads=1 memlim=default named=FALSE write_s=0.01  file_mb=0.2   rss_peak_mb=161  roundtrip=identical
RESULT ok=TRUE  rows=100  doubles=50000 threads=1 memlim=default named=FALSE write_s=1.02  file_mb=18.1  rss_peak_mb=398  roundtrip=identical
RESULT ok=TRUE  rows=1000 doubles=50000 threads=1 memlim=default named=FALSE write_s=10.82 file_mb=181   rss_peak_mb=2352 roundtrip=identical
RESULT ok=TRUE  rows=1    doubles=50000 threads=default memlim=1GB named=FALSE write_s=0.01 file_mb=0.2  rss_peak_mb=161 roundtrip=identical
RESULT ok=TRUE  rows=100  doubles=50000 threads=default memlim=1GB named=FALSE write_s=0.99 file_mb=18.1 rss_peak_mb=391 roundtrip=identical
RESULT ok=FALSE rows=1000 doubles=50000 threads=default memlim=1GB named=FALSE write_s=7.45 rss_peak_mb=1806  error=Out of Memory: failed to allocate data of size 381.4 MiB (919.3 MiB/953.6 MiB used)
RESULT ok=TRUE  rows=1    doubles=50000 threads=1 memlim=1GB named=FALSE write_s=0.01 file_mb=0.2  rss_peak_mb=161 roundtrip=identical
RESULT ok=TRUE  rows=100  doubles=50000 threads=1 memlim=1GB named=FALSE write_s=1.00 file_mb=18.1 rss_peak_mb=398 roundtrip=identical
RESULT ok=FALSE rows=1000 doubles=50000 threads=1 memlim=1GB named=FALSE write_s=7.57 rss_peak_mb=1822  error=Out of Memory (as above)
RESULT ok=TRUE  rows=1    doubles=50000 threads=default memlim=100MB named=FALSE write_s=0.02 file_mb=0.2 rss_peak_mb=162 roundtrip=identical
RESULT ok=FALSE rows=100  doubles=50000 threads=default memlim=100MB named=FALSE write_s=0.46 rss_peak_mb=244 error=Out of Memory: failed to allocate data of size 64.0 MiB (32.3 MiB/95.3 MiB used)
RESULT ok=FALSE rows=1000 doubles=50000 threads=default memlim=100MB named=FALSE write_s=0.44 rss_peak_mb=571 error=Out of Memory (as above)
RESULT ok=FALSE rows=100  doubles=50000 threads=1 memlim=100MB named=FALSE write_s=0.44 rss_peak_mb=245 error=Out of Memory (as above)
RESULT ok=FALSE rows=1000 doubles=50000 threads=1 memlim=100MB named=FALSE write_s=0.43 rss_peak_mb=588 error=Out of Memory (as above)
RESULT ok=TRUE  rows=1    doubles=50000 threads=default memlim=default named=FALSE write_s=0.13 file_mb=0.2 rss_peak_mb=163 roundtrip=identical
RESULT ok=TRUE  rows=10   doubles=50000 threads=default memlim=default named=TRUE  write_s=0.23 file_mb=1.8 rss_peak_mb=195 roundtrip=identical
```

Readings: the verbatim question — one nested cell of 50k doubles — is
trivially fine at any tested limit. Cost scales with the whole shard's nested
payload. `threads = 1` is free (2× *faster* on the 1000-row shard). Named
cells write too (names dropped by Parquet; the runner's unname `FIXME` is
about R-side identity, not writability).

## experiment-nested-mitigations.R

The failing case (1000 × 50k nested, threads = 1) under every combination of
DuckDB's suggested knobs. All 16 cells fail identically; the failed allocation
is `rows × doubles × 8` bytes — the LIST child array as one buffer.

```
RESULT ok=FALSE memlim=1GB   order=default spill=no  rowgroup=default ... error=failed to allocate 381.4 MiB
RESULT ok=FALSE memlim=500MB order=default spill=no  rowgroup=default ... error=failed to allocate 512.0 MiB
RESULT ok=FALSE memlim=1GB   order=false   spill=no  rowgroup=default ... (identical)
RESULT ok=FALSE memlim=500MB order=false   spill=no  rowgroup=default ... (identical)
RESULT ok=FALSE memlim=1GB   order=default spill=yes rowgroup=default ... (identical)
RESULT ok=FALSE memlim=500MB order=default spill=yes rowgroup=default ... (identical)
RESULT ok=FALSE memlim=1GB   order=false   spill=yes rowgroup=default ... (identical)
RESULT ok=FALSE memlim=500MB order=false   spill=yes rowgroup=default ... (identical)
RESULT ok=FALSE memlim=1GB   order=default spill=no  rowgroup=2048    ... (identical)
RESULT ok=FALSE memlim=500MB order=default spill=no  rowgroup=2048    ... (identical)
RESULT ok=FALSE memlim=1GB   order=false   spill=no  rowgroup=2048    ... (identical)
RESULT ok=FALSE memlim=500MB order=false   spill=no  rowgroup=2048    ... (identical)
RESULT ok=FALSE memlim=1GB   order=default spill=yes rowgroup=2048    ... (identical)
RESULT ok=FALSE memlim=500MB order=default spill=yes rowgroup=2048    ... (identical)
RESULT ok=FALSE memlim=1GB   order=false   spill=yes rowgroup=2048    ... (identical)
RESULT ok=FALSE memlim=500MB order=false   spill=yes rowgroup=2048    ... (identical)
```

## experiment-nested-vs-flat.R

Same 50M-double payload; floor, shape control, and stage split (threads = 1).

```
RESULT ok=FALSE shape=nested stage=parquet memlim=1.2GB write_s=7.95  rss_peak_mb=1822 error=failed to allocate 381.4 MiB
RESULT ok=FALSE shape=nested stage=parquet memlim=1.5GB write_s=9.24  rss_peak_mb=2172 error=failed to allocate 445 MiB
RESULT ok=TRUE  shape=nested stage=parquet memlim=2GB   write_s=10.91 file_mb=181   rss_peak_mb=2352
RESULT ok=TRUE  shape=flat   stage=parquet memlim=1GB   write_s=6.56  file_mb=186.5 rss_peak_mb=925
RESULT ok=TRUE  shape=flat   stage=parquet memlim=500MB write_s=6.86  file_mb=186.5 rss_peak_mb=925
RESULT ok=TRUE  shape=flat   stage=parquet memlim=100MB write_s=6.51  file_mb=186.5 rss_peak_mb=926
RESULT ok=TRUE  shape=nested stage=table   memlim=1GB   write_s=5.95  rss_peak_mb=1320
RESULT ok=FALSE shape=nested stage=table   memlim=500MB write_s=3.39  rss_peak_mb=812 error=failed to allocate 512.0 MiB
```

Readings: nesting is the culprit — flat passes at 100 MB where nested needs
2 GB (~5× the 400 MB payload). Both stages contribute: nested ingestion to a
table passes at 1 GB / fails at 500 MB; the Parquet writer pushes the floor
to 2 GB.

## experiment-duckplyr-noise.R

```
==== scenario: default ====
-- phase 1: namespace-only pipeline surface --
no conditions emitted
-- phase 2: library(duckplyr) attach --
[startup] ... dplyr masking notes ...
[startup] The duckplyr package is configured to fall back to dplyr when it encounters an
[startup] incompatibility. Fallback events can be collected and uploaded for analysis to
[startup] guide future development. By default, data will be collected but no data will
[startup] be uploaded.
[startup] i Automatic fallback uploading is not controlled and therefore disabled, see
[startup]   `?duckplyr::fallback()`.
[startup] v Number of reports ready for upload: 1.
[startup] -> Review with `duckplyr::fallback_review()`, upload with
[startup]   `duckplyr::fallback_upload()`.
[startup] i Configure automatic uploading with `duckplyr::fallback_config()`.
[startup] v Overwriting dplyr methods with duckplyr methods.
[startup] i Turn off with `duckplyr::methods_restore()`.
==== scenario: silenced (DUCKPLYR_FALLBACK_COLLECT=0 DUCKPLYR_FALLBACK_AUTOUPLOAD=0) ====
-- phase 1: namespace-only pipeline surface --
no conditions emitted
-- phase 2: library(duckplyr) attach --
[startup] ... dplyr masking notes ...
[startup] v Overwriting dplyr methods with duckplyr methods.
[startup] i Turn off with `duckplyr::methods_restore()`.
```

Readings: the pipeline's own (namespace-only) surface is silent either way.
The noise is the attach-time telemetry banner, gated on fallback logs
existing; `DUCKPLYR_FALLBACK_COLLECT=0` prevents the logs and
`DUCKPLYR_FALLBACK_AUTOUPLOAD=0` pre-answers the ask.
