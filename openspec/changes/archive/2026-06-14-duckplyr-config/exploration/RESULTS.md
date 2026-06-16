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
DuckDB's suggested configuration options. All 16 cells fail identically; the failed allocation
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

## experiment-summary-union.R

Review question: DuckDB's writer was believed to have a 2048-row minimum row
group, so with 50k-double cells one minimum group would buffer
2048 × 50000 × 8 ≈ 819 MB — making the **full-summary union**
(`ssd_summarise(path_with_samples =)`, which crosses the 2048-row boundary)
the pinch point, and raising: what is the minimum memory for unioning >2048
one-row nested Parquets? Corpus: 4100 one-row Parquet files (one 50k-double
`samples` cell each, ~0.2 MB/file, built in 56 s), unioned via the verbatim
`ssd_summarise()` path (`read_parquet_duckdb(files)` |> `compute_parquet()`,
no R materialisation), threads = 1. `row_groups` is the output's actual
per-group row counts from `parquet_metadata()`.

```
RESULT ok=FALSE n_files=2100 memlim=500MB ... rss_peak_mb=734  error=failed to allocate 512.0 MiB
RESULT ok=FALSE n_files=4100 memlim=500MB ... rss_peak_mb=746  error=(as above)
RESULT ok=FALSE n_files=2100 memlim=800MB ... rss_peak_mb=1090 error=(as above)
RESULT ok=FALSE n_files=4100 memlim=800MB ... rss_peak_mb=1103 error=(as above)
RESULT ok=FALSE n_files=2100 memlim=1GB   ... rss_peak_mb=1211 error=failed to allocate 781 MiB
RESULT ok=FALSE n_files=4100 memlim=1GB   ... rss_peak_mb=1409 error=(as above)
RESULT ok=FALSE n_files=2100 memlim=1.5GB ... rss_peak_mb=1210 error=(as above)
RESULT ok=FALSE n_files=4100 memlim=1.5GB ... rss_peak_mb=2014 error=(as above)
RESULT ok=FALSE n_files=2100 memlim=2GB   ... rss_peak_mb=2809 error=(as above)
RESULT ok=FALSE n_files=4100 memlim=2GB   ... rss_peak_mb=2240 error=(as above)
RESULT ok=TRUE  n_files=2100 memlim=3GB   ... write_s=18.36 file_mb=380.1 rss_peak_mb=3520 row_groups=2100
RESULT ok=FALSE n_files=4100 memlim=3GB   ... rss_peak_mb=4159 error=failed to pin block of size 256.0 KiB

RESULT ok=TRUE n_files=4100 memlim=1GB   rowgroup=100   ... write_s=23.68 file_mb=741.6 rss_peak_mb=465 row_groups=100×41
RESULT ok=TRUE n_files=4100 memlim=1GB   rgbytes=100MB order=false ... write_s=23.48 file_mb=741.6 rss_peak_mb=751 row_groups=246×16+164
RESULT ok=TRUE n_files=4100 memlim=500MB rgbytes=100MB order=false ... write_s=23.52 file_mb=741.6 rss_peak_mb=723 row_groups=246×16+164

RESULT ok=TRUE n_files=4100 memlim=100MB compact=TRUE ... write_s=0.71 file_mb=0.0 rss_peak_mb=235 row_groups=4100
```

Readings:

- **Default writer config: the floor scales with TOTAL rows, not with a
  2048-row minimum.** The writer accumulates rows toward its default
  `ROW_GROUP_SIZE` of 122 880, so the whole union lands in one row group
  (`row_groups=2100` at 3 GB): 2100 rows need 3 GB; 4100 rows fail even at
  3 GB. "Relax the summary task's memory limit" is therefore not a stable fix
  — the requirement grows with `nsim`/grid size.
- **The 2048-row minimum premise does not hold on duckdb 1.5.2:**
  `ROW_GROUP_SIZE 100` was honoured exactly (41 groups of 100 rows), and made
  the same 4100-file union pass at `memory_limit='1GB'` with **465 MB** peak
  RSS — memory now flat in total rows. No compression penalty either:
  0.181 MB/row in both the one-group and 100-row-group layouts.
- `ROW_GROUP_SIZE_BYTES '100MB'` (+ the required
  `preserve_insertion_order=false`) is the payload-aware variant: groups of
  246 rows (246 × 50k × 8 ≈ 98 MB), passing even at `memory_limit='500MB'` —
  but it needs insertion order relaxed, which `ROW_GROUP_SIZE` does not.
- The compact summary (`samples` projected out at the DuckDB level) remains
  trivial: 4100 rows at `memory_limit='100MB'` in 0.7 s.

## experiment-rgbytes.R

Review preference: accept nondeterministic order; `ROW_GROUP_SIZE_BYTES` is
attractive because it adapts the row-group row count to the cell size (large
groups for small `samples`, small groups for huge ones). Corpora: `big` =
4100 one-row files × 50k-double cells (shared with
`experiment-summary-union.R`); `small` = 30 000 rows × 1000-double cells
(the `nboot = 1000` default scale). All cases threads = 1, union via the
verbatim `ssd_summarise()` path.

```
A. RESULT ok=FALSE corpus=big   memlim=1GB   rgbytes=100MB order=default ... error=Binder Error: "ROW_GROUP_SIZE_BYTES does not work while preserving insertion order. Use SET preserve_insertion_order=false;"
A. RESULT ok=FALSE corpus=small memlim=500MB rgbytes=100MB order=default ... error=(same Binder Error)
B. RESULT ok=TRUE  corpus=big   memlim=1GB   rgbytes=100MB order=false mode=determinism write_s=48.47(2 writes) file_mb=741.6 rss_peak_mb=761 row_groups=17groups[246+246+246...] bytes_identical=TRUE order_preserved=TRUE
C. RESULT ok=TRUE  corpus=small memlim=500MB rgbytes=100MB order=false write_s=2.50 file_mb=23 rss_peak_mb=574 row_groups=13000+13000+4000 order_preserved=TRUE
D. RESULT ok=FALSE corpus=big   memlim=250MB rgbytes=100MB order=false ... error=Out of Memory: failed to allocate 93.8 MiB
D. RESULT ok=FALSE corpus=big   memlim=150MB rgbytes=100MB order=false ... error=(as above)
D. RESULT ok=TRUE  corpus=big   memlim=150MB rgbytes=32MB  order=false write_s=21.04 file_mb=741.6 rss_peak_mb=342 row_groups=52groups[79+79+79...] order_preserved=TRUE
```

Readings:

- **A — the `preserve_insertion_order = false` requirement is real and
  LOUD**: with ordering preserved the engine refuses the option outright with
  a Binder error naming the fix, so a mispaired configuration can never be
  silently ignored. (Mechanism: the order-preserving sink must reassemble
  upstream batches into source order before emitting, cutting row groups on
  the ordered batcher's row-count boundaries — it cannot flush a group early
  when a byte budget fills because later-ordered batches may still be
  pending. With ordering relaxed, the sink flushes whenever the byte budget
  fills, which is also why the *default* ordered writer accumulated our whole
  union toward one 122 880-row group.)
- **B — with `threads = 1`, relaxing order costs nothing in practice**: two
  runs produced byte-identical files (`md5` equal) AND the output retained
  the input row order. Not an engine contract — just the observed
  single-producer behaviour — so specs should promise value-identity, not
  byte-identity, for this file.
- **C — the adaptivity is real**: 1000-double cells yielded 13 000-row groups
  (13 000 × 1000 × 8 ≈ 104 MB ≈ the budget), against 246-row groups for the
  50k-double cells. One byte budget serves every `nboot` with no
  scenario-derived arithmetic.
- **D — the floor tracks the budget, ~5×**: a 100 MB budget needs between
  250 MB and 500 MB of `memory_limit`; a 32 MB budget passes at 150 MB
  (groups of 79 rows ≈ 31.6 MB). The same ≳ 5× rule as the shard writes, now
  applied to the *budget* instead of the whole union — i.e. arbitrarily
  tunable.

## experiment-preserve-order-copy-option.R

Implementation-review question: could the insertion-order relaxation ride on
`compute_parquet(options = )` as a COPY option (next to
`row_group_size_bytes`), avoiding any global `SET`? duckdb 1.5.2,
threads = 1, the 5000-row x 10-double fixture.

```
RESULT copy-option preserve_insertion_order=FALSE                  -> ERR: Not implemented: Unrecognized option "preserve_insertion_order"
RESULT copy-option preserve_order=FALSE alone                      -> OK groups: 5000
RESULT copy-option preserve_order=FALSE + rgbytes=100KB            -> ERR: Binder: ROW_GROUP_SIZE_BYTES does not work while preserving insertion order
RESULT global false + copy-option preserve_order=TRUE + rgbytes=100KB -> OK groups: 2048+2048+904
RESULT global false + rgbytes=100KB only                           -> OK groups: 2048+2048+904
```

Readings: `PRESERVE_ORDER` *is* a recognised COPY option, but the
`ROW_GROUP_SIZE_BYTES` Binder validation consults only the **global**
`preserve_insertion_order` setting - per-copy `PRESERVE_ORDER FALSE` with a
byte budget is still refused, while global-false with per-copy
`PRESERVE_ORDER TRUE` is accepted. So the byte-budgeted write cannot be
expressed per COPY; the global setting must be `false` regardless. Decision:
fold `preserve_insertion_order = false` into the `local_duckplyr_config()`
scope (snapshot/restored like `threads`/`memory_limit`) and drop the
`compute_parquet_unordered()` wrapper - the full-summary write becomes a
plain `compute_parquet(options = list(row_group_size_bytes = ...))` inside
the already-open scope. Implication recorded in the spec: with the scope
relaxing order globally, per-file byte-identity of pipeline writes is the
observed single-producer behaviour of the default `threads = 1` - raising
`SSDSIMS_DUCKDB_THREADS` may reorder rows (value-identity still holds).
