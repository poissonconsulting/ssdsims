## Context

`ssd_summarise(dir_sample, dir_fit, dir_hc, path)` (`R/targets-runner.R:457-472`)
is the §6 fan-in: it reads the `hc` shard glob with duckplyr, projects out the
`dists`/`samples` list-columns with `select(-any_of(c("dists", "samples")))`
**before** collecting into R, and writes the compact estimate table to `path`.
The projection is load-bearing — the `samples` draws (populated when the scenario
sets `samples = TRUE`, `scenario-definition/spec.md:198`) can be large, and the
`shard-runner` spec pins the rule that a reader that does not need a heavy column
drops it by column projection alone so no bytes cross into R
(`shard-runner/spec.md:83-85`).

The draws are not lost — they remain in the `hc` shards on disk — but the single
fan-in artifact does not carry them, so a `samples = TRUE` user must re-glob the
shards to recover what they explicitly asked the run to retain. Dropping columns
from Parquet is cheap, so writing a second, wider projection alongside the
compact one is near-free.

The pipeline call site (`ssd_scenario_targets()`, `R/targets-runner.R:742-749`)
wraps `ssd_summarise()` in a single `format = "file"` `summary` target that names
every `hc` shard (so it re-runs on any shard change and survives a partial
failure) and returns `summary_path`.

## Goals / Non-Goals

**Goals:**

- Add a second, *optional* full summary that retains `dists`/`samples`, written by
  the same fan-in read.
- Preserve the no-R-materialise guarantee for **both** outputs — the full write
  must not pull the draws into R either.
- Keep the compact output (bytes and path) and every existing call site unchanged
  when the new output is not requested.
- Emit the full summary from the pipeline exactly when it carries information —
  i.e. when `scenario$hc$samples` is `TRUE`.

**Non-Goals:**

- Changing the compact summary's columns or projection.
- Recomputing or re-seeding anything — this is a wider read of the same shards.
- A full summary for `samples = FALSE` runs in the pipeline (no extra information;
  the function still allows it, but the factory does not request it).
- Uploading/serving the second file (`cloud-upload` already offers a path to the
  summary; extending it to the full summary is out of scope here).

## Decisions

### Decision: an optional `path_full = NULL` argument, not a new function or a flag

`ssd_summarise()` grows a trailing `path_full = NULL`. `NULL` ⇒ today's
behaviour, so the four-positional call sites
(`inst/targets-templates/`, the vignette) are untouched. A non-`NULL` value adds
the second write.

- *Why not a separate `ssd_summarise_full()`?* It would duplicate the glob read
  and the partial-failure-survival wording for one differing line (the
  projection). One function reading the directory once and emitting one or two
  projections is the smaller surface.
- *Why not a `samples = TRUE/FALSE` flag selecting which single file to write?*
  The value is having **both** — a compact table for analysis and a complete
  artifact — so the API writes the compact one always and the full one on demand,
  rather than forcing a choice.

### Decision: the full write stays lazy in DuckDB (never collects the draws into R)

The compact path collects into R *after* projecting the heavy columns out. The
full path must keep the draws, so it must **not** collect — instead it reads the
glob and writes Parquet entirely at the DuckDB level:

```r
duckplyr::compute_parquet(
  duckplyr::read_parquet_duckdb(glob, options = list(hive_partitioning = FALSE)),
  path_full
)
```

This is the same DuckDB read→write `ssd_write_parquet()` already uses, and the
`samples` list-column round-trips as a DuckDB `LIST` (it was written from a
tibble list-column the same way, `R/targets-runner.R:408-411`), so the draws move
shard→summary without ever materialising in R. This honours the
`shard-runner/spec.md:83-85` spirit for the full output too: the heavy column is
carried by the engine, not by R.

### Decision: the factory gates the full summary on `scenario$hc$samples`

`scenario$hc$samples` is known at sourcing time, so `ssd_scenario_targets()`
decides **statically** whether to pass `path_full`. When `TRUE` it passes
`file.path(root, "summary-samples.parquet")` and the `summary` target returns
`c(summary_path, summary_full_path)`; a `format = "file"` target accepts a path
vector, so `targets` tracks both files. When `FALSE` the command and return arity
are exactly as today.

- *Naming:* `summary-samples.parquet` names the distinguishing feature (the
  retained draws). It sits beside `summary.parquet` under the same layout-keyed
  root, so a changed `partition_by` still isolates it
  (`task-shards/spec.md:78`).
- *Why gate on `samples`?* With `samples = FALSE` the `samples` column is empty,
  so the full file would differ from the compact only by an empty list-column and
  the small `dists` column — not worth a second tracked artifact in the
  happy-path pipeline. The function still permits it (callers outside the factory
  may want the `dists` column); the factory just does not request it.

## Risks / Trade-offs

- **DuckDB cannot write the `samples` `LIST` column** → Mitigation: the column is
  *read back* from Parquet that duckplyr itself wrote from a list-column, so the
  round-trip is already exercised by the hc step; a test asserts the full file's
  `samples` is non-empty and matches the shard draws.
- **A `format = "file"` target returning a vector behaves differently** →
  Mitigation: `targets` supports multi-file `format = "file"` (the return is a
  character vector of paths); the arity is fixed per pipeline because it is chosen
  statically from `scenario$hc$samples`, not at runtime.
- **Double scan of the `hc` glob (compact + full reads)** → Mitigation: the scan
  is negligible against the simulation cost the summary fans in, and only the
  `samples = TRUE` path pays for the second read.
- **Two artifacts can drift in consumers' minds** → Mitigation: doc the contract
  (`?ssd_summarise`, the vignette) — same rows/estimates, the full one adds the
  retained list-columns; `tar_read("summary")` returns the vector when both exist.
