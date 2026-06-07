## Why

`ssd_summarize()` writes a single `summary.parquet` that **projects out** the
`dists` and `samples` list-columns at the DuckDB level
(`R/targets-runner.R:457-472`): the potentially-large per-row bootstrap draws are
never pulled into R, and the summary is the compact, analysis-ready estimate
table. But a user who set `samples = TRUE` on the scenario specifically to
**retain** those draws then finds them absent from the only fan-in artifact — to
get them back they must re-read the `hc` shard glob themselves. Dropping columns
from Parquet is cheap, so there is no reason to make the run choose: it can write
**both** a compact estimate table and a complete artifact that keeps the draws.

## What Changes

- **`ssd_summarize()` gains an optional second output.** A new trailing
  `path_full = NULL` argument: when supplied, the function **also** writes a
  *full* hc union to `path_full` that **retains** the `dists`/`samples`
  list-columns, in addition to the compact `path` (whose projection is
  unchanged). The full write stays at the DuckDB level (read the Hive glob → write
  Parquet) so the draws are **never collected into R** — the same no-R-materialise
  guarantee the compact path already has. With `path_full = NULL` the behaviour is
  exactly as today (single compact summary), so existing call sites are
  unaffected.
- **The targets pipeline writes the full summary when, and only when, the
  scenario retains draws.** `ssd_scenario_targets()` passes
  `path_full = <root>/summary-samples.parquet` to the `summary` target **iff**
  `scenario$hc$samples` is `TRUE` (the case where the draws carry information the
  compact summary cannot). The target returns the path **vector** so `targets`
  tracks both files under its `format = "file"` contract. When
  `samples = FALSE`, the pipeline is byte-for-byte unchanged (one
  `summary.parquet`, no extra file).
- **Both summaries union the same landed shards.** The full read uses the same
  `hc` glob and `hive_partitioning = FALSE` read as the compact one, so it
  inherits the partial-failure-survival property (`error = "null"`, §6.2) — it is
  the same fan-in with a wider projection.
- **Sweep docs and templates** (`?ssd_summarize`, the sharded-pipeline vignette,
  `inst/targets-templates/`) to describe the two outputs and the
  `samples = TRUE` trigger.

## Capabilities

### New Capabilities
<!-- None: this extends an existing capability. -->

### Modified Capabilities
- `task-shards`: the summary-fan-in requirement gains a second, optional output —
  a *full* summary that retains the `dists`/`samples` list-columns, written by the
  same directory read without pulling the draws into R, and emitted by the
  pipeline exactly when `scenario$hc$samples` is `TRUE`. The existing compact
  summary and its no-R-materialise projection are unchanged.

## Impact

- **Specs**: `task-shards` delta.
- **Code**: `R/targets-runner.R` — `ssd_summarize()` (`path_full` argument and the
  lazy full write); `ssd_scenario_targets()` (conditional `path_full`, the
  `summary` target returning a path vector).
- **API**: additive, backward-compatible (`path_full` defaults to `NULL`); no
  change to the compact `summary.parquet` bytes or path.
- **Docs/templates**: `man/ssd_summarize.Rd` (roxygen), `vignettes/sharded-pipeline.qmd`,
  `inst/targets-templates/` run scripts/READMEs, `GLOSSARY.md`/`TARGETS-DESIGN.md`
  as needed.
- **Tests**: a scenario with `samples = TRUE` yields both files (full retains a
  non-empty `samples` column, compact omits it, estimate columns identical); with
  `samples = FALSE` only the compact file is written.
