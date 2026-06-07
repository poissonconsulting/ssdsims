## ADDED Requirements

### Requirement: An optional full summary retains the dists/samples list-columns

`ssd_summarise()` SHALL accept an optional trailing `path_full` argument
(default `NULL`). The compact summary written to `path` SHALL be unchanged — it
SHALL continue to project the `dists` and `samples` list-columns out at the
DuckDB level so the potentially-large per-row bootstrap draws are never pulled
into R. When `path_full` is non-`NULL`, the function SHALL **additionally** write
a *full* summary to `path_full` that unions the same `hc` shard glob but
**retains** the `dists`/`samples` list-columns. The full write SHALL be performed
at the DuckDB level (read the Hive glob, write Parquet) so the retained draws are
**never collected into R** — the same no-R-materialise guarantee as the compact
path. Both summaries SHALL read the result directory (`hive_partitioning =
FALSE`) rather than the shard target values, so the full summary inherits the
partial-failure-survival property and unions whatever shards landed
(`TARGETS-DESIGN.md` §6.2). When `path_full` is `NULL`, `ssd_summarise()` SHALL
behave exactly as before (a single compact summary, no extra file).

#### Scenario: Full summary retains the draws the compact summary drops
- **WHEN** `ssd_summarise()` is run with a non-`NULL` `path_full` over `hc` shards
  produced with `samples = TRUE`
- **THEN** the compact `path` SHALL omit the `dists`/`samples` columns, the full
  `path_full` SHALL contain a populated `samples` list-column (and `dists`), and
  the estimate columns (`est`/`lcl`/`ucl`) SHALL be identical across the two files

#### Scenario: Without path_full the behaviour is unchanged
- **WHEN** `ssd_summarise()` is run with `path_full = NULL` (the default)
- **THEN** it SHALL write only the compact summary at `path`, projecting out
  `dists`/`samples` as before, and SHALL NOT write any second file

### Requirement: The pipeline emits the full summary only when the scenario retains draws

`ssd_scenario_targets()` SHALL pass `path_full = <root>/summary-samples.parquet`
to the `summary` target's `ssd_summarise()` call **if and only if**
`scenario$hc$samples` is `TRUE` — the case where the retained draws carry
information the compact summary cannot. In that case the `summary` target SHALL
return the **vector** of both file paths so `targets` tracks both under its
`format = "file"` contract. When `scenario$hc$samples` is `FALSE`, the pipeline
SHALL be unchanged: the `summary` target writes the single compact
`summary.parquet` and returns its path.

#### Scenario: samples = TRUE yields two tracked summary files
- **WHEN** a scenario defined with `samples = TRUE` is run through the targets
  pipeline
- **THEN** the `summary` target SHALL write both `summary.parquet` and
  `summary-samples.parquet` and SHALL return both paths so `targets` tracks each
  as a `format = "file"` output

#### Scenario: samples = FALSE leaves the pipeline unchanged
- **WHEN** a scenario defined with `samples = FALSE` is run through the targets
  pipeline
- **THEN** the `summary` target SHALL write only `summary.parquet` and SHALL NOT
  write `summary-samples.parquet`
