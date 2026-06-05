## ADDED Requirements

### Requirement: Per-task non-tabular results use an efficient lossless blob encoding
The `fit` step's per-task result is a non-tabular `fitdists` object that the runner stores in a column of the `fit` shard's Parquet (the "per-task results as columns" of the single-core sharded runner). That object SHALL be carried through a single encode/decode seam (`encode_obj()`/`decode_obj()` in `R/targets-runner.R`) using an **efficient, lossless** blob encoding that satisfies three constraints: it SHALL round-trip the object **byte-identically** through the shard Parquet (the byte-identity oracle against `ssd_run_scenario_baseline()`); it SHALL use a Parquet column type that the duckplyr engine can store (duckplyr cannot materialise a `raw`/list column, so the result is carried in a string/`VARCHAR` column rather than a raw column); and the blob column SHALL be **projectable out** at the DuckDB level so a reader that does not need the object never decodes it (the §6 summary read path). The encoding SHALL be confined to the `encode_obj()`/`decode_obj()` seam so the call sites (`ssd_run_fit_step()` writing the blob, `ssd_run_hc_step()` decoding the parent blob) are unchanged beyond the column type/name.

#### Scenario: A fit object round-trips losslessly through the shard Parquet
- **WHEN** a `fitdists` object is encoded into a `fit` shard's Parquet column and later read back and decoded by the `hc` step (the byte-identity oracle)
- **THEN** the decoded object SHALL be identical to the object `ssd_run_scenario_baseline()` produced for the same task, so the swapped encoding changes only the on-disk blob bytes, never the per-task result value

#### Scenario: The encoding round-trips through the encode/decode seam
- **WHEN** an arbitrary `fitdists` object is passed through `decode_obj(encode_obj(x))`
- **THEN** the result SHALL be identical to `x`, and the encoded value SHALL be a single Parquet-storable string-column value (no `raw`/list column, which duckplyr cannot store)

#### Scenario: The summary read path projects the blob column out without decoding
- **WHEN** a reader projects the heavy/non-tabular column out at the DuckDB level (as the §6 summary read path does with `select(-any_of(...))` before collecting into R)
- **THEN** the blob column SHALL be droppable by column projection alone, so the reader pulls no blob bytes into R and never decodes the object
