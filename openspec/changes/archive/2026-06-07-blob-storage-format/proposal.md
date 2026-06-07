## Why

The `fit` step's per-task result is a `fitdists` object — non-tabular — yet it has to land in the shard's Parquet alongside the tabular `fit_id` key (`shard-runner`: each shard Parquet carries "the per-task results as columns"). `shard-runner-baseline` ships an interim encoding in `R/targets-runner.R`: `encode_obj()` does `rawToChar(serialize(x, ascii = TRUE))` into a Parquet `VARCHAR`, and `decode_obj()` reverses it. ASCII serialisation was chosen because duckplyr cannot store a raw or list column, and an ASCII string round-trips losslessly through a `VARCHAR`. But ASCII serialisation is ~2× the size of the binary form and is CPU-heavier to encode and decode, so for many or large fits the blob layer dominates shard size and write/read time. The encoding was always flagged as interim; this change evaluates the alternatives against the constraints and lands the chosen format.

## What Changes

- **Evaluate** the blob-storage alternatives against three hard constraints — the byte-identity oracle (the per-task result must stay byte-identical to `ssd_run_scenario_baseline()`, `shard-runner`), the duckplyr/Parquet column-type constraint (duckplyr cannot store a raw or list column, which is why the interim path serialises to a string at all), and the §6 summary read path (which projects the heavy/list columns out at the DuckDB level and must keep being able to skip the blob column without decoding it): (a) binary `serialize(ascii = FALSE)` carried in a base64-text or `BLOB` column; (b) an Arrow-native nested representation; (c) a sidecar object store keyed by `fit_id` rather than an inline column; (d) jsonlite object serialisation (`jsonlite::serializeJSON()` / `unserializeJSON()`) into a JSON `VARCHAR`. The evaluation and the resulting decision are recorded in `design.md`.
- **Land the chosen encoding** by swapping `encode_obj()`/`decode_obj()` in `R/targets-runner.R` (and the `fit_blob = encode_obj(fit)` write in `ssd_run_fit_step()` / the `decode_obj()` read in `ssd_run_hc_step()` if the column type or name changes), keeping the encode/decode seam so the engine stays swappable.
- **Keep the contracts intact**: the swapped encoding SHALL round-trip a `fitdists` object losslessly through the shard Parquet (byte-identity oracle), and the §6 summary read path SHALL still be able to project the blob column out without decoding it.

## Capabilities

### Modified Capabilities
- `shard-runner`: the per-task non-tabular `fit` result stored in the shard Parquet uses an efficient lossless blob encoding (replacing the interim ASCII-`VARCHAR` `encode_obj()`/`decode_obj()` pair), with the byte-identity and projectable-blob contracts stated explicitly.

## Impact

- **Code**: `R/targets-runner.R` — `encode_obj()`/`decode_obj()` (the encode/decode seam), `ssd_run_fit_step()` (the `fit_blob = encode_obj(fit)` write), and `ssd_run_hc_step()` (the `decode_obj()` read). The seam is preserved so callers are untouched beyond the column type/name.
- **Tests**: `tests/testthat/` — a round-trip / byte-identity test for the new encoding through a shard Parquet, and a check that the summary read path still projects the blob column out without decoding.
- **Dependencies**: only if the chosen format requires one (e.g. a base64 helper for the binary option); the binary-`serialize` option needs no new dependency, and the jsonlite option adds a `jsonlite` dependency (lightweight and near-ubiquitous). Recorded with the decision in `design.md`.
- **On-disk layout**: the `fit` shard Parquet's blob column changes type and/or encoding (and possibly name); no change to the Hive partition layout, the inner/path axis split, or any other step's shards.
- **Dependencies (direction)**: an independent tidy-up — it has no prerequisites and nothing depends on it (`TARGETS-DESIGN.md` §12, off the dependency DAG). It can land at any time; the only coupling is the encode/decode seam staying internal to `R/targets-runner.R`.
- **Out of scope**: the Hive partition layout, the m:n parent-shard resolution, the summary fan-in's column set, and any change to which step stores a blob (only the `fit` step does).
