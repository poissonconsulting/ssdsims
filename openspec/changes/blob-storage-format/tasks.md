## 1. Evaluate the blob-storage alternatives

- [ ] 1.1 Locate the encode/decode seam and its call sites in `R/targets-runner.R`: `encode_obj()`/`decode_obj()`, the `fit_blob = encode_obj(fit)` write in `ssd_run_fit_step()`, the `decode_obj()` read in `ssd_run_hc_step()`, and the §6 `ssd_summarize()` projection (`select(-any_of(...))`)
- [ ] 1.2 Evaluate the three candidates against the byte-identity oracle, the duckplyr/Parquet column-type constraint (no `raw`/list column), and the projectable-blob property: (a) binary `serialize(ascii = FALSE)` as base64 text in a `VARCHAR`, (b) an Arrow-native nested representation, (c) a sidecar object store keyed by `fit_id`; record the evaluation and the decision in `design.md`
- [ ] 1.3 Benchmark `encode_obj`/`decode_obj` size and encode/decode time on a representative `fitdists` (ASCII-`VARCHAR` vs binary+base64), and decide the base64 codec (small dependency vs vendored helper) — the swap lands only if it beats the interim ASCII encoding

## 2. Land the chosen encoding

- [ ] 2.1 Swap `encode_obj()`/`decode_obj()` in `R/targets-runner.R` to the chosen lossless encoding, keeping the seam intact so the call sites are untouched beyond the column type/name
- [ ] 2.2 Update the `fit_blob = encode_obj(fit)` write in `ssd_run_fit_step()` and the `decode_obj()` read in `ssd_run_hc_step()` only if the blob column's type or name changes; leave the inner/path axis columns and the Hive layout unchanged
- [ ] 2.3 Add the base64 dependency to `DESCRIPTION` `Imports` (or add the vendored helper) only if the benchmark selected the binary+base64 option; update the seam's roxygen/`@noRd` comment describing the new encoding

## 3. Tests

- [ ] 3.1 Round-trip / byte-identity test: `decode_obj(encode_obj(fit))` is identical to `fit`, and a `fitdists` written to a `fit` shard Parquet and read back through the `hc` path equals the object `ssd_run_scenario_baseline()` produced (the existing oracle)
- [ ] 3.2 Projection test: the summary-style DuckDB projection (`select(-any_of(...))`) drops the blob column without pulling its bytes into R or decoding the object

## 4. Document and validate

- [ ] 4.1 Run `devtools::document()`, `air format .`, and `devtools::check()`; update `NAMESPACE`/`man/` if anything changed
- [ ] 4.2 Run `openspec validate blob-storage-format --strict` until clean
