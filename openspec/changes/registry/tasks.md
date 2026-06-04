## 1. Dependencies and shared helpers

- [ ] 1.1 Add `duckplyr` and `digest` to `DESCRIPTION` `Imports`
- [ ] 1.2 Add thin internals `ssd_write_parquet(x, path)` / `ssd_read_parquet(path)` wrapping `duckplyr` (`duckplyr::compute_parquet()` / `duckplyr::read_parquet_duckdb()`) — the single audit point for Parquet I/O (team preference, `AGENTS.md`)
- [ ] 1.3 Add a shared `ssd_file_sha256(path)` internal (`digest::digest(file = path, algo = "sha256")`); place it in a shared utils file so `manifest` can reuse it

## 2. Dataset registry

- [ ] 2.1 Add `ssd_register_dataset(name, data)` in `R/registry.R`: verify the numeric `Conc` invariant (abort in the user-facing frame, naming the dataset), write `results/datasets/<name>.parquet`, and upsert the `_index.json` entry `name -> { rows, conc_col, sha256, source }`
- [ ] 2.2 Compute the index `sha256` over the written Parquet bytes via `ssd_file_sha256()`
- [ ] 2.3 Implement refuse-unless-identical re-registration: byte-identical re-registration is a no-op; a conflicting payload aborts with an informative error naming the conflict
- [ ] 2.4 Add `_index.json` reader/writer helpers (single-writer read-modify-write)

## 3. min_pmix registry

- [ ] 3.1 Add `ssd_register_min_pmix(name, fn)`: validate `fn` is a single-argument function (reuse `check_min_pmix_function()`), pin it under `name`, apply the same refuse-unless-identical policy
- [ ] 3.2 Rewire `resolve_min_pmix()` in `R/task-lists.R` to consult the registry first, falling back to the existing `ssdtools` / global-env lookup; keep the baseline runner unaffected when no registry exists

## 4. Scenario resolution

- [ ] 4.1 Add the registry resolver entry point: given a scenario, persist each referenced dataset name from its inline `scenario$data[[name]]` tibble (no regeneration) and resolve each referenced `min_pmix` name, once per project
- [ ] 4.2 Document that generator-input regeneration (name + generator + `.seed`) is the deferred `dataset-provenance` step; here datasets are persisted, not regenerated

## 5. Docs and reference

- [ ] 5.1 Roxygen for the new exports documenting the name-only indirection (§1.1), the `Conc` invariant, the index fields, and the re-registration policy
- [ ] 5.2 Add a `registry` reference group to `_pkgdown.yml`

## 6. Tests and checks

- [ ] 6.1 `tests/testthat/test-registry.R`: a valid dataset is persisted and indexed; the round-trip read equals the input; a `Conc`-less dataset is rejected with no side effects
- [ ] 6.2 Re-registration tests: byte-identical is a no-op; a conflicting payload aborts
- [ ] 6.3 `min_pmix` tests: a valid function registers and resolves; a non-conforming function is rejected; resolution falls back to `ssdtools` when no entry exists
- [ ] 6.4 Resolution test: a scenario's referenced datasets are persisted to Parquet with matching index entries
- [ ] 6.5 Regression: a fit task's primer is byte-identical with the registry empty vs. after registering a `min_pmix` body (primer hashes the name, not the value)
- [ ] 6.6 Run `devtools::document()`, `air format .`, and `devtools::check()`; update `NAMESPACE`/`man/`
