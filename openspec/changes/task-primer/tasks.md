## 1. Primer derivation

- [ ] 1.1 Add `R/task-primer.R` with internal `hex8_to_int32(hex8)`: split into two 16-bit halves, combine in double precision, wrap `u >= 2^31` to signed int32, with `0x80000000` mapping to `NA_integer_` (lift from `scripts/experiment-dqrng-hash.R`)
- [ ] 1.2 Add internal `normalize_task_row(row)` (inverse of `tibble::tibble_row()`): `as.list()`, drop attributes, decide per column — df-column (`is.data.frame()`) kept, list-column (`is.list()`) unwrapped to `[[1]]`, else the atomic scalar
- [ ] 1.3 Add exported `task_primer(params)`: accept a plain list or single-row data frame (normalise the latter via `normalize_task_row()`), then `rlang::hash(.)` and `c(hex8_to_int32(substr(h, 1, 8)), hex8_to_int32(substr(h, 9, 16)))`
- [ ] 1.4 Add a light `chk` guard that `params` is a plain list or single-row data frame, aborting in the user-facing frame on misuse (incl. a multi-row data frame)

## 2. Docs

- [ ] 2.1 Roxygen for `task_primer()` with `@export`: document the length-2 integer primer, the 64-bit / NA-INT_MIN encoding, and that it pairs with `local_dqrng_state(seed, primer)` / `dqrng::dqset.seed(seed, stream = primer)`
- [ ] 2.2 Document the canonical name-keyed input contract per step (data `(dataset, sim, replace)`; fit/hc identities + grid rows), `min_pmix` by name, `nrow` excluded — pointing to `task-tables` as where `params` is assembled
- [ ] 2.3 `@seealso` `local_dqrng_state()`, `local_dqrng_backend()`; add to `_pkgdown.yml`

## 3. Tests and checks

- [ ] 3.1 `tests/testthat/test-task-primer.R`: primer is a length-2 integer; deterministic/reproducible for identical `params`; differs for differing `params`
- [ ] 3.2 Encoding tests: `0x80000000` slice ⇒ `NA_integer_`; other slices ⇒ signed int32; matches the `scripts/experiment-dqrng-hash.R` smoke values (`stream_a`/`stream_b`)
- [ ] 3.3 dqrng round-trip (under a `local_dqrng_backend()` scope): same `(seed, params)` ⇒ identical draws; different `params` ⇒ different draws
- [ ] 3.4 Caller-contract demonstration: building `params` with `min_pmix` by name gives stable primers across function-value changes, and omitting `nrow` gives equal primers for differing `nrow` (the contract is enforced/owned by `task-tables`, which builds `params`)
- [ ] 3.5 Stability guard: assert `task_primer()` matches a recorded primer for a fixed `params` (catches an unexpected `rlang::hash()` change)
- [ ] 3.6 Row-normalisation tests: primer from a single-row tibble equals the primer from the unwrapped plain list; tibble attributes/list-columns do not affect it; a row with both a list-column and a df-column unwraps the former and keeps the latter
- [ ] 3.7 Guard validation test: a multi-row data frame and a non-list/non-data-frame value each abort with the user-facing error
- [ ] 3.8 Run `devtools::document()`, `air format .`, and `devtools::check()`; update `NAMESPACE`/`man/`
