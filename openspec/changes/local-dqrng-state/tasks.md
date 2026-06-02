## 1. State capture/restore internals

- [ ] 1.1 Add internal dqrng get/set-state helpers in `R/dqrng-state.R` (or co-located with the `dqrng-init` backend wiring): capture the active dqrng RNG state and reinstate it
- [ ] 1.2 Choose the snapshot mechanism that round-trips a draw sequence exactly (dqrng get/set-state, or re-seed to a captured `(seed, state)`); document the choice inline

## 2. local_dqrng_state()

- [ ] 2.1 Add `local_dqrng_state(seed, state, .local_envir = parent.frame())` wrapping `dqrng::dqset.seed(seed, stream = state)`
- [ ] 2.2 Capture RNG state on entry, install the new `(seed, state)`, and `withr::defer(restore, envir = .local_envir)` to restore on frame exit (mirror `local_lecuyer_cmrg_state()`)
- [ ] 2.3 Validate inputs with `chk`: whole-number `seed`; length-2 integer `state` allowing `NA_integer_` (the §2 INT_MIN encoding); `.local_envir` an environment
- [ ] 2.4 Roxygen docs + `@export`; `@seealso` `withr::local_seed()` and `local_lecuyer_cmrg_state()`

## 3. Optional with_dqrng_state() companion

- [ ] 3.1 Add `with_dqrng_state(seed, state, code)` delegating to `local_dqrng_state()` (only if it reduces churn for `state-primitives`); roxygen + `@export`

## 4. Tests and docs

- [ ] 4.1 `tests/testthat/test-dqrng-state.R`: state installed for the frame and restored on exit (RNG state unchanged across a `local_dqrng_state()` scope), under the active dqrng backend (activate via `dqrng-init` helpers with `on.exit(restore)`)
- [ ] 4.2 Reproducibility: same `(seed, state)` reproduces identical draw sequences
- [ ] 4.3 Surrounding-stream test: draws → nested `local_dqrng_state()` scope that draws → outer stream continues byte-identically to a no-nested-scope control
- [ ] 4.4 Validation tests: non-whole-number `seed`, malformed `state`, and acceptance of `NA_integer_` in `state`
- [ ] 4.5 Run `devtools::document()`, `air format`, and `devtools::check()`; update `NAMESPACE`/`man/`
