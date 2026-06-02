## 1. State capture/restore internals

- [ ] 1.1 Use `dqrng::dqrng_get_state()` to capture and `dqrng::dqrng_set_state()` to reinstate the active dqrng generator state (≥ 0.4.0); confirmed faithful on 0.4.1

## 2. local_dqrng_state()

- [ ] 2.1 Add `local_dqrng_state(seed, state, .local_envir = parent.frame())` wrapping `dqrng::dqset.seed(seed, stream = state)`
- [ ] 2.2 Backend guard: abort with an actionable message (point to `local_dqrng_backend()`) unless `dqrng_backend_active()` (the helper from `dqrng-init`)
- [ ] 2.3 Capture state via `dqrng_get_state()` on entry, install the new `(seed, state)`, and `withr::defer(dqrng_set_state(old), envir = .local_envir)` to restore on frame exit (mirror `local_lecuyer_cmrg_state()`)
- [ ] 2.4 Validate inputs with `chk`: whole-number `seed`; length-2 integer `state` allowing `NA_integer_` (the §2 INT_MIN encoding); `.local_envir` an environment
- [ ] 2.5 Roxygen docs + `@export`; `@seealso` `withr::local_seed()`, `local_dqrng_backend()`, and `local_lecuyer_cmrg_state()`

## 3. Optional with_dqrng_state() companion

- [ ] 3.1 Add `with_dqrng_state(seed, state, code)` delegating to `local_dqrng_state()` (only if it reduces churn for `state-primitives`); roxygen + `@export`

## 4. Tests and docs

- [ ] 4.1 `tests/testthat/test-dqrng-state.R`: state installed for the frame and restored on exit (RNG state unchanged across a `local_dqrng_state()` scope), exercised inside a `local_dqrng_backend()` scope
- [ ] 4.2 Reproducibility: same `(seed, state)` reproduces identical draw sequences
- [ ] 4.3 Surrounding-stream test: draws → nested `local_dqrng_state()` scope that draws → outer stream continues byte-identically to a no-nested-scope control
- [ ] 4.4 Backend-guard test: calling `local_dqrng_state()` outside a `local_dqrng_backend()` scope aborts with the actionable error
- [ ] 4.5 Validation tests: non-whole-number `seed`, malformed `state`, and acceptance of `NA_integer_` in `state`
- [ ] 4.6 Run `devtools::document()`, `air format`, and `devtools::check()`; update `NAMESPACE`/`man/`
