## 1. Dependency

- [ ] 1.1 Add `dqrng (>= 0.4.0)` to `Imports` in `DESCRIPTION` (≥ 0.4.0 for `dqrng_get_state()` / `dqrng_set_state()`)
- [ ] 1.2 Install/refresh the dependency and confirm it loads

## 2. Scoped backend helper

- [ ] 2.1 Add an internal predicate `dqrng_backend_active()` returning `identical(RNGkind()[1], "user-supplied")` (the observable signal that `register_methods()` is in effect; dqrng 0.4.1 exposes no `dqRNGkind()` getter)
- [ ] 2.2 Add `local_dqrng_backend(.local_envir = parent.frame())` in `R/dqrng-backend.R`: if `dqrng_backend_active()`, return a no-op invisibly; otherwise set `dqRNGkind("pcg64")`, `register_methods()`, and `withr::defer(restore_methods(), envir = .local_envir)`
- [ ] 2.3 Validate `.local_envir` with `chk::chk_environment()`; roxygen docs + `@export`, cross-referencing `withr::local_seed()`
- [ ] 2.4 Scenario execution entry point (in `ssd_run_scenario()` or dedicated `ssd_execute_scenario()`) SHALL open a `local_dqrng_backend()` scope at the top so the whole run is backed by dqrng and restored on exit

## 3. Verification

- [ ] 3.1 Confirm `local_dqrng_backend()` activates dqrng: inside a scope `RNGkind()[1] == "user-supplied"`, and it reverts to the pre-scope value after the frame exits
- [ ] 3.2 Confirm base R `runif()`/`sample.int()` are served by dqrng inside the scope and restored after; confirm registered `runif()` matches `dqrunif()` for a fixed `(seed, stream)`
- [ ] 3.3 Reentrancy: a nested `local_dqrng_backend()` call is a no-op and the draw sequence is **identical with vs. without** the nested call
- [ ] 3.4 Run `scripts/experiment-dqrng-hash.R` inside a `local_dqrng_backend()` scope and confirm it still passes
- [ ] 3.5 Verify `dqset.seed(seed, stream)` reproduces the same draw sequence for a fixed `(seed, stream)`; verify `dqrng_get_state()` / `dqrng_set_state()` round-trips a draw sequence exactly

## 4. Tests and docs

- [ ] 4.1 `tests/testthat/test-dqrng-backend.R`: scope activates/restores backend; reentrant nesting transparent to the stream (with == without); leaves base R RNG unchanged after exit; reproducible draws for fixed `(seed, stream)`
- [ ] 4.2 Audit existing RNG-touching tests/snapshots: only those inside a `local_dqrng_backend()` scope should show backend-driven changes; base-RNG path and helpers remain on base R
- [ ] 4.3 Document the dqrng backend in package docs and CLAUDE.md (§RNG discipline); clarify that package load is inert, activation is scope-bound, and nesting is reentrant
- [ ] 4.4 Run `devtools::document()`, `air` formatting, and `devtools::check()`
