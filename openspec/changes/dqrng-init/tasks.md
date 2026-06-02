## 1. Dependency

- [x] 1.1 Add `dqrng` to `Imports` in `DESCRIPTION`
- [x] 1.2 Install/refresh the dependency and confirm it loads

## 2. Scoped backend helper

- [x] 2.1 Add helpers in `R/dqrng-backend.R`: internal `set_dqrng_backend()` (calls `dqRNGkind("pcg64")` + `register_methods()`) and `reset_dqrng_backend()` (calls `restore_methods()`), plus an exported withr-style `local_dqrng_backend(.local_envir = parent.frame())` that defers `reset_dqrng_backend()` to the calling frame's exit
- [x] 2.2 Tests and scripts that touch dqrng mid-session shall use `local_dqrng_backend()` (or `set_dqrng_backend()` paired with `on.exit(reset_dqrng_backend())`) to ensure the backend is reset on exit
- [x] 2.3 Scenario execution entry point (in `ssd_run_scenario()` or dedicated `ssd_execute_scenario()`) SHALL call `local_dqrng_backend()` at start so the backend is reset on exit, including on error

## 3. Verification

- [x] 3.1 Confirm scenario execution activates dqrng: use `local_dqrng_backend()` (or `set_dqrng_backend()`/`reset_dqrng_backend()`) and verify the pcg64 backend serves base R RNG mid-execution and is reset afterward
- [x] 3.2 Confirm base R `runif()`/`sample.int()` are served by dqrng during scenario execution and reset after
- [x] 3.3 Run `scripts/experiment-dqrng-hash.R` with scenario execution guards and confirm it still passes
- [x] 3.4 Verify `dqset.seed(seed, stream)` reproduces the same draw sequence for a fixed `(seed, stream)` during execution

## 4. Tests and docs

- [x] 4.1 `tests/testthat/test-dqrng-backend.R`: helpers activate/reset backend correctly; `local_dqrng_backend()` resets on scope exit; scenario-scoped execution leaves base R RNG unchanged; reproducible draws for fixed `(seed, stream)` under dqrng
- [x] 4.2 Audit existing RNG-touching tests/snapshots: found one brittle *unseeded* `run_scenario()` coef test (sensitive to the `.Random.seed` shift from `register_methods()`); pinned its seed in a separate, forward-portable commit so the backend changes no snapshots. Base-RNG path and helpers remain on base R.
- [x] 4.3 Document the dqrng backend in package docs and CLAUDE.md (§RNG discipline); clarify that package load is inert and registration is scenario-scoped
- [x] 4.4 Run `devtools::document()`, `air` formatting, and `devtools::check()`

## 5. Reentrant nesting (follow-up)

Adds the nesting guarantee on top of the implemented backend: `register_methods()` / `restore_methods()` keep a single global save-slot, so a nested `reset_dqrng_backend()` would tear the backend down for the still-open outer scope. The fix is to make activation reentrant so a nested `local_dqrng_backend()` call is a no-op and the RNG stream is identical with or without it (verified against dqrng 0.4.1).

- [x] 5.1 Add an internal predicate `dqrng_backend_active()` returning `identical(RNGkind()[1], "user-supplied")` (the observable signal that `register_methods()` is in effect; dqrng 0.4.1 exposes no `dqRNGkind()` getter)
- [x] 5.2 Make `local_dqrng_backend()` reentrant: if `dqrng_backend_active()`, return a no-op invisibly (no re-`set`, no deferred `reset`); only the outermost scope registers on entry and resets on exit
- [x] 5.3 Reentrancy test in `test-dqrng-backend.R`: a nested `local_dqrng_backend()` call is a no-op and the draw sequence is **identical with vs. without** the nested call; outer backend stays active until the outermost scope exits
- [x] 5.4 Note the reentrant nesting contract in the docs and CLAUDE.md; re-run `devtools::document()`, `air` formatting, and `devtools::check()`
