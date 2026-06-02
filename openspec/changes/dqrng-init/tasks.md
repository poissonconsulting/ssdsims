## 1. Dependency

- [ ] 1.1 Add `dqrng` to `Imports` in `DESCRIPTION`
- [ ] 1.2 Install/refresh the dependency and confirm it loads

## 2. Internal scenario execution helpers

- [ ] 2.1 Add internal helpers in `R/zzz.R` or appropriate module: `set_dqrng_backend()` (calls `dqRNGkind("pcg64")` + `register_methods()`) and `restore_dqrng_backend()` (calls `restore_methods()`)
- [ ] 2.2 Tests and scripts that touch dqrng mid-session shall wrap with `on.exit(restore_dqrng_backend())` to ensure clean restoration
- [ ] 2.3 Scenario execution entry point (in `ssd_run_scenario()` or dedicated `ssd_execute_scenario()`) SHALL call `set_dqrng_backend()` at start and guard restoration with `on.exit(restore_dqrng_backend())`

## 3. Verification

- [ ] 3.1 Confirm scenario execution activates dqrng: wrap `set_dqrng_backend()`/`restore_dqrng_backend()` and verify `dqRNGkind()` reports `pcg64` mid-execution and reverts afterward
- [ ] 3.2 Confirm base R `runif()`/`sample.int()` are served by dqrng during scenario execution and restored after
- [ ] 3.3 Run `scripts/experiment-dqrng-hash.R` with scenario execution guards and confirm it still passes
- [ ] 3.4 Verify `dqset.seed(seed, stream)` reproduces the same draw sequence for a fixed `(seed, stream)` during execution

## 4. Tests and docs

- [ ] 4.1 `tests/testthat/test-dqrng-backend.R`: helpers activate/restore backend correctly; scenario-scoped execution leaves base R RNG unchanged; reproducible draws for fixed `(seed, stream)` under dqrng
- [ ] 4.2 Audit existing RNG-touching tests/snapshots: only those called within scenario execution contexts should show backend-driven changes; base-RNG path and helpers remain on base R
- [ ] 4.3 Document the dqrng backend in package docs and CLAUDE.md (§RNG discipline); clarify that package load is inert and registration is scenario-scoped
- [ ] 4.4 Run `devtools::document()`, `air` formatting, and `devtools::check()`
