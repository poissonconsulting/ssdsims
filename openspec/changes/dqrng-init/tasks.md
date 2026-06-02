## 1. Dependency

- [ ] 1.1 Add `dqrng` to `Imports` in `DESCRIPTION`
- [ ] 1.2 Install/refresh the dependency and confirm it loads

## 2. Load/unload hooks

- [ ] 2.1 Add `R/zzz.R` with `.onLoad()` that calls `dqrng::dqRNGkind("pcg64")` then `dqrng::register_methods()`
- [ ] 2.2 Add `.onUnload()` that calls `dqrng::restore_methods()`
- [ ] 2.3 Factor the set/restore into small internal helpers so tests and scripts can reuse the same discipline (`on.exit(restore_methods())`)

## 3. Verification

- [ ] 3.1 Confirm `dqRNGkind()` reports `pcg64` after load
- [ ] 3.2 Confirm base R `runif()`/`sample.int()` are served by dqrng after load and restored after unload
- [ ] 3.3 Run `scripts/experiment-dqrng-hash.R` and confirm it still passes
- [ ] 3.4 Verify `dqset.seed(seed, stream)` reproduces the same draw sequence for a fixed `(seed, stream)`

## 4. Tests and docs

- [ ] 4.1 `tests/testthat/test-dqrng-backend.R`: backend active after load; reproducible draws for fixed `(seed, stream)`
- [ ] 4.2 Audit existing RNG-touching tests/snapshots for changes caused by the backend switch; update only those genuinely affected
- [ ] 4.3 Document the process-global backend switch and the restore discipline (§9 limitation) in package docs
- [ ] 4.4 Run `devtools::document()`, `air` formatting, and `devtools::check()`
