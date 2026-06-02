## 1. Rename the argument

- [x] 1.1 In `R/dqrng-state.R`, rename `state` → `primer` in `local_dqrng_state(seed, primer, .local_envir = parent.frame())`: the `chk_integer`/`chk_length` calls, `dqrng::dqset.seed(seed, stream = primer)`, and `invisible(primer)`
- [x] 1.2 Rename `state` → `primer` in `with_dqrng_state(seed, primer, code)`
- [x] 1.3 Leave the internal `get_dqrng_state()` / `set_dqrng_state()` (real generator state) and the function names `local_dqrng_state` / `with_dqrng_state` unchanged
- [x] 1.4 Update the roxygen: `@param primer` (drop `@param state`), the `@return` ("invisibly returns `primer`"), and the prose so the argument is described as the primer

## 2. Callers and tests

- [x] 2.1 Update any call sites passing `state =` by name (none in package R/ at present; check tests)
- [x] 2.2 Update `tests/testthat/test-dqrng-state.R` to the `primer` argument (named calls, messages/snapshots if any)

## 3. Checks

- [x] 3.1 Run `devtools::document()` (refresh `man/local_dqrng_state.Rd`), `air format .`, `devtools::test(filter = "dqrng-state")`, `devtools::check()` (0/0), `pkgdown::check_pkgdown()`
