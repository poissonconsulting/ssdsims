## 1. Integrity witness

- [ ] 1.1 Add internal `chk_dqrng_backend_intact(call = rlang::caller_call())` to `R/dqrng-backend.R` (peer to `chk_dqrng_backend_active()`): record `dqrng::dqrng_get_state()`, take one base-R `runif(1)` draw, re-read the state, then restore the recorded state via `dqrng::dqrng_set_state()`; return invisibly iff the state advanced, else `chk::abort_chk(..., call = call)` with an actionable message (dqrng is not the bound user-supplied RNG)
- [ ] 1.2 Document (in the function's comment block) the distinction from `dqrng_backend_active()`: the `RNGkind()` probe answers "is *a* user-supplied RNG active" (and stays the cheap reentrancy gate for `local_dqrng_backend()`), while `chk_dqrng_backend_intact()` answers "is **dqrng** the active one", referencing `openspec/changes/task-rng-postcheck/exploration/user-rng-conflict/`

## 2. Per-task postcondition wiring

- [ ] 2.1 Call `chk_dqrng_backend_intact()` as the exit bookend of each `*_data_task_primer()` wrapper (`sample_data_task_primer()`, `fit_data_task_primer()`, `hc_data_task_primer()`) — after the op returns, before the wrapper returns — so entry (`local_dqrng_state`) and exit guards bracket the body
- [ ] 2.2 Confirm placement is inside the per-task wrapper (reused by the `targets` shard body and §7 `replay-helper`), not in `ssd_run_scenario_baseline()`, so each task/shard self-verifies

## 3. Tests and checks

- [ ] 3.1 `tests/testthat/test-dqrng-backend.R`: under an active backend, `chk_dqrng_backend_intact()` returns invisibly (intact); assert it advanced-then-restored dqrng's state (draw sequence unchanged across the call)
- [ ] 3.2 Foreign-hijack test: with a non-dqrng user-supplied RNG bound to the slot while `RNGkind()[1] == "user-supplied"`, `chk_dqrng_backend_intact()` aborts with the expected `chk` error (mirror `exploration/.../case6-witness-vs-hijack.R`; use `skip_if_not_installed("randtoolbox")`)
- [ ] 3.3 Torn-down test: with the backend reset to a non-user-supplied generator, `chk_dqrng_backend_intact()` aborts
- [ ] 3.4 Non-destructive test: a seeded draw sequence is byte-identical with and without an intervening `chk_dqrng_backend_intact()` call
- [ ] 3.5 Primitives test: each `*_data_task_primer()` returns normally on a healthy backend and aborts when the backend is corrupted before the task ends; error origin is the user-facing frame
- [ ] 3.6 Run `devtools::document()`, `air format .`, and `devtools::check()`
