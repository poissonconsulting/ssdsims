## 0. dqrng as a conditionally-used Suggested dependency

- [x] 0.1 `DESCRIPTION`: move `dqrng (>= 0.4.1)` from `Imports` to `Suggests`
- [x] 0.2 Add internal `dqrng_usable()` to `R/dqrng-backend.R`: `isNamespaceLoaded("dqrng") && getNamespaceVersion("dqrng") >= "0.4.1"` — gate on *already-loaded*; do **not** use `requireNamespace("dqrng")` or a bare `dqrng::` call (both would load dqrng, the act to avoid)
- [x] 0.3 Gate `set_dqrng_backend()` / `local_dqrng_backend()` on `dqrng_usable()`: when `FALSE`, `chk::abort_chk(..., call = ...)` with actionable guidance (`library(dqrng)`, `>= 0.4.1`); never load dqrng, never fall back to base RNG
- [x] 0.4 Audit every `dqrng::` reference in package code (`set_dqrng_backend`, `reset_dqrng_backend`, `local_dqrng_state`, the witness) so each is reached only behind `dqrng_usable()` (true inside an already-activated backend scope); update examples/vignettes/tests to `library(dqrng)` or `skip_if_not_installed("dqrng")`

## 1. Integrity witness

- [x] 1.1 Add internal `chk_dqrng_backend_intact(call = rlang::caller_call())` to `R/dqrng-backend.R` (peer to `chk_dqrng_backend_active()`): record `dqrng::dqrng_get_state()`, take one base-R `runif(1)` draw, re-read the state, then restore the recorded state via `dqrng::dqrng_set_state()`; return invisibly iff the state advanced, else `chk::abort_chk(..., call = call)` with an actionable message (dqrng is not the bound user-supplied RNG)
- [x] 1.2 Document (in the function's comment block) the distinction from `dqrng_backend_active()`: the `RNGkind()` probe answers "is *a* user-supplied RNG active" (and stays the cheap reentrancy gate for `local_dqrng_backend()`), while `chk_dqrng_backend_intact()` answers "is **dqrng** the active one", referencing `openspec/changes/task-rng-postcheck/exploration/user-rng-conflict/`
- [x] 1.3 Add internal diagnostic helpers for the abort message (lift from `exploration/.../case7-who-owns-rng.R`): `rng_slot_owner()` → `getNativeSymbolInfo("user_unif_rand")$dll[["name"]]` (use `[["name"]]`, **not** `$name` — `DLLInfo` overloads `$` to look up a native symbol); `user_rng_providers()` → the loaded DLLs (`getLoadedDLLs()`) that export `user_unif_rand`
- [x] 1.4 Branch the abort message on `RNGkind()[1]`: if `!= "user-supplied"` report the current `RNGkind()` + "backend was reset" and do **not** name an owner; if `== "user-supplied"` name `rng_slot_owner()` and list `user_rng_providers()`

## 2. Per-task postcondition wiring

- [x] 2.1 Call `chk_dqrng_backend_intact()` as the exit bookend of each `*_data_task_primer()` wrapper (`sample_data_task_primer()`, `fit_data_task_primer()`, `hc_data_task_primer()`) — after the op returns, before the wrapper returns — so entry (`local_dqrng_state`) and exit guards bracket the body
- [x] 2.2 Confirm placement is inside the per-task wrapper (reused by the `targets` shard body and §7 `replay-helper`), not in `ssd_run_scenario_baseline()`, so each task/shard self-verifies

## 3. Tests and checks

- [x] 3.1 `tests/testthat/test-dqrng-backend.R`: under an active backend, `chk_dqrng_backend_intact()` returns invisibly (intact); assert it advanced-then-restored dqrng's state (draw sequence unchanged across the call)
- [x] 3.2 Foreign-hijack test: with a non-dqrng user-supplied RNG bound to the slot while `RNGkind()[1] == "user-supplied"`, `chk_dqrng_backend_intact()` aborts with the expected `chk` error (mirror `exploration/.../case6-witness-vs-hijack.R`; use `skip_if_not_installed("randtoolbox")`)
- [x] 3.3 Torn-down test: with the backend reset to a non-user-supplied generator, `chk_dqrng_backend_intact()` aborts; the message reports the current `RNGkind()` and does not name a symbol owner
- [x] 3.3a Message-content tests: the foreign-hijack abort names the owning package (`randtoolbox`) and lists the loaded user-RNG providers; assert via a snapshot or `expect_match`
- [x] 3.4 Non-destructive test: a seeded draw sequence is byte-identical with and without an intervening `chk_dqrng_backend_intact()` call
- [x] 3.5 Primitives test: each `*_data_task_primer()` returns normally on a healthy backend and aborts when the backend is corrupted before the task ends; error origin is the user-facing frame
- [x] 3.6a Conditional-dependency tests: with dqrng not loaded, `dqrng_usable()` is `FALSE` and `local_dqrng_backend()` (and a scenario run) abort with the `library(dqrng)` guidance and do **not** load dqrng (assert `!isNamespaceLoaded("dqrng")` after the failed call); with dqrng loaded at `>= 0.4.1`, activation succeeds
- [x] 3.6b `DESCRIPTION` test/regression: `dqrng` is in `Suggests`, not `Imports`; loading ssdsims alone does not load dqrng
- [x] 3.6 Run `devtools::document()`, `air format .`, and `devtools::check()`
- [x] 3.7 Confirm `devtools::check()` is clean for the diagnostic helpers: `getNativeSymbolInfo()` and `getLoadedDLLs()` are exported base R functions (not the C-level non-API entry points the "checking compiled code" NOTE concerns), so no NOTE/WARNING is expected — verify none appears, and that the cross-package `user_unif_rand` lookup does not trip the "checking dependencies"/"foreign function calls" checks
