> **Scope:** keep only the anti-hijack runtime witness (PR #117 feedback). `dqrng`
> stays in `Imports`; the "good-citizen" `Imports`→`Suggests` move, the
> `dqrng_usable()` gate, and the `library(dqrng)` example/vignette/scaffold
> friction are **dropped**. See the *keep dqrng in Imports* decision in `design.md`.

## 1. Integrity witness

- [ ] 1.1 Add internal `chk_dqrng_backend_intact(call = rlang::caller_call())` to `R/dqrng-backend.R` (peer to `chk_dqrng_backend_active()`): record `dqrng::dqrng_get_state()`, take one base-R `runif(1)` draw, re-read the state, then restore the recorded state via `dqrng::dqrng_set_state()`; return invisibly iff the state advanced, else `chk::abort_chk(..., call = call)` with an actionable message (dqrng is not the bound user-supplied RNG)
- [ ] 1.2 Document (in the function's comment block) the distinction from `dqrng_backend_active()`: the `RNGkind()` probe answers "is *a* user-supplied RNG active" (and stays the cheap reentrancy gate for `local_dqrng_backend()`), while `chk_dqrng_backend_intact()` answers "is **dqrng** the active one", referencing `openspec/changes/task-rng-postcheck/exploration/user-rng-conflict/`
- [ ] 1.3 Add internal diagnostic helpers for the abort message (lift from `exploration/.../case7-who-owns-rng.R`): `rng_slot_owner()` → `getNativeSymbolInfo("user_unif_rand")$dll[["name"]]` (use `[["name"]]`, **not** `$name` — `DLLInfo` overloads `$` to look up a native symbol); `user_rng_providers()` → the loaded DLLs (`getLoadedDLLs()`) that export `user_unif_rand`
- [ ] 1.4 Branch the abort message on `RNGkind()[1]`: if `!= "user-supplied"` report the current `RNGkind()` + "backend was reset" and do **not** name an owner; if `== "user-supplied"` name `rng_slot_owner()` and list `user_rng_providers()`

## 2. Bracket every task's draws (entry + exit)

- [ ] 2.1 **Exit bookend:** call `chk_dqrng_backend_intact()` at the end of each `*_data_task_primer()` wrapper (`sample_data_task_primer()`, `fit_data_task_primer()`, `hc_data_task_primer()`) — after the op returns, before the wrapper returns
- [ ] 2.2 **Entry precondition:** upgrade `local_dqrng_state()`'s guard from `chk_dqrng_backend_active()` to `chk_dqrng_backend_intact()` (called before `dqset.seed()`, which overwrites the state, so the entry witness leaves the seed untouched), so a task refuses to *start* on a foreign-hijacked or torn-down backend
- [ ] 2.3 Retain `dqrng_backend_active()` / `chk_dqrng_backend_active()` as `local_dqrng_backend()`'s reentrancy no-op gate (it must stay cheap and side-effect-free; re-asserting `register_methods()` reseeds)
- [ ] 2.4 Confirm placement is inside the per-task wrapper / `local_dqrng_state()` (reused by the `targets` shard body and §7 `replay-helper`), not in `ssd_run_scenario_baseline()`, so each task/shard self-verifies

## 3. Tests and checks

- [ ] 3.1 `tests/testthat/test-dqrng-backend.R`: under an active backend, `chk_dqrng_backend_intact()` returns invisibly (intact); assert it advanced-then-restored dqrng's state (draw sequence unchanged across the call)
- [ ] 3.2 Torn-down test: with the backend reset to a non-user-supplied generator, `chk_dqrng_backend_intact()` aborts; the message reports the current `RNGkind()` and does not name a symbol owner (reachable without a foreign user-RNG package)
- [ ] 3.3 Non-destructive test: a seeded draw sequence is byte-identical with and without an intervening `chk_dqrng_backend_intact()` call
- [ ] 3.4 Entry-guard test: `local_dqrng_state()` aborts on entry when the backend is not intact (use the torn-down case — no foreign package needed)
- [ ] 3.5 Primitives test: each `*_data_task_primer()` returns normally on a healthy backend and aborts when the backend is corrupted before the task ends; error origin is the user-facing frame
- [ ] 3.6 Run `devtools::document()`, `air format .`, and `devtools::check()`
- [ ] 3.7 Confirm `devtools::check()` is clean for the diagnostic helpers: `getNativeSymbolInfo()` and `getLoadedDLLs()` are exported base R functions (not the C-level non-API entry points the "checking compiled code" NOTE concerns), so no NOTE/WARNING is expected — verify none appears, and that the cross-package `user_unif_rand` lookup does not trip the "checking dependencies"/"foreign function calls" checks

> **Validated by the exploration reprexes, not in-suite** (no test-only `randtoolbox`/`callr`
> dependency; co-loading a second user-RNG package in the test process is itself the hazard
> this change guards against): the **foreign-hijack** path (`case6` — the cheap probe is fooled
> but the state witness is not) and the **message-content** path (`case7` — naming the slot
> owner and listing the loaded user-RNG providers). See `exploration/user-rng-conflict/`.
