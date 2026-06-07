> **Scope:** keep only the anti-hijack runtime witness (PR #117 feedback). `dqrng`
> stays in `Imports`; the "good-citizen" `Imports`→`Suggests` move, the
> `dqrng_usable()` gate, and the `library(dqrng)` example/vignette/scaffold
> friction are **dropped**. See the *keep dqrng in Imports* decision in `design.md`.

## 1. Integrity witness

- [x] 1.1 Add internal `chk_dqrng_backend_intact(call = rlang::caller_call())` to `R/dqrng-backend.R` (peer to `chk_dqrng_backend_active()`): record `dqrng::dqrng_get_state()`, take one base-R `runif(1)` draw, re-read the state, then restore the recorded state via `dqrng::dqrng_set_state()`; return invisibly iff the state advanced, else `chk::abort_chk(..., call = call)` with an actionable message (dqrng is not the bound user-supplied RNG)
- [x] 1.2 Document (in the function's comment block) the distinction from `dqrng_backend_active()`: the `RNGkind()` probe answers "is *a* user-supplied RNG active" (and stays the cheap reentrancy gate for `local_dqrng_backend()`), while `chk_dqrng_backend_intact()` answers "is **dqrng** the active one", referencing `openspec/changes/task-rng-postcheck/exploration/user-rng-conflict/`
- [x] 1.3 Add internal diagnostic helpers for the abort message (lift from `exploration/.../case7-who-owns-rng.R`): `rng_slot_owner()` → `getNativeSymbolInfo("user_unif_rand")$dll[["name"]]` (use `[["name"]]`, **not** `$name` — `DLLInfo` overloads `$` to look up a native symbol); `user_rng_providers()` → the loaded DLLs (`getLoadedDLLs()`) that export `user_unif_rand`
- [x] 1.4 Branch the abort message on `RNGkind()[1]`: if `!= "user-supplied"` report the current `RNGkind()` + "backend was reset" and do **not** name an owner; if `== "user-supplied"` name `rng_slot_owner()` and list `user_rng_providers()`

## 2. Bracket every task's draws — both brackets inside `local_dqrng_state()`

- [x] 2.1 **Entry precondition:** upgrade `local_dqrng_state()`'s guard (`R/dqrng-state.R`) from `chk_dqrng_backend_active()` to `chk_dqrng_backend_intact()` (called before `dqset.seed()`, which overwrites the state, so the entry witness leaves the seed untouched), so a task refuses to *start* on a foreign-hijacked or torn-down backend
- [x] 2.2 **Exit postcondition:** in `local_dqrng_state()`, register a deferred witness on `.local_envir` alongside the existing `withr::defer(set_dqrng_state(old), ...)`, gated to the success path: `sentinel <- new.env(); withr::defer(if (!identical(returnValue(sentinel), sentinel)) chk_dqrng_backend_intact(), envir = .local_envir)`. The three `*_data_task_primer()` wrappers and `with_dqrng_state()` are **unchanged** (they already call `local_dqrng_state()`)
- [x] 2.3 Retain `dqrng_backend_active()` / `chk_dqrng_backend_active()` as `local_dqrng_backend()`'s reentrancy no-op gate (it must stay cheap and side-effect-free; re-asserting `register_methods()` reseeds)
- [x] 2.4 Confirm placement is inside `local_dqrng_state()` (per-task, reused by the `targets` shard body and §7 `replay-helper`), not in `ssd_run_scenario_baseline()` and not on `local_dqrng_backend()` (per-scenario), so each task/shard self-verifies and the offending task is localised

## 3. Tests and checks

- [x] 3.1 `tests/testthat/test-dqrng-backend.R`: under an active backend, `chk_dqrng_backend_intact()` returns invisibly (intact); assert it advanced-then-restored dqrng's state (draw sequence unchanged across the call)
- [x] 3.2 Torn-down test: with the backend reset to a non-user-supplied generator, `chk_dqrng_backend_intact()` aborts; the message reports the current `RNGkind()` and does not name a symbol owner (reachable without a foreign user-RNG package)
- [x] 3.3 Non-destructive test: a seeded draw sequence is byte-identical with and without an intervening `chk_dqrng_backend_intact()` call
- [x] 3.4 Entry-guard test: `local_dqrng_state()` aborts on entry when the backend is not intact (use the torn-down case — no foreign package needed)
- [x] 3.4a Exit-postcondition test: a task that completes normally with the backend torn down before return aborts via the deferred witness; on a healthy backend the deferred witness passes and the result returns (covers `local_dqrng_state()` / `with_dqrng_state()` directly)
- [x] 3.4b On-error skip test: a task body that `stop()`s while the backend is torn down propagates its **own** error (the deferred exit witness is skipped on the error unwind via the `returnValue()` gate, so the witness abort does not mask it)
- [x] 3.5 Primitives test: each `*_data_task_primer()` returns normally on a healthy backend and aborts when the backend is corrupted before a normal return; error origin is the user-facing frame
- [x] 3.6 Run `devtools::document()`, `air format .`, and `devtools::check()`
- [x] 3.7 Confirm `devtools::check()` is clean for the diagnostic helpers: `getNativeSymbolInfo()` and `getLoadedDLLs()` are exported base R functions (not the C-level non-API entry points the "checking compiled code" NOTE concerns), so no NOTE/WARNING is expected — verify none appears, and that the cross-package `user_unif_rand` lookup does not trip the "checking dependencies"/"foreign function calls" checks

> **Validated by the exploration reprexes, not in-suite** (no test-only `randtoolbox`/`callr`
> dependency; co-loading a second user-RNG package in the test process is itself the hazard
> this change guards against): the **foreign-hijack** path (`case6` — the cheap probe is fooled
> but the state witness is not) and the **message-content** path (`case7` — naming the slot
> owner and listing the loaded user-RNG providers). See `exploration/user-rng-conflict/`.
