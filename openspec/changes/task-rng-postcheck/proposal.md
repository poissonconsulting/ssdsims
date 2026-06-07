## Why

The `exploration/user-rng-conflict/` findings (dqrng vs `randtoolbox`, validated on R 4.5.3 / dqrng 0.4.1 / randtoolbox 2.0.5) show that base R has a **single process-global user-supplied RNG slot** keyed on the C symbol `user_unif_rand`, resolved across *all* loaded DLLs to the most-recently-loaded match. ssdsims activates dqrng's pcg64 backend per scenario and routes every per-task draw through that slot — but the slot is **last-loaded-wins**: a second user-RNG package (e.g. `randtoolbox`) loaded into the session after ssdsims silently takes the slot (case2), and re-registering can reseed a live stream.

Every RNG-consuming task draws inside a `local_dqrng_backend()` scope, and `local_dqrng_state()` guards the **entry** with `chk_dqrng_backend_active()`. But that probe is the cheap `RNGkind()[1] == "user-supplied"` test, which answers *"is **a** user RNG active"*, not *"is **dqrng** active"* — so it is **fooled by a foreign hijack** (case6) and never re-checks during the body. A mid-task teardown (→ silent Mersenne-Twister fallback) or a foreign hijack (→ wrong generator) therefore corrupts a task's draws with no signal.

base R's last-loaded-wins rule means ssdsims **cannot prevent** another user-RNG package from taking the slot. What it can do is **detect** corruption at runtime and refuse to emit silently-wrong draws. case5 shows the witness: dqrng's **own state** is the evidence — a base `runif()` draw *must* advance dqrng's internal state iff dqrng is bound; rolled back via `dqrng_set_state()`, so the check is non-destructive. This change adds that witness and brackets every task's draws with it on entry and exit.

> **Scope note.** An earlier revision of this change also tried to make ssdsims a "good citizen" by moving `dqrng` from `Imports` to `Suggests` (so loading ssdsims would not load dqrng), gating every `dqrng::` touch on `dqrng_usable()`, and aborting with `library(dqrng)` guidance. That half was **dropped** (PR #117 feedback): because the slot is last-loaded-wins, *not* being the one who loads a user-RNG buys politeness, not safety — if any user-RNG package is in play the slot is taken regardless. The friction (conditional `@examplesIf`, `library(dqrng)` in vignettes/scaffolds/test fixtures, the `dqrng_usable()` gate threaded through every helper) was not worth a guarantee base R will not let ssdsims keep. **`dqrng` stays in `Imports`**; the value is the runtime witness.

## What Changes

- Add an internal `chk_dqrng_backend_intact(call = ...)` (peer to `chk_dqrng_backend_active()`, in `R/dqrng-backend.R`) that:
  - records `dqrng::dqrng_get_state()`, takes one base-R `runif(1)` draw, re-reads the state, then restores the recorded state via `dqrng::dqrng_set_state()` (non-destructive);
  - returns invisibly iff the state advanced (dqrng is the bound generator); otherwise **aborts** (chk-style), reporting in the user-facing frame.
  - on failure, emits a **diagnostic** message: it names the package that owns the `user_unif_rand` symbol (`getNativeSymbolInfo("user_unif_rand")$dll[["name"]]`) and lists the loaded packages that provide a user-supplied RNG (`getLoadedDLLs()` exporting that symbol), branching on `RNGkind()[1]` to distinguish a foreign hijack (name the owner) from a torn-down backend (report the current `RNGkind()`, name no owner).
- **Bracket every task's draws with the witness on both ends — both inside `local_dqrng_state()`.** It is already a withr-style wrapper that `withr::defer()`s a state-restore onto the task frame (`R/dqrng-state.R`), so the bracket rides that existing machinery with **no per-wrapper wiring**:
  - **Entry:** upgrade `local_dqrng_state()`'s guard from the cheap `chk_dqrng_backend_active()` probe to the full `chk_dqrng_backend_intact()` witness, so a task refuses to *start* on a foreign-hijacked or torn-down backend.
  - **Exit:** add a deferred witness alongside the existing state-restore defer, gated to the **success path** so a failing task body's error is never masked: `withr::defer(if (!identical(returnValue(sentinel), sentinel)) chk_dqrng_backend_intact(), envir = .local_envir)`, where `sentinel <- new.env()` is a per-call unique object. `base::returnValue(default)` returns the frame's return value on normal exit and `default` on error/non-local unwinding, so the witness runs only when the task returned normally. The three `*_data_task_primer()` wrappers (and `with_dqrng_state()`) are **unchanged** — they already call `local_dqrng_state()`, and the bracket lives with the seeding rather than scattered across the wrappers. No separate per-task check function/target is needed; it rides into the `targets` shard node and the §7 replay helper for free.
- The cheap `dqrng_backend_active()` / `chk_dqrng_backend_active()` probe is **retained** solely as the reentrancy no-op gate inside `local_dqrng_backend()` (re-asserting `register_methods()` reseeds, so that gate must stay cheap and side-effect-free) — it is not the integrity assertion.
- Document the distinction: `dqrng_backend_active()` (the `RNGkind()` probe) answers "is *a* user-supplied RNG active"; `chk_dqrng_backend_intact()` answers "is **dqrng** the active one".

## Capabilities

### New Capabilities
<!-- None: extends existing capabilities. -->

### Modified Capabilities
- `dqrng-backend`: add the dqrng-specific backend **integrity witness** (`chk_dqrng_backend_intact()`) — a non-destructive state-advance check that verifies dqrng (not merely *some* user-supplied RNG) holds base R's `user_unif_rand` slot, aborting with a diagnostic message otherwise.
- `parallel-safe-seeding`: bracket every RNG-consuming task's draws with the integrity witness — verify the dqrng backend is intact both when the task *starts* (entry precondition, upgraded from the cheap probe) and when it *ends* (exit postcondition), so a mid-task teardown or foreign-RNG hijack aborts the task instead of silently producing non-dqrng draws.

## Impact

- **New code**: `chk_dqrng_backend_intact()` and the diagnostic helpers (`rng_slot_owner()`, `user_rng_providers()`) in `R/dqrng-backend.R`; in `R/dqrng-state.R`, `local_dqrng_state()`'s entry guard upgraded to the witness and a `returnValue()`-gated deferred exit witness added alongside the existing state-restore defer. The `*_data_task_primer()` wrappers (`R/task-lists.R`) are **unchanged**. Tests in `tests/testthat/test-dqrng-backend.R` / `test-dqrng-state.R`.
- **APIs**: internal only — no public surface change, and **no behavioural change for callers**: `dqrng` stays in `Imports`, so it is still auto-loaded with ssdsims and the witness is always able to run.
- **Dependencies**: **none changed** — `dqrng` stays in `Imports` (`>= 0.4.1`). Uses dqrng's already-exported `dqrng_get_state()` / `dqrng_set_state()`. Builds on `primer-primitives` (**landed**, archived `2026-06-04-primer-primitives`); orthogonal to `task-primer` (also landed).
- **Downstream**: the brackets ride inside `local_dqrng_state()`, so they carry into the `targets` shard body and the §7 `replay-helper` for free — each shard self-verifies its RNG backend in its own process, where an independently-loaded foreign package is exactly the risk.
- **Exploration**: `exploration/user-rng-conflict/` (the dqrng-vs-randtoolbox reprexes that motivated and validated this) lives in this change.
