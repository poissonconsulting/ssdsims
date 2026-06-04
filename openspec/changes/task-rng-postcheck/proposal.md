## Why

The `exploration/user-rng-conflict/` findings (dqrng vs `randtoolbox`) show that **loading a package that registers a user-supplied RNG is a potentially destructive act**: base R has a single process-global user-supplied RNG slot keyed on the C symbol `user_unif_rand`, resolved across *all* loaded DLLs to the most-recently-loaded match. A second such package in the session silently hijacks base `runif()` (case2), can crash on an uninitialised generator (case3), and re-registering can reseed a live stream. Two consequences for ssdsims:

**(A) ssdsims must not *impose* a user-RNG provider on every session.** Today `dqrng` is in `Imports`, so it loads whenever ssdsims loads — forcing dqrng's `user_unif_rand` DLL into sessions that may also use `randtoolbox` (or any user-RNG package), creating the collision hazard uninvited, even for callers using non-RNG functionality. ssdsims should be a good citizen: use dqrng only when the *user* has already chosen to load it.

**(B) ssdsims must detect a hijack or teardown at runtime.** Every RNG-consuming task draws inside a `local_dqrng_backend()` scope; `local_dqrng_state()` guards the **entry** with `chk_dqrng_backend_active()`, but that only confirms the backend at task *start*. The draws happen in the body, and the cheap `RNGkind()[1] == "user-supplied"` probe is *fooled* by a foreign hijack (case6) — it answers "is *a* user RNG active", not "is *dqrng* active". So a mid-task teardown (→ silent Mersenne-Twister fallback) or foreign hijack (→ wrong generator) corrupts the draws with no signal. case5 shows the fix: dqrng's **own state** is the witness — a base `runif()` draw *must* advance dqrng's internal state iff dqrng is bound; rolled back via `dqrng_set_state()`, so the check is non-destructive.

This change addresses both: make dqrng a **conditionally-used Suggested** dependency (A), and add a per-task **postcondition** that verifies dqrng is still intact when each task ends (B).

## What Changes

**(A) dqrng becomes a conditionally-used Suggested dependency.**

- Move `dqrng` from `Imports` to `Suggests` (still `>= 0.4.1`) in `DESCRIPTION`, so loading ssdsims does not load dqrng.
- Add an internal `dqrng_usable()` guard: `TRUE` iff `isNamespaceLoaded("dqrng")` **and** `getNamespaceVersion("dqrng") >= "0.4.1"`. Crucially it tests *already-loaded*, not `requireNamespace()` — because `dqrng::foo()` and `requireNamespace()` would themselves *load* dqrng, which is the act we must not trigger. **Every** dqrng touch (the `dqrng::` calls in `set_dqrng_backend()`, `local_dqrng_state()`, the witness) is gated behind this.
- When the RNG backend is required but `dqrng_usable()` is `FALSE`, **abort** with actionable guidance — `library(dqrng)` (>= 0.4.1) first. ssdsims never loads dqrng itself; the reproducible dqrng path is opt-in by loading dqrng. (Chosen over a silent base-RNG fallback so reproducibility is never quietly lost.)

**(B) Per-task dqrng-integrity postcondition.**

- Add an internal `chk_dqrng_backend_intact(call = ...)` (peer to `chk_dqrng_backend_active()`, in `R/dqrng-backend.R`) that:
  - records `dqrng::dqrng_get_state()`, takes one base-R `runif(1)` draw, re-reads the state, then restores the recorded state via `dqrng::dqrng_set_state()` (non-destructive);
  - returns invisibly iff the state advanced (dqrng is the bound generator); otherwise **aborts** (chk-style), symmetric with the entry guard, reporting in the user-facing frame.
  - on failure, emits a **diagnostic** message: it names the package that owns the `user_unif_rand` symbol (`getNativeSymbolInfo("user_unif_rand")$dll[["name"]]`) and lists the loaded packages that provide a user-supplied RNG (`getLoadedDLLs()` exporting that symbol), branching on `RNGkind()[1]` to distinguish a foreign hijack (name the owner) from a torn-down backend (report the current `RNGkind()`, name no owner).
- Wire it into each per-task seed-and-run wrapper from `primer-primitives` (`sample_data_task_primer()`, `fit_data_task_primer()`, `hc_data_task_primer()`) as the **exit bookend**: the wrapper runs the task body, then asserts the backend is intact before returning. Entry guard (`local_dqrng_state` → `chk_dqrng_backend_active`) and exit guard (this) bracket every task's draws.
- Document the limit the witness removes: `dqrng_backend_active()` (the `RNGkind()` probe) answers "is *a* user-supplied RNG active"; `chk_dqrng_backend_intact()` answers "is **dqrng** the active one". The former stays the reentrancy gate for `local_dqrng_backend()`; the latter is the per-task integrity assertion.

## Capabilities

### New Capabilities
<!-- None: extends existing capabilities. -->

### Modified Capabilities
- `dqrng-backend`:
  - make dqrng a **Suggested, conditionally-used** dependency — declared in `Suggests` (`>= 0.4.1`), never loaded by ssdsims, used only when already loaded (gated on `isNamespaceLoaded("dqrng")` + version); the backend helpers abort with actionable guidance when it is required but unavailable.
  - add the dqrng-specific backend **integrity witness** (`chk_dqrng_backend_intact()`) — a non-destructive state-advance check that verifies dqrng (not merely *some* user-supplied RNG) holds base R's `user_unif_rand` slot, aborting otherwise.
- `parallel-safe-seeding`: add the per-task **postcondition** — each RNG-consuming task verifies the dqrng backend is intact when the task ends, so a mid-task teardown or foreign-RNG hijack aborts the task instead of silently producing non-dqrng draws.

## Impact

- **New code**: `dqrng_usable()` and `chk_dqrng_backend_intact()` in `R/dqrng-backend.R`; the `dqrng_usable()` gate threaded through `set_dqrng_backend()` / `local_dqrng_backend()` / `local_dqrng_state()`; exit-check wiring in the `*_data_task_primer()` wrappers (`R/task-lists.R`); tests in `tests/testthat/test-dqrng-backend.R` and the primitives test.
- **APIs**: internal only — no public surface change. Behavioural change: a scenario run now requires the caller to have `library(dqrng)` loaded (>= 0.4.1), else an actionable abort.
- **Dependencies**: `dqrng` moves `Imports` → `Suggests` (`>= 0.4.1`). Uses dqrng's already-exported `dqrng_get_state()` / `dqrng_set_state()`. Builds on `primer-primitives` (**landed**, archived `2026-06-04-primer-primitives`); orthogonal to `task-primer` (also landed).
- **Downstream**: the postcondition rides inside the `*_data_task_primer()` primitives, so it carries into the `targets` shard body and the §7 `replay-helper` for free — each shard self-verifies its RNG backend in its own process, where an independently-loaded foreign package is exactly the risk. The Suggests move means the targets project / scenario callers must load dqrng explicitly.
- **Exploration**: `exploration/user-rng-conflict/` (the dqrng-vs-randtoolbox reprexes that motivated and validated this) lives in this change.
