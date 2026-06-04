## Why

Every RNG-consuming task draws inside a `local_dqrng_backend()` scope, and `local_dqrng_state()` already guards the **entry** with `chk_dqrng_backend_active()`. But that guard only confirms the backend was active when the task *started* — the actual draws (`dplyr::slice_sample()`, `ssdtools::ssd_fit_dists()`, `ssdtools::ssd_hc()`) happen in the task body, so only an **exit** check can certify the numbers the task just produced actually came from dqrng's pcg64.

Two failure modes can corrupt a task's draws mid-body with no signal today:

1. **Backend torn down** — a dependency or stray call (`RNGkind()`, `dqrng::restore_methods()`) flips base R back to Mersenne-Twister; subsequent draws silently fall back, breaking reproducibility.
2. **Foreign user-RNG hijack** — a second package that registers a user-supplied RNG (validated in `exploration/user-rng-conflict/`: dqrng vs `randtoolbox`) is loaded in the session. Base R resolves the single global `user_unif_rand` symbol to the most-recently-loaded DLL, so base `runif()` is silently served by the foreign RNG while `RNGkind()[1]` *still reads* `"user-supplied"`. The existing `dqrng_backend_active()` probe is blind to this — its own code documents the caveat (`R/dqrng-backend.R:52`).

The cheap probe catches (1) but not (2). The exploration's `case6` shows the probe being fooled, and `case5` shows the fix: dqrng's **own state** is the witness — a draw through base `runif()` *must* advance dqrng's internal state iff dqrng is the bound generator; a frozen state means a foreign RNG holds the slot. The witness draw is rolled back with `dqrng_set_state()`, so the check is non-destructive. This change adds that witness as a per-task **postcondition**.

## What Changes

- Add an internal `chk_dqrng_backend_intact(call = ...)` (peer to `chk_dqrng_backend_active()`, in `R/dqrng-backend.R`) that:
  - records `dqrng::dqrng_get_state()`, takes one base-R `runif(1)` draw, re-reads the state, then restores the recorded state via `dqrng::dqrng_set_state()` (non-destructive);
  - returns invisibly iff the state advanced (dqrng is the bound generator); otherwise **aborts** (chk-style), symmetric with the entry guard, reporting in the user-facing frame.
- Wire it into each per-task seed-and-run wrapper from `primer-primitives` (`sample_data_task_primer()`, `fit_data_task_primer()`, `hc_data_task_primer()`) as the **exit bookend**: the wrapper runs the task body, then asserts the backend is intact before returning. Entry guard (`local_dqrng_state` → `chk_dqrng_backend_active`) and exit guard (this) bracket every task's draws.
- Document the limit the witness removes: `dqrng_backend_active()` (the `RNGkind()` probe) answers "is *a* user-supplied RNG active"; `chk_dqrng_backend_intact()` answers "is **dqrng** the active one". The former stays the reentrancy gate for `local_dqrng_backend()`; the latter is the per-task integrity assertion.

## Capabilities

### New Capabilities
<!-- None: extends existing capabilities. -->

### Modified Capabilities
- `dqrng-backend`: add the dqrng-specific backend **integrity witness** (`chk_dqrng_backend_intact()`) — a non-destructive state-advance check that verifies dqrng (not merely *some* user-supplied RNG) holds base R's `user_unif_rand` slot, aborting otherwise.
- `parallel-safe-seeding`: add the per-task **postcondition** — each RNG-consuming task verifies the dqrng backend is intact when the task ends, so a mid-task teardown or foreign-RNG hijack aborts the task instead of silently producing non-dqrng draws.

## Impact

- **New code**: `chk_dqrng_backend_intact()` in `R/dqrng-backend.R`; exit-check wiring in the `*_data_task_primer()` wrappers (`R/primer-primitives.R` / `R/task-lists.R`); tests in `tests/testthat/test-dqrng-backend.R` and the primitives test.
- **APIs**: internal only — no public surface change.
- **Dependencies**: none added. Uses dqrng's already-exported `dqrng_get_state()` / `dqrng_set_state()`. Builds on `primer-primitives` (**landed**, archived `2026-06-04-primer-primitives`), which defines the per-task wrappers this hooks into; orthogonal to `task-primer` (also landed). The dependency is satisfied, so this change is ready to implement.
- **Downstream**: the postcondition rides inside the `*_data_task_primer()` primitives, so it carries into the `targets` shard body and the §7 `replay-helper` for free — each shard self-verifies its RNG backend in its own process, where an independently-loaded foreign package is exactly the risk.
- **Exploration**: `exploration/user-rng-conflict/` (the dqrng-vs-randtoolbox reprexes that motivated and validated this) lives in this change.
