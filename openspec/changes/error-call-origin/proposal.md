## Why

When a user passes an invalid argument to a `ssd_*()` function, the `Error in ...:` header should name the function they actually called, not an internal frame they have never heard of. Today only `ssd_data()` and `ssd_define_scenario()` follow the `AGENTS.md` "Error origin" convention (thread the public frame down to validators as `call = environment()`, loop instead of `purrr::walk`/`chk_all`); the rest of the exported surface either delegates validation to a private helper (so the header names that helper) or uses `chk::chk_all()` (so the header names `chk_all`). The result is misleading provenance — `Error in \`scenario_shards()\`:` or `Error in \`chk_all()\`:` for a mistake in `ssd_scenario_fit_shards()` / `ssd_fit_dists_sims()` — which the `error-call-origin` cleanup exists to retire package-wide.

## What Changes

- Establish a **package-wide behavioural contract**: every user-facing (exported `ssd_*()` / `scenario_*()`) function's validation errors SHALL report the calling function as the error origin, never an internal frame (`purrr::map()`/`lapply()`, a private helper, or `chk::chk_all()`).
- Thread the public function's frame into every validator it calls: capture `call <- environment()` in the exported function and pass it as `call = call` to `chk::abort_chk()` and to private validators (which take `call = rlang::caller_env()`), matching the `ssd_data()` / `ssd_define_scenario()` reference implementations.
- **Distinguish anticipated user errors from unexpected ones, explicitly.** The origin contract is about *anticipated, legitimate user errors* (validation of user input): those SHALL name the calling function. Their element-wise validation keeps a functional (`purrr::map()` / `walk()`) wrapped in **`rlang::try_fetch()`**, re-raising the caught condition with the public frame via `chk::abort_chk(..., call = call)` (or `rlang::abort(..., parent = cnd, call = call)`) — so the origin is the public function *and* the functional/tidyverse style is preserved. This is what replaces the raw `chk::chk_all(min_pmix, chk::chk_function, …)` and `range_shape*` checks in `ssd_fit_dists_sims()`: `try_fetch()` around a `map()`, not a hand-rolled `for` loop.
- *Unexpected* errors — conditions we do not anticipate (internal/programming failures, not a legitimate user mistake) — need no origin gymnastics: plain looping primitives (or ordinary functionals) are fine, and an unexpected error surfacing an internal frame is acceptable (it signals a bug to fix, not a user-facing validation message). The contract does **not** require wrapping these.
- Audit and fix the exported functions that still leak an internal frame: `ssd_fit_dists_sims()` (`chk_all`), `ssd_scenario_sample_shards()` / `ssd_scenario_fit_shards()` / `ssd_scenario_hc_shards()` (delegate to `scenario_shards()`), and any `ssd_sim_data()` / `ssd_hc_sims()` / `ssd_scenario_*_tasks()` / `ssd_run_*` / `scenario_dataset()` / `scenario_min_pmix()` paths where a check or `abort_chk()` runs in a non-public frame without threaded `call`.
- Add regression tests that assert the error origin (condition `call` / header names the public function, not the helper/`chk_all`).

## Capabilities

### New Capabilities
- `error-origin`: a cross-cutting contract that user-facing validation errors name the calling function as origin. A new capability (not a feature spec) keeps this orthogonal to — and independent of — every feature spec.

### Modified Capabilities
<!-- None: the contract is new and cross-cutting; no existing spec's requirements change. The exported functions' validation *behaviour* (what they accept/reject) is unchanged — only the error header's origin moves to the public frame. -->

## Impact

- **Affected code**: `R/fit-dists-sims.R`, `R/hc-sims.R`, `R/simulate-data.R`, `R/task-shards.R`, `R/task-lists.R`, `R/accessors.R`, `R/run-scenario.R`, `R/shard-runner.R`, `R/targets-runner.R` — wherever an exported function's validators run without a threaded public `call`. `R/data.R` and `R/scenario.R` already conform and are the reference.
- **Behaviour**: only the `Error in ...:` origin (the condition's `call`) changes; the set of inputs accepted/rejected and the message bodies are unchanged. No API, signature, or return-value change.
- **No new dependencies**: uses the existing `chk` (`abort_chk`, `chk_*`) and `rlang` (`try_fetch()`, `caller_env()`, `environment()`); anticipated-validation functionals are wrapped in `rlang::try_fetch()` (re-raising with the public `call`) rather than rewritten as base `for` loops — keeping the functional style while fixing the origin.
- **Tests**: per-function regression tests (`expect_error(..., class = ...)` plus `expect_snapshot()` on the rendered error) asserting the origin frame.
- **Dependencies (direction)**: none. This is cosmetic and **independent** — it can land at any time and gates nothing.
- **Upstream risk**: fully threading `call` through *every* `chk_*()` may need an upstream `chk` change (a `call`/`error_call` argument on `chk_*()`), so the origin can be set without hand-rolling each check via `abort_chk()`. Captured as an open question / risk in `design.md`; the in-package pass uses `abort_chk(..., call = call)` and plain loops, which need no upstream change.
- **When to land it**: any time. Independent, cosmetic, no prerequisites.
