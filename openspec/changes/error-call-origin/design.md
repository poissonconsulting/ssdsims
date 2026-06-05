## Context

`AGENTS.md`'s "Error origin" note states the convention: raise validation
errors in the context of the *user-facing* function. The exported function
captures its own frame (`call <- environment()`) and threads it down to every
validator — both directly to `chk::abort_chk(..., call = call)` and as a
`call =` argument to private validator helpers (which default to
`rlang::caller_env()`). Where a check is applied element-wise, the convention
prefers a plain `for` loop over `purrr::walk()` / `chk::chk_all()`, because
those wrappers push their own frame onto the call stack and so name themselves
(`Error in \`chk_all()\`:`) in the header.

`R/data.R` (`ssd_data()` / `ssd_data_validate()` / `ssd_data_names()`) and
`R/scenario.R` (`ssd_define_scenario()`) already implement this end to end and
are the reference. The rest of the exported surface does not: a quick audit
(`grep` for `chk_all`, `purrr::walk`, `abort_chk`, `chk::chk_`) shows
`ssd_fit_dists_sims()` validating list arguments with
`chk::chk_all(min_pmix, chk::chk_function, formals = 1L)` and `chk_all` on
`range_shape1` / `range_shape2`, and the remaining functions
(`ssd_hc_sims()`, `ssd_sim_data()`, the `ssd_run_*()` step functions, the
`ssd_scenario_*()` task/shard builders, `scenario_*()` accessors, and the
`*_dqrng_*` / `*_lecuyer_cmrg_*` RNG helpers) calling `chk::chk_*()` without an
explicit `call`, so any check that runs inside a private helper or a
`purrr::map()` / `lapply()` body reports the internal frame.

This change is a **cross-cutting audit**, not a feature: it standardises the
error *origin* across the public surface without changing what any function
accepts or rejects. It is cosmetic, off the targets dependency DAG, and can
land at any time (`TARGETS-DESIGN.md` §12).

## Goals / Non-Goals

**Goals:**

- Every exported `ssd_*()` / `scenario_*()` function's validation errors report
  the calling function as origin (the condition's `call` / `Error in ...:`
  header names the public function).
- Apply the reference pattern uniformly: `call <- environment()` at the top of
  the exported function, threaded as `call =` to `abort_chk()` and to private
  validators (default `call = rlang::caller_env()`).
- Element-wise validation that would otherwise surface a wrapper frame
  (`chk_all` / `purrr::walk`) uses a plain `for` loop.
- Regression coverage asserting the origin per audited function.

**Non-Goals:**

- Changing which inputs are accepted or rejected, or the semantic content of
  the messages (beyond the per-element wording that switching off `chk_all`
  necessarily changes — pinned by snapshots).
- Patching `chk` upstream as part of this change (the in-package fix uses
  `abort_chk(..., call = call)` and plain loops; an upstream `call`/`error_call`
  argument on `chk_*()` is noted as a follow-up, see Open Questions).
- Touching internal-only (non-exported) functions' error origins where they are
  never reached directly by a user (they inherit a public `call` once threaded).

## Decisions

### Decision: a dedicated `error-origin` capability, not edits to feature specs

The contract is cross-cutting — it constrains *every* exported function's error
behaviour. Encoding it as `## ADDED Requirements` in a single new `error-origin`
capability (rather than amending each feature spec's requirements) keeps the
change orthogonal to, and independent of, every feature spec and the targets
DAG. A feature spec can be added, modified, or archived without disturbing this
contract, and this contract can land at any time without touching a feature
spec. *Alternative considered:* adding a "report origin" scenario to each
function's own spec — rejected as scattering one contract across many specs and
coupling a cosmetic pass to feature work.

### Decision: thread `call = environment()`, matching the `ssd_data()` reference

Each exported function captures its own evaluation frame with
`call <- environment()` as its first statements and passes `call = call` to
`chk::abort_chk()` and to any private validator it delegates to; private
validators take `call = rlang::caller_env()` so a direct internal call still
gets a sensible default. This is exactly the pattern in `R/data.R`
(`ssd_data()` → `ssd_data_names()` / `ssd_data_validate()`) and `R/scenario.R`,
so the audit *propagates an existing, tested idiom* rather than inventing one.
Using the captured frame (not `rlang::caller_env()` from inside the public
function) ensures the header names the public function itself, even when the
user called it via another wrapper. *Alternative considered:* setting
`error_call` only at the outermost `tryCatch` — rejected; it does not generalise
to the many independent `chk_*()` sites and loses the per-check message
locations.

### Decision: plain `for` loops replace `chk_all` / `purrr::walk` in validation paths

`chk::chk_all(x, chk_fun, ...)` and `purrr::walk(x, chk_fun)` both add their own
frame, so a failing element reports `Error in \`chk_all()\`:` /
`Error in \`map()\`:`. Where such a wrapper sits on a validation path of an
exported function (concretely `chk_all(min_pmix, chk::chk_function, formals = 1L)`
and the `range_shape1` / `range_shape2` `chk_all` checks in
`ssd_fit_dists_sims()`), it is rewritten as a plain `for` loop that calls the
element check with `call = call` (or wraps the failing element in
`abort_chk(..., call = call)`). This is the explicit exception to the
"prefer `purrr` functionals" rule recorded in `AGENTS.md`'s coding rules. Loops
**not** on a user-reachable validation path are left as `purrr` functionals.

### Decision: prefer the validator's own `call` argument; fall back to `abort_chk`

Where a `chk::chk_*()` function accepts a way to set the reported call, it is
used. Where it does not (several `chk_*()` predicates have no `call`/`error_call`
argument), the check is expressed as a guard plus `chk::abort_chk(msg, call = call)`,
which *does* accept `call`. This keeps every audited site fixable without an
upstream change, at the cost of a little hand-rolling on the no-`call` checks —
the residual ergonomics gap is the Open Question below.

### Decision: assert the origin in regression tests

Each audited function gets a test that triggers a validation failure and asserts
the origin: `testthat::expect_error(..., class = "chk_error")` (or the relevant
condition class) combined with `testthat::expect_snapshot()` of the rendered
error so the `Error in \`ssd_*()\`:` header is pinned and a future re-leak of
an internal frame (`chk_all` / `purrr` / a helper) fails the snapshot. Snapshots
also pin the per-element message wording changed by dropping `chk_all`.

## Risks / Trade-offs

- **Message wording drift from dropping `chk_all`** → the per-element message
  text changes when `chk_all` is replaced by a loop; mitigated by snapshotting
  the rendered errors so the new wording is reviewed once and pinned.
- **`call <- environment()` vs `rlang::caller_env()` confusion** → standardise
  on `environment()` captured in the *public* function and `caller_env()` as the
  default in *private* validators, exactly as `R/data.R` does; documented in the
  tasks so each audited file follows the same shape.
- **Incomplete audit (a leaked frame slips through)** → the per-function
  snapshot tests are the backstop; the audit list in `tasks.md` enumerates every
  exported function so none is skipped.
- **Loops where `purrr` reads better** → accepted, narrowly: only validation
  paths that would surface a wrapper frame are converted; non-validation
  functionals stay `purrr`.

## Open Questions

- **Upstream `chk` ergonomics.** Several `chk::chk_*()` predicates expose no
  `call`/`error_call` argument, so today the only way to set the origin on them
  is to wrap the check in `chk::abort_chk(..., call = call)` by hand. A cleaner
  fix would be an upstream `chk` change adding a `call`/`error_call` argument to
  the `chk_*()` family (so the origin propagates without re-implementing each
  check). This is a follow-up, not a blocker — the in-package pass already
  achieves the contract with `abort_chk` + plain loops. Tracked here as the
  upstream risk noted in the proposal.
- **Condition class to assert.** Whether to assert on `chk`'s condition class
  (e.g. `"chk_error"`) or only on the rendered header is settled per function in
  the tests; `expect_snapshot()` of the rendered error is the portable check and
  is used everywhere, with a class assertion added where `chk` guarantees one.
