## Context

`TARGETS-DESIGN.md` §2 roots every per-task draw in `dqrng::dqset.seed(seed, stream = primer)` under an active `local_dqrng_backend()` (pcg64) scope. `primer-primitives` and `task-primer` have **landed** (archived `2026-06-04-primer-primitives` / `2026-06-04-task-primer`): the per-task seed-and-run wrappers `sample_data_task_primer()` / `fit_data_task_primer()` / `hc_data_task_primer()` (`R/task-lists.R`) now install the primer once via `local_dqrng_state(seed, primer)` and run the operation. The entry side is guarded — `local_dqrng_state()` calls `chk_dqrng_backend_active()` before seeding (`R/dqrng-state.R:69`) — but that guard is the cheap `RNGkind()` probe, and there is no exit side: nothing certifies the backend was *still* dqrng when the task finished drawing. This change adds a stronger witness and brackets each task's draws with it on both ends; its dependency has landed, so it is ready to implement.

The `exploration/user-rng-conflict/` reprexes (dqrng vs `randtoolbox`, validated on R 4.5.3 / dqrng 0.4.1 / randtoolbox 2.0.5) establish the threat: base R has one global user-supplied RNG slot keyed on the C symbol `user_unif_rand`, resolved across all loaded DLLs to the **most-recently-loaded** match. A second user-RNG package can take the slot while `RNGkind()[1]` still reads `"user-supplied"` — so the existing `dqrng_backend_active()` probe is satisfied by a *foreign* RNG (`case6`). The probe answers "is *a* user RNG active", not "is *dqrng* active".

Because the slot is last-loaded-wins, ssdsims **cannot prevent** a foreign user-RNG package from taking it; the achievable guarantee is *detection*, not *avoidance*. ssdsims keeps `dqrng` in `Imports` (so it is always loaded and the witness can always run) and adds the runtime witness that refuses to emit silently-wrong draws. (See the *keep dqrng in Imports* decision for why the alternative — making dqrng a Suggested, used-only-when-already-loaded dependency — was dropped.)

## Goals / Non-Goals

**Goals:**

- An internal `chk_dqrng_backend_intact()` that verifies **dqrng specifically** holds the slot, non-destructively, and aborts with a diagnostic message otherwise.
- Bracket every RNG-consuming task body with that witness on **both** ends — an entry precondition (upgraded from the cheap probe) and an exit postcondition — so the draws a task is judged on come from dqrng or the task fails loudly.

**Non-Goals:**

- Making `dqrng` optional / moving it to `Suggests` — explicitly out of scope (dropped per PR #117 feedback; see the *keep dqrng in Imports* decision).
- Replacing `dqrng_backend_active()` / the `RNGkind()` probe outright — it stays the cheap reentrancy gate for `local_dqrng_backend()` (re-asserting `register_methods()` reseeds, so that gate must stay cheap and side-effect-free). It is only the per-task *integrity* assertion (entry + exit) that is upgraded to the witness.
- Defending against the *segfault* hijack (`case3`): an uninitialised foreign RNG bound to the slot crashes on the task body's *own* first draw, before any exit check runs — the load-time-inert design and "don't load two user-RNG packages" remain the mitigation there.
- Detecting corruption *during* a long task body (the check is a boundary precondition/postcondition, not a per-draw monitor).

## Decisions

### Decision: keep dqrng in `Imports`; do not pursue the good-citizen `Suggests` move

An earlier revision moved `dqrng` from `Imports` to `Suggests` so that loading ssdsims would not load dqrng, gating every `dqrng::` touch on a `dqrng_usable() <- isNamespaceLoaded("dqrng") && getNamespaceVersion(...) >= "0.4.1"` test and aborting with `library(dqrng)` guidance when unavailable. That half is **dropped**. The motivation was to avoid *imposing* a `user_unif_rand` provider on every session — but base R's last-loaded-wins rule means that *not* being the one who loads a user-RNG does not prevent the collision: if any user-RNG package is co-loaded, it takes the slot regardless of how polite ssdsims is. So the Suggests move buys politeness, not safety, while imposing real friction (`dqrng_usable()` threaded through `set_dqrng_backend()` / `local_dqrng_backend()` / `local_dqrng_state()` / the witness; `@examplesIf` on every example; `library(dqrng)` in both vignettes, the `inst/targets-templates/*/scenario.R` scaffolds, and the test fixtures' `_targets.R`; a degraded "not reproducible out of the box" default UX). The detection-at-runtime witness is the part that actually protects correctness, so it is kept and the Suggests friction is dropped. *Consequence:* `dqrng` stays in `Imports` (`>= 0.4.1`), is always loaded with ssdsims, and the witness can always run with no availability gate. *Alternative considered:* the Suggests + `dqrng_usable()` + abort design — rejected as above.

### Decision: witness via dqrng's own state, not the `RNGkind()` probe

`RNGkind()[1] == "user-supplied"` is true for *any* user-supplied RNG, so it cannot distinguish dqrng from `randtoolbox` (`case6` fools it). Instead, use dqrng's own state as the witness (validated in `case5-state-witness.R`):

```r
s0 <- dqrng::dqrng_get_state()
runif(1)                       # routes through base R's user_unif_rand slot
s1 <- dqrng::dqrng_get_state()
intact <- !identical(s0, s1)   # dqrng advanced  <=>  dqrng is the bound generator
```

If dqrng holds the slot, the base-R draw advances dqrng's internal state (`s0 != s1`). If a foreign RNG holds it, the draw advances *that* generator and dqrng's state is frozen (`s0 == s1`) → not intact → abort. This verifies dqrng *specifically*, closing the gap the cheap probe leaves. *Alternatives considered:* (a) the cheap probe — rejected, blind to foreign hijack; (b) a maintained blocklist of known user-RNG namespaces (`loadedNamespaces()`) — rejected, fragile and always one package behind; (c) drawing and comparing against a recorded dqrng value — rejected, needs its own seeding and is more stateful than the state delta.

### Decision: make the check non-destructive by restoring state

The witness consumes one draw. Rather than rely on "a draw at task end is harmless", restore the recorded state immediately: `dqrng::dqrng_set_state(s0)`. `case5` confirms the round-trip is exact — the task's next draw is byte-identical with or without the witness (`0.908370` either way). So the check consumes **no net randomness** and cannot perturb reproducibility, even on the entry side where it runs before seeding (and `dqset.seed()` overwrites the state regardless). *Alternative considered:* skip the restore and document the consumed draw — rejected; a non-destructive check has no reproducibility footprint to reason about and is safe to run unconditionally.

### Decision: abort (chk-style), symmetric with the entry guard

A failed witness means the task's draws did **not** come from dqrng — the result is silently wrong. That is a correctness violation, so it aborts via `chk::abort_chk(..., call = ...)`, exactly like `chk_dqrng_backend_active()` did on entry, reporting in the user-facing frame (AGENTS.md error-origin rule). A warning that lets the run finish would let corrupted results flow into the summary — the opposite of what the check is for. In a `targets` run each shard is its own process, so an abort fails *that* shard (recoverable via `error = "null"` / `shard-failure-survival`) without poisoning the others.

### Decision: bracket the task body — both brackets inside `local_dqrng_state()`

Both the entry precondition and the exit postcondition live in **one** function, `local_dqrng_state()` (`R/dqrng-state.R`), not scattered across the three `*_data_task_primer()` wrappers. `local_dqrng_state()` is the per-task seeding step every RNG-consuming body already goes through, and it is already a withr-style wrapper that `withr::defer()`s a `set_dqrng_state(old)` restore onto the task frame — so the exit bracket rides that existing machinery with no new call sites and no wrapper edits:

- **Entry** — upgrade `local_dqrng_state()`'s guard from `chk_dqrng_backend_active()` (cheap `RNGkind()` probe) to `chk_dqrng_backend_intact()` (the full witness), so a task refuses to *start* on a foreign-hijacked or torn-down backend rather than discovering the corruption only at exit. Intact ⟹ user-supplied (a state advance proves dqrng served the draw), so it strictly subsumes the old probe at this site; it runs before `dqset.seed()`, which overwrites the state, so the entry witness leaves the seed untouched.
- **Exit** — add a second `withr::defer(...)` onto the same `.local_envir`, so the witness fires when the task frame unwinds. It is gated to the **success path** so a failing task body's own error is never masked by a witness abort during unwinding (see the next decision). The three wrappers and `with_dqrng_state()` are unchanged.

This resolves the prior open question (the entry-side upgrade) in the affirmative, and chooses `local_dqrng_state()` over the wrappers because it is per-task (so it still localises *which* task corrupted the backend, including per-shard/per-process in the cluster path) yet collapses four edit sites (entry + three wrappers) into one, co-located with the seeding. The witness is cheap enough (two `dqrng_get_state()`, one `runif(1)`, one `dqrng_set_state()`) that running it on entry and on exit per task is negligible. *Alternatives considered:* (a) a single check at the end of `ssd_run_scenario_baseline()` — rejected; it would not localise the offending task and would miss per-shard granularity; (b) an explicit exit call in each of the three wrappers — rejected; three duplicated call sites for what the existing `local_dqrng_state()` defer already provides; (c) placing the bracket on `local_dqrng_backend()` — rejected; that scope is per-scenario, so in the in-process baseline path it would not catch which task tore the backend down.

### Decision: gate the deferred exit witness to the success path via `returnValue()`

`withr::defer` fires on *every* frame exit, including error unwinding — so a naive deferred witness would also run when the task body throws, and if the backend were also corrupt the witness's `abort_chk` would replace (mask) the task's original error. To avoid that without reintroducing a per-wrapper success flag, the deferred witness is gated on `base::returnValue()`:

```r
sentinel <- new.env()  # per-call unique; a task body cannot reproduce it
withr::defer(
  if (!identical(returnValue(sentinel), sentinel)) chk_dqrng_backend_intact(),
  envir = .local_envir
)
```

`returnValue(default)` returns the frame's return value on a normal exit and `default` on an error / non-local exit; with a private `new.env()` sentinel (compared by reference via `identical()`, so no real return value can collide) the witness runs **only** on the happy path. Verified through `withr::defer` registered on the caller's frame (R 4.5.3 / withr 3.0.2): normal return → witness runs; `stop()` in the body → witness skipped, original error propagates. The full bracket — entry abort, exit abort, on-error skip, and reproducibility — is demonstrated end-to-end in `exploration/user-rng-conflict/case8-task-brackets.R` (self-contained, dqrng-only). This keeps the postcondition's real job — catching silently-wrong *successful* results — while leaving genuine task failures to surface their own cause. *Alternatives considered:* (a) fire on every exit and accept occasional error-masking — rejected; the `returnValue()` gate removes the downside at the cost of one `new.env()` + one `identical()`; (b) a success flag set by each wrapper — rejected; it would push wiring back into the three wrappers, defeating the single-site placement.

### Decision: name the culprit in the abort message

When the witness fails, the operator's first question is "what took the RNG?". R can answer it (validated in `case7-who-owns-rng.R`), so the abort message SHALL be diagnostic rather than generic:

- **Owner** — `getNativeSymbolInfo("user_unif_rand")$dll[["name"]]` names the package whose `user_unif_rand` R currently resolves (the same `R_FindSymbol` all-DLL search `RNG_Init` uses). *Gotcha:* a `DLLInfo` overloads `$` to look up a *native symbol* by name, so read the DLL name with `[["name"]]`, never `$name` (which tries to resolve a symbol called `name` and errors).
- **Providers** — enumerating `getLoadedDLLs()` and testing each for the `user_unif_rand` symbol lists *every* loaded package that could take the single global slot (e.g. `dqrng`, `randtoolbox`), which points at the offending co-load even when more than one is present.

The message branches on `RNGkind()[1]`, because the owner is only meaningful while a user-supplied RNG is active:

- `RNGkind()[1] != "user-supplied"` → the backend was **torn down** mid-task; report the current `RNGkind()` (e.g. `"Mersenne-Twister"`) and that the dqrng backend was reset. *Do not* name the symbol owner — the symbol still resolves to some loaded DLL that is not serving RNG, so naming it would mislead.
- `RNGkind()[1] == "user-supplied"` but dqrng did not advance → a **foreign user-RNG hijack**; name the owner and list the loaded user-RNG providers.

*Alternative considered:* a generic "dqrng backend not intact" message — rejected; the diagnosis is cheap and the failure (a co-loaded user-RNG package) is otherwise hard to pin down.

### Decision: validate the foreign-hijack paths via the exploration reprexes, not in-suite

The witness's failure paths against a **foreign user-RNG hijack** (the cheap probe is fooled but the state witness is not — `case6`; naming the slot owner — `case7`) are validated by the `exploration/user-rng-conflict/` reprexes rather than re-tested in the package suite. Co-loading a second user-RNG package in the test process is itself the hazard this change guards against, and `skip_if_not_installed("randtoolbox")` would actually *load* it — so **no new test dependencies** are added (no `randtoolbox`, no `callr` subprocess harness). In-process, dependency-free coverage remains for the intact / non-destructive witness, the torn-down branch (dqrng not the active backend, reachable without a foreign package), the per-task wrapper success path, and the upgraded entry guard. *Alternative considered:* a hand-rolled fake user-RNG stub to exercise the message path in-suite — declined (no appetite); the reprexes are the reference.

### Decision: a standalone change, depending on `primer-primitives`

The witness helper and the per-task brackets are a self-contained robustness layer with one dependency — the `*_data_task_primer()` wrappers (and `local_dqrng_state()`) it hooks into, **now landed** (archived `2026-06-04-primer-primitives`), so the change is unblocked. Keeping it separate gives the dqrng-vs-randtoolbox exploration an obvious home (`exploration/`) and a clean spec delta on two capabilities, rather than retrofitting it into the already-archived `primer-primitives`.

## Risks / Trade-offs

- **dqrng state representation changes across versions** → the witness only compares `dqrng_get_state()` to itself within one process, so it is robust to the *format*; it relies only on "a draw advances the state and `set_state` round-trips", which is dqrng's core contract. A test pins both behaviours.
- **A pathological generator whose state does not change on a single draw** → not a concern for pcg64 (every draw advances the counter); the backend forces pcg64 (`set_dqrng_backend()`), so the witness is valid for the only generator ssdsims uses.
- **Cost** → two `dqrng_get_state()` calls, one `runif(1)`, one `dqrng_set_state()` per task on entry *and* on exit. Negligible against a fit/hc, and per-task (not per-draw).
- **Does not catch the segfault hijack** (`case3`) → accepted and documented; that failure crashes the body's own draw first, and is prevented upstream by not registering at load and not co-loading user-RNG packages.
- **`getNativeSymbolInfo()` / `getLoadedDLLs()` in the message helpers** → these are **exported, documented base R functions**, used widely on CRAN; they are *not* the C-level non-API entry points (e.g. `R_FindSymbol`) that the `R CMD check` "checking compiled code" NOTE concerns, so they raise no check note. A verification task confirms this rather than relying on the assumption.
- **`dqrng` stays in `Imports`** → loading ssdsims still loads dqrng's `user_unif_rand` provider into the session (the status quo). Accepted: avoiding it does not prevent collisions (base R last-loaded-wins), and the runtime witness is what protects correctness.

## Migration Plan

Internal only — no dependency change and no caller-visible behavioural change:

- Add `chk_dqrng_backend_intact()` and the diagnostic helpers (`rng_slot_owner()`, `user_rng_providers()`) to `R/dqrng-backend.R`.
- In `R/dqrng-state.R`: upgrade `local_dqrng_state()`'s entry guard from `chk_dqrng_backend_active()` to `chk_dqrng_backend_intact()`, and add the `returnValue()`-gated deferred exit witness alongside the existing state-restore defer. The three `*_data_task_primer()` wrappers are **unchanged**.
- Retain `dqrng_backend_active()` / `chk_dqrng_backend_active()` as `local_dqrng_backend()`'s reentrancy no-op gate.
- `dqrng` stays in `Imports`; examples / vignettes / scaffolds / tests are **unchanged** (no `@examplesIf`, no added `library(dqrng)`), since dqrng is still auto-loaded.

Reverting means removing the new functions and reverting `local_dqrng_state()` (entry guard back to `chk_dqrng_backend_active()`, drop the exit defer).

## Open Questions

- None outstanding. (The prior open question — whether to run the witness on the entry side too — is resolved *yes*; see the *bracket the task body* decision.)
