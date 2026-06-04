## Context

`TARGETS-DESIGN.md` §2 roots every per-task draw in `dqrng::dqset.seed(seed, stream = primer)` under an active `local_dqrng_backend()` (pcg64) scope. `primer-primitives` and `task-primer` have **landed** (archived `2026-06-04-primer-primitives` / `2026-06-04-task-primer`): the per-task seed-and-run wrappers `sample_data_task_primer()` / `fit_data_task_primer()` / `hc_data_task_primer()` (`R/task-lists.R`) now install the primer once via `local_dqrng_state(seed, primer)` and run the operation. The entry side is guarded — `local_dqrng_state()` calls `chk_dqrng_backend_active()` before seeding (`R/dqrng-state.R:69`). There is no exit side — nothing certifies the backend was *still* dqrng when the task finished drawing. This change adds that bookend; its dependency has landed, so it is ready to implement.

The `exploration/user-rng-conflict/` reprexes (dqrng vs `randtoolbox`, validated on R 4.5.3 / dqrng 0.4.1 / randtoolbox 2.0.5) establish the threat: base R has one global user-supplied RNG slot keyed on the C symbol `user_unif_rand`, resolved across all loaded DLLs to the most-recently-loaded match. A second user-RNG package can take the slot while `RNGkind()[1]` still reads `"user-supplied"` — so the existing `dqrng_backend_active()` probe is satisfied by a *foreign* RNG (`case6`). The probe answers "is *a* user RNG active", not "is *dqrng* active".

## Goals / Non-Goals

**Goals:**

- An internal `chk_dqrng_backend_intact()` that verifies **dqrng specifically** holds the slot, non-destructively, and aborts otherwise.
- Run it as the exit bookend of every RNG-consuming task body, so the draws a task is judged on are bracketed by an entry guard and an exit guard.

**Non-Goals:**

- Replacing `dqrng_backend_active()` / the `RNGkind()` probe — it stays the cheap reentrancy gate for `local_dqrng_backend()` (re-asserting `register_methods()` reseeds, so the gate must stay cheap and side-effect-free).
- Defending against the *segfault* hijack (`case3`): an uninitialised foreign RNG bound to the slot crashes on the task body's *own* first draw, before any exit check runs — the load-time-inert design and "don't load two user-RNG packages" remain the mitigation there.
- Detecting corruption *during* a long task body (the check is a boundary postcondition, not a per-draw monitor).

## Decisions

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

The witness consumes one draw. Rather than rely on "a draw at task end is harmless", restore the recorded state immediately: `dqrng::dqrng_set_state(s0)`. `case5` confirms the round-trip is exact — the task's next draw is byte-identical with or without the witness (`0.908370` either way). So the postcondition consumes **no net randomness** and cannot perturb reproducibility, even if a task body's structure changes later. *Alternative considered:* skip the restore and document the consumed draw — rejected; a non-destructive check has no reproducibility footprint to reason about and is safe to run unconditionally.

### Decision: abort (chk-style), symmetric with the entry guard

A failed postcondition means the task's draws did **not** come from dqrng — the result is silently wrong. That is a correctness violation, so it aborts via `chk::abort_chk(..., call = ...)`, exactly like `chk_dqrng_backend_active()` on entry, reporting in the user-facing frame (AGENTS.md error-origin rule). A warning that lets the run finish would let corrupted results flow into the summary — the opposite of what the check is for. In a `targets` run each shard is its own process, so an abort fails *that* shard (recoverable via `error = "null"` / `shard-failure-survival`) without poisoning the others.

### Decision: the check lives in the `*_data_task_primer()` wrappers, as the exit bookend

`primer-primitives` (landed) centralises "install the primer once, then run the op" in the three `*_data_task_primer()` wrappers (`R/task-lists.R`) — the exact per-task bodies, and the same entry point `targets` shards and the §7 replay helper reuse. The postcondition belongs there, after the op returns, so it covers every path that draws without each caller re-implementing it. `local_dqrng_state()` (entry) and `chk_dqrng_backend_intact()` (exit) become the matching brackets around the body. *Alternative considered:* a single check at the end of `ssd_run_scenario_baseline()` — rejected; it would not localise *which* task corrupted the backend, and would miss the per-shard/per-process granularity that is the whole point in the cluster path.

### Decision: a standalone change, depending on `primer-primitives`

The witness helper and the per-task postcondition are a self-contained robustness layer with one dependency — the `*_data_task_primer()` wrappers it hooks into, **now landed** (archived `2026-06-04-primer-primitives`), so the change is unblocked. Keeping it separate gives the dqrng-vs-randtoolbox exploration an obvious home (`exploration/`) and a clean spec delta on two capabilities, rather than retrofitting it into the already-archived `primer-primitives`.

## Risks / Trade-offs

- **dqrng state representation changes across versions** → the witness only compares `dqrng_get_state()` to itself within one process, so it is robust to the *format*; it relies only on "a draw advances the state and `set_state` round-trips", which is dqrng's core contract. A test pins both behaviours.
- **A pathological generator whose state does not change on a single draw** → not a concern for pcg64 (every draw advances the counter); the backend forces pcg64 (`set_dqrng_backend()`), so the witness is valid for the only generator ssdsims uses.
- **Cost** → two `dqrng_get_state()` calls, one `runif(1)`, one `dqrng_set_state()` per task. Negligible against a fit/hc, and per-task (not per-draw).
- **Does not catch the segfault hijack** (`case3`) → accepted and documented; that failure crashes the body's own draw first, and is prevented upstream by not registering at load and not co-loading user-RNG packages.

## Migration Plan

Additive and internal: one new internal function and a few lines in each existing `*_data_task_primer()` wrapper, plus tests. No public API, no data migration. Removing the function and the wrapper calls fully reverts.

## Open Questions

- Should the witness also run on the **entry** side of `local_dqrng_state()` (upgrading `chk_dqrng_backend_active()` to `chk_dqrng_backend_intact()`), so a task refuses to *start* on a foreign-hijacked backend? Leaning yes as a cheap follow-up, but the entry path is performance-sensitive (called once per task before any work) and the exit check already makes corruption fatal; deferred until the postcondition lands.
