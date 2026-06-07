## ADDED Requirements

### Requirement: Per-task dqrng backend integrity brackets
The per-task dqrng state installer (`local_dqrng_state()`) SHALL bracket each task's draws with the dqrng-specific integrity witness (`dqrng-backend` capability) on **both** ends, verifying that dqrng specifically (not merely *some* user-supplied RNG) holds base R's RNG slot. Both brackets SHALL live in `local_dqrng_state()` itself — the seed-and-run wrappers (`sample_data_task_primer()`, `fit_data_task_primer()`, `hc_data_task_primer()`) and `with_dqrng_state()` already route through it and SHALL require no additional integrity wiring:

- **Entry precondition** — before installing the task seed, `local_dqrng_state()` SHALL assert backend integrity via the witness, upgrading the previous cheap `RNGkind()`-based `chk_dqrng_backend_active()` guard, so a task refuses to *start* on a foreign-hijacked or torn-down backend.
- **Exit postcondition** — `local_dqrng_state()` SHALL register a deferred witness on the task frame (alongside its existing state-restore defer) that runs when the frame exits, asserting backend integrity again. The deferred witness SHALL run only when the task frame exits **normally** (the success path) and SHALL be skipped when the frame is unwinding due to an error or other non-local exit, so it never masks the task body's own error. (`base::returnValue()` with a per-call unique sentinel distinguishes the two.)

In either bracket the witness SHALL abort if the backend is not intact, so a foreign-RNG hijack present before the task starts, or a mid-task teardown/hijack on an otherwise-successful task, causes the task to fail loudly rather than silently return draws that did not come from dqrng's pcg64. The cheap `dqrng_backend_active()` probe SHALL be retained only as the reentrancy no-op gate inside `local_dqrng_backend()`.

#### Scenario: Task succeeds when the backend stays intact
- **WHEN** a task runs through `local_dqrng_state()` under an active `local_dqrng_backend()` scope and the dqrng backend is the bound generator at task entry and remains so until the task frame exits normally
- **THEN** both the entry and the exit integrity checks SHALL pass and the task SHALL return its result normally

#### Scenario: Task aborts at entry on a corrupted backend
- **WHEN** the dqrng backend is not intact when the task starts (a foreign user-supplied RNG already holds the slot, or base R RNG has been routed back to a non-user-supplied generator) and `local_dqrng_state()` is called
- **THEN** the entry integrity check SHALL abort the task with an informative error reported in the user-facing frame, before the task body draws

#### Scenario: Task aborts when the backend is torn down before a normal return
- **WHEN** the dqrng backend is reset or switched away from dqrng during a task body (e.g. base R RNG is routed back to a non-user-supplied generator, or a foreign user-supplied RNG is installed) such that dqrng no longer serves base R's draws, and the task body nonetheless completes and the frame begins to exit normally
- **THEN** the deferred exit integrity check SHALL abort the task with an informative error reported in the user-facing frame, rather than returning the task's (non-dqrng) draws

#### Scenario: Exit check does not mask a task-body error
- **WHEN** a task body raises an error (so the task frame unwinds abnormally), regardless of whether the dqrng backend is intact at that point
- **THEN** the deferred exit integrity check SHALL NOT run, and the task body's original error SHALL propagate unchanged

#### Scenario: Brackets do not perturb reproducibility
- **WHEN** the same seeded task is run twice, once exercising the entry and exit integrity checks and once with them disabled
- **THEN** the two task results SHALL be identical — each witness draw is rolled back and consumes no net randomness
