## ADDED Requirements

### Requirement: Per-task dqrng backend integrity brackets
Each RNG-consuming per-task seed-and-run wrapper (`sample_data_task_primer()`, `fit_data_task_primer()`, `hc_data_task_primer()`) SHALL bracket its task body with the dqrng-specific integrity witness (`dqrng-backend` capability) on **both** ends, verifying that dqrng specifically (not merely *some* user-supplied RNG) holds base R's RNG slot:

- **Entry precondition** — before the task body draws, the per-task seeding step (`local_dqrng_state()`) SHALL assert backend integrity via the witness, upgrading the previous cheap `RNGkind()`-based `chk_dqrng_backend_active()` guard, so a task refuses to *start* on a foreign-hijacked or torn-down backend.
- **Exit postcondition** — after the task body has run and before the wrapper returns, the wrapper SHALL assert backend integrity via the witness again.

In either case the wrapper SHALL abort the task if the backend is not intact, so a foreign-RNG hijack present before the task starts, or a mid-task teardown/hijack, causes the task to fail loudly rather than silently return draws that did not come from dqrng's pcg64. The cheap `dqrng_backend_active()` probe SHALL be retained only as the reentrancy no-op gate inside `local_dqrng_backend()`.

#### Scenario: Task succeeds when the backend stays intact
- **WHEN** a `*_data_task_primer()` wrapper runs under an active `local_dqrng_backend()` scope and the dqrng backend is the bound generator at task entry and remains so throughout the task body
- **THEN** both the entry and the exit integrity checks SHALL pass and the wrapper SHALL return the task result normally

#### Scenario: Task aborts at entry on a corrupted backend
- **WHEN** the dqrng backend is not intact when the task starts (a foreign user-supplied RNG already holds the slot, or base R RNG has been routed back to a non-user-supplied generator) and a `*_data_task_primer()` wrapper begins
- **THEN** the entry integrity check SHALL abort the task with an informative error reported in the user-facing frame, before the task body draws

#### Scenario: Task aborts when the backend is torn down mid-body
- **WHEN** the dqrng backend is reset or switched away from dqrng during a task body (e.g. base R RNG is routed back to a non-user-supplied generator, or a foreign user-supplied RNG is installed) such that dqrng no longer serves base R's draws by the time the task ends
- **THEN** the exit integrity check SHALL abort the task with an informative error reported in the user-facing frame, rather than returning the task's (non-dqrng) draws

#### Scenario: Brackets do not perturb reproducibility
- **WHEN** the same seeded task is run twice, once exercising the entry and exit integrity checks and once with them disabled
- **THEN** the two task results SHALL be identical — each witness draw is rolled back and consumes no net randomness
