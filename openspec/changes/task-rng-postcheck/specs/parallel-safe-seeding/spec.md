## ADDED Requirements

### Requirement: Per-task dqrng backend postcondition
Each RNG-consuming per-task seed-and-run wrapper (`sample_data_task_primer()`, `fit_data_task_primer()`, `hc_data_task_primer()`) SHALL verify, **when the task ends** (after the task body has run and before the wrapper returns), that the dqrng backend is still intact — i.e. that dqrng specifically (not merely *some* user-supplied RNG) held base R's RNG slot for the duration of the body. The wrapper SHALL perform this check via the dqrng-specific integrity witness (`dqrng-backend` capability) and SHALL abort the task if the backend is not intact. Together with the entry guard (`local_dqrng_state()` → `chk_dqrng_backend_active()`), this brackets every task's draws with a precondition and a postcondition, so a mid-task teardown of the backend or a foreign-RNG hijack causes the task to fail loudly rather than silently return draws that did not come from dqrng's pcg64.

#### Scenario: Task succeeds when the backend stays intact
- **WHEN** a `*_data_task_primer()` wrapper runs under an active `local_dqrng_backend()` scope and the dqrng backend remains the bound generator throughout the task body
- **THEN** the end-of-task integrity check SHALL pass and the wrapper SHALL return the task result normally

#### Scenario: Task aborts when the backend is torn down mid-body
- **WHEN** the dqrng backend is reset or switched away from dqrng during a task body (e.g. base R RNG is routed back to a non-user-supplied generator, or a foreign user-supplied RNG is installed) such that dqrng no longer serves base R's draws by the time the task ends
- **THEN** the end-of-task integrity check SHALL abort the task with an informative error reported in the user-facing frame, rather than returning the task's (non-dqrng) draws

#### Scenario: Postcondition does not perturb reproducibility
- **WHEN** the same seeded task is run twice, once exercising the end-of-task integrity check and once with it disabled
- **THEN** the two task results SHALL be identical — the postcondition's witness draw is rolled back and consumes no net randomness
