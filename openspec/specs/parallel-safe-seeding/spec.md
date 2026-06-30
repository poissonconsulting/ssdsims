# parallel-safe-seeding Specification

## Purpose

Provide RNG helpers that produce reproducible, non-overlapping random number streams across simulations and streams, so that parallel or distributed simulation runs yield deterministic results without statistical correlation between workers. The capability is built on the targets-based dqrng path. It distinguishes a *seed* (a scalar integer) from a *state* — for dqrng the length-2 integer *primer* passed to `dqrng::dqset.seed()`'s `stream` argument — exposing scoped helpers for each.

## Requirements
### Requirement: Scoped dqrng state installation from a (seed, primer) pair
The package SHALL provide `local_dqrng_state(seed, primer, .local_envir = parent.frame())`, a thin wrapper around `dqrng::dqset.seed(seed, stream = primer)` that installs the `(seed, primer)` starting point as the running RNG state and registers a deferred restore of the prior RNG state when `.local_envir` exits. The `primer` argument is the per-task primer (the value passed to dqrng's `stream` argument, per `TARGETS-DESIGN.md` §2 and GLOSSARY); the helper's `_state` name reflects that it scopes the running RNG *state*, while the value it installs is a *primer*. The package MAY additionally provide a `with_dqrng_state(seed, primer, code)` companion that delegates to `local_dqrng_state()` and evaluates `code`.

#### Scenario: Install and restore dqrng state
- **WHEN** `local_dqrng_state(seed, primer)` is called within a function and that function returns
- **THEN** the RNG state SHALL be set to `(seed, primer)` for the duration of the calling frame and SHALL be restored to its prior value when the frame exits

#### Scenario: Same seed and primer reproduce draws
- **WHEN** `local_dqrng_state(seed, primer)` is called twice with the same `(seed, primer)` and the same draws are taken under the active dqrng backend
- **THEN** the two draw sequences SHALL be identical

#### Scenario: Surrounding RNG stream undisturbed
- **WHEN** code takes RNG draws, calls `local_dqrng_state(seed, primer)` in a nested scope that itself takes draws, and then continues
- **THEN** after the nested scope exits the outer RNG stream SHALL continue as if the nested `local_dqrng_state()` scope had not consumed any RNG

### Requirement: Abort when the dqrng backend is not active
`local_dqrng_state()` (and `with_dqrng_state()` if provided) SHALL verify that the dqrng backend is active before seeding, using the `dqrng_backend_active()` helper provided by `dqrng-init`. If the backend is not active, the function SHALL abort with an informative error directing the caller to open a `local_dqrng_backend()` scope first, rather than seeding the wrong generator.

#### Scenario: Abort outside an active backend scope
- **WHEN** `local_dqrng_state(seed, primer)` is called while the dqrng backend is not active (`dqrng_backend_active()` is `FALSE`)
- **THEN** the call SHALL abort with an informative error instructing the caller to activate the backend via `local_dqrng_backend()`

#### Scenario: Proceed inside an active backend scope
- **WHEN** `local_dqrng_state(seed, primer)` is called within an active `local_dqrng_backend()` scope
- **THEN** the RNG-kind check SHALL pass and the function SHALL install the `(seed, primer)` starting point

### Requirement: dqrng state argument validation
`local_dqrng_state()` (and `with_dqrng_state()` if provided) SHALL validate its arguments: `seed` SHALL be a whole number, `primer` SHALL be a length-2 integer vector (the §2 primer encoding, with `NA_integer_` permitted as the reserved INT_MIN value), and `.local_envir` SHALL be an environment. Invalid input SHALL abort with an informative `chk`-style message.

#### Scenario: Non-whole-number seed
- **WHEN** `local_dqrng_state()` is called with a non-whole-number `seed`
- **THEN** the call SHALL abort with an informative error

#### Scenario: Malformed primer
- **WHEN** `local_dqrng_state()` is called with a `primer` that is not a length-2 integer vector
- **THEN** the call SHALL abort with an informative error

#### Scenario: NA_integer_ accepted in the primer
- **WHEN** `local_dqrng_state()` is called with a `primer` whose elements include `NA_integer_` (the INT_MIN encoding of §2)
- **THEN** the call SHALL be accepted and seed dqrng without error

### Requirement: Task primer derivation from a task-parameter hash
The package SHALL expose `task_primer(params)` returning a length-2 integer vector — the per-task **primer** — derived from `rlang::hash(params)`. The primer SHALL pack 64 bits of the hash as `c(hi32, lo32)` suitable for dqrng's `stream` argument, so that `dqrng::dqset.seed(seed, stream = task_primer(params))` fully specifies a task's RNG starting point together with the scenario `seed`.

#### Scenario: Primer is a length-2 integer vector
- **WHEN** `task_primer(list(dataset = "boron", sim = 1L, replace = FALSE))` is called
- **THEN** the result SHALL be an integer vector of length 2

#### Scenario: Deterministic and reproducible
- **WHEN** `task_primer(p)` is called twice with identical `p`
- **THEN** the two results SHALL be `identical()`

#### Scenario: Sensitive to task parameters
- **WHEN** `task_primer(p1)` and `task_primer(p2)` are called with `p1` and `p2` differing in any parameter (e.g. `rescale = FALSE` vs `rescale = TRUE`)
- **THEN** the two primers SHALL differ

#### Scenario: Primer seeds dqrng reproducibly
- **WHEN** `dqrng::dqset.seed(seed, stream = task_primer(p))` is set twice with the same `seed` and `p`, drawing the same number of values each time
- **THEN** the two draw sequences SHALL be identical; a different `p` (or `seed`) SHALL yield a different sequence

### Requirement: Accepts a task-table row, normalised to a canonical plain list
`task_primer()` SHALL accept its argument either as a plain named list or as a single-row data frame (one row of a `{data,fit,hc}_tasks` table). A data-frame input SHALL be normalised to a canonical plain list — the inverse of `tibble::tibble_row()` — by dropping all attributes, unwrapping length-1 list-style columns to their element, and leaving df-style (nested data-frame) columns as data frames, before hashing. The resulting primer SHALL be identical whether derived from the row or from the equivalent plain list. The function SHALL abort, in the user-facing frame, when given input that is neither a plain list nor a single-row data frame.

#### Scenario: Row and equivalent list agree
- **WHEN** `task_primer()` is called on a single-row tibble and on the plain named list obtained by unwrapping that row
- **THEN** the two primers SHALL be identical

#### Scenario: Tibble attributes do not affect the primer
- **WHEN** the same task parameters are passed once as a one-row tibble (carrying class / `row.names` attributes and list-style columns) and once as a plain list
- **THEN** the primers SHALL be identical

#### Scenario: df-style columns preserved, list-style columns unwrapped
- **WHEN** a row carries both a list-style column (a length-1 list) and a df-style column (a one-row data frame)
- **THEN** normalisation SHALL unwrap the list-style column to its element and keep the df-style column as a data frame

#### Scenario: Multi-row data frame rejected
- **WHEN** `task_primer()` is given a data frame with more than one row (or a non-list, non-data-frame value)
- **THEN** the function SHALL abort with an informative error in the user-facing frame

### Requirement: 64-bit primer encoding with NA/INT_MIN mapping
`task_primer()` SHALL encode each 32-bit half of the hash slice as a signed int32, mapping the reserved bit pattern `0x80000000` (INT_MIN, which R cannot represent as a non-NA integer) to `NA_integer_`. dqrng accepts `NA_integer_` in `stream` and treats it as INT_MIN, so the encoding SHALL recover the full 64 bits of stream entropy.

#### Scenario: INT_MIN bit pattern encoded as NA
- **WHEN** a hash slice produces the 32-bit value `0x80000000`
- **THEN** the corresponding primer element SHALL be `NA_integer_`

#### Scenario: Other values encoded as signed int32
- **WHEN** a hash slice produces a 32-bit value other than `0x80000000`
- **THEN** the corresponding primer element SHALL be that value as a (possibly negative) non-`NA` int32

#### Scenario: Collision-resistant over many tasks
- **WHEN** `task_primer()` is applied to the distinct task-parameter sets in `scripts/experiment-dqrng-hash.R`
- **THEN** distinct task parameters SHALL yield distinct primers (no empirical collisions over the validated example set)

### Requirement: Canonical name-keyed hash input is a caller contract
`task_primer()` normalises **structure** (attributes, list-column wrapping) but **not meaning** — it hashes whatever `params` it is given. The canonical, name-keyed representation SHALL therefore be assembled by the caller that builds `params` (the `task-tables` construction over the `task-lists` tables, `TARGETS-DESIGN.md` §1.2/§5), not enforced inside `task_primer()`. Per the three-step model (`task-list-loop-baseline` + `task-list-loop-baseline-fold`) the RNG-consuming steps each take a primer over their task identity: the **`sample`** draw is keyed `(dataset, sim, replace)` only; the **`fit`** and **`hc`** tasks extend their parent identity with their argument-grid row (fit: `nrow`, `rescale`, `computable`, `at_boundary_ok`, `min_pmix` name, `range_shape1`, `range_shape2`; hc: `ci`, `nboot`, `est_method`, `ci_method`, `parametric`). The `fit` step truncates its parent sample inline (`head(sample, nrow)`, RNG-free) before fitting; the truncation itself takes no primer. `nrow` SHALL be absent from the `sample` primer — this is load-bearing for the §5 sub-truncation property (all `nrow` share one draw, which `head()` then truncates) — but `nrow` IS part of the `fit` and `hc` primers (a `fit` axis), because a different `nrow` is a genuinely different fit/hc input. Function-valued parameters (e.g. `min_pmix`) SHALL be referenced **by name**, not by function value, so a recompile or JIT does not change a task's primer. These scenarios SHALL be verified where `params` is built.

#### Scenario: Name order is significant
- **WHEN** `task_primer()` is called on two plain lists carrying the same names and values but in a different name order
- **THEN** the resulting primers SHALL differ, because `rlang::hash()` is order-sensitive and `task_primer()` does NOT re-order names — pinning the canonical name order as a caller contract rather than something normalised inside `task_primer()`

#### Scenario: min_pmix referenced by name
- **WHEN** a caller builds `params` referencing `min_pmix` by its name (a string) for two fit tasks whose underlying function *values* differ but whose names match
- **THEN** the resulting primers SHALL be identical

#### Scenario: sample primer keyed by (dataset, sim, replace) only
- **WHEN** a caller builds two `sample`-task `params` sharing `(dataset, sim, replace)` (the draw carries no `nrow` — the `fit` step truncates it inline later)
- **THEN** the resulting primers SHALL be identical

#### Scenario: nrow is part of the fit/hc primer
- **WHEN** a caller builds two `fit`-task (or `hc`-task) `params` that share everything except `nrow` (inherited from the `data` identity)
- **THEN** the resulting primers SHALL differ, because the fit/hc operates on a different data truncation

### Requirement: Per-task seed-and-run wrappers install a primer exactly once
The package SHALL provide three internal per-task **seed-and-run** wrappers — `sample_data_task_primer()`, `fit_data_task_primer()`, and `hc_data_task_primer()` — on the dqrng + primer contract. The naming follows the corrected GLOSSARY convention: the per-task value is a **primer** (not a `state`), so the functions use the `_primer` suffix and a `primer` argument. Each takes a scalar `seed` and a length-2 integer `primer`, calls `local_dqrng_state(seed, primer)` **exactly once** to install the per-task RNG starting point, then invokes the matching state-less op (`sample_data_task()`, `fit_data_task()`, `hc_data_task()`) against the now-set ambient RNG. The state-less ops SHALL NOT take a `primer`/`state`/`stream` argument. A wrapper SHALL leave the surrounding RNG state unchanged beyond its `local_dqrng_state()` scope, and SHALL assume an already-active `local_dqrng_backend()`.

#### Scenario: sample wrapper seeds then draws
- **WHEN** `sample_data_task_primer(data, n_max, replace, seed, primer)` is called within an active dqrng backend
- **THEN** it SHALL install `(seed, primer)` once via `local_dqrng_state()` and return `sample_data_task(data, n_max, replace)` — i.e. `dplyr::slice_sample(data, n = n_max, replace = replace)` drawn from that state

#### Scenario: fit wrapper seeds then fits
- **WHEN** `fit_data_task_primer(data, <fit grid>, seed, primer)` is called
- **THEN** it SHALL install `(seed, primer)` once and return `fit_data_task(data, <fit grid>)`, whose inner `ssdtools::ssd_fit_dists()` call takes no `primer`/`state`/`stream` argument

#### Scenario: hc wrapper seeds then estimates
- **WHEN** `hc_data_task_primer(fits, <hc grid>, seed, primer)` is called
- **THEN** it SHALL install `(seed, primer)` once and return `hc_data_task(fits, <hc grid>)`, whose inner `ssdtools::ssd_hc()` call takes no `primer`/`state`/`stream` argument

#### Scenario: Seeding happens exactly once per wrapper
- **WHEN** any `*_data_task_primer()` wrapper runs
- **THEN** `local_dqrng_state()` SHALL be invoked exactly once, and the state-less op SHALL consume RNG only from that installed state

#### Scenario: State-less ops are RNG-agnostic
- **WHEN** a state-less op (`sample_data_task()`, `fit_data_task()`, `hc_data_task()`) is called directly
- **THEN** it SHALL perform its operation against the ambient RNG with no `seed`/`primer`/`state`/`stream` argument, leaving seeding entirely to the wrapper

### Requirement: Same (seed, primer) reproduces a wrapper's result
For a fixed `seed` and `primer`, a `*_data_task_primer()` wrapper SHALL produce an identical result on repeated calls, and a different `primer` (or `seed`) SHALL in general produce a different result. The wrappers take the primer as an argument (computed by the caller via `task_primer()`); they do not derive it themselves.

#### Scenario: Reproducible draw
- **WHEN** `sample_data_task_primer(data, n_max, replace, seed, primer)` is called twice with the same `(seed, primer)`
- **THEN** the two draws SHALL be identical

#### Scenario: Distinct primers diverge
- **WHEN** two calls share `seed` but differ in `primer`
- **THEN** their results SHALL in general differ (independent streams)

### Requirement: nrow sub-truncation under seeding
The `sample`-step draw SHALL be a single `sample_data_task_primer()` of the scenario's **effective draw size** `D` rows keyed by the `(dataset, sim, replace)` primer, where `D = min(nrow_max, nrow(data))` for `replace = FALSE` and `D = nrow_max` for `replace = TRUE` — `nrow_max` being the scenario's fixed draw-size setting, **not** `max(nrow)`. The `fit` step SHALL truncate the draw with `head(sample, nrow)` (RNG-free). A size-`n` truncation SHALL be a byte-identical prefix of the size-`D` draw, for both `replace = FALSE` and `replace = TRUE`. Because `D` does not depend on `max(nrow)`, adding an `nrow` value (within `D`) SHALL NOT change the draw.

#### Scenario: head(n) is a prefix of the effective draw
- **WHEN** a `sample` draw of `D` rows is produced by `sample_data_task_primer()` and truncated to two sizes `n1 < n2 <= D`
- **THEN** `head(draw, n1)` SHALL be a byte-identical prefix of `head(draw, n2)`, so all `nrow` values share the one seeded draw

#### Scenario: Adding an nrow value does not change the draw
- **WHEN** a scenario's `sample` draw is produced, then an additional `nrow` value (no greater than the effective draw size) is added to the scenario and the draw is produced again with the same `seed`
- **THEN** the two draws SHALL be byte-identical, because the draw size is the fixed `nrow_max`, not `max(nrow)`

### Requirement: Baseline runner is reproducible per task
`ssd_run_scenario_baseline()` SHALL seed each `sample`/`fit`/`hc` task exactly once, via its `*_data_task_primer()` wrapper, with `seed = scenario$seed` and the per-task primer derived from the task's identity (`task_primer()`), under a single `local_dqrng_backend()` scope for the run. The runner's results SHALL be reproducible for a fixed `scenario$seed` **without** an externally pinned RNG, and SHALL be independent of the order in which tasks run.

#### Scenario: Re-running yields identical results
- **WHEN** `ssd_run_scenario_baseline(scenario)` is called twice for a scenario with a fixed `seed`, with no external `set.seed()`/`withr::with_seed()`
- **THEN** the two runs SHALL produce identical per-task `sample`/`fit`/`hc` results

#### Scenario: Results are order-independent
- **WHEN** the same task is run as part of the full scenario and again in isolation (same `scenario$seed` and task identity)
- **THEN** its result SHALL be identical, because its primer fully determines its RNG starting point

#### Scenario: Backend reset after the run
- **WHEN** `ssd_run_scenario_baseline()` returns (or errors)
- **THEN** the dqrng backend SHALL be reset and the caller's base RNG state SHALL be unchanged

### Requirement: Per-task dqrng backend integrity brackets
The per-task dqrng state installer (`local_dqrng_state()`) SHALL bracket each task's draws with the dqrng-specific integrity witness (`dqrng-backend` capability) on **both** ends, verifying that dqrng specifically (not merely *some* user-supplied RNG) holds base R's RNG slot. Both brackets SHALL live in `local_dqrng_state()` itself — the seed-and-run wrappers (`sample_data_task_primer()`, `fit_data_task_primer()`, `hc_data_task_primer()`) and `with_dqrng_state()` already route through it and SHALL require no additional integrity wiring:

- **Entry precondition** — before installing the task seed, `local_dqrng_state()` SHALL assert backend integrity via the witness, upgrading the previous cheap `RNGkind()`-based guard, so a task refuses to *start* on a foreign-hijacked or torn-down backend.
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

