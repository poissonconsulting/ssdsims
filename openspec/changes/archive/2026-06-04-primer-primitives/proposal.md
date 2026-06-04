## Why

The merged three-step baseline runner (`ssd_run_scenario_baseline()`, `task-list-loop-baseline` + `…-fold`) is explicitly **not reproducible**: it draws from the ambient RNG with no per-task seeding (its docs say to pin `withr::with_seed()` for a deterministic run). The targets design (`TARGETS-DESIGN.md` §2) fixes per-task RNG by giving every task its own primer and seeding `dqrng::dqset.seed(seed = scenario$seed, stream = primer)` once per task. This step adds that seeding as a reusable per-task **seed-and-run** function and wires it into the runner. The same function is what a future `targets` shard body (and `replay-helper`, §7) will call: a worker runs in its own process and must install the RNG itself, so the seed-and-run step must be a named, self-contained function, not an ambient side effect of a loop. Its inputs have landed (`local-dqrng-state` #78; `task-list-loop-baseline` + fold) except `task-primer`, which must be applied first.

## What Changes

- Keep the existing state-less per-task **ops** and add a peer for the draw, for symmetry — none of them touch the RNG (no `primer`/`state`/`stream` argument):
  - `sample_data_task(data, n_max, replace)` — `dplyr::slice_sample(data, n = n_max, replace = replace)` (lifts the runner's inline draw into a named op).
  - `fit_data_task(data, <fit grid>)` — `ssdtools::ssd_fit_dists(...)` (unchanged).
  - `hc_data_task(fits, <hc grid>)` — `ssdtools::ssd_hc(...)` (unchanged).
- Add the **seed-and-run wrappers** (`*_data_task_primer()`), the per-task "everything" primitives. The naming follows the corrected GLOSSARY convention (`primer =` argument of the `_primer` functions — the value is a **primer**, not a `state`; "state" was a documented historical misnomer). Each takes the op inputs plus `(seed, primer)`, calls `local_dqrng_state(seed, primer)` **exactly once** (assuming an already-active `local_dqrng_backend()`), then the matching state-less op:
  - `sample_data_task_primer(data, n_max, replace, seed, primer)`
  - `fit_data_task_primer(data, <fit grid>, seed, primer)`
  - `hc_data_task_primer(fits, <hc grid>, seed, primer)`
  - `seed = scenario$seed`; `primer` is the per-task primer (`task_primer()`), computed by the caller from the row's identity. Wrappers stay schema-agnostic — the row→(args, primer) unpacking lives in the caller. They pass the primer to `local_dqrng_state()` (whose own argument is still named `state`, a #78 leftover left for a later rename).
- Wire the wrappers into `ssd_run_scenario_baseline()`: open one `local_dqrng_backend()` scope for the run, then each `sample`/`fit`/`hc` task installs its primer exactly once. The runner becomes **reproducible without an external seed** — re-running, or running tasks in any order, yields identical results for a fixed `scenario$seed`. The wrappers are the same primitives the future `targets` shard body and `replay-helper` will call.
- **Absorbs `nrow-sub-truncation`** (now redundant): the seeded `n_max` draw is `sample_data_task_primer()`; the `head(sample, nrow)` truncation is the fit step's inline, RNG-free step (already merged). The byte-equivalent-prefix property holds under seeding.
- The `ci = FALSE` collapse in the hc task table is already implemented (`task-list-loop-baseline`) and consumed as-is.
- Update `GLOSSARY.md` so the corrected `_primer`/`primer` naming points at these new functions; the legacy L'Ecuyer `slice_sample_state()` / `fit_dists_state()` / `hc_state()` in `R/internal.R` keep the old names (and back the old `ssd_run_scenario()` path) until `cleanup-lecuyer`.

## Capabilities

### New Capabilities
<!-- None: extends the existing parallel-safe-seeding capability. -->

### Modified Capabilities
- `parallel-safe-seeding`: add the dqrng + primer per-task seeding contract — the `*_data_task_primer()` seed-and-run wrappers that install a per-task primer exactly once via `local_dqrng_state()`, and the resulting per-task reproducibility of the baseline runner.

## Impact

- **New code**: the `*_data_task_primer()` wrappers and `sample_data_task()` (in `R/task-lists.R` alongside the existing ops, or a co-located `R/primer-primitives.R`); runner wiring to seed each task via `task_primer()`; tests in `tests/testthat/test-task-lists.R` (runner reproducibility) and a focused primitives test; `GLOSSARY.md` naming update.
- **APIs**: internal only (not exported); no public surface change. `ssd_run_scenario_baseline()` keeps its signature but gains per-task seeding (its "not reproducible" caveat is lifted).
- **Dependencies**: `task-primer` (provides `task_primer()`) **must be applied first**; builds on `local-dqrng-state` (#78) and `task-list-loop-baseline` (+ fold). No new package dependencies.
- **Downstream**: the `*_data_task_primer()` wrappers are the per-task entry point the `targets` shard body and `replay-helper` (§7) will reuse. Unblocks `migrate-public-api` (migrate `ssd_sim_data.data.frame` / `ssd_fit_dists_sims` / `ssd_hc_sims` to this contract, keep `_seed` shims) and a later rename of the `local_dqrng_state()` argument. Lets `nrow-sub-truncation` and `ci-false-collapse` be struck from §12 as completed.
