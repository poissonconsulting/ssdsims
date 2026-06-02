## Why

The merged three-step baseline runner (`ssd_run_scenario_baseline()`, `task-list-loop-baseline` + `…-fold`) is explicitly **not reproducible**: it draws from the ambient RNG with no per-task seeding (its docs say to pin `withr::with_seed()` for a deterministic run). The targets design (`TARGETS-DESIGN.md` §2) fixes per-task RNG by giving every task its own primer and seeding `dqrng::dqset.seed(seed = scenario$seed, stream = primer)` once per task. The primitives that install that primer — `slice_sample_state`, `fit_dists_state`, `hc_state` — currently exist only in their legacy L'Ecuyer-CMRG form (`R/internal.R`, used by the old `ssd_run_scenario()` path). This step lands the dqrng + primer versions and wires them into the new runner, making it reproducible independently of run order. Its inputs have landed (`local-dqrng-state` #78; `task-list-loop-baseline` + fold) except `task-primer`, which must be applied first.

## What Changes

- Establish the three per-task **`_state` primitives** on the dqrng + primer contract. Each takes `(…, seed, state)` where `seed = scenario$seed` and `state` is the per-task primer (`task_primer()`), calls `local_dqrng_state(seed, state)` **exactly once**, then runs the state-less ssdtools/dplyr operation against the now-set ambient RNG — no `state =` argument on the inner op:
  - `slice_sample_state(data, n_max, seed, state, replace)` — seeds, then `dplyr::slice_sample(data, n = n_max, replace = replace)` (the `sample`-step draw of `n_max = max(nrow)` rows).
  - `fit_dists_state(data, seed, state, <fit grid>)` — seeds, then `ssdtools::ssd_fit_dists(...)` on the inline `head(sample, nrow)` truncation.
  - `hc_state(fits, seed, state, <hc grid>)` — seeds, then `ssdtools::ssd_hc(...)`.
- Wire these into `ssd_run_scenario_baseline()`: each `sample`/`fit`/`hc` task installs its primer exactly once (under a `local_dqrng_backend()` scope), derived from the row's task identity via `task_primer()`. The runner becomes **reproducible without an external seed** — re-running, or running tasks in any order, yields identical results for a fixed `scenario$seed`.
- **Absorbs `nrow-sub-truncation`** (now redundant): the seeded `n_max` draw is `slice_sample_state` and the `head(sample, nrow)` truncation is the fit step's inline, RNG-free step (already merged). The byte-equivalent-prefix property holds under seeding.
- The `ci = FALSE` collapse in the hc task table is already implemented (`task-list-loop-baseline`) and is consumed as-is.

## Capabilities

### New Capabilities
<!-- None: extends the existing parallel-safe-seeding capability. -->

### Modified Capabilities
- `parallel-safe-seeding`: add the dqrng + primer per-task seeding contract — the three `_state` primitives that install a per-task primer exactly once via `local_dqrng_state()`, and the resulting per-task reproducibility of the baseline runner.

## Impact

- **New code**: dqrng-seeded `slice_sample_state()` / `fit_dists_state()` / `hc_state()` (new file, e.g. `R/state-primitives.R`, kept separate from the legacy L'Ecuyer ones in `R/internal.R`); wiring in `ssd_run_scenario_baseline()` (`R/task-lists.R`) to seed each task via `task_primer()`; tests in `tests/testthat/test-state-primitives.R` and updates to `test-task-lists.R` (runner reproducibility).
- **APIs**: internal primitives (not exported); no public surface change. `ssd_run_scenario_baseline()` keeps its signature but gains per-task seeding (its "not reproducible" caveat is lifted).
- **Dependencies**: `task-primer` (provides `task_primer()`) **must be applied first**; builds on `local-dqrng-state` (#78) and `task-list-loop-baseline` (+ fold). No new package dependencies.
- **Downstream**: unblocks `migrate-public-api` (migrate `ssd_sim_data.data.frame` / `ssd_fit_dists_sims` / `ssd_hc_sims` to this contract, keep `_seed` shims). The legacy L'Ecuyer `_state`/`_seed` primitives stay until `cleanup-lecuyer`. Lets `nrow-sub-truncation` and `ci-false-collapse` be struck from §12 as completed.
