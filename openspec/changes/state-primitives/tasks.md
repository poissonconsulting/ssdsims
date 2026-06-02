## 1. State-less ops

- [ ] 1.1 Add `sample_data_task(data, n_max, replace)` — `dplyr::slice_sample(data, n = n_max, replace = replace)` — lifting the runner's inline draw into a named op (symmetry with `fit_data_task`/`hc_data_task`)
- [ ] 1.2 Confirm `fit_data_task()` / `hc_data_task()` stay state-less (no `seed`/`state`/`stream` argument; `min_pmix` resolved by name; `ci = FALSE` branch preserved)

## 2. Seed-and-run wrappers

- [ ] 2.1 Add `sample_data_task_state(data, n_max, replace, seed, state)`: `local_dqrng_state(seed, state)` once, then `sample_data_task(...)`
- [ ] 2.2 Add `fit_data_task_state(data, dists, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2, seed, state)`: seed once, then `fit_data_task(...)`
- [ ] 2.3 Add `hc_data_task_state(fits, proportion, ci, nboot, est_method, ci_method, parametric, seed, state)`: seed once, then `hc_data_task(...)`
- [ ] 2.4 Wrappers assume an active `local_dqrng_backend()`, take the primer as `state` (caller-computed), and leave the surrounding RNG unchanged beyond the `local_dqrng_state()` scope
- [ ] 2.5 Do NOT touch the legacy L'Ecuyer `slice_sample_state`/`fit_dists_state`/`hc_state` in `R/internal.R` (distinct names; removed at `cleanup-lecuyer`)

## 3. Wire into the baseline runner

- [ ] 3.1 In `ssd_run_scenario_baseline()`, open one `local_dqrng_backend()` scope for the run
- [ ] 3.2 `sample` step: per task `primer <- task_primer(<dataset, sim, replace>)`; call `sample_data_task_state(data[[dataset]], n_max, replace, scenario$seed, primer)`
- [ ] 3.3 `fit` step: per task truncate `head(sample, nrow)`, then `fit_data_task_state(trunc, <fit grid>, scenario$seed, task_primer(<fit identity>))`
- [ ] 3.4 `hc` step: per task `hc_data_task_state(fits, <hc grid>, scenario$seed, task_primer(<hc identity>))`
- [ ] 3.5 Assemble each primer from the task's canonical name-keyed identity (`task_axes(step)` columns); replace the unseeded inline `slice_sample` / direct `fit_data_task` / `hc_data_task` calls
- [ ] 3.6 Update the runner roxygen: remove the "not reproducible / pin the ambient RNG" caveat; document per-task seeding from `scenario$seed` and that the `*_data_task_state()` wrappers are the per-task entry point targets/replay reuse

## 4. Tests

- [ ] 4.1 Focused wrapper tests (under a `local_dqrng_backend()` scope): each wrapper seeds once; same `(seed, state)` reproduces, different `state` diverges; surrounding RNG unchanged; the state-less ops take no RNG argument
- [ ] 4.2 Sub-truncation under seeding: a seeded `n_max` draw's `head(n1)` is a byte-identical prefix of `head(n2)`, for `replace = FALSE` and `replace = TRUE`
- [ ] 4.3 Runner reproducibility: two `ssd_run_scenario_baseline()` calls with a fixed `scenario$seed` and **no** external seed give identical `sample`/`fit`/`hc` results; backend reset and base `.Random.seed` unchanged afterwards
- [ ] 4.4 Order-independence: a task run in isolation matches its result within the full run (same `seed`, same identity)
- [ ] 4.5 Update `test-task-lists.R` runner tests to the seeded contract; re-pin any value-level snapshots to the dqrng streams

## 5. Docs and checks

- [ ] 5.1 Cross-reference the legacy vs dqrng primitives (note dqrng is the path forward; legacy removed in `cleanup-lecuyer`)
- [ ] 5.2 Run `devtools::document()`, `air format .`, `devtools::test()`, `devtools::check()` (0/0), and `pkgdown::check_pkgdown()`
