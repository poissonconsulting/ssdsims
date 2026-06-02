## 1. dqrng-seeded state primitives

- [ ] 1.1 Add `R/state-primitives.R` (separate from the legacy L'Ecuyer primitives in `R/internal.R`)
- [ ] 1.2 `slice_sample_state(data, n_max, seed, state, replace)`: `local_dqrng_state(seed, state)` once, then `dplyr::slice_sample(data, n = n_max, replace = replace)`
- [ ] 1.3 `fit_dists_state(data, seed, state, dists, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2)`: seed once, then `ssdtools::ssd_fit_dists(...)` (resolve `min_pmix` by name via the existing helper); no `state`/`stream` on the inner call
- [ ] 1.4 `hc_state(fits, seed, state, proportion, ci, nboot, est_method, ci_method, parametric)`: seed once, then `ssdtools::ssd_hc(...)`, honouring the `ci = FALSE` branch (no bootstrap args)
- [ ] 1.5 Confirm each primitive calls `local_dqrng_state()` exactly once and leaves the surrounding RNG unchanged beyond its scope

## 2. Wire into the baseline runner

- [ ] 2.1 In `ssd_run_scenario_baseline()` (`R/task-lists.R`), open one `local_dqrng_backend()` scope for the run
- [ ] 2.2 `sample` step: per task, `primer <- task_primer(<dataset, sim, replace>)`; call `slice_sample_state(data[[dataset]], n_max, scenario$seed, primer, replace)`
- [ ] 2.3 `fit` step: per task, truncate `head(sample, nrow)` then `fit_dists_state(trunc, scenario$seed, task_primer(<fit identity>), <fit grid>)`
- [ ] 2.4 `hc` step: per task, `hc_state(fits, scenario$seed, task_primer(<hc identity>), <hc grid>)`
- [ ] 2.5 Assemble each primer from the task's canonical name-keyed identity (`task_axes(step)` columns); replace the unseeded `dplyr::slice_sample`/`fit_data_task`/`hc_data_task` internals (subsume the latter two)
- [ ] 2.6 Update the runner roxygen: remove the "not reproducible / pin the ambient RNG" caveat; document per-task seeding from `scenario$seed`

## 3. Tests

- [ ] 3.1 `tests/testthat/test-state-primitives.R` (under a `local_dqrng_backend()` scope): each primitive seeds once; same `(seed, state)` reproduces, different `state` diverges; surrounding RNG unchanged
- [ ] 3.2 Sub-truncation under seeding: a seeded `n_max` draw's `head(n1)` is a byte-identical prefix of `head(n2)`, for `replace = FALSE` and `replace = TRUE`
- [ ] 3.3 Runner reproducibility: two `ssd_run_scenario_baseline()` calls with a fixed `scenario$seed` and **no** external seed give identical `sample`/`fit`/`hc` results; backend reset and base `.Random.seed` unchanged afterwards
- [ ] 3.4 Order-independence: a task run in isolation matches its result within the full run (same `seed`, same identity)
- [ ] 3.5 Update `test-task-lists.R` runner tests to the seeded contract; re-pin any value-level snapshots to the dqrng streams

## 4. Docs and checks

- [ ] 4.1 Cross-reference the legacy vs dqrng primitives (note dqrng is the path forward; legacy removed in `cleanup-lecuyer`)
- [ ] 4.2 Run `devtools::document()`, `air format .`, `devtools::test()`, `devtools::check()` (0/0), and `pkgdown::check_pkgdown()`
