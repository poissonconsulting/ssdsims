## 1. Materialise min_pmix on the scenario

- [x] 1.1 Extend `ssd_define_scenario()` (`R/scenario.R`) to materialise `min_pmix` functions keyed by name alongside the names it already stores: keep a supplied function under its derived name; resolve a name-string to a function at construction (from `ssdtools` / the caller's environment)
- [x] 1.2 Fail fast in the constructor frame when a `min_pmix` name cannot be resolved to a single-argument function
- [x] 1.3 Leave the stored `min_pmix` names (`$fit$min_pmix`) and thus `task_axes("fit")`/`task_primer()` unchanged; store functions in a parallel name-keyed field (e.g. `$min_pmix_fns`)

## 2. Accessors

- [x] 2.1 Add `scenario_dataset(scenario, name)`: return the materialised dataset tibble for `name`; abort (user-facing frame) on an unknown name
- [x] 2.2 Add `scenario_min_pmix(scenario, name)`: return the materialised `min_pmix` function for `name`; abort on an unknown name

## 3. Rewire resolution

- [x] 3.1 Replace `resolve_min_pmix()` in `R/task-lists.R` with a call to `scenario_min_pmix(scenario, name)`; thread the scenario into the fit path where needed
- [x] 3.2 Confirm the baseline runner's results are unchanged for the default `min_pmix` (constructor resolves `"ssd_min_pmix"` to the same `ssdtools` function the runtime lookup returned)

## 4. Remove the registry

- [x] 4.1 Remove any `ssd_register_dataset()` / `ssd_register_min_pmix()` / Parquet-persistence / `_index.json` plans; this change adds **no** new dependency (no `arrow`/`duckplyr`/`digest` here — those belong to `task-tables`/`manifest`)
- [x] 4.2 Note that name-only regeneration of large datasets is the deferred `dataset-provenance` step

## 5. Docs and reference

- [x] 5.1 Roxygen for `scenario_dataset()` / `scenario_min_pmix()` and the `min_pmix` materialisation; document that names (not values) drive hashing
- [x] 5.2 Add a `scenario-accessors` reference group to `_pkgdown.yml`

## 6. Tests and checks

- [x] 6.1 `tests/testthat/`: a supplied `min_pmix` function is materialised under its derived name; a name-string resolves at construction; an unresolvable name fails fast in the constructor frame
- [x] 6.2 `scenario_dataset()` returns the materialised tibble for a known name and errors on an unknown one
- [x] 6.3 `scenario_min_pmix()` returns the materialised function for a known name and errors on an unknown one
- [x] 6.4 Hash stability: two scenarios with the same `min_pmix` name but different function bodies yield byte-identical fit-task primers (names drive hashing, not values)
- [x] 6.5 `resolve_min_pmix()` resolves via the accessor; the baseline runner's default-`min_pmix` results are unchanged
- [x] 6.6 Run `devtools::document()`, `air format .`, and `devtools::check()`; update `NAMESPACE`/`man/`
