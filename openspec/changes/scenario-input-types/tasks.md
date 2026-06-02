## 1. Generator descriptor

- [ ] 1.1 Add an internal `ssdsims_generator` constructor in `R/data.R` storing `name`, `kind` (`"fitdists"`/`"tmbfit"`/`"function"`), and a resolvable `ref`; store no function body or model payload
- [ ] 1.2 Add an `inherits(x, "ssdsims_generator")` predicate helper for downstream discrimination

## 2. Input classification

- [ ] 2.1 Add `classify_input(value, expr, name, call)` in `R/data.R` returning a validated tibble (data-frame input) or an `ssdsims_generator` (fitdists/tmbfit/function/character input)
- [ ] 2.2 Dispatch on the most specific class first (`tmbfit` before `fitdists`); for a function-name string resolve via `get0()`/`match.fun()` in the caller environment and abort (in the user-facing frame) if it does not resolve to a function
- [ ] 2.3 Validate a generator function structurally (a function; single-argument `n` contract) without executing it; reuse the existing single-arg check pattern
- [ ] 2.4 Reuse the existing name derivation (`expr_to_name()`, list names, explicit `name=`) unchanged; for a function-name string use the string as the name; abort when no name is derivable

## 3. Wire into ssd_data() and the constructor

- [ ] 3.1 Route every element of `ssd_data(...)` through `classify_input()`; allow mixed data-frame and generator elements in one collection; keep unique-name enforcement
- [ ] 3.2 Extend `scenario_dataset_names()` (or its successor) so single-input, named-list, and unnamed-list paths accept generators; store the resulting descriptors/names in the scenario's `datasets` field
- [ ] 3.3 Ensure construction performs no RNG draws and leaves `.Random.seed` untouched for generator inputs
- [ ] 3.4 Keep generator materialisation out of scope — descriptors are inert (note pointing to `dataset-registry`, §1.1)

## 4. Print path

- [ ] 4.1 Render a generator dataset as `name <kind>` (e.g. `ssd_rlnorm <fn>`, `fit <fitdists>`) in `print.ssdsims_scenario()` and any `ssd_data()` print path; keep the data-frame rendering unchanged

## 5. Docs

- [ ] 5.1 Update roxygen for `ssd_data()` and `ssd_define_scenario()` to document the widened input contract (data frame / fitdists / tmbfit / function / function-name string) and the name-only, never-executed generator semantics
- [ ] 5.2 Remove the "data-frame-only" gap note now that generators are accepted; cross-reference `dataset-registry` for materialisation

## 6. Tests and checks

- [ ] 6.1 `tests/testthat/test-data.R`: `ssd_data()` accepts each generator kind singly and in a mixed list; descriptors carry name + kind and no payload; names derived/explicit; duplicate names rejected
- [ ] 6.2 `tests/testthat/test-scenario.R`: `ssd_define_scenario()` accepts each generator kind; `datasets` carries descriptors; `.Random.seed` unchanged; no data generated
- [ ] 6.3 Validation tests: unresolvable function-name string aborts; anonymous function literal with no name aborts; errors report the user-facing function as origin
- [ ] 6.4 Snapshot tests for `print` on generator and mixed-input scenarios
- [ ] 6.5 Run `devtools::document()`, `air format .`, and `devtools::check()`; update `NAMESPACE`/`man/`
