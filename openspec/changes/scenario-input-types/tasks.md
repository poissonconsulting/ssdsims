## 1. Rename `ssd_data()` → `ssd_scenario_data()`

- [ ] 1.1 Rename the exported `ssd_data()` to `ssd_scenario_data()` in `R/data.R` (keep the `ssdsims_data` class and the `ssd_data_validate()`/`ssd_data_names()` internals); update `@export`/roxygen and the function's own examples
- [ ] 1.2 Update every call site and reference: `R/scenario.R` (`scenario_datasets()`), `R/accessors.R` docs, `NAMESPACE`, `_pkgdown.yml`, `man/`, `README`/`README.Rmd`, and the `vignettes/defining-a-scenario.qmd` vignette
- [ ] 1.3 Update tests referencing `ssd_data()` in `tests/testthat/test-scenario.R` (and any snapshots) to `ssd_scenario_data()`

## 2. `ssd_gen()` — generator materialisation

- [ ] 2.1 Add `ssd_gen(..., .n, .seed)` (in `R/gen.R`) returning a classed `ssdsims_gen` named list of validated `Conc` tibbles; make `.n` and `.seed` **required**, dot-prefixed formals (confirm neither is absorbed into `...` nor partial-matched from a `seed=`/`n=` generator name)
- [ ] 2.2 Add a `classify_gen(value, expr, name, env, call)` dispatch (most-specific first: `tmbfit` before `fitdists`) resolving each input to a single-argument draw `fn(n)`: function → itself; function-name string → bare-name `get0()`/`match.fun()` in the caller env then `ssdtools` (no `eval(parse())`); `tmbfit` → `ssdtools::estimates()` + matching `ssd_r<dist>` via `do.call`; `fitdists` → top-weighted dist (`glance(wt = TRUE)` → `which.max`) then the `tmbfit` path
- [ ] 2.3 Reject a `data.frame` input in `ssd_gen()` with an error directing the user to `ssd_scenario_data()`; validate a generator function structurally (is a function) before executing it
- [ ] 2.4 Derive names: argument name → string itself (for the character form) → `expr_to_name()`; abort when no name is derivable; enforce unique names across the call
- [ ] 2.5 Materialise each generator under a scoped `local_dqrng_state(.seed, task_primer(list(dataset = name)))` (name as the dqrng stream, `.seed` as the base seed) so one `.seed` fans out across all generators on independent streams; build each element as `tibble(Conc = fn(.n))`

## 3. dqrng contract (reuse `task-rng-postcheck`, #117)

- [ ] 3.1 Gate every dqrng touch in `ssd_gen()` behind `dqrng_usable()`; abort with actionable `library(dqrng)` (`>= 0.4.1`) guidance when dqrng is not already loaded; open a scoped `local_dqrng_backend()` for the materialisation
- [ ] 3.2 After each generator runs, verify the draw came from dqrng via `chk_dqrng_backend_intact()`; abort (user-facing frame) when a generator escaped dqrng; a pure (no-draw) generator passes
- [ ] 3.3 Confirm `ssd_gen()` leaves the global `.Random.seed` unchanged on return (scoped restore), even though generation draws RNG internally
- [ ] 3.4 Record the dependency edge `task-rng-postcheck → scenario-input-types` (DESCRIPTION/`TARGETS-DESIGN.md` §12); sequence after #117 (or branch from `claude/laughing-tesla-SmDdr`)

## 4. Wire into `ssd_scenario_data()` and the constructor

- [ ] 4.1 Teach `ssd_scenario_data()` to accept an `ssdsims_gen` argument: an unnamed `ssd_gen(...)` argument is flattened into the collection (`inherits("ssdsims_gen")`), and `!!!ssd_gen(...)` splices identically; keep unique-name enforcement; the collection stays a homogeneous named list of tibbles
- [ ] 4.2 Change `ssd_define_scenario()` to accept dataset input **only** as an `ssd_scenario_data()` collection: drop the `name=` argument and the bare-data-frame/bare-list/`data_expr` routing in `scenario_datasets()` (collapse to "assert `ssdsims_data`, return it"); update the constructor roxygen and `man/`
- [ ] 4.3 Confirm a materialised generator dataset is indistinguishable downstream from a data-frame dataset (no descriptor type, no `inherits()` discrimination needed) and that `ssd_define_scenario()` performs no RNG (the "No side effects on RNG state" requirement still holds)

## 5. Baseline runner (#80)

- [ ] 5.1 Confirm `ssd_run_scenario_baseline()` draws from a generator-backed dataset with no change — it is a tibble in `scenario$data` — and that task-table derivation (`ssd_scenario_*_tasks()`) is unaffected (the `dataset` axis is just a name)

## 6. Docs

- [ ] 6.1 Document `ssd_gen()` (the four generator kinds, required `.n`/`.seed`, name-as-stream seeding, the dqrng-only/`library(dqrng)` rule, the `ssdsims_gen` return and both call forms) and update `ssd_scenario_data()`/`ssd_define_scenario()` roxygen for the rename, the collection-only input, and the dropped `name=`
- [ ] 6.2 Remove the "data-frame-only" gap note (and the `name=` single-data-frame forms) now that generators are accepted via `ssd_gen()` and materialised at construction; refresh the `defining-a-scenario` vignette and README examples

## 7. Tests and checks

- [ ] 7.1 `tests/testthat/test-data.R`: `ssd_gen()` accepts each generator kind (function, function-name string, `fitdists`, `tmbfit`) singly and several together; materialises to `Conc` tibbles of `.n` rows; names derived/explicit; a data frame is rejected; duplicate names rejected
- [ ] 7.2 `.seed` reproducibility: same `.seed` → byte-identical; different `.seed` → different; one `.seed` across several generators yields independent (name-streamed) draws; `.seed`/`.n` required (omitting either aborts)
- [ ] 7.3 dqrng contract: `dqrng` not loaded aborts with `library(dqrng)` guidance; a base-R-escaping generator aborts; a pure generator passes; global `.Random.seed` unchanged after `ssd_gen()`
- [ ] 7.4 `ssd_scenario_data()`: an unnamed `ssd_gen(...)` argument and `!!!ssd_gen(...)` produce identical collections mixing data frames and generators
- [ ] 7.5 `tests/testthat/test-scenario.R`: `ssd_define_scenario()` accepts an `ssd_scenario_data()` collection with generator datasets; rejects a bare data frame / `name=`; `scenario$data` holds materialised tibbles and `datasets` holds the names; construction leaves `.Random.seed` unchanged
- [ ] 7.6 Validation tests: unresolvable function-name string aborts; anonymous function literal with no name aborts; errors report the user-facing function (`ssd_gen()`) as origin
- [ ] 7.7 `tests/testthat/test-task-lists.R`: task tables derive and `ssd_run_scenario_baseline()` runs for a generator dataset exactly as for a data-frame dataset
- [ ] 7.8 Snapshot tests for `print` on a scenario with generator and mixed inputs (a materialised generator prints as a data-frame dataset)
- [ ] 7.9 Run `devtools::document()`, `air format .`, and `devtools::check()`; update `NAMESPACE`/`man/`
