## 1. Input classification and materialisation

- [ ] 1.1 Add `classify_input(value, expr, name, call, .seed)` in `R/data.R` returning a validated tibble for **every** input: a data-frame input passes through `Conc` validation; a generator input (fitdists/tmbfit/function/character) is **materialised** to a tibble via its `ssd_sim_data()` method
- [ ] 1.2 Dispatch on the most specific class first (`tmbfit` before `fitdists`); for a function-name string resolve via `get0()`/`match.fun()` in the caller environment (bare name only, no `eval(parse())`) and abort (user-facing frame) if it does not resolve to a function
- [ ] 1.3 Validate a generator function structurally (a function; single-argument `n` contract) before executing it; reuse the existing single-arg check pattern
- [ ] 1.4 Reuse the existing name derivation (`expr_to_name()`, list names, explicit `name=`) unchanged; for a function-name string use the string as the name; abort when no name is derivable

## 2. Generation seeding (`.seed`) and the dqrng-only check

- [ ] 2.1 Add a dot-prefixed `.seed = NULL` formal to `ssd_data()`; confirm it is not absorbed into `...` and that `seed=` does not partial-match it
- [ ] 2.2 Materialise each generator under a scoped `local_dqrng_state(.seed, task_primer(list(dataset = name)))` — name as the dqrng stream, `.seed` as the base seed — so one `.seed` fans out across all generators on independent streams
- [ ] 2.3 Snapshot base R `.Random.seed` and the dqrng state around each generator run; abort (user-facing frame) when base-R RNG moved ("use dqrng, not base R") or when dqrng moved with `.seed = NULL` ("consumes RNG — supply `.seed`"); a pure generator passes
- [ ] 2.4 Abort when `.seed` is supplied but the call contains no generator inputs (Q1)
- [ ] 2.5 Confirm construction leaves global `.Random.seed` unchanged (scoped restore), even though generation draws RNG internally

## 3. Wire into ssd_data() and the constructor

- [ ] 3.1 Route every element of `ssd_data(..., .seed = NULL)` through `classify_input()`; allow mixed data-frame and generator inputs in one collection; keep unique-name enforcement; the collection stays a homogeneous named list of tibbles
- [ ] 3.2 Extend `scenario_dataset_names()` (in `R/scenario.R`) so single-input, named-list, and unnamed-list paths accept generators and store the materialised tibbles in `scenario$data` and their names in `datasets` — no new task-table columns, behind the existing `dataset` axis (#80)
- [ ] 3.3 Confirm a materialised generator dataset is indistinguishable downstream from a data-frame dataset (no descriptor type, no `inherits()` discrimination needed)

## 4. Baseline runner (#80)

- [ ] 4.1 Confirm `ssd_run_scenario_baseline()` draws from a generator-backed dataset with no change — it is a tibble in `scenario$data` — and that task-table derivation (`ssd_scenario_*_tasks()`) is unaffected (the `dataset` axis is just a name)

## 5. Docs

- [ ] 5.1 Update roxygen for `ssd_data()` (including `.seed`) and `ssd_define_scenario()` to document the widened input contract (data frame / fitdists / tmbfit / function / function-name string), eager materialisation, the name-as-stream `.seed`, and the dqrng-only/pure-generator rule
- [ ] 5.2 Remove the "data-frame-only" gap note now that generators are accepted and materialised at construction

## 6. Tests and checks

- [ ] 6.1 `tests/testthat/test-data.R`: `ssd_data()` accepts each generator kind singly and in a mixed list; generators materialise to tibbles with a `Conc` column; names derived/explicit; duplicate names rejected
- [ ] 6.2 `.seed` reproducibility: same `.seed` → byte-identical generated data; different `.seed` → different data; one `.seed` across several generators yields independent (name-streamed) draws; `.seed` supplied with no generators aborts
- [ ] 6.3 dqrng-only check: a base-R-RNG generator aborts ("use dqrng"); a dqrng generator with `.seed = NULL` aborts ("supply `.seed`"); a pure generator passes with or without `.seed`; global `.Random.seed` unchanged after construction
- [ ] 6.4 `tests/testthat/test-scenario.R`: `ssd_define_scenario()` accepts each generator kind; `scenario$data` holds materialised tibbles; `datasets` holds the names
- [ ] 6.5 Validation tests: unresolvable function-name string aborts; anonymous function literal with no name aborts; errors report the user-facing function as origin
- [ ] 6.6 `tests/testthat/test-task-lists.R`: task tables derive and `ssd_run_scenario_baseline()` runs for a generator dataset exactly as for a data-frame dataset
- [ ] 6.7 Snapshot tests for `print` on generator and mixed-input scenarios (a materialised generator prints as a data-frame dataset)
- [ ] 6.8 Run `devtools::document()`, `air format .`, and `devtools::check()`; update `NAMESPACE`/`man/`
