## 1. `ssd_pmix()` constructor

- [x] 1.1 Add `R/pmix.R`: `ssd_pmix(...)` returning an `ssdsims_pmix` collection (named list of single-argument **functions only** — no name-string entries, no string→function resolution). Names from `...` names, else per-argument symbol capture for a bare `symbol`/`pkg::name` (mirroring `ssd_scenario_data()`; capture each `...` promise individually). Validate: each a single-argument function, names unique/non-missing. Abort in the constructor's context on a non-function, wrong arity, a name-string, or a duplicate name.
- [x] 1.2 Add a `print.ssdsims_pmix()` method (names + `<fn>` placeholders, snapshot-stable).
- [x] 1.3 Export `ssd_pmix`; `devtools::document()`.

## 2. Scenario constructor: require the typed collection, by value

- [x] 2.1 In `R/scenario.R`, change `min_pmix` to accept **only** an `ssd_pmix()` collection. Default to `ssd_pmix(ssd_min_pmix = ssdtools::ssd_min_pmix)`. A bare function, plain list, or character vector SHALL abort with a message naming `ssd_pmix()`.
- [x] 2.2 Remove `min_pmix_expr <- rlang::enexpr(min_pmix)` and the `min_pmix` branches of `scenario_min_pmix_materialise()`; materialise functions/names directly from the collection or character vector (value-based, no expression parsing).
- [x] 2.3 Retire `list_expr_names()`/`expr_to_name()` usage for `min_pmix`; keep them only where datasets still use bare-data-frame symbol capture (or remove if no longer referenced).
- [x] 2.4 Confirm the stored scenario shape (`fit$min_pmix` names, materialised `min_pmix_fns`) and therefore task hashes are unchanged versus supplying the same names/functions the old way.

## 3. Tests

- [x] 3.1 `ssd_pmix()` tests: names from `...` names; derived name for a bare `pkg::name`; rejects a non-function, a multi-argument function, a name-string (no resolution attempted), and duplicate names.
- [x] 3.2 Loud-error tests: `min_pmix` as a function, a list, and a character vector each abort with a message naming `ssd_pmix()`; the indirect-value case (`fns <- list(...); min_pmix = fns`) that the old path mishandled now aborts cleanly with the same actionable message.
- [x] 3.3 Hash-stability test: a scenario built via `ssd_pmix()` (or the default `ssd_pmix(ssd_min_pmix = ssdtools::ssd_min_pmix)`) produces the same `fit$min_pmix` names, materialised functions, and per-task primers/hashes as the equivalent names/functions supplied to the prior interface (no results/caching drift).
- [x] 3.4 No-`enexpr` test: `min_pmix` naming is unaffected by how the argument is constructed (literal vs variable vs programmatic), since the expression is never read.

## 4. Call sites, docs, snapshots

- [x] 4.1 Sweep all `min_pmix` call sites — `@examples`, `tests/`, `scripts/`, `vignettes/`, `inst/targets-templates/`, `man/` — to `ssd_pmix(...)` or the default (drop the argument).
- [x] 4.2 Update roxygen `@param min_pmix`, the `defining-a-scenario` vignette, `GLOSSARY.md`, and `TARGETS-DESIGN.md` references to the `min_pmix` input; document the `ssd_min_pmix()`-clash rationale for `ssd_pmix()`.
- [x] 4.3 Re-record printed-scenario / printed-collection snapshots.
- [x] 4.4 Run `air format .`, `devtools::document()`, `devtools::test()`, `devtools::check()`.
- [x] 4.5 Add a `NEWS.md` BREAKING entry (constructor-required `min_pmix`; default `min_pmix` change).
