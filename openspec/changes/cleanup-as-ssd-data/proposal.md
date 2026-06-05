## Why

`ssd_data()` is the documented input normaliser for `ssd_define_scenario()` — it assembles one or more data frames into a validated, named `ssdsims_data` collection. But the *coercion* that takes the constructor's `data`/`name` arguments and turns them into that collection lives in an internal helper, `scenario_datasets()` (`R/scenario.R`). PR #80 review surfaced the gap: a caller who already holds a named list of data frames (or a single data frame plus a name) has no public way to obtain a validated `ssdsims_data` collection without going through the full scenario constructor. The natural public verb — `as_ssd_data()` — does not exist.

The catch (and the reason this is its own small change rather than a one-line export) is that `scenario_datasets()` cannot simply *become* `as_ssd_data()`: two of its input forms — a **bare data frame** and an **unnamed list** — derive their names by **symbol capture** (`expr_to_name()` / `list_expr_names()` over the captured argument expression), which only yields a meaningful name in the `ssd_define_scenario()` caller frame. A public coercer receiving an already-evaluated value cannot recover those names. So the public surface must be scoped to the *already-named* forms, and the symbol-capture forms must stay in the constructor frame.

## What Changes

- Add a **public** `as_ssd_data()` that coerces the already-named input forms into a validated `ssdsims_data` collection: (a) an `ssd_data()` collection passthrough, (b) a named list of data frames, and (c) a single data frame with an explicit `name=`. It validates each dataset through the same `Conc` contract `ssd_data()` uses (`ssd_data_validate()`).
- `as_ssd_data()` SHALL reject a **bare data frame with no name** and an **unnamed list** with a clear message pointing the caller at `ssd_define_scenario()` (or `ssd_data()`), because those forms require symbol capture that only the caller frame can perform.
- Refactor the internal `scenario_datasets()` to **delegate the already-named forms to `as_ssd_data()`**, doing the symbol-capture name derivation (bare data frame / unnamed list) in the constructor frame *first* and then handing the now-named value to `as_ssd_data()`. The symbol-capture logic (`expr_to_name()` / `list_expr_names()`) stays where it is.
- Export `as_ssd_data()` (roxygen `@export` + `NAMESPACE`), document it, and add it to the existing `_pkgdown.yml` reference group alongside `ssd_data`.

## Capabilities

### Modified Capabilities
- `scenario-definition`: adds a public `as_ssd_data()` coercer for the already-named input forms (collection passthrough, named list, single data frame with `name=`), with the bare-data-frame / unnamed-list forms explicitly out of its scope (they remain in the `ssd_define_scenario()` frame via symbol capture). The existing `ssd_data()` / `ssd_define_scenario()` input-assembly behaviour is unchanged.

## Impact

- **New code**: `as_ssd_data()` in `R/data.R` (next to `ssd_data()` / `ssd_data_validate()`); a refactor of `scenario_datasets()` in `R/scenario.R` to delegate the already-named forms to it.
- **APIs**: one new export, `as_ssd_data()`. Roxygen/`man/` and a `_pkgdown.yml` reference entry alongside `ssd_data`.
- **Behaviour**: no change to `ssd_data()` or `ssd_define_scenario()` — every input form the constructor accepts today still works, with the same names and the same `Conc` validation; this change only *factors out* the already-named coercion and surfaces it publicly.
- **Dependencies (DAG)**: an **independent tidy-up** with no dependants — not on the `TARGETS-DESIGN.md` §12 dependency DAG (§12 "Cleanup"). Can land at any time.
- **Cross-reference — orthogonal to `scenario-input-types`**: the in-flight `scenario-input-types` change extends `ssd_data()` to also accept data *generators* (`fitdists` / `tmbfit` / a function / a function-name string). That is orthogonal to this change: generators arrive at the input layer **already materialised and named**, so they flow through the *already-named* path that `as_ssd_data()` owns — there is no conflict, and this change does **not** specify generator handling. The two can land in either order.
