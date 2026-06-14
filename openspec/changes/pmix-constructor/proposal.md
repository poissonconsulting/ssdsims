## Why

`ssd_define_scenario()` infers the name for its `min_pmix` input by **parsing the
unevaluated argument expression** rather than the value. The default
`list(ssdtools::ssd_min_pmix)` is unnamed, yet the name `"ssd_min_pmix"` is
produced by capturing the argument with `rlang::enexpr()` (`R/scenario.R:153`) and
walking the literal `list(...)` call in `list_expr_names()`/`expr_to_name()`. This
is fragile metaprogramming: it works only for a *literal*
`list(<symbol or pkg::name>, ...)`, and breaks the moment a value is passed
indirectly:

```r
fns <- list(ssdtools::ssd_min_pmix)
ssd_define_scenario(..., min_pmix = fns)      # captures `fns`, not list(...)  -> ABORT
ssd_define_scenario(..., min_pmix = list(f))  # name becomes "f", the local var
```

We should adopt the **typed-collection constructor** pattern `ssd_scenario_data()` already
establishes for datasets: a dedicated constructor owns naming and validation
**by value**, the scenario accepts **only** that typed collection, and every
other form **fails loudly** with a message pointing at the constructor — no
string magic, no silent fallback, no indirect-value trap.

## What Changes

- **BREAKING — add `ssd_pmix()`**, returning a validated `ssdsims_pmix`
  collection of single-argument **functions** keyed by name: `ssd_pmix(ssd_min_pmix =
  ssdtools::ssd_min_pmix, strict = my_pmix)`. It owns naming (from `...` names, or
  per-argument symbol capture for a bare `symbol`/`pkg::name`, exactly as
  `ssd_scenario_data()` does — **scoped to the constructor**, not a generic argument) and
  validation (each entry is a single-argument function — **functions only, never a
  name-string**, so the constructor performs no string→function resolution).
- **`ssd_define_scenario(min_pmix = ...)` accepts ONLY an `ssd_pmix()` collection.**
  Every other form — a bare function, a (named or unnamed) plain list, or a
  character vector of names — SHALL abort with an error naming `ssd_pmix()`. There
  is no string path: resolving a name-string to a function would be the very
  "string magic" this change removes.
- **Remove the expression-based name inference** for `min_pmix`:
  `min_pmix_expr <- rlang::enexpr(min_pmix)` and the `min_pmix` branches of
  `scenario_min_pmix_materialise()` are retired in favour of the constructor's
  value-based naming. `expr_to_name()`/`list_expr_names()` remain only where
  datasets still use bare-data-frame symbol capture.
- **Change the `min_pmix` default** to an `ssd_pmix()` collection,
  `ssd_pmix(ssd_min_pmix = ssdtools::ssd_min_pmix)`, built at call time — a
  **value** (an `ssdsims_pmix` object), not a string and not a captured
  unnamed-`list()` expression.
- **Loud-error contract:** every non-`ssd_pmix()` `min_pmix` aborts in the
  user-facing function's context with an actionable message ("supply an
  `ssd_pmix()` collection"), not a silent fallback or an obscure `purrr`/`rlang`
  frame.

The stored scenario shape is unchanged (names + materialised functions keyed by
name); only the *input surface* and *where naming happens* change. Task hashing
remains name-only, so per-task results and caching are unaffected.

## Independence

This change is **independent** of `distset-hc-axis`: it touches only the
`min_pmix` input and the `ssd_pmix()` constructor. `distset-hc-axis` owns its own
`ssd_distset()` constructor for the `dists` input. Neither is a prerequisite for
the other; they share only the typed-constructor *philosophy* (and the
`ssd_scenario_data()` precedent), not code or specs beyond the common
`scenario-definition` capability they each extend in disjoint requirements.

## Naming note (`ssd_min_pmix()` vs `ssd_pmix()`)

The suggested `ssd_min_pmix()` **clashes with `ssdtools::ssd_min_pmix`** — the
very function users pass *into* the collection — so `min_pmix =
ssd_min_pmix(...)` would be ambiguous and shadow the dependency. This change uses
**`ssd_pmix()`** (the same reasoning that renamed `ssd_data()` →
`ssd_scenario_data()` to escape the `ssdtools::ssd_data()` clash, #150). See
design Decision 3.

## Capabilities

### New Capabilities
<!-- None: extends scenario-definition (no new spec file). -->

### Modified Capabilities
- `scenario-definition`: add `ssd_pmix()` as the validated, by-value collection
  constructor for the `min_pmix` input; the constructor owns naming/validation.
  `ssd_define_scenario()` requires **only** an `ssd_pmix()` collection for
  `min_pmix`; every other form (bare function, list, or character vector) aborts
  loudly. The `min_pmix` default becomes
  `ssd_pmix(ssd_min_pmix = ssdtools::ssd_min_pmix)`.

## Impact

- **Specs**: `scenario-definition` delta (`ssd_pmix()` + tightened `min_pmix`
  acceptance + loud errors).
- **Code**: `R/scenario.R` (drop `enexpr(min_pmix)` and the `min_pmix`
  expression-inference branches; require the `ssd_pmix()` collection; loud-error
  messages; default change), new `R/pmix.R` (constructor, validator, print
  method), `R/accessors.R` (`scenario_min_pmix()` reads the collection unchanged),
  `NAMESPACE`/`man/` (one new export).
- **BREAKING**: pre-release API tightening. Existing call sites passing a
  `min_pmix` function, list, or character vector must migrate to `ssd_pmix()`.
  Sweep examples, tests, snapshots, `scripts/`, `vignettes/`,
  `inst/targets-templates/`.
- **No RNG / results impact**: stored names and materialised functions are
  unchanged, so task hashes, per-task results, and caching are identical; this is
  an input-surface change only.
- **Migration**: `min_pmix = ssdtools::ssd_min_pmix` →
  `min_pmix = ssd_pmix(ssd_min_pmix = ssdtools::ssd_min_pmix)` (also the new
  default).
