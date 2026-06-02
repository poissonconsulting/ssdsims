## Context

`ssd-define-scenario` landed a declarative `ssdsims_scenario` and an `ssd_data()` collector that are **data-frame-only**. `ssd_run_scenario()`, by contrast, dispatches over five `ssd_sim_data()` S3 methods — `data.frame`, `fitdists`, `tmbfit`, `function`, and `character` (a function-name string) — where the last four are *data generators*: they produce data at run time rather than carrying it. The targets redesign (`TARGETS-DESIGN.md` §1.1) keeps the scenario serialisable to a tiny manifest by storing **names**, not values, and materialising generated data once per project in a `dataset-registry` target. This change widens the scenario's input contract to those four generator types while preserving the name-only, declarative discipline. `ssd_run_scenario()` is untouched.

## Goals / Non-Goals

**Goals:**

- Accept the same input set as `ssd_run_scenario()` in `ssd_data()` and `ssd_define_scenario()`: data frame, `fitdists`, `tmbfit`, function, function-name string — singly or in a list.
- Record each non-data-frame input declaratively as a *generator descriptor* (name + kind + a captured reference), with no function body or model payload stored.
- Reuse the existing name-derivation machinery (`expr_to_name()`, `list_expr_names()`, argument names) unchanged so naming is identical across data frames and generators.
- Validate generator inputs structurally, in the user-facing frame, without executing them or touching RNG.

**Non-Goals:**

- **Materialising** generator data — running the generator to produce a tibble. That is the targets-only `dataset-registry` change (§1.1); descriptors are inert here.
- Carrying a `seed`/`stream` for a generator (the per-task RNG mechanism, §2). The scenario already owns the root `seed`; generator materialisation seeds are a registry concern.
- Changing the `fit`/`hc` grids, `partition_by`, the `ci = FALSE` rule, or any non-dataset field.
- Touching the legacy `ssd_run_scenario()` / `ssd_sim_data()` dispatch.

## Decisions

### Decision: a generator descriptor is a small classed record, datasets become a heterogeneous collection

The `ssdsims_data` collection today is a named list of tibbles. We make it a named list whose elements are **either** a tibble (inline data-frame dataset) **or** a generator descriptor. A descriptor is a tiny classed list, e.g.

```r
structure(
  list(name = "ssd_rlnorm", kind = "function", ref = "ssd_rlnorm"),
  class = "ssdsims_generator"
)
```

- `kind` ∈ `{"fitdists", "tmbfit", "function"}` — the dispatch target the registry will use.
- `ref` is the resolvable reference: the function name (for `function`/`character` inputs) or `NA`/the dataset name for object inputs whose payload the registry pins separately. We store **no** function body and **no** `fitdists`/`tmbfit` object — only what `dataset-registry` needs to look the generator up later.

*Why a classed descriptor rather than just a name string?* The kind must survive into the scenario so `dataset-registry` knows which `ssd_sim_data()` method to dispatch without re-inspecting a (no-longer-present) object. A classed record also lets `print()` render `<fitdists>` / `<fn>` distinctly and gives downstream code a clean `inherits()` check. *Alternative considered:* a parallel `kinds` vector alongside `datasets`; rejected as it splits one fact across two fields and complicates the list/mixed-input case.

### Decision: route on input type in one place, reuse name derivation

`ssd_data()` and `scenario_dataset_names()` already branch on `is.data.frame()` / `is.list()`. We add the generator branches at the same junction and funnel every element through a single `classify_input(value, expr, name, call)` helper that returns either a validated tibble or an `ssdsims_generator`. Name derivation is unchanged: argument name → `expr_to_name()` (symbol / `::` call) → explicit `name=`/list name. For a function-name **string**, the *string value itself* is the name (and the resolution target). Unlike `ssd_sim_data.character()`, which does `eval(parse(text = x))`, resolution here is **restricted to a bare name** looked up with `get0()`/`match.fun()` (see the validation decision below); arbitrary expressions are not evaluated, matching the declarative intent.

*Why funnel through one helper?* The single-input, named-list, and unnamed-list paths in both `ssd_data()` and the constructor must treat generators identically; one classifier keeps the three call sites in sync and keeps mixed lists (data frame + generator) correct.

### Decision: structural validation only, in the user-facing frame

Per the repo error-origin convention, validation aborts with `chk::abort_chk(..., call = call)` where `call` is the exported function's frame. Checks:

- **function**: `is.function(x)` and (where the contract is a single-arg RNG generator) the single-argument shape already checked for `min_pmix`; we reuse `check_min_pmix_function()`'s pattern but as a dataset-generator check (the generator's first argument is `n`, the number of rows).
- **function-name string**: resolve with `match.fun()` / `get0()` in the caller's environment; abort naming the string if it does not resolve to a function. We do **not** `eval(parse())` arbitrary expressions — only bare names resolve, matching the declarative intent.
- **fitdists / tmbfit**: `inherits()` check; name must be derivable or supplied. No structural drill-down into the model object (the registry pins it).

No input is executed and no RNG is drawn — asserted by an unchanged-`.Random.seed` test.

### Decision: print path renders kind, not shape

`fmt_grid_value()` already prints `<fn>` for functions. We extend the dataset print path (`print.ssdsims_scenario()` and any `ssd_data()` print) so a generator element shows `name <kind>` (e.g. `ssd_rlnorm <fn>`, `fit <fitdists>`) and a data-frame element keeps its existing rendering. Snapshot tests pin both forms.

### Decision: guard #80's baseline runner against unmaterialised generators

`task-list-loop-baseline` (#80) landed `ssd_run_scenario_baseline()`, whose `sample` step reads each inline dataset by name — `dplyr::slice_sample(data[[dataset]], …)` from `scenario$data`. A generator-backed dataset has no inline data frame, so the runner cannot draw from it until `dataset-registry` materialises it. We add a guard that aborts (user-facing frame) with an actionable message pointing at `dataset-registry` when the runner encounters a `dataset` whose scenario entry is an `ssdsims_generator`. *Why a guard rather than silently materialising?* Materialisation has its own seeding and registration semantics (§1.1) that belong to `dataset-registry`; failing fast keeps the baseline runner honest about what it supports. The task *tables* still derive fine — the `dataset` axis is just a name — so only execution is gated, not expansion.

## Risks / Trade-offs

- **Heterogeneous collection complicates downstream consumers** → #80's `ssd_run_scenario_baseline()` reads `scenario$data[[dataset]]`, and `task-tables`/the registry must handle both element types. Mitigation: the classed descriptor gives a clean `inherits(x, "ssdsims_generator")` discriminator; the dataset list is keyed by name so task-table derivation (the `dataset` axis) iterates names uniformly. Only *execution* differs, and the baseline runner gates it with the guard above; expansion is unaffected.
- **Function-name resolution environment is ambiguous** → a string like `"ssd_rlnorm"` must resolve where the user means. Mitigation: resolve in the constructor's caller environment (via `call`/`rlang::caller_env()`), matching how `ssd_sim_data.character()` evaluates today; document that only in-scope bare names are accepted.
- **`tmbfit` vs `fitdists` ambiguity** → a `fitdists` is a list of `tmbfit`s; passing one element is a distinct generator. Mitigation: dispatch on the most specific class first (`tmbfit` before `fitdists`) exactly as the S3 methods do, and key the descriptor by the matched kind.
- **Spec base not yet archived** → `scenario-definition` lives in the sibling `ssd-define-scenario` change, not `openspec/specs/`. Mitigation: the delta `MODIFIED` blocks copy the current requirement text verbatim so archiving stays clean once `ssd-define-scenario` is archived.

## Migration Plan

Additive and backward-compatible: existing data-frame calls behave identically (a tibble element is unchanged). No deprecations. `dataset-registry` (a later change) will consume the new descriptors; until it lands, generator descriptors are carried but never materialised, which is a coherent working state (the scenario is declarative by design).

## Open Questions

- Should a generator descriptor capture generator **arguments** (e.g. the `args` list `ssd_sim_data.function()` accepts) now, or defer that to `dataset-registry`? Leaning defer — the scenario's `nrow`/`nsim` already parametrise generation, and extra generator args are a registry-time concern. Resolve when `dataset-registry` is designed.
- Where exactly does `dataset-registry` get the `fitdists`/`tmbfit` *payload* it must pin, given the scenario stores only a name? Likely the registration call supplies it directly (as in the §1.1 `ssd_register_dataset()` sketch), independent of the scenario. Out of scope here, but the descriptor's `ref` field is shaped to leave room for it.
