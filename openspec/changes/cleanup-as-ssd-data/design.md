## Context

`ssd_define_scenario()` (`R/scenario.R`) takes its `data`/`name` arguments and routes them through the internal `scenario_datasets()` helper, which returns a validated `ssdsims_data` collection (a named list of tibbles, each validated for a numeric `Conc` column by `ssd_data_validate()`). `scenario_datasets()` accepts four convenience forms today:

1. an `ssd_data()` collection — returned as-is (passthrough);
2. a single data frame with an explicit `name=`;
3. a named list of data frames — names taken from the list;
4. a single data frame **without** a name, or an **unnamed** list — names derived by **symbol capture** from the captured argument expression (`expr_to_name()` / `list_expr_names()` over `data_expr`).

Form (4) is the wrinkle. Symbol capture only works in the frame that captured the expression: `ssd_define_scenario(ssddata::ccme_boron, ...)` can recover `"ccme_boron"` because the constructor captured `ssddata::ccme_boron` with `rlang::enexpr()`. A public function handed the *value* of `ssddata::ccme_boron` sees only a data frame — the symbol is gone. PR #80 review asked for a public `as_ssd_data()`; this design scopes it to the forms that do not need the caller frame.

## Goals / Non-Goals

**Goals:**

- A public `as_ssd_data()` that coerces the **already-named** forms — collection passthrough, named list, single data frame with `name=` — into a validated `ssdsims_data` collection, reusing `ssd_data_validate()` so the `Conc` contract is identical to `ssd_data()`.
- A clear, actionable error for the forms `as_ssd_data()` cannot name itself (bare data frame, unnamed list), pointing the caller at `ssd_define_scenario()` / `ssd_data()`.
- `scenario_datasets()` delegating its already-named forms to `as_ssd_data()` so there is a single coercion path, with the symbol-capture name derivation kept in the constructor frame.

**Non-Goals:**

- Handling bare data frames or unnamed lists inside `as_ssd_data()` (they need symbol capture in the caller frame — see the decision below).
- Any change to `ssd_data()` itself, to `ssd_define_scenario()`'s accepted input forms, or to the `Conc` validation contract.
- Data **generators** (`fitdists` / `tmbfit` / function / function-name string). Those are `scenario-input-types`; see the orthogonality note below.

## Decisions

### Decision: `as_ssd_data()` is scoped to the already-named forms; symbol capture stays in the constructor frame

`as_ssd_data()` accepts exactly the forms whose names are present in the *value* it receives:

- an `ssdsims_data` collection → returned unchanged (validated by construction), with `name=` rejected as redundant;
- a **named** list of data frames → names taken from the list; each element validated;
- a **single data frame with an explicit `name=`** → wrapped under that name and validated.

It does **not** accept a bare unnamed data frame or an unnamed list, because deriving their names requires the captured argument expression, which is only available in the frame that captured it (`ssd_define_scenario()` via `rlang::enexpr()`). Receiving an already-evaluated value, `as_ssd_data()` has no expression to inspect. So those forms abort with a message routing the caller to `ssd_define_scenario()` (which performs the capture) or to `ssd_data()` / a named list (where the caller supplies the name explicitly).

*Alternative considered:* make `as_ssd_data()` capture its own argument with `enexpr()` and attempt symbol derivation. Rejected: `as_ssd_data(my_var)` would capture `my_var`, not the data frame's "real" name, and `as_ssd_data(f())` or `as_ssd_data(some_list[[1]])` would capture an unusable call — the derivation is meaningful only at the original `ssd_define_scenario()` call site, not after one or more frames of indirection. Keeping symbol capture in the constructor frame preserves the documented `ssd_define_scenario(ssddata::ccme_boron, ...)` → `"ccme_boron"` behaviour exactly.

### Decision: `scenario_datasets()` derives symbol-capture names first, then delegates the rest to `as_ssd_data()`

`scenario_datasets()` keeps ownership of the two frame-dependent steps and delegates everything else:

1. **Collection passthrough** → delegate straight to `as_ssd_data(data)` (with `name` forwarded so the redundant-`name` rejection is shared).
2. **Bare data frame** → if `name` is `NULL`, derive it via `expr_to_name(data_expr)` (aborting with the existing "supply an explicit `name=` or use `ssd_data()`" message when no name can be derived); then delegate to `as_ssd_data(data, name = <derived-or-supplied>)`.
3. **Named list** → delegate to `as_ssd_data(data)` (forwarding `name` so the "`name` not allowed with a named list" rejection is shared).
4. **Unnamed list** → derive per-element names via `list_expr_names(data_expr, ...)` in the constructor frame, attach them to the list, then delegate to `as_ssd_data(<named-list>)`.

The result is a single coercion/validation path (`as_ssd_data()` → `ssd_data_validate()`), with `scenario_datasets()` reduced to *name derivation by symbol capture* plus delegation. The `name`-conflict messages (e.g. `name` with a collection, `name` with a named list, `name` with an unnamed list) move to `as_ssd_data()` where they apply uniformly, except the unnamed-list case which `scenario_datasets()` resolves before delegating.

*Error-call-origin:* `as_ssd_data()` takes a `call` argument (defaulting to its own environment) so that when `scenario_datasets()` delegates, it forwards the constructor's `call` and validation errors continue to surface as `ssd_define_scenario()`, not `as_ssd_data()` — matching the existing plain-loop / forwarded-`call` discipline in `R/scenario.R`.

### Decision: reuse `ssd_data_validate()`, do not duplicate the `Conc` contract

`as_ssd_data()` validates each element with the existing `ssd_data_validate(data, name = ...)` (numeric `Conc` required, coerced to a tibble, name woven into the message). This keeps a single source of truth for the dataset contract shared by `ssd_data()`, `scenario_datasets()`, and the new `as_ssd_data()`. Duplicate-name detection reuses the same `anyDuplicated()` check.

### Decision: orthogonal to `scenario-input-types` (generators)

`scenario-input-types` extends `ssd_data()` to accept data *generators* (`fitdists` / `tmbfit` / a generator function / a function-name string) in addition to data frames. That is orthogonal to this change: by the time an input reaches the coercion layer, a generator has been **materialised and named** at the input point — it flows through the *already-named* path that `as_ssd_data()` owns, exactly like a named data frame. So `as_ssd_data()` stays scoped to the already-named **data-frame** forms here, the two changes do not touch the same code path in a conflicting way, and they may land in either order. This change does **not** specify generator handling; if `scenario-input-types` lands first, the already-named generator value simply passes through the same delegation.

## Risks / Trade-offs

- **Two name-derivation paths can drift** (the symbol-capture path in `scenario_datasets()` vs. the explicit-name path in `as_ssd_data()`) → mitigated by funnelling *all* validation through `ssd_data_validate()` and keeping `scenario_datasets()` a thin pre-step that only derives names then delegates; the existing scenario-definition tests pin every input form's resulting names.
- **Public-surface error wording** for the rejected bare/unnamed forms must be actionable → the message names the two escape hatches (`ssd_define_scenario()` for symbol capture; a named list or explicit `name=` to supply names), mirroring `scenario_datasets()`'s existing messages.
- **Forwarded `call` plumbing** → `as_ssd_data()` exposes a `call` parameter so delegated errors keep the `ssd_define_scenario()` origin; the public default is `as_ssd_data()`'s own frame, so direct callers see `as_ssd_data()` in the header.

## Open Questions

- **Whether to also accept a single *named-via-`name=`* element inside a list** is out of scope: the named-list form already carries names per element, and `name=` stays the single-data-frame affordance. No change proposed.
- **rlang dots auto-naming as an alternative to symbol capture (exploration round, raised in #101 review).** The design above rests on the premise that the bare-data-frame / unnamed-list forms need `expr_to_name()` / `list_expr_names()` and so *cannot* be public. The review asks whether routing `ssd_data()`'s dots through `rlang::list2()` would "also get the desired result". Two clarifications settle most of it:
  1. **`rlang::list2()` by itself does not auto-name.** It is `list()` plus `!!!` splicing, `:=`, and trailing-comma tolerance — `list2(ccme_boron)` is still an *unnamed* element. The auto-naming the question is really after lives in **`rlang::dots_list(..., .named = TRUE)`** (equivalently `rlang::enexprs()` / `ensyms()`), which labels each unnamed argument via `as_label()` of its defused expression.
  2. **Collected that way, the public verb *could* capture the name at its own frame.** If `as_ssd_data(...)` gathered its dots with `dots_list(.named = TRUE)`, then `as_ssd_data(ccme_boron)` would yield `"ccme_boron"` at the public call site — exactly as `ssd_define_scenario()` does today. That removes the "impossible to name a public-coercer argument" argument: a `...`-based coercer **can** accept the bare-symbol form.

  The catch is the same indirection limit the *"capture its own argument"* alternative above rejects: `as_label()` is a meaningful **dataset** name only for a **bare symbol**. `as_ssd_data(f())`, `as_ssd_data(lst[[1]])`, or a variable that happens to hold a differently-named frame yield labels like `"f()"` / `"lst[[1]]"`, not the data's intended name. So rlang dots **relocate** the capture into the public frame but do **not** make names meaningful through indirection.

  The open decision is therefore *interface-shaped*, not a yes/no on `list2()`: keep the current **evaluated-`data` argument** (names genuinely unavailable after evaluation → scope to already-named forms, the simpler public contract), or switch the public verb to a **defused `...` interface** (`as_ssd_data(...)` via `dots_list(.named = TRUE)`) that captures bare-symbol names like the constructor does, accepting that non-symbol arguments still need an explicit `name=` / named list. If the `...` interface is taken, `scenario_datasets()` could then delegate its symbol-capture forms by forwarding the *defused* dots rather than deriving names itself, collapsing the two name-derivation paths into one. Resolve before locking the public signature.
