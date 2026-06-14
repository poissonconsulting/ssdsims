## Context

`ssd_define_scenario()` carries by-name inputs whose *names* drive task hashing
and Hive paths. Datasets already have a typed constructor, `ssd_scenario_data()`, that owns
naming and validation by value. `min_pmix` does not: it derives names by capturing
the unevaluated argument (`rlang::enexpr(min_pmix)`, `R/scenario.R:153`) and
parsing the literal `list(...)` call (`list_expr_names()`/`expr_to_name()`). That
only works for a literal `list(<symbol|pkg::name>, ...)` and silently misbehaves
(or aborts) for any indirectly-supplied value.

## Goals / Non-Goals

**Goals:**
- Replace expression-based naming for `min_pmix` with **value-based naming owned
  by a typed constructor**, matching the `ssd_scenario_data()` precedent.
- Make the legacy/loose `min_pmix` forms **fail loudly** with a message that
  points at the constructor — no silent fallback, no obscure internal frame.
- Keep the stored scenario shape, task hashes, and results **byte-identical** (an
  input-surface change only).

**Non-Goals:**
- Changing dataset input (owned by `ssd_scenario_data()`).
- Changing the `dists`/`distset` input — that is `distset-hc-axis`'s concern,
  which brings its own `ssd_distset()` constructor. This change is `min_pmix`-only
  and independent.

## Decisions

### 1. A typed constructor owns naming and validation, by value
`ssd_pmix(...)` returns a validated `ssdsims_pmix` collection. Naming comes from
`...` names and, for a bare `symbol`/`pkg::name` argument, per-argument symbol
capture (exactly as `ssd_scenario_data()` does). Crucially the capture is **per-argument
inside the constructor** — each `...` element is its own promise — not a parse of
one composite `list(...)` expression handed to a generic parameter. That is what
makes `ssd_pmix(f)` / `ssd_pmix(pkg::g)` robust where `min_pmix = list(f)` is not.
`ssd_define_scenario()` then does **no** `enexpr` on `min_pmix`.

*Alternative considered:* keep accepting loose forms but improve the inference
(e.g. fall back to value-based names). Rejected: it preserves two code paths and
the indirect-value trap; the constructor is the single, teachable surface.

### 2. Loud failure over silent fallback (BREAKING)
The retired forms (`min_pmix` as a function or a list) abort in the user-facing
call's context with an actionable message. This is the "old interface fails
loudly" requirement: a pre-release tightening preferred over silently accepting an
ambiguous input. `min_pmix` as a **character vector of names** stays valid (it is
already explicit and value-based), so the simplest call
(`min_pmix = "ssd_min_pmix"`, the new default) needs no constructor.

### 3. `ssd_pmix()`, not `ssd_min_pmix()` — avoid the dependency clash
`ssdtools::ssd_min_pmix` is the function users pass *into* the collection. An
`ssdsims::ssd_min_pmix()` constructor would shadow it and make
`min_pmix = ssd_min_pmix(ssd_min_pmix = ...)` self-referential and confusing. The
project already recognises this class of clash — `ssd_data()` was renamed to
`ssd_scenario_data()` to escape `ssdtools::ssd_data()` (#150) — so the collection
constructor is `ssd_pmix()`.

*Alternative considered:* `ssd_min_pmix_set()` / `ssd_min_pmixes()` — wordier and
no clearer; `ssd_pmix()` reads as "a collection of min_pmix functions".

### 4. Default `min_pmix = "ssd_min_pmix"` (a character name)
The current default `list(ssdtools::ssd_min_pmix)` is the worst offender: an
*unnamed* list whose name is recovered only by capturing the **default
expression**. Switching the default to the character name `"ssd_min_pmix"`
(resolved from `ssdtools` at construction) removes that capture entirely and is
the minimal explicit form.

### 5. Independent of `distset-hc-axis`
This change touches only `min_pmix`; `distset-hc-axis` touches only
`dists`/`distset` and brings its own `ssd_distset()` constructor. The two extend
the shared `scenario-definition` capability in **disjoint** requirements, so they
can be implemented, reviewed, and applied in either order. They share only the
typed-constructor philosophy and the `ssd_scenario_data()` precedent.

## Risks / Trade-offs

- **Breaking existing call sites** → every `min_pmix` function/list call site in
  examples, tests, snapshots, `scripts/`, `vignettes/`, and
  `inst/targets-templates/` must migrate. Mitigation: pre-release; the migration
  is mechanical (most become the terse `"ssd_min_pmix"` default), and the error
  message names the replacement.
- **Two pending changes touch `scenario-definition`** → `distset-hc-axis` and this
  one. Mitigation: their delta requirements are disjoint (`dists` vs `min_pmix`),
  so they compose without ordering.

## Migration Plan

Pre-release, no data migration (stored shape and hashes unchanged). Mechanical
call-site sweep: `min_pmix = ssdtools::ssd_min_pmix` → `"ssd_min_pmix"` (or
`ssd_pmix(...)`). Roll back by reverting the constructor commits; no persisted
artefacts depend on the input surface.

## Open Questions

- Should `ssd_pmix()` accept a single **unnamed** bare reference and derive its
  name (as designed, mirroring `ssd_scenario_data()`), or require explicit names
  everywhere? Leaning toward mirroring `ssd_scenario_data()` for consistency.
