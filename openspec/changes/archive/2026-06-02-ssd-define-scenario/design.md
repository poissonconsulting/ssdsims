## Context

`TARGETS-DESIGN.md` §1 specifies a purely declarative scenario object as the root of the targets pipeline; §12 lists `ssd-define-scenario` as a DAG root (no dependencies) that should land before any RNG/dqrng/targets machinery so the data shape is settled first. The current package exposes `ssd_run_scenario()` (a generic that immediately runs the full pipeline) but no standalone scenario object. This change introduces only the constructor and its validation — deliberately inert with respect to RNG and targets.

Constraints:
- No new heavy dependencies (`targets`, `dqrng`) at this step.
- Must not perturb global RNG state (the object is declarative).
- Additive only: existing `ssd_run_scenario()` / `ssd_sim_data()` / `ssd_fit_dists_sims()` / `ssd_hc_sims()` and their specs remain unchanged.

## Goals / Non-Goals

**Goals**
- A serialisable, declarative `ssdsims_scenario` S3 object holding seed, knobs, dataset names, and arg grids.
- A small `ssd_data()` normaliser as the single input-validation entry point.
- Construction-time `ci = FALSE` collapse with a visible notice.
- A readable `print()` method.

**Non-Goals**
- Task-table expansion (`ssd_scenario_*_tasks`) — roadmap entry `task-tables`.
- RNG/primer derivation — roadmap entries `dqrng-init`, `task-primer`.
- The dataset / `min_pmix` *registries* — i.e. the persisted name→value lookup that materialises datasets to Parquet and pins `min_pmix` functions. Those are the single targets-only `registry` step. **The name *reference* itself is in scope here** for both datasets and `min_pmix`; only the registry that resolves a name to a value is deferred.
- Wiring the object into `ssd_run_scenario()` — roadmap entry `migrate-public-api`.
- Accepting the non-data-frame input types `ssd_run_scenario()` handles today (`fitdists`, `tmbfit`, a generator function, a function-name string) — this change is **data-frame-only** (single or list). Those generator inputs are deferred to the new roadmap entry `scenario-input-types` (TARGETS-DESIGN.md §12), which derives a dataset name and defers materialisation to `registry`.

## Decisions

- **Decision: Reject bootstrap-only knobs when `ci = FALSE`, not ignore them.** §1.2 of the design doc says to "ignore" nboot/ci_method/parametric when ci = FALSE, but stricter validation is better for a declarative object. If the user passes `nboot = 1000` and `ci = FALSE`, that's a contradiction (bootstrap makes no sense without CIs); reject with an error instead of silently dropping the arg. This forces explicit intent: either omit the bootstrap knobs, or set `ci = c(FALSE, TRUE)` to enable them. *Trade-off*: breaks code that copies old signatures with extra args, but that's desirable — it catches mistakes early and makes the scenario's contract crystal clear.
- **Decision: Store dataset *names*, not data frames.** Per §1.1, the scenario hashes names (not function/data values) so a code edit or JIT recompile does not move tasks across primers and the manifest stays compact. For local use without targets, the constructor accepts inline data and derives a name (e.g. via the symbol/argument name or an explicit `name=`), materialising the registry lookup implicitly. *Alternative considered*: store the data frame directly — rejected because it bloats the manifest and couples identity to byte-content (Open Question §11.2 in the design).
- **Decision: Store `min_pmix` by *name*, not as a function value.** Symmetric with datasets and required by §1.1 (the per-task hash must key on the name, since `rlang::hash()` over a function is not byte-stable across JIT / `srcref` / environment differences). The constructor accepts `min_pmix` as a character vector of names, or as a function / list of functions whose name is derived by symbol capture (e.g. `ssdtools::ssd_min_pmix` → `"ssd_min_pmix"`), mirroring dataset name derivation. Only the name is stored; resolving a name back to a function is the `registry` step. *Trade-off*: a bare-name reference cannot be validated against a real function here (no registry yet), but provided functions are validated before their name is taken. *Alternative considered*: keep storing the function value until the registry lands — rejected; it would bake a spec violation (function-body values stored for a name-referenced parameter) into the object's contract.
- **Decision: Plain S3 object, not an R6/environment.** Matches the package's existing S3 style (`ssd_sim_data`/`ssd_run_scenario` dispatch) and keeps the object trivially serialisable.
- **Decision: `ssd_data()` is the exported, *rich* dataset entry point.** A variadic `ssd_data(...)` assembles one or more (optionally named) data frames into a validated, named `ssdsims_data` collection, owning both the `Conc`-column contract (the SSD convention asserted at registration in §1.1) and dataset naming (argument names / symbol capture). The constructor accepts that collection (preferred) or bare data frame(s) routed through the same per-dataset validation. *Why rich, not a single-data-frame normaliser*: it is the **extensible** input surface — the planned `scenario-input-types` change teaches each input to also be a generator (`fitdists`/`tmbfit`/function/function-name) that `ssd_run_scenario()` accepts, materialised by `registry`. *Alternative*: inline the checks in the constructor — rejected; the normaliser is reused by the future registry step.
- **Decision: Apply the `ci = FALSE` collapse at construction.** §1.2 requires the ignore to be visible "at scenario construction" and recorded for `print()`. Doing it here (rather than at task-expansion time) makes the object self-describing and the message timely. The collapse only *records* intent; the actual NA-bearing task rows are produced later by `task-tables`.
- **Decision: Use `chk` for validation.** Already a dependency and used throughout the package's existing validation.

## Risks / Trade-offs

- **Dataset-name derivation ambiguity** → For inline data the derived name may be non-obvious; mitigate by accepting an explicit `name=` and documenting the default derivation. Byte-identity collisions under one name are deferred to Open Question §11.2, not solved here.
- **Field set may drift from the design as later steps land** → Keep the object minimal and additive; treat `partition_by`/`upload` as optional fields with documented defaults so later steps can extend without breaking constructors.
- **Premature coupling to the design's full grid semantics** → Mitigate by keeping this step inert (no expansion), so only the *shape* of the declarative inputs is committed now.

## Migration Plan

Additive — no migration. New exports only; no existing behaviour changes. A follow-up (`migrate-public-api`) will later route `ssd_run_scenario()` through the scenario object behind a one-release shim.

## Dataset Input API (resolved)

The **preferred** form is an explicit `ssd_data()` collection, which owns
validation and naming and is the extensible entry point (a later
`scenario-input-types` change teaches it the other `ssd_run_scenario()`
generator inputs):

```r
ssd_define_scenario(
  ssd_data(boron = ccme_boron, cadmium = ccme_cadmium),
  ...
)
# → datasets = c("boron", "cadmium")
```

`ssd_data(...)` is variadic: each argument is a data frame, named via its
argument name or by symbol capture (`ssddata::ccme_boron` → `"ccme_boron"`);
a literal with no derivable name must be named; duplicate names error. It
returns an `ssdsims_data` object (a named list of validated tibbles).

For convenience the constructor also accepts **bare** dataset input, routed
through the same `Conc` validation, in four forms:

1. **Single data frame, implicit name** (symbol capture):
   ```r
   ssd_define_scenario(ssddata::ccme_boron, ...)
   # → datasets = "ccme_boron"
   ```

2. **Single data frame, explicit name**:
   ```r
   ssd_define_scenario(ssddata::ccme_boron, name = "boron", ...)
   # → datasets = "boron"
   ```

3. **Named list** (name derivation skipped):
   ```r
   ssd_define_scenario(list(boron = ccme_boron, cadmium = ccme_cadmium), ...)
   # → datasets = c("boron", "cadmium")
   ```

4. **Unnamed list** (symbol capture per element):
   ```r
   ssd_define_scenario(list(ccme_boron, ccme_cadmium), ...)
   # → datasets = c("ccme_boron", "ccme_cadmium")
   ```

Symbol capture (form 1 & 4) extracts names from the call AST; for data frame literals (`data.frame(...)`) with no meaningful name, the user must supply `name=` (form 2) or use a named list (form 3). If both a named list and `name=` are supplied, error (cannot have conflicting names).

For local use (ssd_run_scenario() without targets), the data frames are materialised inline and stored in an implicit per-scenario registry that `ssd_scenario_tasks()` consults. For cluster use, a `registry` targets step (roadmap entry) materialises and persists them to Parquet.

## Open Questions

- Exact constructor signature for fit/hc arg grids: flat named arguments (`rescale=`, `est_method=`, …) forwarded into `fit`/`hc` lists, versus explicit `fit = list(...)` / `hc = list(...)`. Leaning flat-with-internal-grouping to match `ssd_run_scenario()`'s current surface.
- Whether `ssd_data()` should also assert positivity/finiteness of `Conc` now or defer to the fitting step.

Resolved during implementation:

- **`ssd_data()` signature** — variadic `ssd_data(...)` taking one or more (optionally named) data frames and returning a named `ssdsims_data` collection; chosen as the extensible entry point for the planned `scenario-input-types` change. (Previously open: single-data-frame normaliser vs. richer form.)
- **Symbol-capture implementation** — `rlang::enexprs()` on `ssd_data()`'s `...` (and `rlang::enexpr()` on the constructor's `data`), with `::`/`:::` calls resolving to their RHS symbol.
- **`ssd_min_pmix()` helper** — not added: `ssd_min_pmix` is already an `ssdtools` export (the default function users pass); a same-named ssdsims helper would shadow it. `min_pmix` naming stays internal to the constructor; any registry-style helper belongs with `registry`.
