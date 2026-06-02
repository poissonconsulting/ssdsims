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
- The dataset / `min_pmix` registries — those are targets-only steps (`dataset-registry`, `min-pmix-registry`).
- Wiring the object into `ssd_run_scenario()` — roadmap entry `migrate-public-api`.

## Decisions

- **Decision: Store dataset *names*, not data frames.** Per §1.1, the scenario hashes names (not function/data values) so a code edit or JIT recompile does not move tasks across primers and the manifest stays compact. For local use without targets, the constructor accepts inline data and derives a name (e.g. via the symbol/argument name or an explicit `name=`), materialising the registry lookup implicitly. *Alternative considered*: store the data frame directly — rejected because it bloats the manifest and couples identity to byte-content (Open Question §11.2 in the design).
- **Decision: Plain S3 object, not an R6/environment.** Matches the package's existing S3 style (`ssd_sim_data`/`ssd_run_scenario` dispatch) and keeps the object trivially serialisable.
- **Decision: `ssd_data()` is a separate exported normaliser.** Centralises the `Conc`-column contract (the SSD convention asserted at registration in §1.1) so every later step inherits one validation path. *Alternative*: inline the checks in the constructor — rejected; the normaliser is reused by the future registry step.
- **Decision: Apply the `ci = FALSE` collapse at construction.** §1.2 requires the ignore to be visible "at scenario construction" and recorded for `print()`. Doing it here (rather than at task-expansion time) makes the object self-describing and the message timely. The collapse only *records* intent; the actual NA-bearing task rows are produced later by `task-tables`.
- **Decision: Use `chk` for validation.** Already a dependency and used throughout the package's existing validation.

## Risks / Trade-offs

- **Dataset-name derivation ambiguity** → For inline data the derived name may be non-obvious; mitigate by accepting an explicit `name=` and documenting the default derivation. Byte-identity collisions under one name are deferred to Open Question §11.2, not solved here.
- **Field set may drift from the design as later steps land** → Keep the object minimal and additive; treat `partition_by`/`upload` as optional fields with documented defaults so later steps can extend without breaking constructors.
- **Premature coupling to the design's full grid semantics** → Mitigate by keeping this step inert (no expansion), so only the *shape* of the declarative inputs is committed now.

## Migration Plan

Additive — no migration. New exports only; no existing behaviour changes. A follow-up (`migrate-public-api`) will later route `ssd_run_scenario()` through the scenario object behind a one-release shim.

## Open Questions

- Exact constructor signature for fit/hc arg grids: flat named arguments (`rescale=`, `est_method=`, …) forwarded into `fit`/`hc` lists, versus explicit `fit = list(...)` / `hc = list(...)`. Leaning flat-with-internal-grouping to match `ssd_run_scenario()`'s current surface.
- How dataset names are derived for inline data (symbol capture vs required `name=`) — see Risks; resolve during implementation.
- Whether `ssd_data()` should also assert positivity/finiteness of `Conc` now or defer to the fitting step.
