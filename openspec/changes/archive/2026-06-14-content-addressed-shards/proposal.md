# Proposal: content-addressed-shards

> **ARCHIVED — moot, not implemented (specs not synced).** This change proposed
> to make shard addressing content-pure by *adding* the scalar settings
> (`est_method`/`ci`/`nrow_max`) to the path as per-step discriminators and
> splitting `hc` into a `draw` + `summarise`. On review the addressing premise was
> already satisfied: the `GLOSSARY` *simulation setting* contract forbids a setting
> from becoming a partition level, and single-scenario addressing already carries
> no scenario identity — so there was nothing to add. Scalar-setting divergence at
> a shared cell is left to the computed result (recomputed/overwritten on change),
> not engineered. `scenario-combine-v2` therefore composes the **existing**
> content-pure targets directly, with no prerequisite. Retained as reference for
> the decision trail (the rejected layout-hash and filename-encoding alternatives,
> and the `nrow_max` prefix-superset analysis).

## Why

Even a **single** scenario today bakes its scalar simulation settings —
`est_method`, `ci`, `nrow_max` — into a shard's *content* (they change the
bytes) but **not** into its *address*: `ssd_scenario_targets()` names targets by
path cells only and roots them under `scenario_results_dir()` =
`<root>/layout=<hash of partition_by>`. It also folds the bootstrap *inside* the
`hc` shard. Both choices are invisible while a scenario runs alone, but they
**foreclose sharing**: the moment you want two scenarios (a settings comparison)
to run together and reuse the work they have in common, there is no address at
which identical content can collide, so it must be recomputed.

The enabling fact is already in the package: a task's `(seed, primer)` is a
**content identity** that recursively folds in its parents (`R/task-primer.R`),
so *same `(seed, primer)` ⇒ byte-identical result, regardless of which scenario
produced it*. Settings comparisons are exactly paired because the primer hashes
**axes only** — the settings are scalar, deliberately not in the primer
(`est-method-setting`, `scalar-ci-flag`, `nrow-max-setting`).

This change makes single-scenario addressing **content-pure** and splits `hc` so
that the *foundation* for sharing is in place before any design API exists. It
**ships first**; the design layer that reaps the benefit — `ssd_design()` /
`ssd_design_targets()`, dedup, extend-without-recompute, the combined summary —
is the **follow-up `scenario-combine-v2`** (proposed alongside, depending on this
change). Designing the addressing here *with that follow-up in view* is what lets
the design layer get sharing for free, with no throwaway per-scenario addressing
ever written (the reason the original `scenario-combine` is archived; see
`scenario-combine-v2`).

## What Changes

- **Content-pure shard addressing.** A step's shard target name and Hive storage
  path SHALL be a pure function of content — its `partition_by` path cells plus,
  **at the step where it bites**, every scalar setting that changes that step's
  bytes (`nrow_max` at `sample`, `est_method` at the hc summary, `ci` as the
  superset projection) — carrying **no scenario identity**. The discriminator is
  **always present** (never conditional on a comparison), so the address is a
  pure function of the shard alone: *identical content ⇒ identical address*. This
  is the property a design layer composes on.
- **`dists` is an axis, not a discriminator.** `distset-hc-axis` (this change's
  prerequisite) makes `distset` a real hc axis (`ssd_distset()`, fit the union
  once, `distset` in `task_axes("hc")` and the primer), so distribution-set
  comparison rides the normal axis/primer/partition machinery and needs no path
  discriminator here.
- **`hc` splits into a content-addressed `draw` step and a cheap `summarise`
  step.** The bootstrap sample set — already computed once per cell and shared
  across `est_method` in-memory (`hazard-concentrations` "single bootstrap sample
  set") — becomes a first-class, content-addressed `draw` shard keyed on the
  bootstrap identity (`(fit, distset, nboot, ci_method, parametric)`; `distset`
  enters because each pool bootstraps its own members, `est_method` does **not**
  — it is applied at summary time). The RNG-free `summarise` step reads it and
  applies `est_method`/`ci`.
- **Single-scenario behaviour preserved in value.** A scenario's per-task
  `sample`/`fit`/`hc` results are byte-identical **as read-back values** to
  today; only the **layout** changes (settings now in the path, `hc` split into
  `draw`+`summarise`), so existing stores recompute once on upgrade (pre-1.0).

## Capabilities

### New Capabilities
- `content-addressed-shards`: the content-pure addressing contract — a shard's
  target name and storage path are a pure function of its content, carrying no
  scenario identity, so identical content yields one address (the design-ready
  property a later design layer composes on).

### Modified Capabilities
- `task-shards`: the results root and the `ssd_scenario_targets()` factory address
  shards by content (cells + per-step scalar discriminators), not by a
  `partition_by`-only layout root.
- `hazard-concentrations`: the single bootstrap sample set is materialised as a
  content-addressed `draw` artifact (keyed on `(fit, distset, nboot, ci_method,
  parametric)`) that an RNG-free summary step consumes.

## Impact

- **Ships first; foundational.** No design API is added here — this change is the
  addressing primitive plus the `hc` split, delivered on the single-scenario
  factory and designed to be composed by the follow-up `scenario-combine-v2`.
- **One-time re-path on upgrade.** Settings-in-path and the `hc` split change the
  standalone layout; existing stores recompute once. Pre-1.0, nothing to migrate.
- **Code:** content-addressed naming/rooting in `R/task-shards.R` /
  `R/filepath.R` / `R/targets-runner.R`; the `hc` step split across `R/hc-sims.R`
  / `R/shard-runner.R`.
- **Docs:** `TARGETS-DESIGN.md` (§5/§6 addressing, §8 invalidation, the hc split),
  `vignettes/sharded-pipeline.qmd`, `ROADMAP.md`.
- **Tests:** the addressing function is a pure function of content (identical
  content via two scenario configs yields the same target name and path; a
  setting that bites at a step appears in that step's address and not upstream);
  the `hc` split (one `draw` feeds many `est_method` summaries; same-seed
  identity); single-scenario byte-identity of values.

## Dependencies

- **Depends on:** `distset-hc-axis` (already makes `distset` an axis, so `dists`
  needs no discriminator here; `distset` enters the `draw` identity).
- **Depended on by:** `scenario-combine-v2` (the design layer that composes these
  content-addressed targets to get dedup, extend-without-recompute, and the
  combined summary) and `cost-analysis-targets` (the derived scenario→selection
  mapping). Chain: `distset-hc-axis` → `content-addressed-shards` →
  `scenario-combine-v2`.
- **Supersedes (archived as reference):** the original `scenario-combine`, whose
  per-scenario `scenario=<name>` decoupling this content-pure addressing makes
  unnecessary.
- **Builds on:** `est-method-setting` (one-bootstrap-many-methods, now an
  artifact), `nrow-max-setting` (the draw-size setting that becomes the `sample`
  discriminator), `path-axis-growth` and the `hive-partitioning` invalidation
  model.
