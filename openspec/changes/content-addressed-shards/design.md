# Design: content-addressed-shards

## Context

`ssd_scenario_targets()` mints, per `partition_by` path cell per step, one
`format = "file"` target named by the cell's path axes, rooted at
`scenario_results_dir(scenario)` = `<root>/layout=<hash of partition_by>`, with a
`summary` fan-in. Two structural facts limit it to a single scenario:

- **Scalar settings are in the content but not the address.** `est_method`, `ci`,
  and `nrow_max` change a shard's bytes (`est-method-setting`, `scalar-ci-flag`,
  `nrow-max-setting`) but appear in neither the target name nor the path. For one
  scenario that is harmless (one value of each); for any two scenarios that differ
  only in a setting, their differing shards would collide at one address and their
  *identical* shards have no shared address to collide at.
- **The bootstrap is sealed inside the `hc` shard.** It is already run once per
  cell and shared across `est_method` in-memory (`hazard-concentrations` "single
  bootstrap sample set"), but it is not materialised separately, so it cannot be
  reused across scenarios.

The enabling fact is the per-task `(seed, primer)` content identity: the primer
hashes a step's task axes and recursively folds in its parent (`R/task-primer.R`),
so *same `(seed, primer)` ⇒ byte-identical result*, scenario-independently.

This change turns that latent identity into a **content-pure address** and
materialises the bootstrap as an artifact. It ships first and **stands alone on
the single-scenario factory**; the design layer (`scenario-combine-v2`) is the
follow-up that composes these addresses to *realise* sharing. The addressing is
designed here with that follow-up explicitly in view.

`distset-hc-axis` lands before this change, so `distset` is already a real hc
axis (`ssd_distset()`, one union fit subset per pool, `distset` in
`task_axes("hc")` and the primer); distribution-set comparison rides the ordinary
axis machinery and is **not** a scalar discriminator here.

## Goals / Non-Goals

**Goals:**

- Make each shard's target name **and** storage path a pure function of its
  content — `partition_by` cells plus the scalar settings that discriminate
  *that step's* bytes — with **no scenario identity**, so identical content
  yields one address.
- Materialise the bootstrap as a content-addressed `draw` artifact an RNG-free
  `summarise` step consumes.
- Preserve single-scenario results **as values** (byte-identical read-back);
  accept the one-time layout re-path.
- Make the addressing **design-ready**: a later design layer composing these
  targets gets sharing, dedup, and extend-without-recompute as consequences,
  with nothing throwaway built first.

**Non-Goals (these belong to the follow-up `scenario-combine-v2`):**

- **The design API and its payoffs.** `ssd_design()` / `ssd_design_targets()`,
  dedup across a design's members, extend-/grow-without-recompute, the
  coordinate-keyed combined summary, and the derived scenario→selection mapping
  are all `scenario-combine-v2`. This change only supplies the addressing they
  compose on.
- **`dists`/`distset` addressing.** Owned by `distset-hc-axis` (an axis).
- **The `nrow_max` "draw the max, truncate" optimisation.** Here `nrow_max` is
  simply a `sample`-step discriminator; the prefix-reuse trick is separable.
- **Re-litigating the scalar-setting decisions.** `est_method`/`ci`/`nrow_max`
  stay scalar settings (one value per scenario, not task axes, not in the primer);
  we only enrich *storage addressing* to record the value in the path.

## Decisions

### Decision: addressing is a pure, per-step function of content

A shard's target name and Hive path are derived from its `partition_by` path
cells **plus** one Hive level per scalar setting that changes *that step's*
output bytes — and nothing about the scenario. The discriminator is added **at
the step where it bites and only there**, so upstream steps stay shared:

| Step | Path | Discriminator |
|---|---|---|
| `sample` | `sample/[nrow_max=…/]<sample cells>/` | `nrow_max` (draw size) |
| `fit` | `fit/<fit cells>/` | none (the union fit; `distset` lives at hc) |
| `draw` *(new)* | `draw/<hc cells incl. distset>/` | none beyond the bootstrap identity |
| `summarise` *(was hc tail)* | `hc/[est_method=…/]<hc cells>/` | `est_method` |

The discriminator is **always present**, never "only when it varies across a
comparison". That makes the address a pure function of the shard alone, so
*identical content ⇒ identical address* — testable at the addressing-function
level, before any design pipeline exists, and the precondition for a design layer
to collapse identical content to one target. A constant setting is a constant
Hive level: self-describing, design-ready, and identical in *value* to today's
result.

*Alternative considered — keep settings out of the address (today's standalone
layout):* rejected; identical cross-scenario content would then have no shared
address, so the follow-up could not dedup it without re-deriving addressing.

*Alternative considered — "add the level only when a setting varies":* rejected;
that couples a shard's address to the set of scenarios it is run with, so adding a
second `est_method` later would re-path the first — breaking the pure-function
property and the follow-up's extend-without-recompute.

*Alternative considered — fold the scalar settings into the global
`layout=<hash>`:* rejected; two scenarios differing only in `est_method` would get
different hashes for the **whole** tree, losing the sharing of their identical
`sample`/`fit` shards. The discriminator must be **per-step**, present only where
it bites.

### Decision: split `hc` into a content-addressed `draw` and an RNG-free `summarise`

The bootstrap sample set becomes a `draw` shard keyed on the bootstrap identity
`(fit identity, distset, nboot, ci_method, parametric)` — `distset` enters because
each model-averaging pool bootstraps its own members (`distset-hc-axis`), while
`est_method` and the summary choice do **not** (the draw is invariant to them;
`ci` only gates whether a draw exists). The RNG-free `summarise` step reads the
`draw` shard (and the `fit` shard for the analytic point estimate) and applies
`est_method`/`ci`. This exposes the already-shared in-memory bootstrap as a
cacheable artifact — within one scenario it is the existing one-per-cell bootstrap;
across scenarios (under the follow-up) it is reused rather than recomputed. The
retained-samples persistence (`hazard-concentrations` "Optional on-disk
persistence of bootstrap samples", from `dual-summary-outputs`) is exactly this
`draw` artifact.

*Alternative considered — keep the bootstrap inside the `hc` shard:* rejected; a
divergent summary (`est_method`) would then re-run the expensive bootstrap even
though its draws are identical — foreclosing the reuse the follow-up needs.

### Decision: ship first, design for the follow-up

This change carries no design API and no multi-scenario guarantees; those are
`scenario-combine-v2`. What it commits to is that the addressing is a pure
function of content and therefore *composes*: when `scenario-combine-v2` emits the
content-addressed targets for several scenarios into one pipeline, identical
content already resolves to one target name and path, so `targets` collapses it to
a single build (dedup) and an unchanged shard is skipped (extend-without-recompute)
— no per-scenario `scenario=<name>` layer is ever built and discarded. Designing
the primitive with the consumer in view is the whole point of splitting the work
this way.

## Risks / Trade-offs

- **One-time re-path on upgrade.** Settings-in-path and the `hc` split change the
  standalone layout; existing stores recompute once. Pre-1.0, documented.
- **Value-without-payoff until the follow-up.** On its own this change reorganises
  addressing and splits a step without a user-visible feature; the dedup/extend
  payoff arrives with `scenario-combine-v2`. Accepted: it is the foundation, and
  the `hc` split already trims redundant bootstrap work within a scenario.
- **Identity precision.** Addressing uses the *full* discriminators (cells +
  settings + layout hash), not the 64-bit-truncated primer, so path identity is
  exact; the truncated primer remains only the RNG selector. `targets`'
  duplicate-name abort is the backstop against any constructed-name collision.

## Migration Plan

Land after `distset-hc-axis`. Single-scenario per-task results are unchanged as
values; the layout changes once (settings-in-path, `hc` split), so existing stores
recompute on upgrade. No data migration; roll back by reverting the commit. The
follow-up `scenario-combine-v2` builds the design layer on top.

## Open Questions

- The exact Hive ordering of setting-discriminator levels relative to
  `layout=<hash>` and the path cells (self-describing vs glob-friendly) —
  resolved in implementation against the depth-agnostic readers.
- Whether `summarise` reads `draw` + `fit` directly or a thin `draw` index —
  resolved against the m:n fan-in edge model.
