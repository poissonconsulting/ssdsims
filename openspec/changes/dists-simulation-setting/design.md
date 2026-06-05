# Design — `dists` is a fit-level simulation setting

## Context

`ssd_define_scenario()` accepts `dists` (default `ssdtools::ssd_dists_bcanz()`),
validated as a unique character vector and stored at `scenario$fit$dists`. At
the fit step it is handed *whole* to one `ssd_fit_dists()` call per task
(`R/task-lists.R:256`), uniformly across every fit task. It is **not** in
`task_axes("fit")`, so it does not fan out, does not enter the per-task primer,
and is not partition-eligible.

That makes `dists` a **simulation setting** by the glossary definition. Yet the
`scenario-definition` spec's `Constructor arguments are grouped by role`
requirement lists `dists` among the cross-join axes and places it in the
fit-axis block of the signature. This change reconciles the spec and signature
with the implementation.

## Two clarifications this change pins down

### 1. "Inner axis" vs "simulation setting" — same cost, different mechanism

These are easy to conflate because *adding a value to either* rewrites all
shards of the step. They are categorically different:

| | **inner axis** (e.g. `nboot`, `est_method`, bundled `rescale`) | **simulation setting** (e.g. `dists`, `ci`, `samples`, `proportion`) |
|---|---|---|
| In `task_axes(step)`? | **yes** | **no** |
| Multiplies the task graph? | yes — one new **task** per value | no — same task count |
| Enters the per-task **primer** / identity? | yes | no |
| On disk | an ordinary Parquet **column** varying row-to-row *inside* a shard (the complement of `partition_by`) | not a column-axis at all; realised in the task body |
| What "adding a value" does | appends a **row** (a new task) to every shard → atomic rewrite | changes the **content** of every existing row (re-fit / re-estimate) → rewrite |
| Escape hatch for the rewrite | **move it into `partition_by`** → new shards instead of rewrites | **none** — a setting can't be a partition level; the rewrite is intrinsic |

So an **inner axis** is still a genuine cross-join axis — it has just been left
out of `partition_by`, so its fan-out lives as columns *within* a shard rather
than as Hive directories *across* shards (`path ⊎ inner = task_axes(step)`,
GLOSSARY). A **simulation setting** is not an axis in either position: it never
multiplies the task graph. `dists` adding a distribution and `nboot` adding a
value both "rewrite all fit/hc shards", but `nboot`'s rewrite adds a *row per
shard* (and can be avoided by sharding on `nboot`), whereas `dists`'s rewrite
changes *existing rows' contents* and cannot be partitioned away.

### 2. Why `nboot` enters the per-task primer (and what dropping it would buy)

**Bootstrapping is the only RNG consumer in hc estimation.** `ssd_hc()`
computes the point estimate `est` analytically from the fit, independent of the
bootstrap and the RNG (TARGETS-DESIGN.md §1.2; verified byte-identical across
`ci`). So a `ci = FALSE` hc task draws *no* random numbers, and the entire RNG
footprint of an hc task is the `nboot`-iteration bootstrap resampling.

The per-task primer is `task_primer()` over `task_axes("hc")`, which **includes
`nboot`**. Two consequences and one alternative:

- **Current (in the primer):** each distinct `nboot` value gets its own dqrng
  stream → statistically **independent** bootstrap draws, each fully
  reproducible standalone. `nboot = 100` and `nboot = 1000` share nothing.
- **Alternative (out of the primer):** hash the hc primer over
  `task_axes("hc")` *minus* `nboot`, decoupling the primer from the task
  *identity* (which keeps `nboot`, so shards/rows still separate per value).
  Then every `nboot` value would draw from **one shared primed stream**, so
  `nboot = 100`'s draws would be a **prefix** of `nboot = 1000`'s — a subset
  property exactly analogous to how §5 keeps `nrow` out of the *sample* draw's
  primer (every `nrow` is a `head()` of one shared draw). This nested-draw
  property is the RNG **precondition** that would make any future inner-loop
  reuse *coherent*: the extra 900 iterations continue the same stream instead
  of starting an independent one. It does **not**, by itself, save computation
  (ssdtools still runs 1000 from scratch — §9).

**Why we keep `nboot` in the primer.** Unlike `nrow` — whose
`head(sample, nrow)` truncation is *our* deterministic code, validated by
`scripts/experiment-subset-property.R` — the bootstrap loop is *internal to
ssdtools* (the opaque-RNG limitation, §9). We cannot guarantee or cheaply
validate that ssdtools consumes the stream as a stable prefix across `nboot`,
so the subset property is not assured. Independent-stream-per-`nboot` is the
honest, robust default: each value is reproducible on its own with no reliance
on hidden draw order. The shared-stream nesting becomes worth revisiting only
if ssdtools exposes the bootstrap loop, or its prefix-stability is validated and
version-pinned the way `sample.int` is for `nrow`.

This change does **not** alter primer behaviour; it records the rationale so the
decision is explicit. Dropping `nboot` from the primer would be a separate
change with statistical implications (it sacrifices inter-`nboot` independence)
and a new fragility (reliance on opaque prefix-stability).

## Signature placement

`dists` is a *fit*-level setting while `proportion`/`ci`/`samples` are *hc*-level.
To honour "simulation settings are contiguous" we place `dists` **first** in the
settings block (fit before hc): `… nboot, est_method, ci_method, parametric,`
**`dists, proportion, ci, samples,`** `partition_by, bundle, upload`. Storage is
unchanged and step-based: `dists` stays in `scenario$fit`, the hc settings in
`scenario$hc`. All reordered formals follow `...`, so call sites name them and
the reorder is behaviour-preserving.

## Alternatives considered

- **Leave `dists` in the fit-axis block, just fix the prose.** Rejected: the
  chosen resolution is strict contiguity of settings (the role-grouping
  requirement already mandates it for `proportion`/`ci`/`samples`).
- **Make `dists` a real fit axis (fan out per dists-set).** Rejected: a single
  character vector cannot express multiple sets, and per-distribution fan-out
  would destroy model averaging — it would change the science, not just the
  plumbing.
