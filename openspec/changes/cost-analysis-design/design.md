## Context

`cost-analysis-targets` builds `ssd_analyse_cost(scenario, ...)` /
`ssd_compare_cost()` / `ssd_calibrate_cost_from_run()` on the per-task
`.start`/`.end`/`.host` timing columns of a single scenario's `fit`/`hc` shards,
with the scenario's `targets` store supplying the per-shard envelope and the
pre-timing fallback. `scenario-combine` introduces the **design**: several
scenarios run as one pipeline under one store, each member's shards under
`<root>/scenario=<name>/layout=<hash>`, target names carrying a `<name>_` prefix,
and a combined `<root>/summary.parquet` that unions the per-member compact
summaries with a `scenario` identity column.

A design is therefore not a new kind of run — it is N scenario runs sharing one
store, one root tree, and one combined summary. Design-level cost analysis is the
**read-side rollup** of the scenario method over the members, plus the two things
only the design surfaces make cheap: one `tar_meta()` for all members, and one
combined summary read for all members' hc timings. This change adds the
`ssdsims_design` dispatch; it adds no timing capture and no pipeline.

This change **stacks on two unlanded siblings**. Neither `ssd_design()`/
`ssdsims_design` (scenario-combine) nor the analyse/compare/recalibrate functions
and the `ssdsims_cost_analysis` object (cost-analysis-targets) exist in the
codebase yet, so the tasks here are not implementable until both land — the
proposal records this explicitly.

## Goals / Non-Goals

**Goals:**
- An `ssdsims_design` method on each of the three cost-analysis functions that
  rolls the scenario method up across the design's members, returning design
  totals and a per-`scenario` breakdown dimension.
- Read every member's hc observed cost from the combined `<root>/summary.parquet`
  in one DuckDB read when it is available, falling back to per-member shard globs
  otherwise.
- A design-aware `tar_meta()` resolver: one store, names regenerated per member
  with the `<name>_` prefix and the `scenario=<name>` root, joined on `name`.
- Pooled host-aware recalibration across members; design-aware printing.
- Keep the design methods strictly read-only and the scenario-level contract
  byte-unchanged.

**Non-Goals:**
- The **study** level — cross-design, cross-version, cross-host aggregation;
  `scenario-combine`'s deferred read-side concept, out of scope here.
- Any change to the cost model's form, to timing capture, to the
  `ssd_summarise()`/combined-summary writers, or to the scenario-level functions'
  behaviour.
- Pooling measured durations across distinct `.host` values (the recalibration
  refuses this for a design exactly as for a scenario).
- Cross-scenario shard dedup or any reconciliation of overlapping task draws —
  the design keeps per-member trees (scenario-combine's Non-Goal).

## Decisions

### Dispatch on `ssdsims_design`, reuse the scenario method per member
The three functions gain an `ssdsims_design` branch (S3 method or a class check
at the top) that loops the design's named members, calls the existing
scenario-level machinery for each under that member's `scenario=<name>` root, and
binds the results with a `scenario` column. The scenario method is the unit of
correctness and the oracle: a design analysis is *defined* as the row-bind of its
members' analyses (design total = Σ member totals; design longest = max member
longest). No member's computation depends on another member — the design rollup
is pure aggregation, mirroring `scenario-combine`'s "member addressing is
independent" contract on the read side.

*Alternative considered:* a bespoke design-wide reader that ignores the scenario
method — rejected; it would duplicate the envelope/fallback/host logic and risk
drift from the scenario oracle.

### The combined summary is a fast path, not the contract
hc observed cost for the whole design can come from one read of
`<root>/summary.parquet` (grouped by its `scenario` column) because the combined
summary carries the members' hc timings — an **emergent** property of
`cost-analysis-targets` (compact summary keeps `.start`/`.end`/`.host`) ∧
`scenario-combine` (combined summary unions the compact summaries verbatim). This
is a performance shortcut layered over the same per-member shard-glob path the
scenario method already has: if the combined summary is absent or pre-timing, the
analysis falls back to per-member globs with identical totals. The `fit` addend
always comes from the per-member `fit` globs (summaries are hc-only), exactly as
at the scenario level. We depend on the emergent property but do not require
either sibling to add anything for it — and we test both the fast path and the
fallback so a future change to either sibling cannot silently break correctness.

### One store, names regenerated per member with the design prefix
A design pipeline is a single store, so one `tar_meta()` covers every member. The
scenario resolver regenerates `<step>_step_<pathcell>` names from the scenario and
joins on `name`; the design resolver wraps it per member, prepending the design's
`<name>_` prefix (the same prefix `ssd_design_targets()` mints) and pointing at the
member's `scenario=<name>` root. Reusing `scenario-combine`'s prefix/root helpers
(rather than re-deriving the naming) keeps one source of truth for the addressing,
so a prefix-scheme change there flows through here. The combined `summary` target
and `<name>_upload_<step>` targets are excluded from shard attribution; unmatched
and `NA`-seconds targets are reported/excluded, never fatal — the scenario
method's join-and-report discipline, applied per member and summed.

### Recalibration pools across members but never across hosts
The cost model keys on `nrow`/`ci_method`/`nboot`/`.host` — never on scenario
name — so measured hc durations from different members at the *same* host are
legitimately one calibration sweep, giving more data than any single member.
`ssd_calibrate_cost_from_run(design)` therefore pools the members' measured
durations into one frame and calls the existing fitters. The architecture
constraint is unchanged: distinct `.host` values are never silently pooled, even
when they arise from different members (a cluster design whose members ran on
different node types) — the function requires an explicit host or aborts listing
them, identical to the scenario method's rule.

### A `scenario` dimension on the existing objects, design-aware printing
`ssdsims_cost_analysis`/`ssdsims_cost_comparison` keep their shape; the breakdown
tibble gains a leading `scenario` column when built from a design (absent for a
single scenario), and provenance records the design and its member names. The
`format`/`print` methods branch on the presence of that column, so scenario-level
output is byte-unchanged and design-level output shows the per-`scenario`
breakdown above the design totals. This reuses `format_duration()` and the
existing print scaffolding rather than adding a parallel object.

## Risks / Trade-offs

- **[Hard dependency on two unlanded changes]** → This change cannot be
  implemented or merged before `cost-analysis-targets` and `scenario-combine`
  both land; the proposal and tasks state this, and the verify step will find the
  tasks unimplementable until then. The ordering is the cost; there is no code
  workaround.
- **[The combined-summary fast path couples to two siblings' choices]** (either
  could stop carrying hc timings on the compact/combined summary) → treated as a
  fast path with a tested per-member-glob fallback of identical totals, so a
  sibling change degrades performance, not correctness.
- **[Prefixed target names may not match the store if the design or a member's
  `partition_by` changed since the run]** → join-and-report per member: unmatched
  targets surfaced with counts, never silently dropped, never fatal — the
  scenario resolver's contract, one level up.
- **[Cross-node clock skew within a design]** → durations are within-node
  `.end − .start` differences (unaffected); only cross-member Gantt
  reconstruction is approximate, documented as at the scenario level.
- **[A member that did not run could bias the design total]** → members are read
  independently; a member with no readable run is excluded and its absence
  reported with the contributing-member count, mirroring the keep-going
  survivors-union property.

## Migration Plan

Additive: the three exported functions gain an `ssdsims_design` method; no
signature or behaviour change to the scenario-level path, no on-disk change.
Lands strictly after `cost-analysis-targets` and `scenario-combine`. Order inside
the change: (1) the analyse design method (rollup + combined-summary fast path +
design-aware resolver); (2) compare and recalibrate design methods; (3)
design-aware printing; (4) the vignette design section and docs. Rollback is
reverting the added methods; nothing else is touched.

## Open Questions

- Whether the design analysis print should also show a per-member *shard*
  envelope summary (the design has many more shards than a scenario) or keep that
  to the per-member object — lean: design print shows design totals + per-scenario
  task breakdown, and the per-shard envelope stays a scenario-level detail.
  Decide during implementation against the vignette's worked design.
