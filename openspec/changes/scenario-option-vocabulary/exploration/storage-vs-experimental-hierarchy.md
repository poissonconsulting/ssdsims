# Exploration: storage layout is a separate hierarchy from study ⊃ design ⊃ scenario ⊃ task

Captured from an explore-mode discussion (does the experimental hierarchy
include **shard**?). Conclusion: no — and the storage side isn't even congruent
with the experimental one. This note records the two hierarchies and the
on-disk facts that ground them, so a future reader (or a `## Storage layout`
glossary subsection, if promoted) has the picture. It is *reference*, not a
requirement.

## Two hierarchies, meeting only at the task

```
EXPERIMENTAL (nests by identity)        STORAGE (on disk; grouped by seed & partition_by)
────────────────────────────────       ─────────────────────────────────────────────────
 study .............................▶   (no on-disk form — read-side union of trees)
   │
 design ───────────────────────────▶    results root  =  <root>/
   │                                       ├── summary.parquet        ◀ combined design summary
   │                                       │                            (scenario = a COLUMN)
   │                                       ├── summary-<name>.parquet  ◀ per-member summaries
 scenario ··· dissolves ···········▶       └── seed=<s>/layout=<h>/    ◀ shared shard subtree
   │     (a) seed= level only when seeds differ        ├── sample/<path>/part.parquet
   │     (b) the `scenario` identity column            ├── fit/…/part.parquet
   │     (c) nothing when members share (seed, layout) ├── hc/…/part.parquet
   │         — they BLEND (common random numbers)      └── summary.parquet  ◀ STANDALONE run only
 task ─────────────────────────────▶    …/<step>/<partition path>/part.parquet  (one row)
```

The two ladders share the **top** (design ≈ results root) and the **bottom**
(task ≈ row), but the **middle differs**:

- experimental middle: **scenario** (identity = a regular grid + a seed)
- storage middle: **`seed=`**, **`layout=`**, **shard** (groupings by RNG root
  and `partition_by`)

The storage ladder has no `scenario` rung, and gains rungs the experimental one
lacks. So "is **shard** a rung of `study ⊃ design ⊃ scenario ⊃ task`?" is doubly
no: shard is a configurable storage grouping (its boundaries, set by
`partition_by`, never change results — the byte-identity invariant), and the
ladder it lives in isn't congruent with the experimental one anyway.

## On-disk facts (verified against current `main`)

- `scenario_results_dir(scenario, root)` = `<root>/seed=<seed>/layout=<hash of partition_by>`
  (`R/targets-runner.R`).
- **Design run** (`ssd_design_targets`, `R/design-targets.R`): combined summary
  at `<root>/summary.parquet` (with a `scenario` identity column); per-member
  compact summaries at `<root>/summary-<name>.parquet`; shards under the shared
  `<root>/seed=/layout=/…`. Members with different seeds land under distinct
  `seed=` trees; members sharing `(seed, layout)` blend into one subtree
  (deliberate — common random numbers for paired comparisons).
- **Standalone run** (`ssd_scenario_targets`): summary lives *inside*
  `<root>/seed=/layout=/summary.parquet`. So a design-of-one and a standalone
  run share **byte-identical shards** but place the summary differently (design
  root vs inside the `seed=/layout=` tree).

## Why `seed=` gets a directory rung but `scenario` does not

Accepted as fine, recorded for context: `seed` is an identity-bearing scenario
*field* promoted to a directory level because it is the RNG root that must
isolate (or, when shared, couple) streams. The `scenario` as a whole is also
identity-bearing but is *not* promoted — its membership is re-derivable from
task identity, so the storage layer needs it only as the summary's `scenario`
column, not as a container. This asymmetry is about RNG isolation, not storage
layout per se.

## Disposition

- A **one-line pointer** into the glossary (see design.md D8) closes the
  "is shard in the hierarchy?" question for the next reader. Landed with this
  change's glossary work.
- The **full diagram above** is *not* added to `GLOSSARY.md` by this change
  (it documents the design/seed-keying machinery, #172/#174, not the option
  vocabulary). Promoting it to a `## Storage layout` glossary subsection — or a
  dedicated docs change — is deferred.
