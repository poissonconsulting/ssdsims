# Design: scenario-option-vocabulary

## Context

The repo grew three overlapping vocabularies: **knob** (genus, ~264
occurrences), **axis** (species that fans out), and **simulation setting**
(species applied within a task), plus stray "crew configuration knob" uses.
The terms were settled in discussion (see PR thread): "knob" is informal and
literature-free; "simulation setting" is inconsistently prefixed; DoE-derived
alternatives ("factor", "level", "study") were challenged and rejected as
working terms because a DoE *factor varies by definition* (making "constant
factor" an oxymoron and colliding with R's `factor()` and big-O "constant
factor"), "level" collides with the repo's pervasive `-level` qualifiers and
Hive directory levels, and "study" is already anchored at the top of the
hierarchy (`DESCRIPTION`, README: "simulation studies").

## Goals / Non-Goals

**Goals:**

- One coherent, always-qualified vocabulary: **scenario option** (genus) =
  **scenario axis** | **scenario setting**; **crew option** for crew config.
- Full-repo rename including archives, `NEWS.md`, and file names; old terms
  survive only in this change's artifacts.
- GLOSSARY hierarchy (*simulation study* ⊃ *design* ⊃ *scenario* ⊃ *task*)
  and the DoE/Morris-et-al. mapping table.

**Non-Goals:**

- No behaviour change beyond error-message text; no signature changes; no
  on-disk layout changes (`(seed, primer)`, shard paths untouched).
- No rename of "axis" itself (established, geometric, correct).
- The `scenario-combine` change (branch `claude/eager-hawking-6si3cp`) is
  reworded on its own branch in tandem — not here.

## Decisions

### D1: Taxonomy boundaries — what is (and isn't) a scenario option

A **scenario option** is a declarative `ssd_define_scenario()` parameter that
shapes the *results*: every scenario option is either a **scenario axis**
(in `task_axes(step)`: multiplies tasks, enters the per-task primer and shard
paths) or a **scenario setting** (absent from `task_axes(step)`: consumed
inside each task). Excluded from the genus, as today's GLOSSARY already
excludes them from "knob":

- `data`, `nsim`, `seed` — the required positional inputs; `name`.
- `partition_by`, `bundle`, `upload` — **layout arguments** (they relocate
  shards, never change task results). The requirement name "Configurable,
  validated partition_by knob …" becomes "… partition_by argument …", and
  task-shards' "a non-layout knob" becomes "a non-layout argument".

*Alternative considered*: making layout arguments a third option species —
rejected; "option" should keep the axis/setting dichotomy crisp, and the
specs already say "the partitioning and remaining arguments".

### D2: Always-qualified discipline, with anaphora

Bare "option"/"setting" never *introduce* the term; "scenario option",
"scenario axis", "scenario setting", "crew option" do. Anaphoric
back-references in the same passage ("… or omit the option") are fine. The
`print()` markers stay bare (`(setting)`), as the scenario context qualifies
them — this also avoids print-width churn.

### D3: Error message wording

`validate_scenario_ci()` aborts with: *"Bootstrap-only scenario option
('nboot') cannot be set when `ci = FALSE`. Set `ci = TRUE` to enable
bootstrap, or omit the option."* (plural: "… scenario options … omit the
options"). Snapshots regenerate in the same commit.

### D4: File renames

- `openspec/changes/archive/2026-06-07-dists-simulation-setting/` →
  `2026-06-07-dists-scenario-setting/` via `git mv` (its *content* is also
  reworded, per the full-rename scope).
- The runtime test fixture `knobs.rds` (written by `test-task-shards.R`, read
  by `fixtures/slice-invalidation-targets.R`) → `opts.rds`, variable `knobs`
  → `opts`; prose in the comments says "scenario options".
- `est-method-setting` / `nrow-max-setting` change names keep their bare
  `-setting` suffix: kebab-case change names are labels, not glossary terms,
  and "setting" itself survives in "scenario setting".

### D5: GLOSSARY structure

The "axis" entry is retitled **scenario axis (cross-join axis)** and
**simulation setting** becomes **scenario setting**; a new **scenario option**
entry defines the genus and the exclusions (D1). A new "Hierarchy" section
states: **simulation study** (the endeavour — may span several collections,
e.g. different performance-measure schemas, pilot vs cluster runs, or run
epochs) ⊃ **design** (`ssd_design()`, a named set of scenarios run as one
pipeline / store / union summary, DoE sense — in-flight `scenario-combine`
change) ⊃ **scenario** ⊃
**task**. A mapping table glosses the DoE/Morris-White-Crowther (2019,
*Statistics in Medicine* 38:2074–2102) terms: scenario axis ≈ factor, axis
value ≈ level, task ≈ factorial cell (the literature's "scenario"), scenario
setting ≈ held-constant condition, `nsim` ≈ repetitions per cell. "Factor",
"level", "study" remain glosses, never working terms.

### D6: Commit plan (per user instruction)

1. `refactor: rename knob to scenario option` — bulk knob → scenario option /
   crew option (code, docs, specs, archives, NEWS.md, templates), including
   the GLOSSARY edits for this rename, the error-message change, regenerated
   `man/` and snapshots, fixture rename.
2. `refactor: rename simulation setting to scenario setting` — bulk rename
   including the archive directory `git mv` and its GLOSSARY edits.
3. `docs: add glossary hierarchy and DoE mapping` — cleanup pass (grammar
   around bulk-renamed phrases, missed stragglers, `air` formatting) plus the
   GLOSSARY hierarchy section and mapping table.

The PR squash title: `refactor: rename knob/simulation-setting vocabulary to
scenario option/axis/setting`.

## Risks / Trade-offs

- [Bulk sed mangles grammar ("a option", broken line-wraps in 80-col prose)]
  → rename via per-file review, not blind `sed`; commit 3 is a dedicated
  re-read of every renamed paragraph; `air` re-wraps code, manual re-wrap for
  Markdown.
- [Rewriting archives/NEWS.md loses historical wording] → accepted by
  explicit user decision; git history preserves the originals.
- [Snapshot/man churn hides real diffs] → regenerate in the same commit as
  the source change so each commit is self-consistent and `R CMD check`-green.
- [In-flight changes (`nrow-max-setting`, `scenario-input-types`,
  `migrate-public-api`, `error-call-origin`) edited here may conflict with
  their own branches] → wording-only edits; conflicts are trivial to resolve
  in favour of the new vocabulary.
- [GLOSSARY references `ssd_design()` before scenario-combine lands] →
  marked as in-flight with a pointer to the change, like other
  forward-references in the glossary.

## Open Questions

None — scope (full rename incl. file names), genus ("scenario option"), and
commit plan were settled by the user in discussion.
