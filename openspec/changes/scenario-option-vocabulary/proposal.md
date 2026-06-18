# Proposal: scenario-option-vocabulary

## Why

The repo's umbrella term for a declarative scenario parameter — **knob**
(~290 occurrences across ~90 files) — is informal, hard to pronounce, and
absent from the simulation-study literature, while its species terms are
inconsistently prefixed ("axis" vs "**simulation** setting") and collide with
unrelated uses ("`crew` configuration knob"). The longer this waits the more it
costs: since this change was first proposed, `ssd_design()`
(`scenario-combine`), the `distset` hc axis, `ssd_pmix()`/`ssd_distset()`,
`nrow_max`, and `duckplyr-config` have all landed *using the old vocabulary*, so
the rename now spans those too. Settle it once, coherently, before yet more
documents accrete.

## What Changes

- **Retire "knob"** in favour of the genus **scenario option**: any declarative
  parameter of `ssd_define_scenario()`. A scenario option is either a
  **scenario axis** (fans out into tasks, enters the per-task primer and shard
  paths) or a **scenario setting** (renamed from **simulation setting**:
  applied within each task, never multiplies the task graph).
- **Always-qualified discipline**: bare "option"/"setting" are never used as
  terms — every use is qualified ("scenario option", "scenario axis",
  "scenario setting", "crew option"). Anaphoric back-references in running
  prose after the qualified form has been introduced are fine.
- The three unrelated "`crew` configuration knob" uses become **crew option**.
- **BREAKING (cosmetic)**: the `ci = FALSE` rejection error text changes from
  "Bootstrap-only knob ('nboot') …" to the scenario-option wording. No
  signature, return value, or on-disk layout changes.
- **Full-repo rename including historical records and file names**: archives
  (`openspec/changes/archive/`), `NEWS.md`, and file/directory names carrying
  the old terms (e.g. `…-dists-simulation-setting/` →
  `…-dists-scenario-setting/`, the `knobs.rds` test fixture). The old terms
  survive only in this change's own artifacts, which document the rename.
- **GLOSSARY.md gains the genus entry and a literature mapping**. The
  *simulation study* ⊃ *design* (`ssd_design()`) ⊃ *scenario* ⊃ *task*
  hierarchy already exists as the `## Design terms` section (landed with
  `scenario-combine`), so this change does **not** re-add it; it adds the
  **scenario option** genus entry, retitles **axis** → **scenario axis** and
  **simulation setting** → **scenario setting**, and adds a mapping table to
  DoE / Morris-White-Crowther (2019) terminology — scenario axis ≈ factor,
  axis value ≈ level, task ≈ factorial cell, scenario setting ≈ held-constant
  condition, `nsim` ≈ repetitions per cell — noting the literature's
  "scenario" is our *task*. "Factor", "level", "study" stay glosses only.

## Capabilities

### New Capabilities

<!-- None: this is a vocabulary change; no new behaviour. -->

### Modified Capabilities

- `scenario-definition`: the bootstrap-only rejection requirement's observable
  error text changes from "Bootstrap-only knob …" to "Bootstrap-only scenario
  option …"; all requirement prose switches from knob/simulation-setting to
  scenario-option/scenario-setting vocabulary.
- `task-shards`, `task-lists`, `manifest`, `hazard-concentrations`,
  `scenario-accessors`, `duckplyr-config`: terminology-only requirement
  rewording (no observable behaviour change). The delta specs are reconciled
  against the **current** main requirement text (which has since gained the
  `distset` axis, `ssd_pmix()`/`ssd_distset()`, and `nrow_max`), so syncing
  this change reproduces today's wording plus the rename — never reverting
  those landed changes.

## Impact

- **Code**: comments/roxygen in `R/scenario.R`, `R/task-lists.R`,
  `R/manifest.R`, `R/targets-runner.R`, `R/hc-sims.R`, `R/internal.R`,
  `R/params.R`, and `R/duckplyr-config.R`; the error message in
  `validate_scenario_ci()` (snapshot churn in `tests/testthat/_snaps/`); test
  names and the `knobs.rds`/`knobs` fixture naming in
  `tests/testthat/fixtures/slice-invalidation-targets.R`,
  `test-task-shards.R`, and `test-duckplyr-config.R`;
  `inst/targets-templates/` (`cluster/controller.R`, `cluster/README.md`,
  `small/_targets.R`, `large/_targets.R`).
- **Docs**: `GLOSSARY.md` (genus entry + retitles + mapping table; the
  hierarchy already exists), `TARGETS-DESIGN.md` (~23 knob uses), `AGENTS.md`,
  `ROADMAP.md`, vignettes, `scripts/example2.R`, `NEWS.md`, regenerated
  `man/`.
- **OpenSpec**: main specs via the reconciled delta specs above; the active
  changes (`distset-hc-axis`, `migrate-public-api`, `scenario-input-types`,
  `error-call-origin`) and the archive reworded in place; the
  `2026-06-07-dists-simulation-setting` archive directory renamed
  `…-dists-scenario-setting`. The `scenario-combine` work has landed, so its
  `proposal.md` is reworded in place here too.
- **APIs**: no exported signatures change; error text only.
- **Dependencies**: none.
