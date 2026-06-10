# Proposal: scenario-option-vocabulary

## Why

The repo's umbrella term for a declarative scenario parameter — **knob** (~90
live uses, ~264 total) — is informal, hard to pronounce, and absent from the
simulation-study literature, while its species terms are inconsistently
prefixed ("axis" vs "**simulation** setting") and collide with unrelated uses
("`crew` configuration knob"). With `scenario-combine` introducing a scenario
*collection* layer, the vocabulary needs to be settled once, coherently, before
more documents accrete on top of it.

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
- **GLOSSARY.md gains the full hierarchy and a literature mapping**:
  *simulation study* (the endeavour) ⊃ *design* (`ssd_design()`: a named set
  of scenarios run as one pipeline/store/summary, DoE sense — named by the
  in-flight `scenario-combine` change) ⊃
  *scenario* ⊃ *task*; plus a mapping table to DoE / Morris-White-Crowther
  (2019) terminology — scenario axis ≈ factor, axis value ≈ level, task ≈
  factorial cell, scenario setting ≈ held-constant condition, `nsim` ≈
  repetitions per cell — noting that the literature's "scenario" corresponds
  to our *task*, not our *scenario*. "Factor", "level", and "study" remain
  glossary glosses only, never working terms.

## Capabilities

### New Capabilities

<!-- None: this is a vocabulary change; no new behaviour. -->

### Modified Capabilities

- `scenario-definition`: the bootstrap-only rejection requirement's observable
  error text changes from "Bootstrap-only knob …" to "Bootstrap-only scenario
  option …"; all requirement prose switches from knob/simulation-setting to
  scenario-option/scenario-setting vocabulary.
- `task-shards`, `task-lists`, `manifest`, `hazard-concentrations`:
  terminology-only requirement rewording (no observable behaviour change);
  handled as delta specs so the synced main specs stay canonical.

## Impact

- **Code**: comments/roxygen in `R/scenario.R`, `R/task-lists.R`,
  `R/manifest.R`, `R/targets-runner.R`; the error message in
  `validate_scenario_ci()` (snapshot churn in `tests/testthat/_snaps/`); test
  names and the `knobs.rds`/`knobs` fixture naming in
  `tests/testthat/fixtures/slice-invalidation-targets.R` and
  `test-task-shards.R`; `inst/targets-templates/cluster/`
  (`controller.R`, `README.md`).
- **Docs**: `GLOSSARY.md` (rename + hierarchy + mapping table),
  `TARGETS-DESIGN.md` (~23 knob uses), `AGENTS.md`, `ROADMAP.md`, `README`,
  vignettes, regenerated `man/`.
- **OpenSpec**: main specs via delta specs above; active changes
  (`nrow-max-setting`, `scenario-input-types`, `migrate-public-api`,
  `error-call-origin`) and the archive reworded in place; one archive
  directory renamed. The `scenario-combine` change lives on another branch
  and is reworded there in tandem.
- **APIs**: no exported signatures change; error text only.
- **Dependencies**: none.
