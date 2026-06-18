# Tasks: scenario-option-vocabulary

> Redone against the evolved `main` (see design.md *Second-attempt note*). The
> proposal, design, and **delta specs are reconciled** against current `main`
> already (this propose pass). The tasks below are the implement phase.

## 1. Commit 1 — `refactor: rename knob to scenario option`

- [x] 1.1 Rename "knob" → "scenario option" (always qualified per design D2;
      "bootstrap-only knobs" → "bootstrap-only scenario options"; fan-out
      "bootstrap knobs" → "bootstrap axes") across `R/` roxygen/comments:
      `scenario.R`, `task-lists.R`, `manifest.R`, `targets-runner.R`,
      `hc-sims.R`, `internal.R`, `params.R` — reviewing each hunk, no blind sed
- [x] 1.2 Rename the **env-var** sense (not a scenario option) in
      `R/duckplyr-config.R` and `openspec/specs/duckplyr-config/spec.md`:
      "the `SSDSIMS_DUCKDB_*` knob" → "… environment variable"; the two
      scenario titles → "… environment variable" (matches the reconciled
      `duckplyr-config` delta)
- [x] 1.3 Change the `validate_scenario_ci()` error to design D3 wording
      ("Bootstrap-only scenario option ('nboot') … omit the option."), update
      `tests/testthat/test-scenario.R` and regenerate `_snaps/scenario.md`
- [x] 1.4 Rename "knob" in test names/comments (`test-scenario.R`,
      `test-task-lists.R`, `test-task-shards.R`, `test-manifest.R`,
      `test-duckplyr-config.R`) and the runtime fixture `knobs.rds`/`knobs` →
      `opts.rds`/`opts` (`fixtures/slice-invalidation-targets.R`,
      `test-task-shards.R`) (design D4)
- [x] 1.5 Rename the "crew configuration knob" / env-var uses → "crew option"
      / "environment variable" in `inst/targets-templates/`
      (`cluster/controller.R`, `cluster/README.md`, `small/_targets.R`,
      `large/_targets.R`)
- [x] 1.6 Rename "knob" across docs: `GLOSSARY.md` (entry prose only — genus
      entry lands in 3.2), `TARGETS-DESIGN.md`, `AGENTS.md`, `ROADMAP.md`,
      vignettes, `scripts/example2.R`, `NEWS.md`, and the live `openspec/specs/`
      main specs (apply the reconciled delta wording, incl. the two RENAMED
      headers and the Purpose line of `scenario-definition`)
- [x] 1.7 Rename "knob" in `openspec/changes/` active changes
      (`distset-hc-axis`, `migrate-public-api`, `scenario-input-types`,
      `error-call-origin`) and `archive/` (all files; old term survives only in
      this change's `proposal.md`/`design.md`)
- [x] 1.8 Regenerate `man/` (`devtools::document()`), `air format .`,
      `devtools::test()`; verify `git grep -i knob` hits only this change's
      proposal/design; commit

## 2. Commit 2 — `refactor: rename simulation setting to scenario setting`

- [x] 2.1 Rename "simulation setting" → "scenario setting" across `R/`
      (`scenario.R`, `task-lists.R`, `hc-sims.R`, `internal.R`, `params.R`),
      tests, vignettes, `TARGETS-DESIGN.md`, `ROADMAP.md`, `NEWS.md`,
      `GLOSSARY.md` (retitle the **simulation setting** entry → **scenario
      setting**; retitle **axis** → **scenario axis (cross-join axis)**, D5)
- [x] 2.2 Apply the reconciled wording to the live `openspec/specs/`
      (`scenario-definition`, `task-lists`, `hazard-concentrations`,
      `scenario-accessors`, incl. the est_method RENAMED header) and rename the
      term across active changes and `archive/`
- [x] 2.3 `git mv openspec/changes/archive/2026-06-07-dists-simulation-setting
      2026-06-07-dists-scenario-setting` and fix references in live docs (D4)
- [x] 2.4 Regenerate `man/`, `air format .`, `devtools::test()`; verify
      `git grep -i "simulation setting"` hits only this change's
      proposal/design; commit

## 3. Commit 3 — `docs: add glossary genus entry and DoE mapping`

- [x] 3.1 Cleanup pass: re-read every paragraph touched by 1.x/2.x for grammar,
      80-col wrap, and always-qualified discipline; catch stragglers via
      `git grep -niE "knob|simulation setting"`
- [x] 3.2 Add the **scenario option** genus entry to `GLOSSARY.md` (design D1:
      axis|setting dichotomy; `data`/`nsim`/`seed`/`name` and the layout
      arguments `partition_by`/`bundle`/`upload` excluded) and align the
      scenario-axis/scenario-setting entries with it
- [x] 3.3 Add the DoE / Morris-White-Crowther (2019) mapping table to
      `GLOSSARY.md` (scenario axis ≈ factor, axis value ≈ level, task ≈
      factorial cell, scenario setting ≈ held-constant condition, `nsim` ≈
      repetitions per cell; "factor"/"level"/"study" are glosses). **Do not**
      add a competing hierarchy section — cross-reference the existing
      `## Design terms` (D5)
- [x] 3.4 Add the one-line storage-hierarchy pointer to the `shard` glossary
      entry (design D8 — the exact wording is there); do **not** add the full
      diagram (it lives in `exploration/`, deferred to a separate docs change)
- [x] 3.5 Verify the live main specs match this change's reconciled delta specs
      (`openspec validate scenario-option-vocabulary --strict`; a dry sync
      shows no unintended drift)
- [x] 3.6 Final verification: `devtools::test()`, `devtools::document()`
      diff-clean, `air format .` clean, `R CMD check` if time permits; commit

## 4. PR

- [ ] 4.1 Force-push `claude/happy-curie-8te0ci` (rebased onto current `main`)
      and update PR #154 (base now current `main`; description reflects the
      reconciled scope)
