# Tasks: scenario-option-vocabulary

## 1. Commit 1 — `refactor: rename knob to scenario option`

- [ ] 1.1 Rename "knob" → "scenario option" (with anaphora per design D2, and
      "bootstrap-only knobs" → "bootstrap-only scenario options" /
      "bootstrap axes" in fan-out contexts) across `R/` roxygen and comments
      (`scenario.R`, `task-lists.R`, `manifest.R`, `targets-runner.R`),
      reviewing each hunk for grammar — no blind `sed`
- [ ] 1.2 Change the `validate_scenario_ci()` error to the design D3 wording
      ("Bootstrap-only scenario option ('nboot') cannot be set when
      `ci = FALSE`. Set `ci = TRUE` to enable bootstrap, or omit the
      option."), update `tests/testthat/test-scenario.R` expectations and
      regenerate `_snaps/scenario.md`
- [ ] 1.3 Rename "knob" in test names/comments (`test-scenario.R`,
      `test-task-lists.R`, `test-task-shards.R`, `test-manifest.R`) and the
      runtime fixture `knobs.rds`/`knobs` → `opts.rds`/`opts` in
      `fixtures/slice-invalidation-targets.R` + `test-task-shards.R` (design
      D4)
- [ ] 1.4 Rename the three "crew configuration knob" uses to "crew option"
      (`inst/targets-templates/cluster/controller.R`, its `README.md`, and
      the archived `cluster-pipeline` design)
- [ ] 1.5 Rename "knob" across docs: `GLOSSARY.md` (entry prose only — the
      genus entry itself lands in 3.2), `TARGETS-DESIGN.md`, `AGENTS.md`,
      `ROADMAP.md`, vignettes, `scripts/example2.R`, `NEWS.md`, and
      `openspec/specs/` main specs (apply the delta-spec wording, including
      the two RENAMED requirement headers and the Purpose line of
      `scenario-definition`)
- [ ] 1.6 Rename "knob" in `openspec/changes/` active changes and `archive/`
      (all files; old term remains only in this change's artifacts)
- [ ] 1.7 Regenerate `man/` (`devtools::document()`), run `air format .`,
      run `devtools::test()`; verify `git grep -i knob` hits only this
      change's artifacts; commit

## 2. Commit 2 — `refactor: rename simulation setting to scenario setting`

- [ ] 2.1 Rename "simulation setting" → "scenario setting" across `R/`
      roxygen/comments, tests, vignettes, `TARGETS-DESIGN.md`, `ROADMAP.md`,
      `NEWS.md`, and `GLOSSARY.md` (retitle the **simulation setting** entry
      to **scenario setting**; retitle **axis** to **scenario axis
      (cross-join axis)** per design D5)
- [ ] 2.2 Apply the delta-spec wording to `openspec/specs/` (scenario-setting
      occurrences incl. the `est_method` RENAMED header) and rename the term
      across active changes and `archive/` contents
- [ ] 2.3 `git mv openspec/changes/archive/2026-06-07-dists-simulation-setting
      2026-06-07-dists-scenario-setting` and fix references to the old change
      name in live docs (design D4)
- [ ] 2.4 Regenerate `man/`, run `air format .`, run `devtools::test()`;
      verify `git grep -i "simulation setting"` hits only this change's
      artifacts; commit

## 3. Commit 3 — `docs: add glossary hierarchy and DoE mapping`

- [ ] 3.1 Cleanup pass: re-read every paragraph touched by 1.x/2.x for
      grammar, line-wrap (80-col Markdown), and always-qualified discipline
      (no bare "option"/"setting" introducing a term); catch stragglers via
      `git grep -niE "knob|simulation setting"`
- [ ] 3.2 Add the **scenario option** genus entry to `GLOSSARY.md` (design
      D1: axis|setting dichotomy; `data`/`nsim`/`seed`/`name` and the layout
      arguments `partition_by`/`bundle`/`upload` are excluded) and align the
      scenario-axis/scenario-setting entries with it
- [ ] 3.3 Add the GLOSSARY "Hierarchy" section: simulation study ⊃ design
      (`ssd_design()`, marked in-flight → `scenario-combine`) ⊃ scenario ⊃
      task, with the multi-design-study examples (design D5)
- [ ] 3.4 Add the literature mapping table to `GLOSSARY.md` with citations —
      Morris, White & Crowther (2019) *Statistics in Medicine* 38:2074–2102
      (ADEMP, factors varied factorially) and DoE factor/level/cell — mapping
      scenario axis ≈ factor, axis value ≈ level, task ≈ factorial cell (the
      literature's "scenario"), scenario setting ≈ held-constant condition,
      `nsim` ≈ repetitions per cell; note "factor"/"level"/"study" are
      glosses, never working terms
- [ ] 3.5 Final verification: `devtools::test()`, `devtools::document()`
      diff-clean, `air format .` clean, full `R CMD check` if time permits;
      commit

## 4. PR

- [ ] 4.1 Push branch `claude/happy-curie-8te0ci` and open the PR titled
      `refactor: rename knob/simulation-setting vocabulary to scenario
      option/axis/setting`, description capturing the final vocabulary table
      and scope decisions
