# Tasks: scenario-combine

## 1. `ssd_scenarios()` collection constructor

- [ ] 1.1 Add `R/scenarios.R` with exported `ssd_scenarios(...)`: capture names
      from explicit argument names or derive from the argument expression
      (reuse/mirror the `ssd_data()` derivation helpers in `R/data.R`), return
      a named list classed `ssdsims_scenarios`
- [ ] 1.2 Validate: every element is an `ssdsims_scenario` (`chk_s3_class`),
      names unique, non-empty, non-`NA`, and matching the safe shape
      `^[A-Za-z][A-Za-z0-9_]*$` (target-name prefix + directory level), with
      informative errors naming the offending element; construction is RNG-free
- [ ] 1.3 Unit tests in `tests/testthat/test-scenarios.R`: derived vs explicit
      names, input order preserved, duplicate/empty/unsafe-name and
      non-scenario-element errors, `.Random.seed` untouched

## 2. Thread a target-name prefix through the single-scenario factory

- [ ] 2.1 In `R/targets-runner.R`, add an internal `prefix` parameter
      (default `""`) flowing through `step_map()` (step and upload target
      names), `shard_cell_names()`, and the `summary` target name, so every
      minted name becomes `<prefix><name>`; per-child `edge_block()` wiring
      resolves through `shard_cell_names()` and needs no separate handling
- [ ] 2.2 Verify `ssd_scenario_targets()` output is unchanged with the empty
      prefix: existing factory tests pass untouched (no snapshot or
      target-name churn)

## 3. `ssd_scenarios_targets()` multi-scenario factory

- [ ] 3.1 Add exported
      `ssd_scenarios_targets(scenarios, ..., root = "results", upload = NULL, cue = NULL)`:
      `rlang::check_dots_empty()`, `chk_s3_class(scenarios, "ssdsims_scenarios")`,
      same `upload` validation as the single factory
- [ ] 3.2 Per scenario, assemble the single-scenario target set with
      `prefix = paste0(name, "_")` and
      `root = scenario_results_dir(scenario, root = file.path(root, paste0("scenario=", name)))`
- [ ] 3.3 With non-`NULL` `upload`, extend the destination's blob prefix by
      `scenario=<name>` per scenario via an internal helper on
      `ssdsims_upload` (azure: join with the existing `prefix`; dryrun:
      pass-through), keeping the factory free of network I/O

## 4. Combined cross-scenario summary

- [ ] 4.1 Add `ssd_summarise_scenarios(summaries, path)` (exported, next to
      `ssd_summarise()` in `R/targets-runner.R`): `summaries` a named character
      vector of per-scenario compact summary paths; lazily read each landed
      file via `duckplyr`, tag a literal `scenario` column, `union_all`, and
      write `path` inside DuckDB (no R materialisation); skip paths that do
      not exist (survivors union)
- [ ] 4.2 In `ssd_scenarios_targets()`, mint the single top-level `summary`
      target (the only unprefixed name): an `edge_block()` over every
      per-scenario `<name>_summary` target, then
      `ssd_summarise_scenarios()` over the per-scenario compact summary paths
      (computed from the roots — never the second `summary-samples` path) to
      `<root>/summary.parquet`, `format = "file"`

## 5. Integration tests

- [ ] 5.1 Pipeline test (mirroring the existing `_targets.R` fixture pattern in
      `tests/testthat/fixtures/`): two tiny scenarios through
      `ssd_scenarios_targets()`; one `tar_make()` builds both scenarios'
      shards under their `scenario=<name>/layout=<hash>` trees with no
      duplicate target names
- [ ] 5.2 Byte-identity: each scenario's per-task `sample`/`fit`/`hc` results
      (aligned by `<step>_id`) equal a standalone `ssd_scenario_targets()` run
      of the same scenario; rerun under a different collection name yields
      identical results (addressing-only)
- [ ] 5.3 Combined summary: `<root>/summary.parquet` unions both compact
      summaries with the per-row `scenario` tag and leaves the per-scenario
      summaries unchanged; with one scenario's summary missing, the combined
      summary unions the survivor without aborting
- [ ] 5.4 Same-layout isolation: two scenarios with identical `partition_by`
      land under distinct `scenario=` subtrees and neither's readers see the
      other's shards
- [ ] 5.5 Upload shape: `upload = ssd_upload_dryrun()` pairs each scenario's
      shards with upload targets and an azure-destination unit test shows the
      `scenario=<name>` prefix extension (no network)

## 6. Documentation

- [ ] 6.1 Roxygen for `ssd_scenarios()`, `ssd_scenarios_targets()` (including
      the common-random-numbers note for shared seeds and the name-safety
      contract), and `ssd_summarise_scenarios()`; regenerate `NAMESPACE`/`man/`
- [ ] 6.2 Add a multi-scenario section to `vignettes/sharded-pipeline.qmd` and
      a pointer in `README.Rmd`; extend the `inst/targets-templates/` template
      comments (or add a template — resolve the design's open question);
      update `_pkgdown.yml`
- [ ] 6.3 `GLOSSARY.md`: add the scenario *name* (collection key → target
      prefix + `scenario=` path level); `ROADMAP.md`: mark the entry in-flight
      and move to `## Done` on archive
- [ ] 6.4 Format with `air`, run `devtools::check()`, and confirm the
      `task-shards` capability needed no delta (single-scenario factory
      contract unchanged)
