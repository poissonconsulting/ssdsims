# Tasks: scenario-combine

## 1. `ssd_design()` collection constructor + consistency contract

- [x] 1.1 Add `R/design.R` with exported `ssd_design(...)`: capture names from
      explicit argument names or derive from the argument expression
      (reuse/mirror the `ssd_data()` derivation helpers in `R/data.R`), return a
      named list of scenarios classed `ssdsims_design`
- [x] 1.2 Validate shape: at least one scenario (empty call aborts; a design of
      one is valid), every element an `ssdsims_scenario` (`chk_s3_class`), names
      unique, non-empty, non-`NA`, matching `^[A-Za-z][A-Za-z0-9_]*$`
- [x] 1.3 Enforce the name→value consistency contract across members: same
      `dataset` name ⟹ identical data, same `min_pmix` name ⟹ identical function,
      same `distset` name ⟹ identical members, and identical `partition_by`;
      informative errors naming the offending binding; construction RNG-free
- [x] 1.4 Unit tests in `tests/testthat/test-design.R`: derived vs explicit
      names, input order, design of one, empty-call, duplicate/unsafe-name and
      non-scenario errors, each consistency violation aborts, `.Random.seed`
      untouched

## 2. Ragged task-set union + cell de-duplication

- [x] 2.1 Add an internal helper that unions the members' per-step task tables
      and de-duplicates by `<step>_id` (the cell key), tagging each task with the
      members that claim it (for later readout aggregation and summary filtering)
- [x] 2.2 Confirm a shared cell yields one shard target (plain `<step>_<cells>`
      name, no prefix) whose command closes over the per-step slice, so two
      members' shared cells produce byte-identical target definitions

## 3. Naked addressing under `seed=` / `layout=`

- [x] 3.1 Add a results-dir helper extending `scenario_results_dir()` with a
      `seed=<value>` level: `<root>/seed=<value>/layout=<hash(partition_by)>`;
      `ssd_scenario_targets()` keeps its `seed=`-free layout addressing unchanged
- [x] 3.2 Tests: distinct-seed members land under distinct `seed=` trees and share
      nothing; same-seed members share coincident cells

## 4. Per-overlap hc readout aggregation

- [ ] 4.1 Group the unioned hc tasks by the hc cell axes
      (`nboot`/`ci_method`/`parametric`/`distset` + fit identity) and reduce the
      four non-axis settings over the claiming members: `union` `proportion`/
      `est_method`, `any` `ci`/`samples`
- [ ] 4.2 Implement the `ci` NA-collapse routing: the computed hc shards are every
      `ci = TRUE` member's cells plus the `ci = FALSE` cells with no overlapping
      `ci = TRUE` shard at the same `(fit-id, distset)`; a `ci = FALSE` task's
      analytical `est` reads from the coincident `ci = TRUE` shard when present
- [ ] 4.3 Verify `ssd_run_hc_step()` consumes the per-cell aggregated demand
      unchanged (vector `est_method`/`proportion`, scalar `ci`/`samples`), reusing
      `hc_collapse_est_methods()` and `ssdtools::ssd_hc()` — no ssdtools edit
- [ ] 4.4 Confirm byte-identity: a served `ci = FALSE` `est` equals the standalone
      `ci = FALSE` value; a `ci = TRUE` member's CI uses its own cell primer

## 5. `ssd_design_targets()` factory

- [x] 5.1 Add exported
      `ssd_design_targets(design, ..., root = "results", upload = NULL, cue = NULL)`:
      `rlang::check_dots_empty()`, `chk_s3_class(design, "ssdsims_design")`, same
      `upload` validation as the single-scenario factory
- [x] 5.2 Assemble the de-duplicated union of sample/fit/hc shard targets under the
      `seed=`/`layout=` roots, with the per-overlap hc demand from §4
- [x] 5.3 With non-`NULL` `upload`, pair each (deduplicated) shard with one
      `upload_<step>` target addressed by the same `seed=`/`layout=`/`<cells>`
      path (no per-scenario prefix), keeping the factory free of network I/O

## 6. Per-scenario and combined summaries

- [x] 6.1 Per member, mint a `summary_<name>` target reading the shared shards its
      tasks reference and filtering to that member's cells and `(proportion,
      est_method, ci, samples)` readout slice, writing its compact summary
- [x] 6.2 Add `ssd_summarise_design(summaries, path)` (exported, next to
      `ssd_summarise()`): named character vector of per-member compact summary
      paths; lazily read each landed file via `duckplyr`, tag a literal `scenario`
      column, `union_all`, write `path` inside DuckDB (no R materialisation); skip
      missing paths (survivors union)
- [x] 6.3 Mint the single top-level `summary` target: an `edge_block()` over every
      `summary_<name>` target, then `ssd_summarise_design()` to
      `<root>/summary.parquet`, `format = "file"`

## 7. Integration tests

- [x] 7.1 Ragged grid: a coarse + refinement design shares the overlapping cells
      (one target, byte-identical per-task results vs standalone) and builds the
      refinement's extra cells once; no duplicate target names
- [ ] 7.2 Per-overlap aggregation: the `ci = FALSE, nsim = 1000` vs
      `ci = TRUE, nsim = 10` design bootstraps only the 10 overlapping sims; the
      `ci = FALSE` member's `est` for those sims equals its standalone value
- [x] 7.3 Distinct seeds: two members differing only in `seed` share nothing (each
      under its own `seed=` tree)
- [ ] 7.4 distset sharing: two members differing only in `distset` share all
      `sample`/`fit` shards, differing only in their `distset` hc cells
- [x] 7.5 Combined summary: `<root>/summary.parquet` unions the per-member filtered
      summaries with the `scenario` tag; with one summary missing, the survivor is
      unioned without aborting
- [ ] 7.6 Upload shape: `upload = ssd_upload_dryrun()` pairs each deduplicated
      shard with one upload target under its `seed=`/`layout=`/`<cells>` path; an
      azure-destination unit test shows the path (no per-scenario level, no network)

## 8. Documentation

- [x] 8.1 Roxygen for `ssd_design()` (the consistency contract), `ssd_design_targets()`
      (ragged union, naked `seed=`/`layout=` addressing, varying-seed support, the
      per-overlap readout aggregation and `ci`-routing, the `nrow_max` undefined-
      behaviour note, and the safe-but-recomputing flat→design note), and
      `ssd_summarise_design()`; regenerate `NAMESPACE`/`man/`
- [x] 8.2 Add a design section to `vignettes/sharded-pipeline.qmd` led by the
      **irregular-grid** use case (finer detail in a subregion without the full
      cross-product), with setting-comparison secondary; pointer in `README.Rmd`;
      extend the `inst/targets-templates/` comments (or add a template — resolve the
      open question); update `_pkgdown.yml`
- [x] 8.3 Add a new `vignettes/scenario-to-design.qmd` walking the
      **single-scenario → design migration** end to end: a standalone
      `ssd_scenario_targets()` run, the one-line switch to
      `ssd_design_targets(ssd_design(scenario))` (byte-identical results, the
      safe-but-recomputing `seed=` note), then adding a refining member that reuses
      the cached cells; register it in `_pkgdown.yml`
- [x] 8.4 `GLOSSARY.md` *Design terms* entries (`scenario`/`design`/`study`);
      `ROADMAP.md`: mark the entry in-flight and move to `## Done` on archive
- [ ] 8.5 Format with `air`, run `devtools::check()`, and confirm the `task-shards`
      / `shard-runner` capabilities needed no delta (single-scenario factory and
      per-shard runner contracts unchanged; no ssdtools edit)
