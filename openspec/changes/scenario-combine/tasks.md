# Tasks: scenario-combine

## 1. `ssd_design()` collection constructor

- [ ] 1.1 Add `R/design.R` with exported `ssd_design(...)`: capture names from
      explicit argument names or derive from the argument expression
      (reuse/mirror the `ssd_data()` derivation helpers in `R/data.R`), return a
      named list of scenarios classed `ssdsims_design`
- [ ] 1.2 Validate: at least one scenario (empty call aborts; a design of one is
      valid and uniformly shaped), every element is an `ssdsims_scenario`
      (`chk_s3_class`), names unique, non-empty, non-`NA`, and matching
      `^[A-Za-z][A-Za-z0-9_]*$`, with informative errors naming the offending
      element; construction is RNG-free
- [ ] 1.3 Unit tests in `tests/testthat/test-design.R`: derived vs explicit
      names, input order preserved, a design of one, empty-call, duplicate/empty/
      unsafe-name and non-scenario-element errors, `.Random.seed` untouched

## 2. Per-step cumulative content keys (`sig=`)

- [ ] 2.1 Add an internal `step_sig(scenario, step, sig_parent)` helper:
      `sample_sig = hash(seed, scenario_step_slice(scenario, "sample"))`,
      `fit_sig = hash(sample_sig, scenario_step_slice(scenario, "fit"))`,
      `hc_sig = hash(fit_sig, hc_draw_slice(scenario))`; short fixed-width digest
      (mirroring the `layout=` hash width) — `seed` is hashed explicitly because
      `scenario_step_slice()` omits it
- [ ] 2.2 Add a sig-addressed results-dir helper (the design sibling of
      `scenario_results_dir()`): a shard's root is `<root>/<step>/sig=<stepsig>`,
      and its file is `<root>/<step>/sig=<stepsig>/<cells>/part.parquet`
- [ ] 2.3 Sig-completeness tests: forking each off-axis byte-changing setting
      (`seed`, `nrow_max`, `dists`) changes that step's sig (and downstream
      sigs); a readout-only difference (`proportion`/`est_method`/`ci`/`samples`)
      does NOT change `hc_sig`

## 3. hc readout aggregation

- [ ] 3.1 Add `hc_draw_slice(scenario, est_method, proportion, ci, samples)`:
      `scenario_step_slice(scenario, "hc")` with the readout fields replaced by
      the design-wide aggregates — `est_method`/`proportion` set to the union and
      `ci`/`samples` to the `any()` — so the per-shard runner receives the
      aggregated slice unchanged
- [ ] 3.2 Verify the existing `ssd_run_hc_step()` consumes the aggregated slice
      with no change (vector `est_method`/`proportion`, scalar `ci`/`samples`),
      reusing `hc_collapse_est_methods()` and `ssdtools::ssd_hc()` (no ssdtools
      edit); confirm `est` is ci-invariant for the `ci = FALSE`-reads-`ci = TRUE`
      path
- [ ] 3.3 Reconcile the `ci`-gated cell collapse: under design `any(ci)` the hc
      task fan-out uses the `ci = TRUE` cells (`nboot`/`ci_method`/`parametric`),
      and a `ci = FALSE` member reads `est` from a bootstrap cell

## 4. `ssd_design_targets()` design factory

- [ ] 4.1 Add exported
      `ssd_design_targets(design, ..., root = "results", upload = NULL, cue = NULL)`:
      `rlang::check_dots_empty()`, `chk_s3_class(design, "ssdsims_design")`, same
      `upload` validation as the single-scenario factory
- [ ] 4.2 Compute the design-wide hc readout aggregates (union of
      `proportion`/`est_method`, `any` of `ci`/`samples`) across the members
- [ ] 4.3 Per scenario, build its sig-addressed shard targets (names from
      `<step>`, `sig`, and cells; roots from the sig results-dir; the command
      closes over the step slice / aggregated hc slice, never the whole scenario)
- [ ] 4.4 Return the **union of all members' shard targets de-duplicated by
      target name** (shared `(step, sig, cells)` → one target); assert no two
      retained targets share a name with differing commands
- [ ] 4.5 With non-`NULL` `upload`, pair each (deduplicated) shard with one
      `upload_<step>` target addressed by the same `sig=<hash>/<cells>` path (no
      `scenario=` prefix), keeping the factory free of network I/O

## 5. Per-scenario and combined summaries

- [ ] 5.1 Per scenario, mint a `summary_<name>` target that reads the shared `hc`
      shards its tasks reference and filters to that scenario's `(proportion,
      est_method, ci, samples)` readout slice and cell slice, writing the
      per-scenario compact summary (compact path; retained-draws stay separate)
- [ ] 5.2 Add `ssd_summarise_design(summaries, path)` (exported, next to
      `ssd_summarise()`): `summaries` a named character vector of per-scenario
      compact summary paths; lazily read each landed file via `duckplyr`, tag a
      literal `scenario` column, `union_all`, write `path` inside DuckDB (no R
      materialisation); skip paths that do not exist (survivors union)
- [ ] 5.3 Mint the single top-level `summary` target (the only unsigned name): an
      `edge_block()` over every `summary_<name>` target, then
      `ssd_summarise_design()` to `<root>/summary.parquet`, `format = "file"`

## 6. Integration tests

- [ ] 6.1 Sharing: a two-scenario design differing only in a readout
      (`est_method`) under one `seed` builds each `sample`/`fit`/`hc` shard once
      (no duplicate target names), and each scenario's per-task results are
      byte-identical to a standalone `ssd_scenario_targets()` run
- [ ] 6.2 `dists` comparison: two scenarios differing only in `dists` share every
      `sample` shard and fork their `fit`/`hc` shards under distinct sigs
- [ ] 6.3 Readout aggregation: a `proportion`/`ci`/`samples` comparison shares all
      shards; each scenario's summary contains exactly its requested readout rows;
      the `ci = FALSE` member's `est` equals its standalone `ci = FALSE` value
- [ ] 6.4 Combined summary: `<root>/summary.parquet` unions the per-scenario
      filtered summaries with the `scenario` tag; with one summary missing, the
      combined summary unions the survivor without aborting
- [ ] 6.5 Distinct seeds: two scenarios with different seeds share nothing (all
      shards under distinct sigs)
- [ ] 6.6 Design growth: run a design of one to completion; add a readout-only
      member and re-`tar_make()` — `sample`/`fit` cached, only `hc` and summaries
      rebuild; add a distinct-seed member — its shards build, all others cached;
      remove a member — remaining shards cached
- [ ] 6.7 Upload shape: `upload = ssd_upload_dryrun()` pairs each deduplicated
      shard with one upload target under its `sig=<hash>/<cells>` path; an azure
      destination unit test shows the sig path (no `scenario=` level, no network)

## 7. Documentation

- [ ] 7.1 Roxygen for `ssd_design()`, `ssd_design_targets()` (the sig addressing,
      cross-scenario sharing, the readout aggregation — `union` over
      `proportion`/`est_method`, `any` over `ci`/`samples` — the common-random-
      numbers note for shared seeds, the name-safety contract, the growth
      contract, and the safe-but-recomputing flat→design note), and
      `ssd_summarise_design()`; regenerate `NAMESPACE`/`man/`
- [ ] 7.2 Add a design section to `vignettes/sharded-pipeline.qmd` and a pointer
      in `README.Rmd`, covering shard sharing and the readout aggregation; extend
      the `inst/targets-templates/` template comments (or add a template — resolve
      the design's open question); update `_pkgdown.yml`
- [ ] 7.3 `GLOSSARY.md` *Design terms* entries (`scenario`/`design`/`study`);
      `ROADMAP.md`: mark the entry in-flight and move to `## Done` on archive
- [ ] 7.4 Format with `air`, run `devtools::check()`, and confirm the
      `task-shards` / `shard-runner` capabilities needed no delta (single-scenario
      factory and per-shard runner contracts unchanged; no ssdtools edit)
