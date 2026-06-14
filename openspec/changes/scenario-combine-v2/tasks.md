# Tasks: scenario-combine-v2

> Prerequisite: `content-addressed-shards` (content-pure addressing + the `hc`
> `draw`/`summarise` split this design layer composes) and, transitively,
> `distset-hc-axis`.

## 1. `ssd_design()` collection constructor

- [ ] 1.1 Add `R/design.R` with exported `ssd_design(...)`: capture names from
      explicit argument names or derive from the argument expression (reuse the
      `ssd_scenario_data()` derivation helpers), return a named list of scenarios
      classed `ssdsims_design`.
- [ ] 1.2 Validate: at least one scenario (empty call aborts; a design of one is
      valid and uniformly shaped), every element an `ssdsims_scenario`
      (`chk_s3_class`), names unique, non-empty, non-`NA`. Names are **selection
      labels** (not addressing — they do not enter target names or paths), so the
      path-safety constraint relaxes; construction is RNG-free.
- [ ] 1.3 Unit tests `tests/testthat/test-design.R`: derived vs explicit names,
      input order preserved, a design of one, empty-call and
      duplicate/empty/non-scenario errors, `.Random.seed` untouched.

## 2. `ssd_design_targets()` composing content-addressed targets

- [ ] 2.1 Add exported
      `ssd_design_targets(design, ..., root = "results", upload = NULL, cue = NULL)`:
      `rlang::check_dots_empty()`, `chk_s3_class(design, "ssdsims_design")`, the
      single-scenario factory's `upload` validation.
- [ ] 2.2 Per member, emit the **content-addressed** target set
      `ssd_scenario_targets()` builds (no per-scenario root or prefix), so members
      sharing content emit the identical target and `targets` collapses it to one
      build; distinct content stays distinct. Rely on `targets`' duplicate-name
      handling for shared targets and its abort as the collision backstop.
- [ ] 2.3 Confirm the factory performs no network I/O; `upload` pairing rides the
      content-addressed `upload_<step>` targets unchanged (no per-scenario prefix).

## 3. Combined design summary, coordinate-keyed

- [ ] 3.1 Add `ssd_summarise_design(summaries, path)` next to `ssd_summarise()`:
      lazily read each landed per-coordinate compact summary via `duckplyr`,
      `union_all`, and write `path` inside DuckDB (no R materialise); skip paths
      that do not exist (survivors union). **No `scenario` column** — keyed by
      partition coordinates.
- [ ] 3.2 In `ssd_design_targets()`, mint the top-level `summary` target: an
      `edge_block()` over the design's compact summary targets, then
      `ssd_summarise_design()` to `<root>/summary.parquet`, `format = "file"`.
- [ ] 3.3 Stub the read-side scenario→selection (membership) derivation; keep it
      **out** of storage; note the coordination with `cost-analysis-targets`.

## 4. Tests

- [ ] 4.1 Dedup: a two-member design whose members share `sample`/`fit`/`draw`
      content builds each shared shard exactly once (one target, one file) in one
      `tar_make()`.
- [ ] 4.2 Extend/grow without recompute: a completed standalone scenario re-run as
      a design member into the same root reports every shard cached and
      byte-identical; adding a member builds only its non-shared shards and the
      combined summary; removing a member leaves the survivors cached.
- [ ] 4.3 Combined summary: coordinate-keyed, no `scenario` column, an overlapping
      coordinate appears once; survivors-union on a missing coordinate.
- [ ] 4.4 Validation: derived/explicit names, design of one, empty-call abort.

## 5. Documentation

- [ ] 5.1 `GLOSSARY.md` *Design terms*: scenario/design as **selections** over one
      shared content-addressed tree (refine the original wording inherited from
      `scenario-combine`).
- [ ] 5.2 Roxygen for `ssd_design()`, `ssd_design_targets()` (the
      common-random-numbers-as-shared-shard note, names-are-labels, the
      extend-without-recompute contract), and `ssd_summarise_design()`; regenerate
      `NAMESPACE`/`man/`.
- [ ] 5.3 A design section in `vignettes/sharded-pipeline.qmd` + a `README.Rmd`
      pointer; `_pkgdown.yml`; `ROADMAP.md` (chain and move on archive).
- [ ] 5.4 Format with `air`, run `devtools::check()`, `openspec validate --strict`.
