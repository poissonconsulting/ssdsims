## 1. Rewrite README.Rmd (declarative path)

> Note: `#196` already moved the README quick-start onto the declarative path
> (`ssd_define_scenario()` + `ssd_run_scenario_baseline()`) and removed the
> legacy `_pkgdown.yml` group, so this group is now refinement, not a from-scratch
> rewrite — the example still sits under a mislabelled `## Installation` heading
> with no real install, overview, capability map, or `vignette("ssdsims")` link.

- [ ] 1.1 Keep the YAML header, the `<!-- README.md is generated from README.Rmd -->` provenance comment, the `knitr::opts_chunk$set(...)` setup chunk, the `# ssdsims` title, and the badges block.
- [ ] 1.2 Rewrite the overview to describe the declarative scenario, expansion into per-step `sample`/`fit`/`hc` task tables, and the `targets`-based, Hive-partitioned shard pipeline (consistent with `DESCRIPTION`).
- [ ] 1.3 Add a real `## Installation` section (`pak::pak("poissonconsulting/ssdsims")` plus a `remotes::install_github(...)` alternative), removing the example mislabelled as "Installation".
- [ ] 1.4 Add a `## Usage` quick-start built only on the declarative path (`ssd_scenario_data()` → `ssd_define_scenario()` → `ssd_run_scenario_baseline()`), evaluated so output is captured. Do NOT use `ssd_sim_data()` / `ssd_fit_dists_sims()` / `ssd_hc_sims()` / `ssd_run_scenario()`.
- [ ] 1.5 Add a capability map (scenarios & task expansion; targets pipeline; designs; cloud upload; cost estimation/analysis; reproducible RNG) and a prominent link to `vignette("ssdsims")`.
- [ ] 1.6 Use Commonwealth (UK/AU) English; keep the diff minimal.

## 2. Create the get-started overview vignette

- [ ] 2.1 Add `vignettes/ssdsims.qmd` (`vignette("ssdsims")`): problem statement, a labelled map of the two tracks (build-and-run pipeline vs. predict-and-measure cost), an explicit recommended reading order, and a short declarative on-ramp.
- [ ] 2.2 Lift the strongest framing from the review: `defining-a-scenario`'s "why declarative" + collection-input model, and `sharded-pipeline`'s "two drivers, one core, byte-identical" mental model — trimmed to a single dataset/minimal scenario, pointing onward rather than duplicating.
- [ ] 2.3 Add `ssdsims` as the FIRST entry in the `_pkgdown.yml` `articles:` list.

## 3. Convert cross-links to vignette()-style (package-wide)

- [ ] 3.1 Convert every vignette-to-vignette link in all seven existing vignettes (and the new overview and README) from `.html`/absolute URLs to `vignette("name")`-style links.
- [ ] 3.2 Point function-name links at the function reference, not at article pages (fixes the `cost-analysis` links to `ssd_estimate_cost()` / `ssd_calibrate_cost()`).

## 4. Apply the per-vignette review fixes

- [ ] 4.1 `cost-estimation`: add a "See also" linking forward to `cost-analysis` and back to `defining-a-scenario` (end its dead-end); fix the 430-hour benchmark attribution (it lives in the archived exploration sweep, not `ssd_define_scenario()`'s docs); reconcile the 44 vs 45.3-minute figure.
- [ ] 4.2 Remove the OpenSpec-internal "the change's `exploration/` directory" phrasing from the vignette and from `R/cost-estimate.R` roxygen (then re-document so `man/` updates).
- [ ] 4.3 `scenario-to-design`: add a "See also" section; add a forward link from `defining-a-scenario` to `scenario-to-design`.
- [ ] 4.4 `sharded-pipeline`: fix "four-file" → the template's actual file set; make the fit-shard arithmetic include the `dataset` path axis (`1 × 2 × 2 × 2 = 8`).
- [ ] 4.5 `cluster-pipeline`: soften "asserts byte-identical" to reflect the printed `all.equal` check on the `est` column in `large/run-serial.R`.
- [ ] 4.6 `cloud-upload`: remove the stray trailing code-fence artifact.
- [ ] 4.7 Sweep for any remaining reader-facing repo-internal references (`TARGETS-DESIGN.md §`, `data-raw/...`) and either resolve to a public anchor or drop.
- [ ] 4.8 Standardise the heading spine / add closing navigation so the seven vignettes read as one set.

## 5. Regenerate and verify

- [ ] 5.1 Re-knit `README.md` from `README.Rmd` with `devtools::build_readme()`.
- [ ] 5.2 Render the pkgdown site and confirm `vignette("ssdsims")` is the Get-started entry and every `vignette()`/reference link resolves (`pkgdown::check_pkgdown()`).
- [ ] 5.3 Confirm all evaluated chunks run without error and printed output is captured; verify no example references a legacy or non-exported function.
- [ ] 5.4 Run `air format` on any touched `R/` (the `cost-estimate.R` roxygen edit) and review the full diff before committing.
