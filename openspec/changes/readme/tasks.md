## 1. Rewrite README.Rmd

- [ ] 1.1 Keep the YAML header, the `<!-- README.md is generated from README.Rmd -->` provenance comment, the `knitr::opts_chunk$set(...)` setup chunk, the `# ssdsims` title, and the badges block unchanged.
- [ ] 1.2 Rewrite the overview paragraph to describe the declarative scenario, expansion into per-step `sample`/`fit`/`hc` task tables, and the `targets`-based, Hive-partitioned shard pipeline for parallel/cluster runs (consistent with `DESCRIPTION`), not just the immediate in-memory pipeline.
- [ ] 1.3 Add a real `## Installation` section with a GitHub install (`pak::pak("poissonconsulting/ssdsims")` plus a `remotes::install_github(...)` alternative), removing the current example mislabelled as "Installation".
- [ ] 1.4 Add a `## Usage` (quick start) section: keep a minimal, `withr::with_seed(42, ...)`-wrapped immediate-pipeline example (`ssd_sim_data()` → `ssd_fit_dists_sims()` → `ssd_hc_sims()`) and the `ssd_run_scenario()` one-liner, evaluated so output is captured.
- [ ] 1.5 Add a short declarative-scenario teaser using `ssd_data()` / `ssd_define_scenario()` / `ssd_scenario_tasks()`; evaluate it if it stays fast and deterministic, otherwise set `eval = FALSE` and rely on the article link.
- [ ] 1.6 Add a compact capability map (immediate pipeline; scenarios & task expansion; targets pipeline; cloud upload; cost estimation; reproducible RNG) linking to the pkgdown articles `defining-a-scenario`, `sharded-pipeline`, `cluster-pipeline`, `cloud-upload`, and `cost-estimation`.
- [ ] 1.7 Use Commonwealth (UK/AU) English in prose per `AGENTS.md`; keep the diff minimal.

## 2. Regenerate and verify

- [ ] 2.1 Re-knit `README.md` from `README.Rmd` with `devtools::build_readme()`.
- [ ] 2.2 Confirm every code chunk ran without error and that printed output appears in `README.md`; check the knit stays fast.
- [ ] 2.3 Verify all article links resolve to existing vignettes and that no example references a non-exported function.
- [ ] 2.4 Run `air format` if any R is touched outside the README chunks (none expected); review the rendered `README.md` and `README.Rmd` diff before committing.
