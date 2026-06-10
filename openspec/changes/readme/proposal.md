## Why

The `README.Rmd`/`README.md` is stale and undersells the package. Its one-line
goal ("facilitate simulation studies with species sensitivity distribution
data") and single fused code block only exercise the immediate, in-memory
pipeline (`ssd_sim_data()` → `ssd_fit_dists_sims()` → `ssd_hc_sims()` and the
`ssd_run_scenario()` shortcut). It never mentions the package's headline
capability — the declarative scenario that expands into per-step task tables and
runs as a `targets`-based, Hive-partitioned shard pipeline on a cluster — nor
combining scenarios into a design, cost estimation/analysis, cloud upload, or
the reproducible per-task RNG. A reader landing on GitHub or the pkgdown home
page cannot tell what `ssdsims` is for or where to start.

## What Changes

- Rewrite the package overview to state what `ssdsims` does in the terms the
  `DESCRIPTION` and pkgdown reference already use: expand a declarative
  scenario into per-step task tables, draw data, fit distributions, estimate
  hazard concentrations, and run reproducibly in parallel or on a cluster via a
  `targets`-based, Hive-partitioned shard pipeline.
- Add an **Installation** section with the actual install instruction
  (`pak::pak("poissonconsulting/ssdsims")` / `remotes`), replacing the current
  block that mislabels a usage example as "Installation".
- Restructure the usage examples into a short "quick start" (the immediate
  pipeline) plus a declarative-scenario teaser (`ssd_scenario_data()` /
  `ssd_define_scenario()` / `ssd_scenario_tasks()`), each kept minimal and
  evaluated so `README.md` regenerates from `README.Rmd`.
- Add a brief map of the package's capability areas (immediate pipeline,
  scenarios & task expansion, the `targets` pipeline, designs that combine
  scenarios, cloud upload, cost estimation/analysis, and reproducible RNG) that
  links to the corresponding pkgdown articles (`defining-a-scenario`,
  `sharded-pipeline`, `scenario-to-design`, `cluster-pipeline`, `cloud-upload`,
  `cost-estimation`, `cost-analysis`).
- Keep the badges block and the "generated from README.Rmd" provenance comment;
  re-knit `README.md` from `README.Rmd` so the two stay in sync.

## Capabilities

### New Capabilities
- `package-readme`: What the rendered `README.md` (generated from
  `README.Rmd`) must contain and keep in sync — the overview framing,
  installation instructions, runnable quick-start example, the capability map
  linking to articles, and the build/sync contract between `README.Rmd` and
  `README.md`.

### Modified Capabilities
<!-- None. This is a documentation change; no code requirement (behaviour) of an
     existing capability changes. -->

## Impact

- Files: `README.Rmd` (edited), `README.md` (regenerated), possibly
  `man/figures/` if any figure is added (none planned).
- No R source, exported API, tests, or dependencies change. The example code in
  the README uses only already-exported functions (`ssd_sim_data()`,
  `ssd_fit_dists_sims()`, `ssd_hc_sims()`, `ssd_run_scenario()`,
  `ssd_scenario_data()`, `ssd_define_scenario()`, `ssd_scenario_tasks()`) and
  `Suggests` packages (`ssddata`, `withr`) already used by the current README.
- pkgdown home page (rendered from `README.md`) improves; no `_pkgdown.yml`
  change required.
