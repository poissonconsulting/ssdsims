# package-readme Specification

## Purpose
TBD - created by archiving change readme. Update Purpose after archive.
## Requirements
### Requirement: Generated README provenance and sync
`README.md` SHALL be generated from `README.Rmd` and the two SHALL be kept in
sync. `README.Rmd` SHALL retain the `<!-- README.md is generated from
README.Rmd. Please edit that file -->` provenance comment, and `README.md`
SHALL be re-knit from `README.Rmd` whenever `README.Rmd` changes.

#### Scenario: README.md regenerated from README.Rmd
- **WHEN** `README.Rmd` is edited
- **THEN** `README.md` SHALL be re-rendered from `README.Rmd` (e.g. via
  `devtools::build_readme()`) so its prose, code, and printed output match the
  source

#### Scenario: Provenance comment retained
- **WHEN** a reader opens `README.Rmd` or `README.md`
- **THEN** the file SHALL carry the comment indicating `README.md` is generated
  from `README.Rmd` and that `README.Rmd` is the file to edit

### Requirement: Declarative-path framing
The README SHALL describe `ssdsims` on the declarative + `targets` path and
SHALL NOT present the legacy step functions. The overview SHALL describe
expanding a declarative scenario into per-step task tables, drawing data,
fitting distributions, and estimating hazard concentrations, run reproducibly in
parallel or on a cluster via a `targets`-based, Hive-partitioned shard pipeline.

#### Scenario: Overview leads with the declarative scenario and targets pipeline
- **WHEN** a reader reads the opening section of the README
- **THEN** the text SHALL describe the declarative scenario, task-table
  expansion, and the `targets`-based shard pipeline

#### Scenario: Legacy step functions absent
- **WHEN** the README's examples and narrative are read
- **THEN** they SHALL NOT use or reference `ssd_sim_data()`,
  `ssd_fit_dists_sims()`, `ssd_hc_sims()`, or `ssd_run_scenario()`

### Requirement: Installation instructions
The README SHALL include an **Installation** section giving a working
instruction to install the package from GitHub, and SHALL NOT label a usage
example as installation.

#### Scenario: Installation section shows a GitHub install
- **WHEN** a reader looks for how to install `ssdsims`
- **THEN** the README SHALL provide a code block installing the package from
  `poissonconsulting/ssdsims` (e.g. via `pak` or `remotes`)

### Requirement: Runnable declarative quick-start
The README SHALL include at least one minimal, evaluated usage example built on
the declarative path (`ssd_scenario_data()` → `ssd_define_scenario()` →
`ssd_run_scenario_baseline()`), written so it executes when `README.Rmd` is knit
and its printed output appears in `README.md`.

#### Scenario: Quick-start example evaluates and prints output
- **WHEN** `README.Rmd` is knit
- **THEN** the quick-start example SHALL run without error using only exported
  declarative/targets functions and available packages, and its printed output
  SHALL be captured in `README.md`

### Requirement: Capability map and get-started link
The README SHALL include a brief map of the package's capability areas
(scenarios and task expansion, the `targets` pipeline, designs that combine
scenarios, cloud upload, cost estimation/analysis, and reproducible RNG) and
SHALL link to the get-started overview `vignette("ssdsims")`.

#### Scenario: Capability map links to articles and the overview
- **WHEN** a reader finishes the overview
- **THEN** the README SHALL link to `vignette("ssdsims")` and to the relevant
  articles (`defining-a-scenario`, `sharded-pipeline`, `scenario-to-design`,
  `cluster-pipeline`, `cloud-upload`, `cost-estimation`, `cost-analysis`) so the
  reader knows where to go next

