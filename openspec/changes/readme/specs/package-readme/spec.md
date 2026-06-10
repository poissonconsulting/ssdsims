## ADDED Requirements

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

### Requirement: Package overview framing
The README SHALL open with an overview that describes what `ssdsims` does in
terms consistent with the package `DESCRIPTION`: expanding a declarative
scenario into per-step task tables, drawing data, fitting distributions, and
estimating hazard concentrations, run reproducibly in parallel or on a cluster
via a `targets`-based, Hive-partitioned shard pipeline.

#### Scenario: Overview states the declarative scenario and targets pipeline
- **WHEN** a reader reads the opening section of the README
- **THEN** the text SHALL mention the declarative scenario, expansion into
  per-step task tables, and the `targets`-based shard pipeline for parallel or
  cluster runs — not only the immediate in-memory pipeline

### Requirement: Installation instructions
The README SHALL include an **Installation** section giving a working
instruction to install the package from GitHub, and SHALL NOT label a usage
example as installation.

#### Scenario: Installation section shows a GitHub install
- **WHEN** a reader looks for how to install `ssdsims`
- **THEN** the README SHALL provide a code block installing the package from
  `poissonconsulting/ssdsims` (e.g. via `pak` or `remotes`)

### Requirement: Runnable quick-start example
The README SHALL include at least one minimal, evaluated usage example
demonstrating the immediate pipeline, written so it executes when `README.Rmd`
is knit and its printed output appears in `README.md`.

#### Scenario: Quick-start example evaluates and prints output
- **WHEN** `README.Rmd` is knit
- **THEN** the quick-start example SHALL run without error using only exported
  functions and `Suggests`/imported packages, and its printed output SHALL be
  captured in `README.md`

### Requirement: Capability map linking to articles
The README SHALL include a brief map of the package's capability areas
(immediate pipeline, scenarios and task expansion, the `targets` pipeline,
cloud upload, cost estimation, and reproducible RNG) with links to the
corresponding pkgdown articles.

#### Scenario: Capability map links to pkgdown articles
- **WHEN** a reader finishes the overview
- **THEN** the README SHALL point to the relevant articles
  (`defining-a-scenario`, `sharded-pipeline`, `cluster-pipeline`,
  `cloud-upload`, `cost-estimation`) so the reader knows where to go next
