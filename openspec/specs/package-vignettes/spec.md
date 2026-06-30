# package-vignettes Specification

## Purpose
TBD - created by archiving change readme. Update Purpose after archive.
## Requirements
### Requirement: Get-started overview vignette
The package SHALL ship a get-started overview vignette `vignette("ssdsims")`
(`vignettes/ssdsims.qmd`) that serves as the documentation entry point. It SHALL
state the problem the package solves, map the two documentation tracks
(build-and-run pipeline vs. predict-and-measure cost), give a recommended
reading order, and provide a short declarative on-ramp. It SHALL appear first in
the `_pkgdown.yml` `articles:` list.

#### Scenario: Overview exists and leads
- **WHEN** a reader opens the pkgdown site or runs `vignette("ssdsims")`
- **THEN** they SHALL reach an overview that names the problem, maps the two
  tracks, and states a recommended reading order before any single-topic
  vignette

#### Scenario: Overview on-ramp uses the declarative path
- **WHEN** the overview shows runnable code
- **THEN** it SHALL use the declarative path
  (`ssd_scenario_data()` / `ssd_define_scenario()` / a scenario runner) and SHALL
  NOT use the legacy step functions

### Requirement: vignette()-style cross-links
Cross-references SHALL use the `vignette("name")` call form for articles and the
`fn()` call form for functions. In the vignettes, these SHALL be written as bare
inline code (no surrounding `[...](...)` link) so pkgdown's downlit auto-links
them to the article / reference page; each reference SHALL be phrased as a clean
standalone call expression (e.g. `vignette("sharded-pipeline")`, `ssd_summarise()`)
so auto-linking applies. The README, which also renders on GitHub where
auto-linking does not occur, SHALL instead use explicit Markdown links to the
pkgdown article/reference URLs while keeping the `vignette("...")` / `fn()` text.

#### Scenario: Vignette cross-references are auto-linked
- **WHEN** one vignette refers a reader to another article or to a function
- **THEN** the reference SHALL be a bare inline `vignette("name")` or `fn()` code
  expression (not a hardcoded `.html`/absolute-URL link), relying on pkgdown
  auto-linking, and SHALL NOT use `?fn` (which does not auto-link)

#### Scenario: README links are explicit
- **WHEN** the README references an article or function
- **THEN** it SHALL use an explicit Markdown link to the pkgdown URL (so it is
  clickable on GitHub), with the `vignette("...")` / `fn()` call as the link text

### Requirement: Navigable, non-orphan vignette set
Every vignette SHALL carry a "See also" / navigation section linking the
relevant sibling vignettes, with no orphan or dead-end vignette. The two cost
vignettes SHALL link to each other.

#### Scenario: Every vignette links onward
- **WHEN** a reader reaches the end of any vignette
- **THEN** that vignette SHALL link to at least one relevant sibling

#### Scenario: Cost vignettes are mutually linked
- **WHEN** a reader is in `cost-estimation` or `cost-analysis`
- **THEN** each SHALL link to the other so the predict-vs-measure pair is
  discoverable from either side

### Requirement: User-facing docs free of repo-internal references
Vignettes and the rendered reference SHALL NOT direct readers to OpenSpec or
repository-internal locations that an installed-package reader cannot resolve
(e.g. "the change's `exploration/` directory", `openspec/...` paths), and SHALL
NOT misattribute the source of figures or benchmarks.

#### Scenario: No OpenSpec-internal pointers in user docs
- **WHEN** a vignette or roxygen-rendered reference page is read
- **THEN** it SHALL NOT point the reader to OpenSpec change directories or other
  paths unavailable from an installed package

#### Scenario: Benchmark provenance is accurate
- **WHEN** a vignette cites a benchmark or example figure
- **THEN** the cited source SHALL actually contain that figure

