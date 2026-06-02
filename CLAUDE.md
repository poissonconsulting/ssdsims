# ssdsims Development Guide

Overview and conventions for developing ssdsims, an R package for
Species Sensitivity Distribution (SSD) simulation studies.

## Project Structure

    ssdsims/
    ├── R/                    # Package source code
    ├── tests/testthat/       # Test suite
    ├── man/                  # Generated roxygen documentation
    ├── DESCRIPTION           # Package metadata
    ├── NAMESPACE             # S3 methods, exports
    ├── NEWS.md              # Changelog (managed by fledge)
    ├── openspec/            # OpenSpec specs and changes (development planning)
    ├── scripts/             # Validation & exploration scripts
    ├── .github/workflows/   # CI/CD pipelines
    └── TARGETS-DESIGN.md    # Design for targets-based refactor (READ THIS)

## Key Design Documents

- **TARGETS-DESIGN.md** — Forward-looking design for the cluster-based
  targets pipeline. Covers the scenario object, dqrng/hash RNG
  mechanism, task shards, and extension patterns. This is the north star
  for the roadmap.
- **RNG-FLOW.md** — RNG design rationale and gaps closed by
  TARGETS-DESIGN.
- **GLOSSARY.md** — Terminology (seed vs. state vs. primer vs. stream).

Read these before major implementation work.

## Code Style & Format

- **Style**: Tidyverse style guide. Use `air` to format before
  committing (configured in `air.toml`).
- **Linting**: Run `air format` to apply formatters.
- **Type checks**: No explicit type hints; rely on `chk::*()` for
  runtime validation in function bodies.
- **Validation**: Use `chk` for all input validation; keep error
  messages informative and actionable.

Example:

``` r

chk::chk_whole_number(seed)
chk::chk_range(nrow, c(5, 1000))
chk::chk_character(dataset_names)
```

## Development Workflow

### Setup

``` bash
# Install development dependencies
devtools::install_dev_deps()

# Load the package for interactive use
devtools::load_all()

# Run tests
devtools::test()

# Build documentation
devtools::document()

# Full check
devtools::check()
```

### RNG Discipline

The package uses two RNG paths:

1.  **L’Ecuyer-CMRG** (legacy, will be removed) —
    [`with_lecuyer_cmrg_seed()`](https://poissonconsulting.github.io/ssdsims/reference/with_lecuyer_cmrg_seed.md),
    [`local_lecuyer_cmrg_state()`](https://poissonconsulting.github.io/ssdsims/reference/local_lecuyer_cmrg_state.md).
2.  **dqrng + hash** (new targets-based path) —
    `dqrng::dqset.seed(seed, stream)` with task-derived primers.

When touching RNG-consuming code: - Do **not** assume a fixed global
`.Random.seed` across function calls. - Use
[`local_lecuyer_cmrg_seed()`](https://poissonconsulting.github.io/ssdsims/reference/local_lecuyer_cmrg_seed.md)
/
[`local_lecuyer_cmrg_state()`](https://poissonconsulting.github.io/ssdsims/reference/local_lecuyer_cmrg_state.md)
/
[`withr::local_seed()`](https://withr.r-lib.org/reference/with_seed.html)
to scope RNG changes. - On exit, RNG state **must** be restored (use
[`on.exit()`](https://rdrr.io/r/base/on.exit.html) or withr scoping
helpers). - Test for `.Random.seed` being unchanged before and after
(see `tests/testthat/test-lecuyer-cmrg-seed.R` for examples).

### Testing

- Write snapshot tests for any output that should be stable (use
  [`testthat::expect_snapshot()`](https://testthat.r-lib.org/reference/expect_snapshot.html)).
- Tests live in `tests/testthat/test-*.R`; run with `devtools::test()`.
- Snapshots update with
  [`testthat::snapshot_review()`](https://testthat.r-lib.org/reference/snapshot_accept.html).
- RNG-touching tests must pin the seed explicitly.

### Documentation

- Functions are documented inline with roxygen comments (`#' @param`,
  `#' @return`, etc.).
- Run `devtools::document()` to generate `man/` pages and update
  `NAMESPACE`.
- Examples in `@examples` are part of the spec — keep them small and
  runnable.

## OpenSpec Workflow

The package uses OpenSpec for spec-driven development (see `.claude/`
and `.github/` directories).

### Commands

- **`/opsx:propose <idea>`** — Create a new change (proposal + design +
  specs + tasks).
- **`/opsx:explore <question>`** — Think through a problem or design
  decision before committing.
- **`/opsx:apply <change-name>`** — Implement tasks from a change.
- **`/opsx:archive <change-name>`** — Finalize a completed change.

Changes live in `openspec/changes/<name>/` with: - `proposal.md` — What
and why. - `design.md` — How. - `specs/<capability>/spec.md` — Detailed
requirements (WHEN/THEN scenarios). - `tasks.md` — Implementation
checklist.

### Capability Specs

The package’s current capabilities are documented in
`openspec/specs/`: - `simulate-data` —
[`ssd_sim_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_sim_data.md)
and its 5 S3 methods. - `fit-distributions` —
[`ssd_fit_dists_sims()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_fit_dists_sims.md)
and factorial expansion. - `hazard-concentrations` —
[`ssd_hc_sims()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_hc_sims.md)
with bootstrap CI options. - `run-scenario` —
[`ssd_run_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario.md)
pipeline. - `parallel-safe-seeding` — L’Ecuyer-CMRG and dqrng seed/state
helpers.

New capabilities land via OpenSpec changes (e.g., `ssd-define-scenario`,
`dqrng-init`); existing specs guide their implementation.

## Key Dependencies

- **tidyverse/dplyr** — Data wrangling.
- **ssdtools** — Core SSD fitting and HC estimation (vendored logic
  wraps this).
- **chk** — Input validation.
- **withr** — Scoped side-effects (RNG, options).
- **rlang** — Hashing (task primers in new design), quoting.
- **dqrng** — New RNG backend (targets path).
- **parallel** — L’Ecuyer-CMRG sub-streams (legacy path).

See `DESCRIPTION` for versions and imports.

## Common Tasks

### Add a new exported function

1.  Write the function in `R/` with roxygen comments.
2.  Mark `@export` in the docstring.
3.  Run `devtools::document()` to update `NAMESPACE`.
4.  Add tests in `tests/testthat/test-*.R`.
5.  Run `devtools::check()` and `air format`.

### Modify an existing function’s arguments

1.  Update the function body and roxygen `@param` / `@return`.
2.  Update all tests that call the function.
3.  Run `devtools::document()`.
4.  Check if snapshots need updating (run tests, review diffs, accept or
    reject).
5.  Commit with a clear message.

### Add a new test file

- Create `tests/testthat/test-<feature>.R`.
- Prefix all tests with the feature name:
  `test_that("feature: description", { ... })`.
- Use `testthat::expect_*()` for assertions.
- RNG-sensitive tests: pin the seed with
  [`withr::with_seed()`](https://withr.r-lib.org/reference/with_seed.html)
  or
  [`local_lecuyer_cmrg_seed()`](https://poissonconsulting.github.io/ssdsims/reference/local_lecuyer_cmrg_seed.md).

### Update package version and news

- Edit `DESCRIPTION` version field (e.g., `0.0.0.9010` → `0.0.0.9011`).
- Add a note to `NEWS.md` (or use fledge: `fledge::bump_version()` to
  automate).
- Commit with a note referencing the change.

## Continuous Integration

GitHub Actions runs on each push: - **R-CMD-check** —
`devtools::check()` on multiple R versions. - **Snapshot updates** —
Auto-PR for snapshot diffs. - **Coverage** — Code coverage tracking
(optional).

See `.github/workflows/` for pipeline configs.

## Architectural Notes

### The targets redesign

The package is transitioning from immediate
[`ssd_run_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario.md)
execution to a cluster-based targets pipeline. Key shifts:

- **Scenario object** (`ssd_define_scenario()`) — Declarative,
  construction-time, contains only `seed`, knobs, dataset names, and arg
  grids.
- **Per-task RNG** — Each task gets a primer derived from
  `rlang::hash(task_params)`, seeded via
  `dqrng::dqset.seed(seed, stream = primer)`.
- **Shards** — Parquet files grouped by `partition_by` axes, stored in a
  Hive-partitioned directory tree.
- **Static branching** (default) — `tar_map()` mints one named target
  per shard at sourcing time.
- **Dynamic branching** (escape hatch) — For extreme fan-outs, task
  tables can be computed inside targets instead.

The roadmap (TARGETS-DESIGN.md §12) lands features in dependency order,
starting from `ssd-define-scenario` and `dqrng-init` (no dependencies).
Each step is a coherent working state; parallel work streams are
encouraged.

## Contact & Contribution

- **Issues & PRs**: GitHub (poissonconsulting/ssdsims).
- **Style questions**: Refer to [tidyverse style
  guide](https://style.tidyverse.org/) and existing code.
- **Design discussions**: Use `/opsx:explore` for thinking through major
  changes before proposing them.
