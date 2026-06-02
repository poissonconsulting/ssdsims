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
- **Error origin**: Raise errors in the context of the *user-facing*
  function. Thread the public function’s frame down to validators
  (`chk::abort_chk(..., call = call)` with `call = environment()`
  captured in the exported function) and avoid leaking internal frames
  like [`purrr::map()`](https://purrr.tidyverse.org/reference/map.html)
  / [`lapply()`](https://rdrr.io/r/base/lapply.html) / private helpers
  into the `Error in ...:` header (loop instead of
  [`purrr::walk`](https://purrr.tidyverse.org/reference/map.html)/`chk_all`
  where they would surface). A package-wide pass to enforce this is
  tracked as the `error-call-origin` roadmap item (TARGETS-DESIGN.md
  §12).
- **Minimal diffs**: touch only the lines your change requires; don’t
  reformat unrelated lines or let editors rewrite whitespace. Leave
  `DESCRIPTION` formatting to `usethis`/`desc` (e.g. keep the trailing
  space after field names like `Imports:`).
- **Orthography**: UK/AU (Commonwealth) English — prefer
  `-ise`/`-isation` (`initialise`, `serialisable`) over `-ize`, and
  `-our` (`behaviour`, `colour`) over `-or`. This matches the package’s
  R source and OpenSpec artifacts and the authors’ Australian/Canadian
  convention. Exceptions: code identifiers and base R/API names keep
  their canonical spelling (e.g. `color`, `center`, `RNGkind`), as do
  proper nouns (e.g. “Apache License”).

Example:

``` r

chk::chk_whole_number(seed)
chk::chk_range(nrow, c(5, 1000))
chk::chk_character(dataset_names)
```

### Coding rules

- Always run `air format .` after generating or editing code.
- Use the base pipe operator (`|>`), not the magrittr pipe (`%>%`).
- Use `\() ...` for single-line anonymous functions. For all other
  cases, use `function() {...}`.
- Permissions for the common tooling (`air`, `R`, `Rscript`, read-only
  `git`/`gh`, `quarto`, `Skill`) are pre-approved in
  `.claude/settings.json`, so these run without a prompt.

## Development Workflow

### Setup

``` r

# Install development dependencies
devtools::install_dev_deps()

# Load the package for interactive use
devtools::load_all()

# Run all tests
devtools::test()

# Run all tests for files starting with {name}
devtools::test(filter = "^{name}")

# Run all tests for R/{name}.R
devtools::test_active_file("R/{name}.R")

# Run a single test "blah" for R/{name}.R
devtools::test_active_file("R/{name}.R", desc = "blah")

# (Re)build documentation
devtools::document()

# Check that all topics are in the reference index
pkgdown::check_pkgdown()

# Full check
devtools::check()
```

> When invoking R from the shell, use `Rscript --no-environ -e "..."`.
> Without `--no-environ` the sandbox blocks reads of `~/.Renviron` and R
> fails to create its
> [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

Format code from the shell:

``` bash
air format .
```

### RNG Discipline

The package uses two RNG paths:

1.  **L’Ecuyer-CMRG** (legacy, will be removed) —
    [`with_lecuyer_cmrg_seed()`](https://poissonconsulting.github.io/ssdsims/reference/local_lecuyer_cmrg_seed.md),
    [`local_lecuyer_cmrg_state()`](https://poissonconsulting.github.io/ssdsims/reference/local_lecuyer_cmrg_state.md).
2.  **dqrng + hash** (new targets-based path) —
    `dqrng::dqset.seed(seed, stream)` with task-derived primers. The
    `pcg64` backend is scenario-scoped via
    [`local_dqrng_backend()`](https://poissonconsulting.github.io/ssdsims/reference/local_dqrng_backend.md),
    which is **reentrant**: a nested call is a no-op (detected via
    `RNGkind()[1] == "user-supplied"`), so only the outermost scope
    activates and resets the backend and the RNG stream is identical
    with or without nesting. See `R/dqrng-backend.R` and
    `openspec/changes/dqrng-init/design.md`.

When touching RNG-consuming code: - Do **not** assume a fixed global
`.Random.seed` across function calls. - Use
[`local_lecuyer_cmrg_seed()`](https://poissonconsulting.github.io/ssdsims/reference/local_lecuyer_cmrg_seed.md)
/
[`local_lecuyer_cmrg_state()`](https://poissonconsulting.github.io/ssdsims/reference/local_lecuyer_cmrg_state.md)
/
[`local_dqrng_backend()`](https://poissonconsulting.github.io/ssdsims/reference/local_dqrng_backend.md)
/
[`withr::local_seed()`](https://withr.r-lib.org/reference/with_seed.html)
to scope RNG changes. - On exit, RNG state **must** be restored (use
[`on.exit()`](https://rdrr.io/r/base/on.exit.html) or withr scoping
helpers). - Test for `.Random.seed` being unchanged before and after
(see `tests/testthat/test-lecuyer-cmrg-seed.R` for examples).

### Testing

Test-suite conventions live in **`tests/testthat/CLAUDE.md`** —
file/naming rules, error/warning and snapshot style, and RNG seeding.
Read that before writing tests.

### Documentation

- Functions are documented inline with roxygen comments (`#' @param`,
  `#' @return`, etc.); wrap roxygen comments at 80 characters.
- Every user-facing function should be exported and documented; internal
  functions should not have roxygen documentation.
- Run `devtools::document()` to generate `man/` pages and update
  `NAMESPACE` — always re-document after changing a roxygen comment, and
  never edit `man/` or `NAMESPACE` by hand.
- Whenever you add a new (non-internal) topic, add it to `_pkgdown.yml`
  and confirm with
  [`pkgdown::check_pkgdown()`](https://pkgdown.r-lib.org/reference/check_pkgdown.html).
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

See **`tests/testthat/CLAUDE.md`** for the full test-suite conventions
(file/naming, expectations, snapshots, RNG seeding).

### Update package version and news

`NEWS.md` and the dev version are managed by
[fledge](https://fledge.cynkra.com) — **do not hand-edit `NEWS.md`**
(its header says as much). Entries are generated from commit messages
and grouped by Conventional Commit type (Features, Bug fixes, Chore,
Refactoring), so the way to shape a changelog entry is to write a clear,
conventionally-typed commit message (see Pull Requests below).

- `fledge::bump_version()` bumps the `DESCRIPTION` version and assembles
  `NEWS.md` from the commits since the last bump; a scheduled GitHub
  Action (`.github/workflows/fledge.yaml`) also runs this on `main`.
- Reference the related issue in the commit message (e.g. `(#64)`) so it
  carries through to the changelog.

> **Deviation from tidyverse/r-lib**: those packages have contributors
> hand-add a `NEWS.md` bullet (and order bullets alphabetically by
> function). This package does **not** — fledge derives the changelog
> from commits, so skip the manual bullet entirely and put the effort
> into the commit message instead.

## Pull Requests

- **Titles follow Conventional Commits**: `<type>: <summary>`, where
  `type` is one of `feat`, `fix`, `docs`, `refactor`, `test`, `chore`,
  etc. (e.g. `feat: add scenario-scoped dqrng pcg64 RNG backend`). Use
  the imperative mood and keep the summary concise.
- **Escape function, object, and file names in backticks** in PR titles
  and descriptions
  (e.g. [`local_dqrng_backend()`](https://poissonconsulting.github.io/ssdsims/reference/local_dqrng_backend.md),
  `run_scenario()`, `DESCRIPTION`).
- Keep the PR title and description in sync with the change as it
  evolves; the description should capture the *current* state, not a
  revision log.

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

- **Scenario object**
  ([`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md))
  — Declarative, construction-time, contains only `seed`, knobs, dataset
  names, and arg grids.
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
