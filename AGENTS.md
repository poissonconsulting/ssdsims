# ssdsims Development Guide

Overview and conventions for developing ssdsims, an R package for Species Sensitivity Distribution (SSD) simulation studies.

## Project Structure

```
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
```

## Key Design Documents

- **TARGETS-DESIGN.md** — Principal design for the cluster-based targets pipeline. Covers the scenario object, dqrng/hash RNG mechanism, task shards, and extension patterns. Not intended for modification.
- **ROADMAP.md** — The actionable roadmap: the `Now`/`Next`/`Later`/`Bluesky` backlog and the `Done` shipped log (`initiative`-template style), keyed by OpenSpec `[change]` identifiers.
- **RNG-FLOW.md** — RNG design rationale and gaps closed by TARGETS-DESIGN.
- **GLOSSARY.md** — Terminology (seed vs. state vs. primer vs. stream).

Read these before major implementation work.

## Code Style & Format

- **Style**: Tidyverse style guide. Use `air` to format before committing (configured in `air.toml`).
- **Linting**: Run `air format` to apply formatters.
- **Type checks**: No explicit type hints; rely on `chk::*()` for runtime validation in function bodies.
- **Validation**: Use `chk` for all input validation; keep error messages informative and actionable.
- **Error origin**: Raise errors in the context of the *user-facing* function. Thread the public function's frame down to validators (`chk::abort_chk(..., call = call)` with `call = environment()` captured in the exported function) and avoid leaking internal frames like `purrr::map()` / `lapply()` / private helpers into the `Error in ...:` header (loop instead of `purrr::walk`/`chk_all` where they would surface). A package-wide pass to enforce this is tracked as the `error-call-origin` roadmap item (TARGETS-DESIGN.md §12).
- **Minimal diffs**: touch only the lines your change requires; don't reformat
  unrelated lines or let editors rewrite whitespace. Leave `DESCRIPTION`
  formatting to `usethis`/`desc` (e.g. keep the trailing space after field names
  like `Imports: `).
- **Orthography**: UK/AU (Commonwealth) English — prefer `-ise`/`-isation`
  (`initialise`, `serialisable`) over `-ize`, and `-our` (`behaviour`, `colour`)
  over `-or`. This matches the package's R source and OpenSpec artifacts and the
  authors' Australian/Canadian convention. Exceptions: code identifiers and base
  R/API names keep their canonical spelling (e.g. `color`, `center`,
  `RNGkind`), as do proper nouns (e.g. "Apache License").

Example:
```r
chk::chk_whole_number(seed)
chk::chk_range(nrow, c(5, 1000))
chk::chk_character(dataset_names)
```

### Coding rules

- Always run `air format .` after generating or editing code.
- Use the base pipe operator (`|>`), not the magrittr pipe (`%>%`).
- Use `\() ...` for single-line anonymous functions. For all other cases, use `function() {...}`.
- Prefer `purrr` functionals (`map()`, `map_chr()`, `map_int()`, `map2()`,
  `pmap()`, …) over the base `*apply`/`Map`/`mapply` family. (Exception: where
  the error-call-origin rule needs a plain `for` loop to keep `purrr` frames out
  of an error header — see Error origin above.)
- Prefer `rlang` helpers over their base equivalents: `rlang::set_names()` over
  `stats::setNames()`, `rlang::exec()` over `do.call()`, `rlang::env_get()` /
  `rlang::is_function()` over `get()` / `getExportedValue()` / `is.function()`.
- Reserve the `with_`/`local_` prefixes for RAII (withr-style) scope helpers; do
  not use them for ordinary transforms (e.g. name a column-adder `add_*()`).
- **Canonical argument order**: at every call site, pass arguments in signature
  order. In particular, implement `ssd_define_scenario()` calls in the exact order of the signature. A full package-wide sweep of remaining call sites for this and the
  other public constructors is a `ROADMAP.md` cleanup item.
- Permissions for the common tooling (`air`, `R`, `Rscript`, read-only `git`/`gh`,
  `quarto`, `Skill`) are pre-approved in `.claude/settings.json`, so these run
  without a prompt.

## Development Workflow

### Setup

```r
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

> When invoking R from the shell, use `Rscript --no-environ -e "..."`. Without
> `--no-environ` the sandbox blocks reads of `~/.Renviron` and R fails to create
> its `tempdir()`.

Format code from the shell:

```bash
air format .
```

### RNG Discipline

The package uses two RNG paths:

1. **L'Ecuyer-CMRG** (legacy, will be removed) — `with_lecuyer_cmrg_seed()`, `local_lecuyer_cmrg_state()`.
2. **dqrng + hash** (new targets-based path) — `dqrng::dqset.seed(seed, stream)` with task-derived primers. The `pcg64` backend is scenario-scoped via `local_dqrng_backend()`, which is **reentrant**: a nested call is a no-op (detected via `RNGkind()[1] == "user-supplied"`), so only the outermost scope activates and resets the backend and the RNG stream is identical with or without nesting. See `R/dqrng-backend.R` and `openspec/changes/dqrng-init/design.md`.

When touching RNG-consuming code:
- Do **not** assume a fixed global `.Random.seed` across function calls.
- Use `local_lecuyer_cmrg_seed()` / `local_lecuyer_cmrg_state()` / `local_dqrng_backend()` / `withr::local_seed()` to scope RNG changes.
- On exit, RNG state **must** be restored (use `on.exit()` or withr scoping helpers).
- Test for `.Random.seed` being unchanged before and after (see `tests/testthat/test-lecuyer-cmrg-seed.R` for examples).

### Testing

Test-suite conventions live in **`tests/testthat/AGENTS.md`** — file/naming
rules, error/warning and snapshot style, and RNG seeding. Read that before
writing tests.

### Documentation

- Functions are documented inline with roxygen comments (`#' @param`, `#' @return`, etc.); wrap roxygen comments at 80 characters.
- Prefer `#' @inheritParams` over copying argument descriptions.
- Every user-facing function should be exported and documented; internal functions should not have roxygen documentation.
- Run `devtools::document()` to generate `man/` pages and update `NAMESPACE` — always re-document after changing a roxygen comment, and never edit `man/` or `NAMESPACE` by hand.
- Whenever you add a new (non-internal) topic, add it to `_pkgdown.yml` and confirm with `pkgdown::check_pkgdown()`.
- Examples in `@examples` are part of the spec — keep them small and runnable.

## OpenSpec Workflow

The package uses OpenSpec for spec-driven development (see `.claude/` and `.github/` directories). **`openspec/AGENTS.md`** is the focused subdirectory guide — read it before running any OpenSpec skill; it spells out the per-action roadmap obligation (below) that the generic skills do not enforce.

### Commands

- **`/opsx:propose <idea>`** — Create a new change (proposal + design + specs + tasks).
- **`/opsx:explore <question>`** — Think through a problem or design decision before committing.
- **`/opsx:apply <change-name>`** — Implement tasks from a change.
- **`/opsx:archive <change-name>`** — Finalize a completed change.

Active changes live in `openspec/changes/<name>/`; the current capability specs
live in `openspec/specs/` — that directory is the authoritative list.

## Key Dependencies

- **tidyverse/dplyr** — Data wrangling.
- **ssdtools** — Core SSD fitting and HC estimation (vendored logic wraps this).
- **chk** — Input validation.
- **withr** — Scoped side-effects (RNG, options).
- **rlang** — Hashing (task primers in new design), quoting.
- **dqrng** — New RNG backend (targets path).
- **parallel** — L'Ecuyer-CMRG sub-streams (legacy path).
- **duckplyr** — Parquet I/O and off-cluster querying (targets path). **Interact with Parquet files through `duckplyr` (DuckDB)** — `duckplyr::read_parquet_duckdb()` for reads, `duckplyr::compute_parquet()` for writes — **not `arrow`**. This is the team preference; confine the call sites behind the `ssd_read_parquet()` / `ssd_write_parquet()` internals.

See `DESCRIPTION` for versions and imports.

## Common Tasks

- **Add / change a function**: edit the roxygen alongside it, `devtools::document()`,
  update or add tests (and review any snapshot diffs), then `air format .` and
  `devtools::check()`. New non-internal topics also go in `_pkgdown.yml` (see
  *Documentation*); test conventions are in **`tests/testthat/AGENTS.md`**.
- **Version & news**: `NEWS.md` and the dev version are managed by
  [fledge](https://fledge.cynkra.com) (`fledge::bump_version()`, plus a scheduled
  Action on `main`) — **do not hand-edit `NEWS.md`**. The changelog is derived
  from commit messages grouped by Conventional Commit type, so shape entries via
  clear, conventionally-typed messages (see *Pull Requests*), not manual bullets.

## Pull Requests

> **PR titles MUST be valid Conventional Commits** — `<type>(<scope>):
> <summary>`. The canonical rule, examples, and rationale (squash-merge → the
> title is the lone commit on `main` that `fledge` turns into `NEWS.md`) live in
> the *Pull Requests* section of `CLAUDE.md`.

- **Always update PR title and description** when opening the PR and after committing.
  The description must capture the
change's *current* state, not a revision/process log.
- **Escape function, object, and file names in backticks** in PR titles and
  descriptions (e.g. `local_dqrng_backend()`, `run_scenario()`, `DESCRIPTION`).
- **Reference the related issue** in the title where one exists (e.g. `(#64)`)
  so it carries through to the changelog.
- Keep the PR title and description in sync with the change as it evolves; the
  description should capture the *current* state, not a revision log.
- **Review threads**: when you address a review comment, reply on the thread
  with what changed (or the answer) and leave it open — the reviewer resolves
  their own threads.

## Continuous Integration

GitHub Actions runs on each push:
- **R-CMD-check** — `devtools::check()` on multiple R versions.
- **Snapshot updates** — Auto-PR for snapshot diffs.
- **Coverage** — Code coverage tracking (optional).

See `.github/workflows/` for pipeline configs.

## Architectural Notes

### The targets redesign

The package is transitioning from immediate `ssd_run_scenario()` execution to a cluster-based targets pipeline. Key shifts:

- **Scenario object** (`ssd_define_scenario()`) — Declarative, construction-time, contains only `seed`, knobs, dataset names, and arg grids.
- **Per-task RNG** — Each task gets a primer derived from `rlang::hash(task_params)`, seeded via `dqrng::dqset.seed(seed, stream = primer)`.
- **Shards** — Parquet files grouped by `partition_by` axes, stored in a Hive-partitioned directory tree.
- **Static branching** (default) — `tar_map()` mints one named target per shard at sourcing time.
- **Dynamic branching** (escape hatch) — For extreme fan-outs, task tables can be computed inside targets instead.

The roadmap lands features in dependency order, starting from `ssd-define-scenario` and `dqrng-init` (no dependencies). Each step is a coherent working state; parallel work streams are encouraged. The forward-looking backlog lives in [`ROADMAP.md`](ROADMAP.md); `TARGETS-DESIGN.md` §12 keeps the dependency DAG and the landed-step (`### Archived`) record. **Keep both current as part of each change** — see `openspec/AGENTS.md` for the node-colour and `archived_box` rules.

## Contact & Contribution

- **Issues & PRs**: GitHub (poissonconsulting/ssdsims).
- **Style questions**: Refer to [tidyverse style guide](https://style.tidyverse.org/) and existing code.
- **Design discussions**: Use `/opsx:explore` for thinking through major changes before proposing them.
