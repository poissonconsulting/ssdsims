## Context

The README is the first thing a visitor sees on GitHub and the body of the
pkgdown home page (rendered from `README.md`). The current `README.Rmd` is
~40 lines: badges, a one-line goal, and a single "Installation" block that is
actually a usage example chaining `ssd_sim_data()` → `ssd_fit_dists_sims()` →
`ssd_hc_sims()` plus an `ssd_run_scenario()` one-liner. It predates the
declarative-scenario + `targets` shard pipeline that is now the package's
centre of gravity (see `DESCRIPTION`, `_pkgdown.yml`, `TARGETS-DESIGN.md`, and
the five vignettes). The build contract is the standard `usethis` one:
`README.md` is generated from `README.Rmd` and must be re-knit after edits.

Constraints from `AGENTS.md`: Commonwealth (UK/AU) English in prose; minimal
diffs; keep the badges and provenance comment; only use already-exported
functions and packages the package already depends on or suggests.

## Goals / Non-Goals

**Goals:**
- Make the README accurately convey what `ssdsims` is for and where to start.
- Fix the mislabelled "Installation" block (add a real install section).
- Surface the declarative scenario and `targets` pipeline, with links to the
  existing vignettes, without duplicating their content.
- Keep examples minimal, evaluated, and reproducible so `README.md` regenerates
  cleanly.

**Non-Goals:**
- No changes to R source, exported API, tests, or dependencies.
- No new vignettes or `_pkgdown.yml` changes.
- Not a full tutorial — the README teases and links; the articles teach.
- No new figures/plots in `man/figures/` (keeps the diff and build light).

## Decisions

- **Keep `README.Rmd` as the single source, re-knit `README.md`.** Matches the
  repo's existing provenance comment and `usethis` convention. Alternative —
  hand-editing `README.md` — was rejected because it would drift from
  `README.Rmd` and break the documented contract. Regenerate with
  `devtools::build_readme()`.
- **Reframe the overview around the declarative scenario + `targets` pipeline,
  with the immediate pipeline as a quick start.** The immediate pipeline is the
  easiest to show in a few lines, so it stays as the runnable example; the
  scenario path is introduced as a short, possibly `eval=FALSE` teaser plus a
  link to `defining-a-scenario`. Alternative — leading with a full scenario →
  shards → `targets` example — was rejected as too heavy for a README and
  redundant with the vignettes.
- **Add a compact capability map with article links rather than prose
  paragraphs per area.** A short bulleted map mirrors the `_pkgdown.yml`
  reference sections (immediate pipeline; scenarios & task expansion; targets
  pipeline; cloud upload; cost estimation; reproducible RNG) and routes readers
  to the right vignette. Keeps the README scannable.
- **Replace the fake "Installation" heading with a genuine one** showing
  `pak::pak("poissonconsulting/ssdsims")` (and a `remotes` alternative), then
  move usage under a separate "Usage"/"Quick start" heading.
- **Keep examples deterministic.** Retain the `withr::with_seed(42, ...)`
  wrapper already used so printed output is stable across re-knits.

## Risks / Trade-offs

- **Example drift / build failure** (an example stops running as the API
  evolves) → keep examples minimal and use only stable exported functions;
  re-knit and skim the output before committing so failures surface at build
  time, not for a visitor.
- **README and vignettes diverge** (the README re-explains scenario mechanics
  that the articles own) → the README only links to articles for depth; it does
  not restate their content.
- **Heavier knit / longer CI** if examples fit many distributions → keep
  `nsim`/`nrow` small (as today) so the knit stays fast.

## Migration Plan

Not applicable — documentation-only change. The deploy step is re-knitting
`README.md` from `README.Rmd`; rollback is reverting both files. No data,
schema, or API migration.

## Open Questions

- Whether to show the scenario teaser as evaluated output or `eval = FALSE`.
  Default: evaluate it if it stays fast and deterministic; otherwise mark
  `eval = FALSE` and link to `defining-a-scenario`. Resolve during
  implementation by knitting and checking timing.
