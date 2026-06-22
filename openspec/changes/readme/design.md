## Context

The README is the first thing a visitor sees on GitHub and the body of the
pkgdown home page (rendered from `README.md`). `README.md` is generated from
`README.Rmd` (the standard `usethis` contract; provenance comment retained).
The package ships seven vignettes — `defining-a-scenario`, `sharded-pipeline`,
`scenario-to-design`, `cluster-pipeline`, `cloud-upload`, `cost-estimation`,
`cost-analysis` — rendered to the `gh-pages` branch.

A comprehensive review (cross-cutting skim of the rendered site + a per-vignette
deep read against `R/` and `man/`) established the current state:

- Vignettes are individually accurate and current — all on the declarative
  `ssd_scenario_data()` path, no stale `ssd_data(`, signatures match code,
  reference coverage complete, rendered HTML clean (the one "error" is an
  intentional validation demo in `defining-a-scenario`).
- The gaps are structural: no get-started overview; no `vignette()`-style links
  anywhere; `cost-estimation` is a dead-end; missing "See also" sections; a
  handful of accuracy/style nits.
- The README alone still leads with the **legacy step functions**.

Decision input from the change's scoping: **assume the legacy API is gone** —
document only the declarative + `targets` path. The legacy step functions
(`ssd_sim_data`/`ssd_fit_dists_sims`/`ssd_hc_sims`/`ssd_run_scenario`) are the
"Simulation pipeline" reference group that `migrate-public-api` /
`cleanup-lecuyer` retire; the docs should read as if that has happened.

Constraints (`AGENTS.md`): Commonwealth (UK/AU) English; minimal diffs; only
already-exported functions and existing deps in examples.

## Goals / Non-Goals

**Goals:**
- A coherent, navigable documentation set with a clear on-ramp and reading
  order.
- A README and a `vignette("ssdsims")` that present the package on the
  declarative + `targets` path.
- `vignette()`-style cross-links package-wide; function links to the reference.
- Close the review findings (dead-end, See-also, accuracy/style nits).

**Non-Goals:**
- No changes to R behaviour, exported API, tests, or dependencies (the only
  `R/` edit is a roxygen wording fix in `cost-estimate.R`).
- Not rewriting the (correct) technical content of the existing vignettes —
  only navigation, links, framing, and the specific nits.
- Not removing the legacy functions themselves — already done by `#196`
  (`cleanup-lecuyer`, which folded in the former `migrate-public-api`).
- No new figures/plots.

## Decisions

- **Document only the declarative + `targets` path.** The legacy step functions
  and the L'Ecuyer-CMRG RNG were retired in `#196`, leaving one runner family on
  dqrng; `#196` also moved the README quick-start onto the declarative path and
  removed the legacy "Simulation pipeline" group from `_pkgdown.yml`. This change
  builds on that final state — no legacy quick-start to re-knit, no engine-swap
  exposure — and makes the README's *framing* (overview, install, capability
  map, on-ramp) match the declarative vignettes.
- **`vignettes/ssdsims.qmd` as the get-started article.** pkgdown treats a
  vignette named after the package as the navbar "Get started" link, so
  `vignette("ssdsims")` becomes the natural entry point. It is an *overview/map*,
  not a tutorial: problem statement, the two-track map (build-and-run vs.
  predict-and-measure cost), recommended reading order, and a short declarative
  on-ramp that then points into `defining-a-scenario`. Slots first in
  `_pkgdown.yml` `articles:`.
- **Bidirectional README ↔ get-started, with the overview as the spine.** The
  README's quick-start is the 30-second on-ramp and links to `vignette("ssdsims")`;
  the overview carries the map and reading order and links back out to each
  track. Neither is the sole source; the overview is the hub.
- **`vignette()`-style links package-wide.** Convert every vignette→vignette
  link to `vignette("name")` and every function mention to its reference page.
  This satisfies the roadmap and (unlike hardcoded `.html`/absolute URLs)
  resolves in local builds and installed-package help. Done across all seven
  vignettes, the overview, and the README.
- **Re-knit/re-render as the build step.** `devtools::build_readme()` for
  `README.md`; `pkgdown`/`quarto` for the site. Output is generated, not
  hand-edited.

## Risks / Trade-offs

- **Roadmap is stale post-`#196`** → `cleanup-lecuyer` is implemented but not
  archived, and `ROADMAP.md` still shows it in-flight with `readme` "blocked by
  cleanup-lecuyer". The blocker's code has landed, so `readme` is effectively
  unblocked. Mitigation: archive `cleanup-lecuyer` and update the roadmap
  (offered separately; not bundled into this change).
- **Cross-link conversion churns all seven vignettes** → larger diff, but
  mechanical and low-risk; verify with a full site render that every
  `vignette()`/reference link resolves.
- **Re-render drift** (machine-specific timings in cost vignettes won't
  byte-match) → expected for live-run vignettes; not introduced by this change.

## Migration Plan

Documentation-only. Deploy = re-knit `README.md` and re-render the site;
rollback = revert the touched files. No data/schema/API migration. Sequence:
README + overview first (they define the framing), then the package-wide
cross-link conversion, then the per-vignette nit fixes, then a full render to
verify links and output.

## Open Questions

- **Archiving `cleanup-lecuyer` and refreshing `ROADMAP.md`** (lines 40–41:
  move `cleanup-lecuyer` to Done, unblock `readme`). The implementation landed
  in `#196`; these are housekeeping for a separate step, offered to the user
  rather than bundled here.
