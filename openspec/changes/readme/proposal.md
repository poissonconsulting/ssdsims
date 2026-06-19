## Why

The package's user-facing documentation has structural gaps that no single
vignette fix closes, and `ROADMAP.md` (`📚⏳ [readme]`) already bundles them as
one item: *"Update README and integrate in the rest of the documentation.
Create `vignette("ssdsims")`. Perform comprehensive review of all vignettes for
content, output artifacts, and style. Use `vignette(...)` style links
everywhere."*

A comprehensive review (one cross-cutting skim of the rendered `gh-pages` site
plus a deep read of each of the seven vignettes against the code and the
function reference) found:

- The `README` is stale and undersells the package: its one-line goal and a
  single fused code block exercise only the **legacy step functions**
  (`ssd_sim_data()` → `ssd_fit_dists_sims()` → `ssd_hc_sims()` and the
  `ssd_run_scenario()` shortcut). It never mentions the declarative scenario,
  task-table expansion, the `targets`-based shard pipeline, designs, cost
  estimation/analysis, cloud upload, or reproducible RNG. The "Installation"
  block is actually a usage example.
- There is **no get-started/overview article** — a reader hits seven peer
  vignettes with no stated reading order and no map of the two tracks
  (build-and-run pipeline vs. predict-and-measure cost).
- **No `vignette()`-style cross-links exist** — all seven vignettes link via
  hardcoded `.html` (one uses absolute `poissonconsulting.github.io` URLs that
  do not resolve in a local build).
- `cost-estimation` is a **dead-end** (links to no sibling; only `cost-analysis`
  links to it), and several vignettes lack "See also" navigation.
- Smaller accuracy/style nits (mis-targeted function links, a benchmark
  mis-attributed to `ssd_define_scenario()`'s docs, OpenSpec-internal phrasing
  leaking into user docs, a "four-file" template that ships five, an overstated
  "asserts byte-identical", a stray trailing code fence).

The vignettes are otherwise accurate and current (all on the declarative
`ssd_scenario_data()` path; no stale `ssd_data(`), so this change is about
**coherence, navigation, and an on-ramp**, not rewriting correct content.

## What Changes

- **Rewrite the README on the declarative + `targets` path.** The legacy step
  functions are treated as gone: examples use `ssd_scenario_data()` →
  `ssd_define_scenario()` → `ssd_run_scenario_baseline()` (and a pointer to the
  shard/`targets` runners), **not** `ssd_sim_data()` / `ssd_fit_dists_sims()` /
  `ssd_hc_sims()` / `ssd_run_scenario()`. Reframe the overview, add a real
  **Installation** section, a minimal declarative quick-start, and a capability
  map linking to the articles.
- **Create `vignette("ssdsims")`** — a get-started/overview article
  (`vignettes/ssdsims.qmd`, pkgdown "Get started"): one-paragraph problem
  statement, a labelled map of the two tracks, an explicit recommended reading
  order, and a short declarative on-ramp. Slot it first in `_pkgdown.yml`
  `articles:`.
- **Apply the comprehensive-review fixes** across the seven vignettes: add
  "See also" navigation everywhere, end the `cost-estimation` dead-end
  (mutual links with `cost-analysis` + a back-link to `defining-a-scenario`),
  forward-link `defining-a-scenario` → `scenario-to-design`, point function
  links at the reference (not articles), and correct the accuracy/style nits
  listed above (including the OpenSpec-internal "the change's `exploration/`
  directory" phrasing in `R/cost-estimate.R` roxygen).
- **Convert cross-links to `vignette()`-style package-wide**: vignette →
  vignette links use `vignette("name")`; function mentions link to the
  reference. Applies to all seven existing vignettes, the new overview, and the
  README.
- Re-knit `README.md` from `README.Rmd` and re-render the site so the rendered
  output matches source.

## Capabilities

### New Capabilities
- `package-readme`: What the rendered `README.md` (generated from `README.Rmd`)
  must contain and keep in sync — declarative-path framing (no legacy step
  functions), real installation instructions, a runnable declarative
  quick-start, a capability map, a link to `vignette("ssdsims")`, and the
  build/sync contract.
- `package-vignettes`: The vignette set as a navigable whole — a get-started
  `vignette("ssdsims")` overview exists and leads; cross-links use
  `vignette()`-style (function links resolve to the reference); every vignette
  carries "See also" navigation with no orphan/dead-end; the cost pair is
  mutually linked; user-facing docs carry no OpenSpec/repo-internal references;
  prose stays accurate against the code and reference.

### Modified Capabilities
<!-- None. No behaviour (code) requirement of an existing capability changes.
     The R/cost-estimate.R edit is a roxygen wording fix, not a requirement
     change. -->

## Impact

- Files: `README.Rmd` (rewritten), `README.md` (regenerated),
  `vignettes/ssdsims.qmd` (new), all seven existing `vignettes/*.qmd` (cross-link
  conversion + review fixes), `_pkgdown.yml` (add `ssdsims` first in `articles:`),
  `R/cost-estimate.R` + `man/` (roxygen wording fix for the leaked phrasing).
- **Assumes the legacy step functions are gone** (coordinated with
  `migrate-public-api` / `cleanup-lecuyer`): no example or narrative documents
  `ssd_sim_data()` / `ssd_fit_dists_sims()` / `ssd_hc_sims()` /
  `ssd_run_scenario()`. This removes the README's dependency on
  `migrate-public-api`'s numeric output churn — see `design.md`.
- No exported API or behaviour changes; no new dependencies. Example code uses
  already-exported declarative/targets functions and `Suggests` packages
  (`ssddata`) already used by the vignettes.
- The rendered pkgdown site (home page + Get-started tab + article navigation)
  improves materially.
