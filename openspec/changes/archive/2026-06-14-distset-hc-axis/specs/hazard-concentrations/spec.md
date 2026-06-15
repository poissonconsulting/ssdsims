## ADDED Requirements

### Requirement: Per-cell distribution-set subsetting reuses one union fit
The hc computation for a given cell SHALL, when a scenario declares distribution
sets, subset its parent **union** fit down to the cell's set members
(`ssdtools::subset.fitdists(fit, select = members, strict = FALSE)`) and run
`ssdtools::ssd_hc()` on that subset, rather than re-fitting the set. The subset
SHALL be performed in the shared per-task primitive (`hc_data_task_primer()`), so
the single-core baseline runner, the single-core shard runner, and the `targets`
pipeline produce byte-identical per-task results. **At a fixed cell seed**, a
pool's hc result SHALL be byte-identical to fitting that pool directly
(`ssd_fit_dists(data, dists = members)`) and running `ssd_hc()` with the same seed
and inputs — the per-distribution fits are independent within `ssd_fit_dists()`,
so re-averaging a subset of the union equals fitting the subset alone. This is a
same-seed invariant, NOT a claim of equality with the pre-change pipeline:
`"distset"` joins the hc primer (`task_axes("hc")`), so its presence re-seeds each
hc task; point estimates (`est`) are analytical and unchanged, bootstrap CIs are
re-seeded (statistically equivalent). A set whose members all failed to fit
(an empty subset) SHALL yield zero hc rows for that cell rather than aborting.

#### Scenario: A pool re-averages one union fit, identical to a direct fit
- **WHEN** a scenario declares `dists = list(BCANZ = ..., Iwasaki = ...)`, the fit step fits the union once, and the hc step computes a cell at a fixed seed for `distset = "Iwasaki"`
- **THEN** the cell SHALL subset the union fit to the Iwasaki members and run `ssd_hc()` on the subset, producing a result byte-identical to `ssd_hc()` on `ssd_fit_dists(data, dists = <Iwasaki members>)` seeded the same way (no re-fit)

#### Scenario: All three runners agree on the subset-and-average result
- **WHEN** the same distset scenario is run via the single-core baseline runner, the single-core shard runner, and the `targets` pipeline
- **THEN** the per-task hc results SHALL be byte-identical across the three runners (the subset is performed in the shared per-task primitive)

#### Scenario: An empty subset yields no rows
- **WHEN** a cell's distribution set has no members that fitted (the subset is zero-length)
- **THEN** the hc computation SHALL emit zero rows for that `(cell, distset)` rather than aborting, leaving the survivors of the run intact
