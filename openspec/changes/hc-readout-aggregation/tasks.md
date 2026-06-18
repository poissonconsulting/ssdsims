# Tasks: hc-readout-aggregation

## 1. Per-task readout overrides on the hc runner

- [ ] 1.1 Extend `ssd_run_hc_step()` (`R/targets-runner.R`) to read optional
      per-task `est_method`/`proportion`/`ci`/`samples` demand from the shard's
      `tasks` columns, falling back to the scenario slice when absent
- [ ] 1.2 Confirm the single-scenario factory and `ssd_run_scenario_shards()` path
      pass no per-task overrides, so their hc results stay byte-identical (existing
      tests pass untouched, no snapshot churn)

## 2. Per-overlap demand reduction in the design factory

- [ ] 2.1 In `ssd_design_targets()`/`design_reference_scenario()`
      (`R/design-targets.R`), drop `require_uniform()` for the shareable settings —
      `proportion`, `est_method`, `ci`, `samples`, and the fit `dists` union —
      keeping it only for `nrow_max` and `partition_by`
- [ ] 2.2 Union the members' hc tasks tagged with each member's readout demand and
      group by the hc cell (`fit` identity + `nboot`/`ci_method`/`parametric`/
      `distset`); reduce per cell: `union` `proportion`/`est_method`, `any`
      `ci`/`samples`; attach the reduced demand to the shard tasks
- [ ] 2.3 Reconcile differing fit `dists` unions: fit the **design-wide union** of
      the members' `dists` once per fit cell and let each member subset via its
      `distset` axis (sound by distset-subset-invariance — fitting extra dists does
      not change a member's subset hc), so members differing in `distset` coverage
      share the `sample`/`fit` shards

## 3. ci NA-collapse routing

- [ ] 3.1 Compute the hc shard set as every `ci = TRUE` member's cells plus the
      `ci = FALSE` cells with no overlapping `ci = TRUE` shard at the same
      `(fit-id, distset)`
- [ ] 3.2 Route each member's `ci = FALSE` hc tasks to the coincident `ci = TRUE`
      serving shard when present (for the per-member summary filter), else to the
      standalone `ci = FALSE` shard

## 4. Tests

- [ ] 4.1 Flip the `scenario-combine` "members differ in hc readouts abort" test:
      a readout-only design now returns targets and runs; each member's summary
      carries exactly its requested `proportion`/`est_method`/`ci`/`samples` rows
- [ ] 4.2 The motivating example: `ci = FALSE, nsim = 1000` vs `ci = TRUE,
      nsim = 10` builds `ci = TRUE` hc shards only for the 10 overlapping sims and
      `ci = FALSE` for the rest; the `ci = FALSE` member's overlapping `est` equals
      its standalone `ci = FALSE` value
- [ ] 4.3 Byte-identity: a `ci = TRUE` member's CI matches its standalone run; the
      single-scenario hc path is unchanged
- [ ] 4.4 distset-coverage sharing (moved from `scenario-combine` task 7.4): two
      members differing only in `distset` coverage (hence in their fit `dists`
      union) share every `sample`/`fit` shard (one design-wide union fit) and
      differ only in their `distset` hc cells; each member's hc subset equals its
      standalone run

## 5. Docs

- [ ] 5.1 Update `ssd_design_targets()` roxygen (per-overlap aggregation + the
      `ci`-routing); regenerate `man/`
- [ ] 5.2 Add a setting-comparison section to `vignettes/sharded-pipeline.qmd`
      (building on the irregular-grid section); `ROADMAP.md`
- [ ] 5.3 Format with `air`, run `devtools::check()`, confirm no ssdtools edit and
      `ssd_scenario_targets()` contract unchanged
