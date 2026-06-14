# Proposal: hc-readout-aggregation

## Why

The `scenario-combine` change ships the **design** — combining scenarios into one
ragged pipeline — but deliberately scoped *out* the per-overlap **hc readout
aggregation**: within a design seed-group, members that share a cell but differ in
the four non-axis hc settings (`proportion`, `est_method`, `ci`, `samples`) are
currently **rejected** at construction (`require_uniform()` aborts with a pointer
here) rather than reconciled. That makes the secondary *setting-comparison* use of
a design impossible, and it leaves real compute on the table: comparing
`ci = FALSE, nsim = 1000` beside `ci = TRUE, nsim = 10` cannot be expressed as one
design today, and even when it could, the naive approach would bootstrap all 1000
sims to serve 10.

This change lifts that restriction by aggregating the readout demand **per shared
hc cell, over only the members that touch that cell** — the cheap, seed-free
readouts (`proportion`, `est_method`, `ci`, `samples`) are computed as a maximal
set in the shared shard and each member's summary filters its slice — so the
expensive bootstrap runs only where a `ci = TRUE` member actually has tasks.

## What Changes

- **Remove the `require_uniform()` guard on the four non-axis hc settings**
  (`proportion`, `est_method`, `ci`, `samples`) in the design factory. Members
  within a seed-group MAY now differ in them; they continue to be rejected only
  for the genuinely byte-shaping settings (`nrow_max`, the fit `dists` union, and
  `partition_by`, which remain `scenario-combine`'s contract).
- **Per-overlap readout aggregation.** For each shared hc cell, reduce the demand
  over the members whose task set contains that cell: **`union()` over
  `proportion` and `est_method`**, **`any()` over `ci` and `samples`**. A cell only
  one member reaches keeps that member's (smaller) demand, so the bootstrap runs
  only where a `ci = TRUE` member has tasks. `nboot`/`ci_method`/`parametric`/
  `distset` stay **cell axes** (they are in the per-task primer; aggregating them
  would move the RNG stream and break byte-identity).
- **`ci` NA-collapse routing.** A `ci = FALSE` hc task collapses `nboot`/
  `ci_method`/`parametric` to `NA`, so its cell differs from a `ci = TRUE` task at
  the same sim. The point `est` is analytical and bootstrap-config-invariant, so a
  `ci = FALSE` task's `est` is served by a coincident `ci = TRUE` shard at the same
  `(fit-id, distset)` when one exists, and only otherwise mints its own
  `ci = FALSE` shard. The computed hc shards are every `ci = TRUE` member's cells
  plus the `ci = FALSE` cells with no overlapping `ci = TRUE` shard.
- **Per-task readout overrides on `ssd_run_hc_step()`.** The runner gains optional
  per-task `est_method`/`proportion`/`ci`/`samples` overrides that **default to the
  scenario slice**, so the single-scenario and standalone paths stay byte-identical
  while the design path supplies the per-cell aggregated demand. **No ssdtools
  refactor** — the aggregation rides `ssdtools::ssd_hc()`'s vectorized
  `proportion`/`est_method` and the package's `hc_collapse_est_methods()`.
- **Byte identity preserved.** A served `ci = FALSE` `est` equals the standalone
  `ci = FALSE` value; a `ci = TRUE` member's CI uses its own cell's
  `(nboot, ci_method, parametric)` primer.

## Capabilities

### New Capabilities
<!-- None: this extends the existing `scenario-combine` capability rather than
     introducing a new one. -->

### Modified Capabilities
- `scenario-combine`: relax the seed-group consistency requirement so members may
  differ in the four non-axis hc settings, and add the per-overlap readout
  aggregation (with `ci`-routing) that reconciles them into shared hc shards
  filtered per member.

## Impact

- **Modified code**: `ssd_run_hc_step()` (per-task readout overrides, defaulting to
  the slice — `R/targets-runner.R`); the design factory's hc assembly
  (`R/design-targets.R`): drop `require_uniform()` for the four settings, union the
  members' hc tasks per cell with the readout reduction, and implement the
  `ci`-routing shard-set construction; the per-member summary filter already keys
  by `hc_id`, so it composes unchanged.
- **APIs**: no new exports; `ssd_run_hc_step()` gains optional arguments with
  back-compatible defaults. **No ssdtools change.** `ssd_scenario_targets()` and
  the single-scenario per-task results unchanged.
- **Tests**: the `ci = FALSE, nsim = 1000` vs `ci = TRUE, nsim = 10` example
  (bootstrap only the overlap; the `ci = FALSE` member's overlapping `est` equals
  its standalone value); a readout-only comparison sharing all shards with each
  member's summary carrying exactly its requested rows; a distset-coverage
  comparison; the previously-aborting "members differ in hc readouts" test flips
  from expect-error to expect-success.
- **Docs**: `vignettes/sharded-pipeline.qmd` setting-comparison section;
  `ssd_design_targets()` roxygen (the aggregation + `ci`-routing); `ROADMAP.md`.
- **Dependencies**: builds on `scenario-combine` (its ragged-union design factory,
  `seed=`/`layout=` addressing, and per-member summaries); land that first.
