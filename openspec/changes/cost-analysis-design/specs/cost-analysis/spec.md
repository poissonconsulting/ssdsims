## ADDED Requirements

### Requirement: Observed cost analysis accepts a design
`ssd_analyse_cost()` SHALL accept an `ssdsims_design` (the `ssd_design()`
collection from the `scenario-combine` capability) and return an
`ssdsims_cost_analysis` aggregating observed cost across the design's member
scenarios. Each member's observed cost SHALL be read from its own
`<root>/scenario=<name>/layout=<hash>` results tree, exactly as the scenario
method reads a standalone run. The returned breakdown SHALL carry a leading
`scenario` column (the member's name within the design) above the existing
`ci_method` × `nboot` axes, and the object SHALL report design-level totals: the
observed total compute summed over all members and the observed longest single
task across all members. The design method SHALL be read-only: it SHALL NOT run
any pipeline, fit any distribution, draw random numbers, or write any file. The
scenario *name* SHALL NOT enter any duration or per-task result value — it is an
addressing and grouping key only.

#### Scenario: Design totals equal the sum of member analyses
- **WHEN** `ssd_analyse_cost(design, root = <root>)` is called on a design whose members have all run
- **THEN** the observed total compute SHALL equal the sum of the per-member observed totals, the breakdown SHALL carry one `scenario` value per member, and the reported observed longest task SHALL be the maximum single-task duration across all members

#### Scenario: Design analysis executes nothing
- **WHEN** `ssd_analyse_cost()` is called on a design
- **THEN** no pipeline SHALL run, no distribution SHALL be fitted, `.Random.seed` SHALL be unchanged, and no file SHALL be written

#### Scenario: A member whose shards did not land is reported, not fatal
- **WHEN** one member of the design has no readable run while the others do
- **THEN** the analysis SHALL aggregate the members that did run, report the count of contributing members, and SHALL NOT abort on the missing member

### Requirement: The combined design summary is the hc read surface
When analysing a design, `ssd_analyse_cost()` SHALL be able to read the hc
per-task timings for every member from the combined `<root>/summary.parquet`
(the `scenario-combine` design summary, which carries a `scenario` identity
column and — because the per-scenario compact summaries retain
`.start`/`.end`/`.host` — the hc timing columns) in a single DuckDB read, without
globbing each member's shard tree. The `fit`-layer durations (the addend) SHALL
still be read from the per-member `fit` shard globs, as at the scenario level.
Reading the combined summary SHALL never decode blob columns and SHALL never
collect the full summary into R beyond the projected id and timing columns.

#### Scenario: Design hc cost comes from the combined summary in one read
- **WHEN** the combined `<root>/summary.parquet` carries the members' hc timing columns and a `scenario` column
- **THEN** `ssd_analyse_cost(design)` SHALL derive every member's hc observed cost from that single file, grouped by `scenario`, without a per-member hc shard glob

#### Scenario: Missing combined summary falls back to per-member shard globs
- **WHEN** the combined summary is absent or lacks the timing columns
- **THEN** the analysis SHALL read each member's hc timings from its `scenario=<name>` shard tree instead, with no change to the reported totals

### Requirement: The targets store path is design-aware (one store, prefixed names)
Given the design's single `targets` store, `ssd_analyse_cost()` SHALL read one
`targets::tar_meta()` covering every member and resolve each shard target back to
its member shard by **regenerating** the expected target names *per member with
the design's `<name>_` prefix* (`<name>_<step>_step_<pathcell>`) and the member's
`scenario=<name>` root — reusing the `scenario-combine` prefix/root logic — then
joining on the store's `name` column. It SHALL NOT parse target-name strings. The
per-shard envelope overhead (target seconds minus the shard's summed measured
task durations) and the pre-timing fallback (shard seconds attributed
proportional to predicted per-task cost, marked inferred) SHALL be computed per
member and summed to the design total. The combined `summary` target and every
`<name>_upload_<step>` target SHALL be excluded from shard attribution. Targets
with `NA` seconds SHALL be excluded from totals; unmatched targets SHALL be
reported with their count, never silently dropped and never fatal.

#### Scenario: Prefixed target names resolve to the right member
- **WHEN** the store holds `a_hc_step_<cell>` and `b_hc_step_<cell>` targets for design members `a` and `b`
- **THEN** each target SHALL resolve to its own member's shard under `scenario=a` / `scenario=b`, and the per-member envelope overhead SHALL aggregate to a design total

#### Scenario: Combined summary and upload targets are not attributed
- **WHEN** the store contains the top-level `summary` target and `<name>_upload_<step>` targets
- **THEN** none of them SHALL be attributed to a member's shards, and the contributing-target count SHALL be reported

#### Scenario: Targets that do not match a member are reported
- **WHEN** a stored target name matches no regenerated member name (e.g. the design or a member's `partition_by` changed since the run)
- **THEN** the analysis SHALL surface the unmatched target count rather than dropping it silently or aborting

### Requirement: Compare predicted against observed cost for a design
`ssd_compare_cost()` SHALL accept an `ssdsims_design` and return an
`ssdsims_cost_comparison` placing the design predicted cost — the sum of the
members' `ssd_estimate_cost()` predictions — beside the design observed analysis
(`ssd_analyse_cost(design, ...)`). It SHALL report the design-total predicted and
observed compute, the design-total predicted and observed longest task, and the
predicted/observed ratio for each, plus a per-`scenario` predicted/observed row.
The comparison SHALL be read-only.

#### Scenario: Design comparison reports totals and per-scenario ratios
- **WHEN** `ssd_compare_cost(design, root = <root>)` is called on a design with a recorded run
- **THEN** the result SHALL report the design predicted total, the design observed total, their ratio (and likewise for the longest task), and a predicted/observed ratio for each member scenario

#### Scenario: Design comparison runs no pipeline
- **WHEN** `ssd_compare_cost()` is called on a design
- **THEN** no pipeline SHALL run, no random numbers SHALL be drawn, and no file SHALL be written

### Requirement: Recalibrate the cost model from a design run, host-aware
`ssd_calibrate_cost_from_run()` SHALL accept an `ssdsims_design` and re-fit the
per-task cost model from the **measured** hc task durations pooled across the
design's members (one sweep frame — `nrow`, `ci_method`, `nboot`, `time` —
spanning every member), deriving the fixed addend from the pooled measured fit
durations, and returning an `ssdsims_cost_calibration` of the same shape as the
scenario method's with provenance marking it run-derived from a design. Because
the scenario *name* does not enter the cost model, pooling measured durations
across members is sound; because the calibration is architecture-specific, the
function SHALL NOT silently pool across distinct `.host` values, even when those
hosts span different members — a mixed-host design SHALL require an explicit host
selection or abort listing the hosts found. The result SHALL be usable directly
by `ssd_estimate_cost()`.

#### Scenario: Pooled design calibration is usable by the estimator
- **WHEN** `ssd_calibrate_cost_from_run(design, ...)` returns a calibration on a single-host design and it is passed to `ssd_estimate_cost()`
- **THEN** the estimate SHALL use the design run's pooled measured coefficients

#### Scenario: A mixed-host design is not silently pooled
- **WHEN** the design's members carry more than one distinct `.host` across their hc timing rows
- **THEN** the function SHALL NOT pool them silently; it SHALL require an explicit host choice or abort naming the hosts found

### Requirement: Cost-analysis objects print design-aware
The `format`/`print` methods SHALL render the per-`scenario` breakdown and the
design totals when an `ssdsims_cost_analysis` or `ssdsims_cost_comparison` was
built from a design, and SHALL leave the scenario-level output unchanged when
built from a single scenario.

#### Scenario: Design analysis prints per-scenario and totals
- **WHEN** an `ssdsims_cost_analysis` built from a design is printed
- **THEN** the output SHALL show the per-`scenario` breakdown and the design observed total and longest task

#### Scenario: Scenario-level printing is unchanged
- **WHEN** an `ssdsims_cost_analysis` built from a single scenario is printed
- **THEN** the output SHALL match the scenario-level format with no `scenario` column

### Requirement: The cost-analysis vignette covers the design level
The cost-analysis vignette SHALL include a section demonstrating the design-level
loop: `ssd_analyse_cost()`, `ssd_compare_cost()`, and
`ssd_calibrate_cost_from_run()` over a small two-scenario design, showing the
per-`scenario` breakdown and the design totals. It SHALL frame the design
observed total as serial-equivalent compute summed across all members. The
vignette is documentation; it SHALL NOT be the mechanism by which design analysis
is performed.

#### Scenario: Vignette demonstrates the design loop
- **WHEN** the cost-analysis vignette is rendered
- **THEN** it SHALL call `ssd_analyse_cost()` and `ssd_compare_cost()` on a design and display a per-`scenario` breakdown and a design observed total
