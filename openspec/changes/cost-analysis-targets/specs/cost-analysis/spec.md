## ADDED Requirements

### Requirement: Per-task timings are captured in-band on the fit and hc layers
The `fit` and `hc` step runners SHALL bracket each task's body
(`ssd_run_fit_step()`, `ssd_run_hc_step()`) and record three columns in the
shard's `part.parquet`: `.start` and `.end` (UTC timestamps of the task body's start and
finish) and `.host` (the machine's CPU description, the grain the
architecture-specific cost calibration pools on). On the `hc` layer the values
repeat across a task's `proportion` rows. The timings SHALL be stored **in the
shard Parquet itself, not in a sidecar file** — the shard-file count is the
object-count budget at deliverable scale. The `sample` step SHALL NOT be timed
and its output SHALL be unchanged. Timing capture SHALL NOT draw random numbers
or alter any per-task result value.

#### Scenario: fit and hc shards carry timing columns
- **WHEN** a `fit` or `hc` shard is written by its step runner
- **THEN** the shard Parquet SHALL contain `.start`, `.end`, and `.host` for every task, with `.end` at or after `.start`, and no additional file SHALL be created beside `part.parquet`

#### Scenario: sample shards are unchanged
- **WHEN** a `sample` shard is written
- **THEN** its Parquet SHALL contain no timing columns and its bytes SHALL remain deterministic for a fixed scenario and seed

#### Scenario: Timing capture is RNG-neutral
- **WHEN** the same scenario and seed are run with timing capture
- **THEN** every per-task result value SHALL be byte-identical to the pre-instrumentation runner's (the timing columns excluded)

### Requirement: The baseline runner carries the same timing columns
`ssd_run_scenario_baseline()` SHALL record the same `.start`/`.end`/`.host`
columns on its in-memory `fit` and `hc` result tibbles, so cost analysis works
on any run — baseline, single-core sharded, or `targets` — and the byte-identity
oracle compares result columns symmetrically. The legacy `ssd_sim_data()` /
`ssd_fit_dists_sims()` / `ssd_hc_sims()` family SHALL NOT change under this
capability.

#### Scenario: Baseline fit/hc tibbles carry timings
- **WHEN** `ssd_run_scenario_baseline()` completes
- **THEN** its `fit` and `hc` tibbles SHALL carry `.start`, `.end`, and `.host` per task, alongside unchanged result columns

### Requirement: Summaries retain the hc timing columns
`ssd_summarise()` SHALL retain the `.start`/`.end`/`.host` columns in both the
compact summary and the optional `path_with_samples` full summary (it SHALL
continue to project out `dists`/`samples` from the compact summary), so observed
cost is queryable from the summary Parquet alone — including when the shards are
remote after upload.

#### Scenario: summary.parquet is enough to read observed hc cost
- **WHEN** `ssd_summarise()` writes the compact summary for a run with timing columns
- **THEN** the summary SHALL contain `.start`/`.end`/`.host` per hc row while still excluding `dists`/`samples`

### Requirement: Observed cost analysis from a run's in-band timings
The package SHALL expose `ssd_analyse_cost(scenario, ...)` that reads the
per-task timing columns off a completed run's `fit`/`hc` shards (or a baseline
result) and returns an `ssdsims_cost_analysis` object carrying the observed
total compute (summed task durations), the observed longest single task, and a
per-axis breakdown keyed the same way `ssd_estimate_cost()`'s breakdown is
(`ci_method` × `nboot`). Per-task durations SHALL be **measured** (from
`.start`/`.end`), not inferred, whenever timing columns are present. The
function SHALL be read-only: it SHALL NOT run the pipeline, fit any
distribution, draw random numbers, or write any file.

#### Scenario: Returns measured observed total and longest task
- **WHEN** `ssd_analyse_cost()` is called on a run whose shards carry timing columns
- **THEN** the result SHALL report the observed total compute and the observed longest single task as time quantities computed from the per-task `.start`/`.end`, with the breakdown aggregated from measured durations

#### Scenario: Analysis does not execute the scenario
- **WHEN** `ssd_analyse_cost()` is called
- **THEN** no distributions SHALL be fitted, no bootstrap SHALL run, `.Random.seed` SHALL be unchanged, and no files SHALL be written

### Requirement: The targets store supplies the shard envelope and the pre-timing fallback
When given a `targets` store, `ssd_analyse_cost()` SHALL additionally read
`targets::tar_meta()` per-target `seconds`, resolve each shard target name
(`<step>_step_<pathcell>`) back to its scenario shard by **regenerating** the
expected names from the scenario (never by parsing the name string), and report
the per-shard **envelope overhead** — target seconds minus the shard's summed
task durations (parent read + Parquet write + dispatch), the number that informs
`partition_by` tuning. Non-shard targets (`summary`, `upload_<step>`) SHALL be
excluded from attribution. For a run whose shards predate the timing columns,
the tar_meta path SHALL serve as the fallback: shard seconds attributed to tasks
proportional to the calibrated predicted per-task cost, with the result marked
as inferred rather than measured. Targets with missing or `NA` `seconds`
(errored or unbuilt shards) SHALL be excluded from totals rather than aborting,
and the analysis SHALL report how many targets contributed.

#### Scenario: Envelope overhead per shard
- **WHEN** a store and timing-bearing shards are both available
- **THEN** the analysis SHALL report, per shard, the target's seconds minus the summed measured task durations

#### Scenario: Pre-timing store falls back to inferred attribution
- **WHEN** the shards carry no timing columns but the store records the run
- **THEN** the analysis SHALL attribute shard seconds to tasks proportional to the predicted per-task cost and SHALL mark the attribution as inferred

#### Scenario: Non-shard and errored targets are handled
- **WHEN** the store contains `summary`/`upload_<step>` targets and a target with `NA` seconds
- **THEN** the former SHALL NOT be attributed to scenario shards and the latter SHALL be excluded from totals, with the contributing-target count reported

### Requirement: Recalibrate the cost model from an observed run
The package SHALL expose `ssd_calibrate_cost_from_run(scenario, ...)` that
re-fits the per-task cost model from a run's **measured** hc task durations
(building the same sweep frame `ssd_calibrate_cost()` fits — `nrow`,
`ci_method`, `nboot`, `time`) and derives the fixed addend from the measured fit
task durations, returning an `ssdsims_cost_calibration` of the same shape with
provenance marking it run-derived (source of the timings and the observed-run
date). Because the calibration is architecture-specific, the function SHALL NOT
silently pool timings from different `.host` values: a mixed-host run SHALL
require an explicit host selection or fail with the hosts listed. The result
SHALL be usable directly by `ssd_estimate_cost()`.

#### Scenario: Run-derived calibration is usable by the estimator
- **WHEN** `ssd_calibrate_cost_from_run()` returns a calibration and it is passed to `ssd_estimate_cost()`
- **THEN** the estimate SHALL use the run's measured coefficients rather than the shipped default

#### Scenario: Mixed hosts are not silently pooled
- **WHEN** the run's timing rows carry more than one distinct `.host`
- **THEN** the function SHALL NOT pool them silently; it SHALL require an explicit host choice or abort naming the hosts found

#### Scenario: Provenance marks the calibration as run-derived
- **WHEN** `ssd_calibrate_cost_from_run()` completes
- **THEN** the returned object's provenance SHALL identify it as derived from a run, distinguishing it from an `ssd_calibrate_cost()` sweep result

### Requirement: Compare predicted against observed cost
The package SHALL expose `ssd_compare_cost(scenario, ..., calibration)` that
places the predicted estimate (`ssd_estimate_cost(scenario, calibration)`)
beside the observed analysis (`ssd_analyse_cost()`) and returns an
`ssdsims_cost_comparison` object reporting, at minimum, the predicted and
observed total compute, the predicted and observed longest task, and the
predicted/observed ratio for each. The comparison SHALL be read-only.

#### Scenario: Comparison reports predicted, observed, and their ratio
- **WHEN** `ssd_compare_cost()` is called on a scenario with a recorded run
- **THEN** the result SHALL report the predicted total, the observed total, and their ratio, and likewise for the longest task

#### Scenario: Comparison runs no pipeline
- **WHEN** `ssd_compare_cost()` is called
- **THEN** no pipeline SHALL run, no random numbers SHALL be drawn, and no file SHALL be written

### Requirement: Cost-analysis documentation vignette
The package SHALL include a vignette that demonstrates the analyse → compare →
recalibrate loop: running (or loading) a small scenario, calling
`ssd_analyse_cost()` to read the observed per-task timings,
`ssd_compare_cost()` to place them beside the prediction, and
`ssd_calibrate_cost_from_run()` to derive a run-based calibration. It SHALL
frame the observed total as serial-equivalent compute (the sum of task
durations), distinct from elapsed wall time under parallel workers. The
vignette is documentation; it SHALL NOT be the mechanism by which analysis or
recalibration is performed.

#### Scenario: Vignette demonstrates the loop
- **WHEN** the cost-analysis vignette is rendered
- **THEN** it SHALL call `ssd_analyse_cost()` and `ssd_compare_cost()` and display an observed total, an observed longest task, and a predicted-vs-observed comparison for a worked run

### Requirement: Cost-analysis functions accept a design (depends on scenario-combine)
The three cost-analysis functions SHALL additionally accept an `ssdsims_design`
(the `ssd_design()` collection from the `scenario-combine` capability) — namely
`ssd_analyse_cost()`, `ssd_compare_cost()`, and `ssd_calibrate_cost_from_run()` —
and roll observed cost up across the design's member scenarios. Each member's observed cost SHALL be read from its own
`<root>/scenario=<name>/layout=<hash>` results tree exactly as the scenario form
reads a standalone run. `ssd_analyse_cost()` SHALL return an
`ssdsims_cost_analysis` whose breakdown carries a leading `scenario` column (the
member name) above the `ci_method` × `nboot` axes and design-level totals
(observed total = sum over members; observed longest = maximum single task across
members), skipping members with no readable run and reporting the
contributing-member count. The scenario *name* SHALL NOT enter any duration or
result value. The design forms SHALL be read-only (no pipeline, no RNG, no
writes), and the scenario-level behaviour SHALL be unchanged.

#### Scenario: Design totals equal the sum of member analyses
- **WHEN** `ssd_analyse_cost(design, root = <root>)` is called on a design whose members have all run
- **THEN** the observed total compute SHALL equal the sum of the per-member observed totals, the breakdown SHALL carry one `scenario` value per member, and the observed longest task SHALL be the maximum single-task duration across members

#### Scenario: A member that did not run is reported, not fatal
- **WHEN** one member of the design has no readable run while the others do
- **THEN** the analysis SHALL aggregate the members that did run, report the contributing-member count, and SHALL NOT abort

#### Scenario: Design comparison and recalibration roll up across members
- **WHEN** `ssd_compare_cost(design)` and `ssd_calibrate_cost_from_run(design)` are called on a recorded design run
- **THEN** the comparison SHALL report design-total predicted/observed, their ratios, and a per-`scenario` row, and the recalibration SHALL pool the members' measured durations into one host-aware calibration usable by `ssd_estimate_cost()`, aborting on mixed `.host` values unless a host is selected

### Requirement: The combined design summary is the design hc read surface
When analysing a design, `ssd_analyse_cost()` SHALL be able to read every
member's hc per-task timings from the combined `<root>/summary.parquet` (the
`scenario-combine` design summary, which carries a `scenario` identity column
and, because the per-scenario compact summaries retain `.start`/`.end`/`.host`,
the hc timing columns) in a single DuckDB read, without globbing each member's
shard tree. The `fit`-layer durations (the addend) SHALL still come from the
per-member `fit` shard globs. When the combined summary is absent or lacks the
timing columns, the analysis SHALL fall back to per-member shard globs with no
change to the reported totals.

#### Scenario: Design hc cost comes from the combined summary in one read
- **WHEN** the combined `<root>/summary.parquet` carries the members' hc timing columns and a `scenario` column
- **THEN** `ssd_analyse_cost(design)` SHALL derive every member's hc observed cost from that single file, grouped by `scenario`, without a per-member hc shard glob

#### Scenario: Missing combined summary falls back to per-member globs
- **WHEN** the combined summary is absent or lacks the timing columns
- **THEN** the analysis SHALL read each member's hc timings from its `scenario=<name>` shard tree, with identical totals

### Requirement: The store resolver is design-aware (one store, prefixed names)
Given a design's single `targets` store, `ssd_analyse_cost()` SHALL read one
`tar_meta()` covering every member and resolve each shard target by regenerating
the expected names *per member with the design's `<name>_` prefix*
(`<name>_<step>_step_<pathcell>`) and the member's `scenario=<name>` root, then
joining on the store's `name` column — never parsing the name string. Per-member
envelope overhead and the pre-timing fallback SHALL be computed per member and
summed to the design total. The combined `summary` target and every
`<name>_upload_<step>` target SHALL be excluded from shard attribution; targets
with `NA` seconds SHALL be excluded from totals; unmatched targets SHALL be
reported with their count, never silently dropped and never fatal.

#### Scenario: Prefixed target names resolve to the right member
- **WHEN** the store holds `a_hc_step_<cell>` and `b_hc_step_<cell>` targets for design members `a` and `b`
- **THEN** each SHALL resolve to its own member's shard under `scenario=a` / `scenario=b`, and the per-member envelope overhead SHALL aggregate to a design total

#### Scenario: Combined summary, upload, and unmatched targets are handled
- **WHEN** the store contains the top-level `summary` target, `<name>_upload_<step>` targets, and a target matching no regenerated member name
- **THEN** none of the former SHALL be attributed to a member's shards and the unmatched target SHALL be reported with its count rather than dropped or aborting
