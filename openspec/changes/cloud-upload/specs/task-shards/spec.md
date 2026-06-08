## MODIFIED Requirements

### Requirement: A target factory builds the whole pipeline from a scenario
The package SHALL provide `ssd_scenario_targets(scenario, ..., root, upload, cue)` that returns the complete list of `targets` objects for the static-branching pipeline — one `format = "file"`, `error = "null"` target per `partition_by` path cell per step (named by the step's path axes), the per-child upstream edges wiring each child shard to the parent shard(s) it reads, and the `summary` — written under `root` (default `scenario_results_dir(scenario)`). The factory SHALL place `...` immediately after `scenario` and call `rlang::check_dots_empty()`, so `root`, `upload`, and `cue` MUST be passed by name and a positional or misspelled argument aborts. The `upload` argument is the **remote-destination sibling of `root`** (default `NULL`): with `upload = NULL` the factory SHALL emit no `upload_<step>` targets; with a non-`NULL` upload object (`ssd_upload_azure(...)` or `ssd_upload_dryrun()`) the factory SHALL pair each step shard with an `upload_<step>` target in the same `tar_map` (per the `cloud-upload` capability). A `_targets.R` SHALL therefore reduce to building a scenario and calling the factory; the per-task results SHALL be unchanged and independent of `upload`.

#### Scenario: A `_targets.R` is just a scenario plus the factory call
- **WHEN** a `_targets.R` does `source("scenario.R"); ssd_scenario_targets(scenario)` and `targets::tar_make()` runs
- **THEN** every shard target SHALL build and the per-task results SHALL equal those of `ssd_run_scenario_baseline()` for the same scenario

#### Scenario: root, upload, and cue must be passed by name
- **WHEN** `ssd_scenario_targets()` is called with a positional argument after `scenario`, or a misspelled named argument
- **THEN** `rlang::check_dots_empty()` SHALL abort with an informative error, so `root`/`upload`/`cue` are only ever supplied by name

#### Scenario: upload defaults to no upload targets
- **WHEN** `ssd_scenario_targets(scenario, root = ...)` is called without `upload`
- **THEN** `upload` SHALL default to `NULL` and the returned target list SHALL contain no `upload_<step>` targets

#### Scenario: a non-NULL upload pairs each shard with an upload target
- **WHEN** `ssd_scenario_targets(scenario, upload = ssd_upload_dryrun())` is called
- **THEN** the returned target list SHALL contain one `upload_<step>` target per step shard, paired in the same `tar_map`, with the per-task results unchanged from the `upload = NULL` run
