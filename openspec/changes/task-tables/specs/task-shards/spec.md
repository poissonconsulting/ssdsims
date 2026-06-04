## ADDED Requirements

### Requirement: Group a step's task rows into shards by partition_by
The package SHALL provide `ssd_scenario_sample_shards()`, `ssd_scenario_fit_shards()`, and `ssd_scenario_hc_shards()` that group the corresponding step's task table into one row per `partition_by` **path** cell, using the path/inner split from `scenario_partition_axes(scenario, step)` (`TARGETS-DESIGN.md` §5/§6). Each shard row SHALL carry the step's path-axis columns (the Hive partition path and the `tar_map` target-name suffix) and a `tasks` list-column containing the task rows whose path-axis values match that shard.

#### Scenario: One shard row per path cell
- **WHEN** a step's shard table is derived under a `partition_by` whose path axes take `P` distinct value-combinations across the step's tasks
- **THEN** the shard table SHALL have `P` rows, each carrying the path-axis columns, and the union of the `tasks` list-columns SHALL equal the step's full task table

#### Scenario: Coarser partitioning bundles more tasks per shard
- **WHEN** the same step is sharded under a `partition_by` with fewer path axes
- **THEN** the shard count SHALL decrease and the per-shard `tasks` row counts SHALL increase correspondingly, with the union of tasks unchanged

### Requirement: Each shard task row carries its seed and primer
Each task row in a shard's `tasks` list-column SHALL carry `seed = scenario$seed` and its per-task `primer` (`task_primer()` over the `task_axes(step)` identity), computed at grouping time. This decoration SHALL be RNG-free (a hash, not a draw) and SHALL match the `(seed, primer)` the baseline runner installs for the same task.

#### Scenario: Shard task rows carry (seed, primer)
- **WHEN** a step's shard table is derived
- **THEN** every task row in every shard's `tasks` SHALL carry the scenario's `seed` and a `primer` equal to `task_primer()` over that task's `task_axes(step)` identity

#### Scenario: Decoration draws no random numbers
- **WHEN** the shard tables are derived
- **THEN** no random numbers SHALL be drawn and `.Random.seed` SHALL be unchanged

### Requirement: A per-shard step runner writes one Parquet per shard
The package SHALL provide per-shard step runners `ssd_run_sample_step()`, `ssd_run_fit_step()`, and `ssd_run_hc_step()` that loop a shard's `tasks`, install each task's `(seed, primer)` exactly once via the existing `*_data_task_primer()` wrappers under an active dqrng backend, and write the shard's results to one Parquet file at the shard's partition path. The `fit` and `hc` runners SHALL read their upstream shard's Parquet by partition path (the `fit` runner truncating `head(sample, nrow)` inline, RNG-free), resolve datasets and `min_pmix` through the `registry`, and record the written shard's sha256 through the `manifest`.

#### Scenario: A shard runs its tasks and writes one Parquet
- **WHEN** a step runner is called on a shard with `K` tasks
- **THEN** it SHALL prime and run all `K` tasks and write a single Parquet file at the shard's partition path containing their results

#### Scenario: Downstream runners read upstream by partition path
- **WHEN** a `fit` (or `hc`) shard runner executes
- **THEN** it SHALL open the matching upstream `sample` (or `fit`) shard's Parquet by partition path and use it as the task input, rather than recomputing the upstream

#### Scenario: A written shard records its sha256
- **WHEN** a step runner writes a shard's Parquet successfully
- **THEN** it SHALL record that shard's sha256 via the manifest recorder

### Requirement: A static-branching targets pipeline runs a scenario
The package SHALL ship a `targets` pipeline template that builds the scenario as a plain construction-time object (not a `tar_target`) and uses `tarchetypes::tar_map()` to mint one named target per shard for each step (static branching, `TARGETS-DESIGN.md` §6), so the shard set is fixed when `_targets.R` is sourced. The pipeline SHALL `targets::tar_make()` a tiny scenario end-to-end, producing one Parquet per shard across the `sample`, `fit`, and `hc` layers.

#### Scenario: tar_make builds one target per shard
- **WHEN** the template pipeline is sourced for a scenario and `tar_make()` is run
- **THEN** one named target per shard SHALL be built for each step, and one Parquet per shard SHALL be written under the step's result directory

#### Scenario: The scenario is not a target
- **WHEN** the pipeline is sourced
- **THEN** the scenario SHALL be a construction-time object and the per-step shard tables SHALL be computed at sourcing time to feed `tar_map()`'s `values`

### Requirement: Targets results match the baseline runner
The per-task results produced by the targets pipeline SHALL be byte-identical (as read-back R values) to those produced by `ssd_run_scenario_baseline()` for the same scenario, because both install the same per-task `(seed, primer)` via the same primitives and results are order-independent (`TARGETS-DESIGN.md` §5/§6).

#### Scenario: Pipeline output equals baseline output
- **WHEN** a scenario is run both through the targets pipeline and through `ssd_run_scenario_baseline()`
- **THEN** the collected per-task `sample`, `fit`, and `hc` results SHALL be equal across the two runs

### Requirement: A summary fan-in reads the result layers without re-running upstream
The package SHALL provide `ssd_summarize()` that reads the `sample`, `fit`, and `hc` result directories (via `duckplyr`) and writes a combined `results/summary.parquet`, without depending on each shard target's value and without re-running upstream steps (`TARGETS-DESIGN.md` §6).

#### Scenario: Summary reads landed shards
- **WHEN** `ssd_summarize()` is run after the shard Parquets are written
- **THEN** it SHALL read the result layers from disk and write a combined summary, without recomputing any shard

#### Scenario: Summary does not pull every shard value into R via targets
- **WHEN** the summary target runs in the pipeline
- **THEN** it SHALL read the result directories directly rather than depending on every shard target's in-memory value
