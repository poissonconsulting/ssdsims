## ADDED Requirements

### Requirement: A cluster targets template builds a scenario via the shared factory
The package SHALL ship an editable cluster template at `inst/targets-templates/cluster/` whose `_targets.R` builds a scenario's whole pipeline via the existing `ssd_scenario_targets()` factory (the `task-shards` capability), reusing the scenario object and factory call **verbatim** from the `large/` template. The only cluster-specific code in `_targets.R` SHALL be the `tar_option_set(controller = ...)` block; the pipeline shape and the per-task content/RNG SHALL be scheduler-independent and unchanged (`TARGETS-DESIGN.md` §4). The template SHALL include a `run.R` driver mirroring `large/run.R` (source the scenario, `tar_make()`, report the summary path and a peek at the estimates).

#### Scenario: The cluster _targets.R reduces to a scenario plus the factory call
- **WHEN** the `cluster/` template's `_targets.R` is sourced for a scenario
- **THEN** the pipeline SHALL be assembled by `ssd_scenario_targets(scenario)` (the same factory the `large/` template uses), and the only cluster-specific code outside that call SHALL be the controller block in `tar_option_set()`

#### Scenario: Per-task results are scheduler-independent
- **WHEN** the same scenario is built through the `cluster/` template and through the `large/` (local) template
- **THEN** the per-task `sample`, `fit`, and `hc` results SHALL be byte-identical as read-back R values (sorted by the task-identity key), because only the controller differs between the two templates

### Requirement: The SLURM crew controller is configured for the target cluster
The `cluster/` template SHALL set `tar_option_set(controller = crew.cluster::crew_controller_slurm(...))` with an editable resource block — the queue/`slurm_partition`, `script_lines` (e.g. module loads such as `module load R/4.x`), scratch/`tempdir`, worker count, and walltime — drafted from the crew labs' validated working block (`TARGETS-DESIGN.md` §4 ingredient B), not invented. The controller block SHALL be the single place a user edits to retarget another SLURM cluster.

#### Scenario: The controller is a configured SLURM crew controller
- **WHEN** the `cluster/` template's `_targets.R` is sourced on a cluster with `crew.cluster` available
- **THEN** `tar_option_set()` SHALL install a `crew.cluster::crew_controller_slurm()` configured with the cluster's queue/partition, `script_lines`, scratch path, worker count, and walltime

#### Scenario: Retargeting another cluster edits only the controller block
- **WHEN** a user moves the template to a different SLURM cluster
- **THEN** the only file region they need to edit SHALL be the controller block (queue, `script_lines`, scratch, workers, walltime); the scenario and the `ssd_scenario_targets()` call SHALL remain unchanged

### Requirement: Cluster connectivity is validated before scenario logic
The template SHALL validate cluster connectivity **before** any scenario logic runs, by building a trivial no-op probe target (the bare shape of ingredient A) that submits and reclaims one SLURM job. `tar_make()` SHALL build this probe first; if the probe fails, the scenario shard targets SHALL NOT run (`TARGETS-DESIGN.md` §4 ingredient B — "validated by submitting one trivial job end-to-end before any ssdsims logic is involved").

#### Scenario: The probe is the first target built
- **WHEN** `tar_make()` is run on the `cluster/` template
- **THEN** a trivial no-op probe target SHALL be submitted to and reclaimed from one SLURM job before any scenario shard target runs

#### Scenario: A failed probe blocks the scenario shards
- **WHEN** the connectivity probe fails (no SLURM job can be submitted or reclaimed)
- **THEN** the scenario shard targets SHALL NOT run, and the failure SHALL surface the cluster-wiring problem rather than a scenario error

### Requirement: Shards are the unit of parallelism dispatched to SLURM jobs
The pipeline SHALL dispatch the scenario's per-shard step targets across SLURM jobs as the unit of parallelism: independent shard targets SHALL run concurrently under the SLURM controller, regardless of how the underlying `crew`/`targets` configuration packs shard targets into jobs (one shard target per job or several packed into one). The template SHALL document the chosen shard-target-to-job packing convention (`TARGETS-DESIGN.md` §11 Q6).

#### Scenario: Independent shards run concurrently across jobs
- **WHEN** a scenario with multiple independent shard targets is built through the `cluster/` template on a SLURM queue
- **THEN** the independent shard targets SHALL be dispatched to SLURM jobs and run concurrently, each writing its own shard Parquet

#### Scenario: The packing convention is documented, not hard-coded as 1:1
- **WHEN** a reader inspects the template
- **THEN** the shard-target-to-SLURM-job packing convention SHALL be documented (and read as "many-to-one or one-to-one depending on configuration"), with shards remaining the unit of parallelism either way

### Requirement: The template guards when crew.cluster or a SLURM queue is unavailable
`crew.cluster` SHALL be declared in `DESCRIPTION` `Suggests` (the cluster path is opt-in; the package SHALL build and test without a scheduler). The `run.R` driver SHALL guard cleanly when `crew.cluster` is not installed or no SLURM queue is reachable — skipping or aborting with a clear message rather than erroring obscurely — and SHALL offer a `crew::crew_controller_local()` smoke path for off-cluster validation, mirroring the crew labs' local fallback (`TARGETS-DESIGN.md` §4).

#### Scenario: Missing crew.cluster skips or aborts cleanly
- **WHEN** the `cluster/` driver is run where `crew.cluster` is not installed
- **THEN** it SHALL skip or abort with a clear message stating `crew.cluster` is required, rather than failing with an obscure error

#### Scenario: No reachable SLURM queue degrades to a local smoke path
- **WHEN** the driver is run off-cluster with no reachable SLURM queue
- **THEN** it SHALL fall back to a `crew::crew_controller_local()` smoke path (or skip cleanly), so the pipeline can be validated without a scheduler
