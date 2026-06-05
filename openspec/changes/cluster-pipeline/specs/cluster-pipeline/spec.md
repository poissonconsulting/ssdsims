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

### Requirement: Cluster connectivity and worker prerequisites are validated before scenario logic
The template SHALL validate, before any scenario logic runs, both that the controller can dispatch a SLURM job and that a worker can actually run ssdsims. It SHALL do this with a probe target that (a) submits and reclaims one SLURM job through the controller and (b) inside that job verifies the worker prerequisites: R resolves at the expected version, `library(ssdsims)` loads (the ManyLinux binary install path), and the scratch/`tempdir` is writable. The probe SHALL return a small witness (e.g. the worker R version and node id). `tar_make()` SHALL build the probe first, and the first scenario shard target SHALL name the probe as an explicit upstream dependency, so a probe failure SHALL prevent the scenario shard targets from running (`TARGETS-DESIGN.md` §4 ingredient B). On failure the probe SHALL abort with an actionable message naming which check failed (no job dispatched, R or ssdsims unavailable on the worker, or scratch not writable), so the user fixes the wiring or prerequisite rather than debugging a scenario error.

#### Scenario: The probe is the first target built
- **WHEN** `tar_make()` is run on the `cluster/` template
- **THEN** the probe target SHALL be submitted to and reclaimed from one SLURM job before any scenario shard target runs, and the first scenario shard SHALL depend on it

#### Scenario: The probe verifies worker prerequisites, not just dispatch
- **WHEN** a SLURM job can be dispatched but the worker cannot load `ssdsims` (e.g. the R module is not loaded or the binary path is missing) or cannot write scratch
- **THEN** the probe SHALL fail with a message identifying the missing prerequisite, rather than passing on dispatch alone and letting the first shard fail obscurely

#### Scenario: A failed probe blocks the scenario shards
- **WHEN** the probe fails (no SLURM job can be submitted or reclaimed, or a worker prerequisite is missing)
- **THEN** the scenario shard targets SHALL NOT run, and the failure SHALL surface the cluster-wiring or prerequisite problem rather than a scenario error

### Requirement: Shards are the unit of parallelism dispatched to SLURM jobs
The pipeline SHALL dispatch the scenario's per-shard step targets across SLURM jobs as the unit of parallelism: independent shard targets SHALL run concurrently under the SLURM controller, regardless of how the underlying `crew`/`targets` configuration packs shard targets into jobs (one shard target per job or several packed into one). The template SHALL document the chosen shard-target-to-job packing convention (`TARGETS-DESIGN.md` §11 Q6).

#### Scenario: Independent shards run concurrently across jobs
- **WHEN** a scenario with multiple independent shard targets is built through the `cluster/` template on a SLURM queue
- **THEN** the independent shard targets SHALL be dispatched to SLURM jobs and run concurrently, each writing its own shard Parquet

#### Scenario: The packing convention is documented, not hard-coded as 1:1
- **WHEN** a reader inspects the template
- **THEN** the shard-target-to-SLURM-job packing convention SHALL be documented (and read as "many-to-one or one-to-one depending on configuration"), with shards remaining the unit of parallelism either way

### Requirement: The template guards when crew.cluster or a SLURM queue is unavailable
`crew.cluster` SHALL be declared in `DESCRIPTION` `Suggests` (the cluster path is opt-in; the package SHALL build and test without a scheduler). The `run.R` driver SHALL guard cleanly when `crew.cluster` is not installed or no SLURM queue is reachable — aborting (or skipping) with a clear message naming the missing prerequisite rather than erroring obscurely — and SHALL point the user at the local `large/` template (the identical factory + scenario under a `crew::crew_controller_local()` controller) for off-cluster runs. The `cluster/` template SHALL NOT carry its own local-controller fallback: a local run is already served by `large/`, so the cluster template stays focused on the SLURM path.

#### Scenario: Missing crew.cluster aborts cleanly
- **WHEN** the `cluster/` driver is run where `crew.cluster` is not installed
- **THEN** it SHALL abort (or skip) with a clear message stating `crew.cluster` is required, rather than failing with an obscure error

#### Scenario: No reachable SLURM queue aborts cleanly
- **WHEN** the driver is run off-cluster with no reachable SLURM queue
- **THEN** it SHALL abort (or skip) with a clear message naming the missing `sbatch`/queue prerequisite and pointing at the `large/` template for off-cluster runs, rather than erroring obscurely

### Requirement: Documentation takes a user from zero to a running cluster job
The package SHALL document the path from a user's own cluster instructions to a running scenario job, and SHALL NOT assume the reader already knows `crew` or `targets`. The documentation SHALL begin from the site's non-R SLURM usage instructions and proceed in four steps: (1) a mapping from each piece of site information — login node, job submission, partition/queue, account/allocation, the module system that provides R, the scratch filesystem, walltime, and cores — to the `crew.cluster::crew_controller_slurm()` argument it sets; (2) running the connectivity-and-prerequisite probe to confirm the mapping; (3) running the built-in `small` scenario end-to-end through the `cluster/` template as the minimal first job; and (4) swapping in the user's own scenario by editing `scenario.R`.

#### Scenario: Site instructions map to controller arguments
- **WHEN** a user has their cluster's own (non-R) instructions for partition/queue, account, module loads for R, scratch path, and walltime
- **THEN** the documentation SHALL show which `crew.cluster::crew_controller_slurm()` argument each piece maps to, so the user can fill the controller block without prior `crew` knowledge

#### Scenario: The minimal first job uses the built-in small scenario
- **WHEN** a user follows the guide after configuring the controller and passing the probe
- **THEN** the guide SHALL have them run the built-in `small` scenario end-to-end through the `cluster/` template as the minimal first job, before adapting their own scenario

#### Scenario: Adapting to the user's own scenario is the final step
- **WHEN** the minimal `small` run has succeeded
- **THEN** the guide SHALL show the user editing `scenario.R` to their own study, with the controller block and the `ssd_scenario_targets()` call unchanged
