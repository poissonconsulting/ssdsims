## ADDED Requirements

### Requirement: A cluster targets template builds a scenario via the shared factory
The package SHALL ship an editable cluster template at `inst/targets-templates/cluster/` whose `_targets.R` builds a scenario's whole pipeline via the existing `ssd_scenario_targets()` factory (the `task-shards` capability), reusing the scenario shape and factory call **verbatim** from the `large/` template. `_targets.R` SHALL stay a clean scenario-and-factory definition: it SHALL reduce to setting the controller (sourced from the separate `controller.R`), defining the scenario object inline, and calling the factory, carrying no probe/preflight code of its own; the pipeline shape and the per-task content/RNG SHALL be scheduler-independent and unchanged (`TARGETS-DESIGN.md` §4). The template SHALL be minimal — only the files that cannot be inlined are separate (`controller.R`, shared by the pipeline and the preflight; `preflight.R`; `run.R`) — and SHALL include a `run.R` driver mirroring `large/run.R` (run the preflight, `tar_make()`, report the summary path and a peek at the estimates).

#### Scenario: The cluster _targets.R reduces to a scenario plus the factory call
- **WHEN** the `cluster/` template's `_targets.R` is sourced for a scenario
- **THEN** the pipeline SHALL be assembled by `ssd_scenario_targets(scenario)` (the same factory the `large/` template uses) over an inline scenario object, the only cluster-specific code SHALL be the controller block sourced from `controller.R`, and `_targets.R` SHALL contain no probe/preflight target (the connectivity check is the standalone `preflight.R`)

#### Scenario: Per-task results are scheduler-independent
- **WHEN** the same scenario is built through the `cluster/` template and through the `large/` (local) template
- **THEN** the per-task `sample`, `fit`, and `hc` results SHALL be byte-identical as read-back R values (sorted by the task-identity key), because only the controller differs between the two templates

### Requirement: The SLURM crew controller is configured for the target cluster
The `cluster/` template SHALL configure a `crew.cluster::crew_controller_slurm(...)` with an editable resource block — the queue/`slurm_partition`, `script_lines` (e.g. module loads such as `module load R/4.x`), scratch/`tempdir`, worker count, and walltime — drafted from the crew labs' validated working block (`TARGETS-DESIGN.md` §4 ingredient B), not invented. This controller block SHALL live in a single file (`controller.R`), sourced by both `_targets.R` (which passes it to `tar_option_set()`) and the preflight, so it is the single place a user edits to retarget another SLURM cluster.

#### Scenario: The controller is a configured SLURM crew controller
- **WHEN** the `cluster/` template's `controller.R` is sourced on a cluster with `crew.cluster` available
- **THEN** it SHALL construct a `crew.cluster::crew_controller_slurm()` configured with the cluster's queue/partition, `script_lines`, scratch path, worker count, and walltime, which `_targets.R` installs via `tar_option_set()`

#### Scenario: Retargeting another cluster edits only the controller block
- **WHEN** a user moves the template to a different SLURM cluster
- **THEN** the only file they need to edit SHALL be `controller.R` (queue, `script_lines`, scratch, workers, walltime); the scenario, `_targets.R`, and the `ssd_scenario_targets()` call SHALL remain unchanged

### Requirement: Cluster connectivity and worker prerequisites are validated before scenario logic
The template SHALL validate, before any scenario logic runs, both that the controller can dispatch a SLURM job and that a worker can actually run ssdsims. It SHALL do this with a **standalone preflight** (`preflight.R`), kept out of the main pipeline (`_targets.R`), that (a) submits and reclaims one SLURM job through the same controller (`controller.R`) the pipeline uses and (b) inside that job verifies the worker prerequisites: R resolves at the expected version, `library(ssdsims)` loads (the ManyLinux binary install path), and the scratch/`tempdir` is writable. The preflight SHALL report a small witness (e.g. the worker R version and node id) on success. The `run.R` driver SHALL run the preflight **before** `tar_make()`, so a preflight failure SHALL prevent the scenario shards from running (`TARGETS-DESIGN.md` §4 ingredient B). On failure the preflight SHALL abort with an actionable message naming which check failed (no job dispatched, R or ssdsims unavailable on the worker, or scratch not writable), so the user fixes the wiring or prerequisite rather than debugging a scenario error.

#### Scenario: The preflight runs before the pipeline
- **WHEN** `run.R` is run on the `cluster/` template
- **THEN** the preflight SHALL submit and reclaim one SLURM job through the controller before `tar_make()` is called, so no scenario shard runs until the preflight passes

#### Scenario: The preflight verifies worker prerequisites, not just dispatch
- **WHEN** a SLURM job can be dispatched but the worker cannot load `ssdsims` (e.g. the R module is not loaded or the binary path is missing) or cannot write scratch
- **THEN** the preflight SHALL fail with a message identifying the missing prerequisite, rather than passing on dispatch alone and letting a shard fail obscurely

#### Scenario: A failed preflight blocks the scenario shards
- **WHEN** the preflight fails (no SLURM job can be submitted or reclaimed, or a worker prerequisite is missing)
- **THEN** `run.R` SHALL abort before `tar_make()`, so the scenario shards SHALL NOT run, and the failure SHALL surface the cluster-wiring or prerequisite problem rather than a scenario error

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
The package SHALL document the path from a user's own cluster instructions to a running scenario job, and SHALL NOT assume the reader already knows `crew` or `targets`. The documentation SHALL begin from the site's non-R SLURM usage instructions and proceed in four steps: (1) a mapping from each piece of site information — login node, job submission, partition/queue, account/allocation, the module system that provides R, the scratch filesystem, walltime, and cores — to the `crew.cluster::crew_controller_slurm()` argument it sets; (2) running the standalone connectivity-and-prerequisite preflight to confirm the mapping; (3) running a minimal (shrunken) scenario end-to-end through the `cluster/` template as the first job; and (4) swapping in the user's own scenario by editing the inline scenario block in `_targets.R`. The guide SHALL ship as a rendered vignette ("Running on a SLURM Cluster") and SHALL be mirrored in the template's own `README.md`, so it is discoverable both on the package site and alongside the files. The guide SHALL also note that, while the example targets SLURM, `crew.cluster` provides controllers for other schedulers (SGE, PBS/TORQUE, LSF) that may also work though they are untested here, and SHALL state what changes to retarget one (the ingredient-B controller constructor and its `crew_options_*()` function, with `script_lines` carrying over and the named resource arguments differing).

#### Scenario: The guide notes other (untested) crew.cluster backends
- **WHEN** a reader wants to run the pipeline on a non-SLURM scheduler
- **THEN** the guide SHALL list the `crew.cluster` controllers for SGE, PBS/TORQUE, and LSF, mark them supported-but-untested, and show that only ingredient B (the controller constructor and its options function) changes — the factory, the scenario, and the preflight are unchanged

#### Scenario: The guide ships as a cluster vignette
- **WHEN** a reader browses the package's vignettes/articles
- **THEN** a "Running on a SLURM Cluster" vignette SHALL present the four-step zero-to-running guide, cross-linked with the sharded-pipeline vignette, and the same guide SHALL be available as the `cluster/` template's `README.md`

#### Scenario: Site instructions map to controller arguments
- **WHEN** a user has their cluster's own (non-R) instructions for partition/queue, account, module loads for R, scratch path, and walltime
- **THEN** the documentation SHALL show which `crew.cluster::crew_controller_slurm()` argument each piece maps to, so the user can fill the controller block without prior `crew` knowledge

#### Scenario: The minimal first job uses a shrunken scenario
- **WHEN** a user follows the guide after configuring the controller and passing the preflight
- **THEN** the guide SHALL have them shrink the inline scenario block to a minimal cheap job and run it end-to-end through the `cluster/` template, before adapting their own scenario

#### Scenario: Adapting to the user's own scenario is the final step
- **WHEN** the minimal run has succeeded
- **THEN** the guide SHALL show the user editing the inline scenario block in `_targets.R` to their own study, with the controller block and the `ssd_scenario_targets()` call unchanged
