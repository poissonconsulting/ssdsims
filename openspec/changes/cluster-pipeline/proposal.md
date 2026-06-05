## Why

`TARGETS-DESIGN.md` §4 describes the package's whole point: take a scenario object that already runs locally and dispatch its shards across a SLURM cluster, changing **only** the `crew` controller and its resource specs — pipeline shape (from ingredient A) and task content + RNG (from ingredient C) are scheduler-independent. The crew labs (sister planning repo) have already assembled and run that shape end to end: a bare `crew.cluster::crew_controller_slurm()` dispatching a no-op workload (A), the operational backend it needs — a ManyLinux binary install path plus a login-node prerequisite checker (B) — and the real ssdsims per-sim pipeline driven through that same controller (C). So `cluster-pipeline` (§12) **ports a proven shape, not an untested sketch**.

Today the package ships `inst/targets-templates/small/` and `large/`; the `large/` template only *gestures* at this ("swap in `crew.cluster` controllers for SLURM/PBS … (`cluster-pipeline`)") with a local `crew::crew_controller_local()` controller. There is no `cluster/` template, no `crew_controller_slurm()` wiring, and `crew.cluster` is not in `DESCRIPTION`. A user who wants to run a study on a cluster has to invent the controller block from scratch — exactly the wiring the labs already de-risked.

## What Changes

- Add `inst/targets-templates/cluster/` (the editable template, resolving §11 Q5 in favour of *ship a template*, not docs-only): a `_targets.R` that builds the scenario's pipeline via the **existing** `ssd_scenario_targets()` factory (the shape, A; the content, C — both unchanged from `large/`), but sets `tar_option_set(controller = crew.cluster::crew_controller_slurm(...))` (the backend, B) with editable `script_lines` (module loads), `slurm_partition`/queue, and scratch/worker config. The factory call and scenario are scheduler-independent and reused verbatim; only the controller block is cluster-specific.
- A controller block that the user edits per cluster: queue/partition, `script_lines` (e.g. `module load R/4.x`), scratch/`tempdir`, worker count, and walltime — drafted from the labs' working block (B), not invented.
- A **connectivity probe run before any scenario logic**: a trivial no-op target (ingredient A's bare shape) that proves the controller can submit and reclaim one Slurm job. `tar_make()` builds this first; if it fails, the scenario shards never run.
- A `run.R` driver mirroring `large/run.R`: source the scenario, `tar_make()`, report the summary path and a peek at the estimates — plus a guard that **skips/aborts cleanly** when `crew.cluster` is not installed or no Slurm queue is reachable, falling back to a `crew::crew_controller_local()` smoke path off-cluster (mirroring the labs' fallback).
- Add `crew.cluster` to `DESCRIPTION` `Suggests` (this proposal *describes* the addition; it is applied by the implementing change, listed in tasks).
- End-to-end validation: `tar_make()` on a real (or sandboxed) Slurm queue builds the probe then the scenario shards, byte-identical to the local pipeline's per-task results.

## Capabilities

### New Capabilities
- `cluster-pipeline`: an editable cluster targets template that builds a scenario via the shared `ssd_scenario_targets()` factory under a SLURM `crew.cluster` controller, validates cluster connectivity before scenario logic, dispatches shards as the unit of parallelism across Slurm jobs, and degrades cleanly when `crew.cluster` or a Slurm queue is unavailable.

### Modified Capabilities
<!-- None: the `task-shards` factory (`ssd_scenario_targets()`) is reused unchanged; this change adds a new template around it and does not alter any existing requirement. -->

## Impact

- **New files**: `inst/targets-templates/cluster/` — `_targets.R`, `scenario.R`, `run.R` (and a `run-serial.R` mirror for the off-cluster comparison, matching the `small/`/`large/` shape).
- **Dependencies**: adds `crew.cluster` to `DESCRIPTION` `Suggests` (`crew` is already there). Both stay in `Suggests` — the cluster path is opt-in and the package builds/tests without a scheduler.
- **No R code or live-spec changes**: the pipeline is assembled by the existing `ssd_scenario_targets()` factory (`task-shards`); this change adds only template files and the controller block around it. Scheduler-independence (§4) is the reason no factory change is needed.
- **Dependencies (direction)**: depends on `task-shards` (the `ssd_scenario_targets()` factory) and the scenario (`ssd-define-scenario`); it is **not** a prerequisite of any other step. `TARGETS-DESIGN.md` §12 DAG node: `task-tables → cluster-pipeline` (the factory must exist; the template wraps it).
- **Resolves open questions**: §11 Q5 (ship the editable `inst/targets-templates/cluster/` template, not docs-only) and Q6 (commits only to *shards being the unit of parallelism*; the precise shard-target ↔ Slurm-job packing is a documented `crew` configuration knob, recorded in this template's prototype).
- **When to land it**: any time after `task-tables`/`task-shards` (the factory) lands; operationally, when the first study is to be run on a cluster. Not needed for the local/small/large templates.
