## Context

`TARGETS-DESIGN.md` §4 ("From local to a cluster") states the package's whole point: a scenario object that already runs locally is dispatched across a SLURM cluster by changing **only** the `crew` controller and its resource specs. Three equally-weighted ingredients assemble into the cluster `_targets.R`: **A** the *shape* (how a `crew` controller is constructed and per-shard branching wired — lifted as a skeleton), **B** the *backend* (a `crew.cluster::crew_controller_slurm()` with the cluster's queue, module loads, and scratch paths, validated by submitting one trivial job end-to-end **before any ssdsims logic is involved**), and **C** the *content* (the scenario: `seed`, dataset names, fit/hc grids — already exercised locally with `ssd_run_scenario()`). §4 records that the crew labs have already assembled and run A + B + C end to end on a SLURM cluster (the bare no-op shape, the real ssdsims pipeline through the same controller, a ManyLinux binary install path, and a login-node prerequisite checker), with a `crew::crew_controller_local()` fallback for off-cluster smoke tests. So `cluster-pipeline` (§12) **ports a proven shape, not an untested sketch**.

Today the package ships `inst/targets-templates/small/` and `large/`. The `large/` template assembles the pipeline with the shared `ssd_scenario_targets()` factory (the `task-shards` capability) under a local `crew::crew_controller_local(workers = 8L)` controller, and only *gestures* at the cluster path in a comment ("swap in `crew.cluster` controllers for SLURM/PBS … (`cluster-pipeline`)"). There is no `cluster/` template, no `crew_controller_slurm()` wiring, and `crew.cluster` is absent from `DESCRIPTION`. This change adds the `cluster/` template around the **unchanged** factory and adds `crew.cluster` to `Suggests`.

## Goals / Non-Goals

**Goals:**

- An editable `inst/targets-templates/cluster/` whose `_targets.R` builds the scenario via the existing `ssd_scenario_targets()` factory, with the SLURM controller as the only cluster-specific code (ingredient A's shape + B's backend + C's scenario, assembled).
- A SLURM `crew.cluster::crew_controller_slurm()` block (queue/partition, `script_lines`, scratch, workers, walltime) drafted from the labs' validated block, editable in one place to retarget another cluster.
- A connectivity probe that submits and reclaims one trivial SLURM job **before** any scenario shard runs.
- A `run.R` driver mirroring `large/run.R`, with a clean guard/fallback when `crew.cluster` or a SLURM queue is unavailable.
- End-to-end validation: `tar_make()` on a real or sandboxed SLURM queue builds the probe then the scenario shards, byte-identical to the local pipeline.

**Non-Goals:**

- Changing the pipeline shape, the per-shard runners, or the `ssd_scenario_targets()` factory (that is `task-shards`); the factory is reused verbatim and scheduler-independent (§4).
- Discovering the crew + SLURM wiring (the crew labs already validated it — this is a port).
- The ManyLinux binary install path and login-node prerequisite checker as *package* code (operational backend B; the template documents/links them, it does not own them).
- Finer per-task failure survival on the cluster (`shard-failure-survival`, which depends on this step) and provenance recording (`manifest` / `cloud-upload`).
- Editing R code, `DESCRIPTION`, or live specs in this proposal — it *describes* adding `crew.cluster` to `Suggests`; the implementing change applies it (see tasks).

## Decisions

### Decision: ship an editable template, not docs-only (resolves §11 Q5)

§11 Q5 asks whether to ship a single editable `inst/targets-templates/cluster/` that the authoring prompt edits, or only documentation pointing at `crew.cluster` examples. This change resolves it in favour of **ship the editable template**. Rationale: the package already ships `small/` and `large/` templates that users copy-and-edit; a `cluster/` sibling is the consistent, discoverable artifact (reachable via `system.file("targets-templates", "cluster", package = "ssdsims")`), and the labs already produced the working block the template embeds. Docs-only would re-impose the from-scratch controller wiring this step exists to remove. *Alternative considered:* a vignette pointing at upstream `crew.cluster` examples — rejected as less actionable and inconsistent with the existing template trio.

### Decision: the controller block is the only cluster-specific code; the factory and scenario are reused verbatim

Per §4, only the controller and resource specs change between clusters; pipeline shape (A) and content + RNG (C) are scheduler-independent. So `cluster/_targets.R` is `large/_targets.R` with the `crew::crew_controller_local(...)` line replaced by a `crew.cluster::crew_controller_slurm(...)` block, plus the probe target. `source("scenario.R")` and `ssd_scenario_targets(scenario)` are byte-for-byte the same as `large/`. This keeps the per-task results byte-identical to the local pipeline (validated against the `task-shards` baseline oracle) and means a future scheduler change touches one block.

### Decision: a connectivity probe is the first target, before any scenario logic (ingredient B)

§4 ingredient B is validated by "submitting one trivial job end-to-end **before any ssdsims logic is involved**." The template encodes this as a no-op probe target (e.g. a target returning a constant computed on a worker) that `tar_make()` builds first; the scenario shard targets depend on the run reaching them, so a probe failure surfaces the cluster-wiring problem rather than a scenario error. This is the bare shape (A) used as a guard, not just a lab artifact.

### Decision: shards are the unit of parallelism; the shard-target-to-job packing is a documented knob (resolves §11 Q6)

§11 Q6 notes that `targets` + `crew` dispatch the per-shard targets across SLURM jobs, but the precise mapping — one shard target per job vs several packed into one — is a `crew` configuration knob. This change commits only to **shards being the unit of parallelism**: independent shard targets run concurrently regardless of packing. The template records the chosen packing convention as a documented `crew` setting (read as "many-to-one or one-to-one depending on configuration"), prototyped here, rather than hard-coding a 1:1 contract. This matches the `task-shards` factory, which already mints one target per `partition_by` path cell.

### Decision: `crew.cluster` stays in `Suggests`; the path is opt-in with a clean guard and local fallback

`crew` is already in `Suggests`; `crew.cluster` joins it there (not `Imports`) so the package builds, checks, and runs the local templates without a scheduler. The `run.R` driver guards: if `crew.cluster` is not installed or no SLURM queue is reachable, it skips/aborts with a clear message and offers a `crew::crew_controller_local()` smoke path for off-cluster validation — mirroring the crew labs' local fallback (§4). This keeps CI and laptops green while the cluster path remains a copy-edit-run away.

## Risks / Trade-offs

- **No SLURM in CI.** End-to-end validation needs a real or sandboxed SLURM queue, which CI may lack → mitigated: the local `crew_controller_local()` fallback smoke-tests the *shape* offline (byte-identical to `large/`), and the SLURM end-to-end run is a documented manual/lab validation step (the labs already ran it).
- **Cluster-specific controller values drift.** Queue names, module versions, and scratch paths differ per site and age → mitigated: the block is confined to one editable region with commented placeholders drafted from the labs' working values, and retargeting edits only that block.
- **Probe masks scenario-level failures.** A passing probe proves wiring, not that a heavy shard fits walltime/memory → accepted: the probe's job is connectivity (B), not capacity; per-task/per-shard failure handling is `error = "null"` (already in the factory) and the downstream `shard-failure-survival` step.

## Open Questions

- **§11 Q5 — template vs docs-only.** Resolved here: ship the editable `inst/targets-templates/cluster/` template (a sibling to `small/`/`large/`), not docs-only. Recorded so the resolution is traceable to the question.
- **§11 Q6 — shard-target ↔ SLURM-job packing.** This change commits only to shards being the unit of parallelism; the exact packing (one shard target per job vs several per job) is a `crew` configuration knob. The template prototypes and documents a packing convention; until a study fixes a site default, `shard target ↔ job` is read as "many-to-one or one-to-one, depending on configuration." Choosing a recommended site default is deferred to the first real cluster study.
