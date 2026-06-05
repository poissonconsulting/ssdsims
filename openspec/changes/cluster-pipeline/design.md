## Context

`TARGETS-DESIGN.md` §4 ("From local to a cluster") states the package's whole point: a scenario object that already runs locally is dispatched across a SLURM cluster by changing **only** the `crew` controller and its resource specs. Three equally-weighted ingredients assemble into the cluster `_targets.R`: **A** the *shape* (how a `crew` controller is constructed and per-shard branching wired — lifted as a skeleton), **B** the *backend* (a `crew.cluster::crew_controller_slurm()` with the cluster's queue, module loads, and scratch paths, validated by submitting one trivial job end-to-end **before any ssdsims logic is involved**), and **C** the *content* (the scenario: `seed`, dataset names, fit/hc grids — already exercised locally with `ssd_run_scenario()`). §4 records that the crew labs have already assembled and run A + B + C end to end on a SLURM cluster (the bare no-op shape, the real ssdsims pipeline through the same controller, a ManyLinux binary install path, and a login-node prerequisite checker), with a `crew::crew_controller_local()` fallback for off-cluster smoke tests. So `cluster-pipeline` (§12) **ports a proven shape, not an untested sketch**.

Today the package ships `inst/targets-templates/small/` and `large/`. The `large/` template assembles the pipeline with the shared `ssd_scenario_targets()` factory (the `task-shards` capability) under a local `crew::crew_controller_local(workers = 8L)` controller, and only *gestures* at the cluster path in a comment ("swap in `crew.cluster` controllers for SLURM/PBS … (`cluster-pipeline`)"). There is no `cluster/` template, no `crew_controller_slurm()` wiring, and `crew.cluster` is absent from `DESCRIPTION`. This change adds the `cluster/` template around the **unchanged** factory and adds `crew.cluster` to `Suggests`.

## Goals / Non-Goals

**Goals:**

- An editable `inst/targets-templates/cluster/` whose `_targets.R` builds the scenario via the existing `ssd_scenario_targets()` factory, with the SLURM controller as the only cluster-specific code (ingredient A's shape + B's backend + C's scenario, assembled).
- A SLURM `crew.cluster::crew_controller_slurm()` block (queue/partition, `script_lines`, scratch, workers, walltime) drafted from the labs' validated block, editable in one place to retarget another cluster.
- A probe that, **before** any scenario shard runs, both proves connectivity (submit + reclaim one SLURM job) and verifies worker prerequisites (R resolves, `library(ssdsims)` loads via the ManyLinux binary path, scratch/`tempdir` writable), as an explicit upstream dependency of the first shard, aborting with an actionable message that names which check failed.
- A "zero to a running cluster job" guide that bridges the cluster's own (non-R) SLURM usage instructions to the `crew_controller_slurm()` arguments, then runs the built-in `small` scenario as the minimal first job, then adapts to the user's own scenario.
- A `run.R` driver mirroring `large/run.R`, with a clean guard/fallback when `crew.cluster` or a SLURM queue is unavailable.
- End-to-end validation: `tar_make()` on a real or sandboxed SLURM queue builds the probe then the scenario shards, byte-identical to the local pipeline.

**Non-Goals:**

- Changing the pipeline shape, the per-shard runners, or the `ssd_scenario_targets()` factory (that is `task-shards`); the factory is reused verbatim and scheduler-independent (§4).
- Discovering the crew + SLURM wiring (the crew labs already validated it — this is a port).
- *Owning* the ManyLinux binary install path as package code (operational backend B; the template documents/links it, it does not provide it). The probe *verifies* the worker prerequisites at run time (folding in the labs' login-node prerequisite checker), but it does not install or own the binary path itself.
- Finer per-task failure survival on the cluster (`shard-failure-survival`, which depends on this step) and provenance recording (`manifest` / `cloud-upload`).
- Editing R code, `DESCRIPTION`, or live specs in this proposal — it *describes* adding `crew.cluster` to `Suggests`; the implementing change applies it (see tasks).

## Decisions

### Decision: ship an editable template, not docs-only (resolves §11 Q5)

§11 Q5 asks whether to ship a single editable `inst/targets-templates/cluster/` that the authoring prompt edits, or only documentation pointing at `crew.cluster` examples. This change resolves it in favour of **ship the editable template**. Rationale: the package already ships `small/` and `large/` templates that users copy-and-edit; a `cluster/` sibling is the consistent, discoverable artifact (reachable via `system.file("targets-templates", "cluster", package = "ssdsims")`), and the labs already produced the working block the template embeds. Docs-only would re-impose the from-scratch controller wiring this step exists to remove. *Alternative considered:* a vignette pointing at upstream `crew.cluster` examples — rejected as less actionable and inconsistent with the existing template trio.

### Decision: the controller block is the only cluster-specific code; the factory and scenario are reused verbatim

Per §4, only the controller and resource specs change between clusters; pipeline shape (A) and content + RNG (C) are scheduler-independent. So `cluster/_targets.R` is `large/_targets.R` with the `crew::crew_controller_local(...)` line replaced by a `crew.cluster::crew_controller_slurm(...)` block, plus the probe target. `source("scenario.R")` and `ssd_scenario_targets(scenario)` are byte-for-byte the same as `large/`. This keeps the per-task results byte-identical to the local pipeline (validated against the `task-shards` baseline oracle) and means a future scheduler change touches one block.

### Decision: the probe validates connectivity AND worker prerequisites, before any scenario logic (ingredient B)

§4 ingredient B is validated by "submitting one trivial job end-to-end **before any ssdsims logic is involved**." The template encodes this as a probe target that `tar_make()` builds first and that the first scenario shard target names as an **explicit upstream dependency** (so a probe failure blocks the shards rather than surfacing later as a scenario error). The probe does two things, not one: **(1) connectivity** — it submits and reclaims one SLURM job through the controller; **(2) worker prerequisites** — inside that job it verifies the operational backend the labs pinned (ingredient B): R resolves at the expected version, `library(ssdsims)` loads from the ManyLinux binary path, and the scratch/`tempdir` is writable. It returns a small witness (e.g. the worker's R version and node id). On failure it aborts with an **actionable message naming which check failed** — no job submitted → controller/queue/account wiring; job ran but R or ssdsims missing → module-load / install-path; scratch not writable → storage config — so the user fixes the wiring or prerequisite, not a scenario bug. A bare no-op job would prove dispatch but not that a worker can actually load ssdsims and write a shard (the most common real failure), so the prerequisite checks are part of the probe, folding in the labs' login-node prerequisite checker. *Alternative considered:* a bare no-op probe with prerequisites left to a separate manual preflight — rejected; the prerequisite failure is exactly what the probe should catch before the expensive shards run.

### Decision: documentation bridges the cluster's non-R instructions to the controller, then to a minimal run

A user's real starting point is their **site's own cluster instructions** — how to log in, submit an `sbatch` job, which partition/queue and account/allocation to use, the `module` system that provides R, the scratch filesystem, and walltime/core limits — none of it about R, `crew`, or `targets`. The template's documentation SHALL outline the path from those non-R instructions to a running job:

1. **Map the site information to the controller** — a table from each piece of site instruction to the `crew.cluster::crew_controller_slurm()` argument it sets: partition/queue → `slurm_partition`, `module load R/4.x …` → `script_lines`, account/allocation → the controller's options, scratch path → the worker `tempdir`/storage, walltime and cores → the resource arguments.
2. **Confirm the mapping with the probe** (connectivity + worker prerequisites, above): a green probe means the controller block matches the site.
3. **Run the built-in `small` scenario end-to-end** through the `cluster/` template as the minimal first job.
4. **Swap in the user's own scenario** (edit `scenario.R`).

The three journeys the review asks for map onto these steps: the *built-in cluster + targets example* is step 3 (the `small` scenario through the `cluster/` template); the *non-targets instructions for their cluster* are step 1 (reading the site's own, non-R job-submission instructions into the controller block); the *particular scenario* is step 4. *Alternative considered:* documenting only a finished controller block — rejected; users cannot reach a finished block without the site-instruction → argument mapping, which is the actual gap.

### Decision: shards are the unit of parallelism; the shard-target-to-job packing is a documented knob (resolves §11 Q6)

§11 Q6 notes that `targets` + `crew` dispatch the per-shard targets across SLURM jobs, but the precise mapping — one shard target per job vs several packed into one — is a `crew` configuration knob. This change commits only to **shards being the unit of parallelism**: independent shard targets run concurrently regardless of packing. The template records the chosen packing convention as a documented `crew` setting (read as "many-to-one or one-to-one depending on configuration"), prototyped here, rather than hard-coding a 1:1 contract. This matches the `task-shards` factory, which already mints one target per `partition_by` path cell.

### Decision: `crew.cluster` stays in `Suggests`; the path is opt-in with a clean guard

`crew` is already in `Suggests`; `crew.cluster` joins it there (not `Imports`) so the package builds, checks, and runs the local templates without a scheduler. The `run.R` driver guards: if `crew.cluster` is not installed or no SLURM queue is reachable, it aborts with a clear message naming the missing prerequisite and points the user at the local `large/` template for off-cluster runs. The `cluster/` template does **not** carry its own `crew::crew_controller_local()` fallback: a local run of the same study is already served by `large/` (the identical factory + scenario under a local controller), so duplicating a local controller inside the cluster template — behind a runtime toggle in `_targets.R` — adds a configuration branch without added value and muddies the "one editable controller block" story. The cluster template stays focused on the SLURM path and aborts cleanly off-cluster. *Alternative considered:* an in-template `crew_controller_local()` smoke path toggled by an environment variable — rejected; it overlaps `large/` and complicates the single-controller-block invariant. The template's wiring is instead kept testable without a scheduler by static `targets`-graph analysis (below).

### Decision: probe body and gating glue live in a sourced `functions.R`, with the wiring covered by a graph test

`_targets.R` stays a readable *pipeline definition* — the editable controller block, the scenario, the factory call, and the probe wiring — rather than carrying the probe's body and the shard-gating helper inline. Those two helpers move to a sourced `functions.R` (the same copy-and-source pattern the template already uses for `scenario.R`), so the example file is not one long inline script. The probe remains a pipeline *target* (the spec requires it to gate the shards via `tar_make()`), not a separate manual preflight — only its implementation is factored out. The gating is verified by a `tests/testthat/test-cluster-pipeline.R` graph test that copies the shipped template, statically analyses its `targets` network (no scheduler), and asserts `probe` is an upstream edge of every entry-step shard. *Alternative considered:* keeping everything inline in `_targets.R` — rejected as hard to read and untestable; promoting the probe to exported package R code with unit tests — rejected as outside this change's scope (no R/ or live-spec edits; §Non-Goals).

## Risks / Trade-offs

- **No SLURM in CI.** End-to-end validation needs a real or sandboxed SLURM queue, which CI may lack → mitigated: the *shape* is already validated offline by the `large/` template (the identical factory + scenario, asserted byte-identical to the single-core oracle) and by a `targets`-graph test that statically asserts the cluster template's probe gates the scenario shards (no scheduler, no jobs dispatched); the SLURM end-to-end run is a documented manual/lab validation step (the labs already ran it).
- **Cluster-specific controller values drift.** Queue names, module versions, and scratch paths differ per site and age → mitigated: the block is confined to one editable region with commented placeholders drafted from the labs' working values, and retargeting edits only that block.
- **Probe masks scenario-level failures.** A passing probe proves wiring, not that a heavy shard fits walltime/memory → accepted: the probe's job is connectivity (B), not capacity; per-task/per-shard failure handling is `error = "null"` (already in the factory) and the downstream `shard-failure-survival` step.

## Open Questions

- **§11 Q5 — template vs docs-only.** Resolved here: ship the editable `inst/targets-templates/cluster/` template (a sibling to `small/`/`large/`), not docs-only. Recorded so the resolution is traceable to the question.
- **§11 Q6 — shard-target ↔ SLURM-job packing.** This change commits only to shards being the unit of parallelism; the exact packing (one shard target per job vs several per job) is a `crew` configuration knob. The template prototypes and documents a packing convention; until a study fixes a site default, `shard target ↔ job` is read as "many-to-one or one-to-one, depending on configuration." Choosing a recommended site default is deferred to the first real cluster study.
