## 1. Dependency

- [ ] 1.1 Add `crew.cluster` to `DESCRIPTION` `Suggests` (alongside the existing `crew`); keep it in `Suggests` so the package builds/checks without a scheduler

## 2. Cluster template `_targets.R`

- [ ] 2.1 Add `inst/targets-templates/cluster/_targets.R` that `source("scenario.R")` then calls `ssd_scenario_targets(scenario)` verbatim (the same factory + scenario as `large/`), so the pipeline shape and content are scheduler-independent
- [ ] 2.2 Replace the local controller with `tar_option_set(controller = crew.cluster::crew_controller_slurm(...))`, with an editable resource block — `slurm_partition`/queue, `script_lines` (e.g. `module load R/4.x`), scratch/`tempdir`, worker count, walltime — drafted from the crew labs' working block (§4 ingredient B), confined to one place
- [ ] 2.3 Add a probe target that (a) submits and reclaims one SLURM job through the controller and (b) inside that job verifies the worker prerequisites — R resolves at the expected version, `library(ssdsims)` loads (the ManyLinux binary path), and scratch/`tempdir` is writable — returning a small witness; make the first scenario shard target name the probe as an explicit upstream dependency so `tar_make()` builds it first, and abort on failure with an actionable message naming which check failed (queue/account wiring, module/install path, or storage)
- [ ] 2.4 Document the shard-target-to-SLURM-job packing convention (§11 Q6) as a commented `crew` setting; keep shards as the unit of parallelism (many-to-one or one-to-one depending on configuration)

## 3. Cluster template `scenario.R` and `run.R`

- [ ] 3.1 Add `inst/targets-templates/cluster/scenario.R` (the scenario object, ingredient C; the same shape as `large/scenario.R`, edited to taste)
- [ ] 3.2 Add `inst/targets-templates/cluster/run.R` mirroring `large/run.R`: source the scenario, `tar_make()`, report the summary path and a peek at the estimates
- [ ] 3.3 Guard `run.R` cleanly when `crew.cluster` is not installed or no SLURM queue is reachable — skip/abort with a clear message, and offer a `crew::crew_controller_local()` smoke path for off-cluster validation (mirroring the labs' local fallback, §4)

## 4. End-to-end SLURM validation

- [ ] 4.1 Run `tar_make()` on a real or sandboxed SLURM queue: the connectivity probe builds first, then the scenario shard targets dispatch across SLURM jobs and run concurrently, each writing one shard Parquet
- [ ] 4.2 Confirm the cluster run's per-task `sample`/`fit`/`hc` results are byte-identical (read-back, sorted by task-identity key) to the local `large/` pipeline for the same scenario
- [ ] 4.3 Confirm the off-cluster fallback: with no SLURM queue, `run.R` runs the `crew::crew_controller_local()` smoke path (or skips cleanly) and the graph builds

## 5. Docs and reference

- [ ] 5.1 Header comments in the `cluster/` files explaining the copy-edit-run flow, the one editable controller block, the connectivity probe, and the §4 ingredients (A shape / B backend / C scenario)
- [ ] 5.2 Document (or link) the operational backend B prerequisites the labs pinned down — ManyLinux binary install path and login-node prerequisite checker — so worker nodes resolve dependencies without compiling
- [ ] 5.3 Note the new `cluster/` template wherever `small/`/`large/` are referenced (e.g. the templates' README/index and any pkgdown article), and mark `cluster-pipeline` done in the `TARGETS-DESIGN.md` §12 DAG
- [ ] 5.4 Write a "zero to a running cluster job" guide (a vignette/pkgdown article, or the `cluster/` README) that starts from the user's **non-R** site instructions and ends at a running job, in four steps: (1) a mapping table from the site's SLURM usage instructions (login node, `sbatch`, partition/queue, account/allocation, `module load`, scratch path, walltime, cores) to the `crew_controller_slurm()` arguments; (2) running the connectivity+prerequisite probe to confirm the mapping; (3) running the built-in `small` scenario end-to-end through the `cluster/` template as the minimal first job; (4) swapping in the user's own scenario (`scenario.R`)

## 6. Validation

- [ ] 6.1 Run `devtools::check()` (builds/passes without a scheduler, with `crew.cluster` only in `Suggests`) and `air format .`
- [ ] 6.2 Run `openspec validate cluster-pipeline --strict` and confirm it passes
