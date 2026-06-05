## 1. Dependency

- [x] 1.1 Add `crew.cluster` to `DESCRIPTION` `Suggests` (alongside the existing `crew`); keep it in `Suggests` so the package builds/checks without a scheduler

## 2. Cluster template `_targets.R`

- [x] 2.1 Add `inst/targets-templates/cluster/_targets.R` that defines the scenario inline then calls `ssd_scenario_targets(scenario)` verbatim (the same factory + scenario shape as `large/`), so the pipeline shape and content are scheduler-independent
- [x] 2.2 Replace the local controller with `tar_option_set(controller = crew.cluster::crew_controller_slurm(...))`, with an editable resource block — `slurm_partition`/queue, `script_lines` (e.g. `module load R/4.x`), scratch/`tempdir`, worker count, walltime — drafted from the crew labs' working block (§4 ingredient B), confined to one place
- [x] 2.3 Add a standalone preflight (`preflight.R`, kept out of the main pipeline) that (a) submits and reclaims one SLURM job through the controller and (b) inside that job verifies the worker prerequisites — R resolves at the expected version, `library(ssdsims)` loads (the ManyLinux binary path), and scratch/`tempdir` is writable — returning a small witness; `run.R` runs it before `tar_make()` so a failure blocks the shards, aborting with an actionable message naming which check failed (queue/account wiring, module/install path, or storage)
- [x] 2.4 Document the shard-target-to-SLURM-job packing convention (§11 Q6) as a commented `crew` setting; keep shards as the unit of parallelism (many-to-one or one-to-one depending on configuration)

## 3. Cluster template scenario and `run.R`

- [x] 3.1 Define the scenario object (ingredient C; the same shape as `large/scenario.R`, edited to taste) inline in `_targets.R`
- [x] 3.2 Add `inst/targets-templates/cluster/run.R` mirroring `large/run.R`: run the preflight, `tar_make()`, report the summary path and a peek at the estimates
- [x] 3.3 Guard `run.R` cleanly when `crew.cluster` is not installed or no SLURM queue is reachable — abort/skip with a clear message naming the missing prerequisite, and point at the local `large/` template for off-cluster runs (the cluster template carries no local-controller fallback of its own)
- [x] 3.4 Keep the template minimal: separate only what cannot be inlined — the controller in a shared `controller.R` (used by both the pipeline and the preflight), and the connectivity/prerequisite check (with its probe body) in a standalone `preflight.R`; inline the scenario into `_targets.R`; drop the single-core `run-serial.R` oracle (too slow to be meaningful at cluster scale; `large/` covers the off-cluster comparison)

## 4. End-to-end SLURM validation

- [ ] 4.1 Run `tar_make()` on a real or sandboxed SLURM queue: the preflight runs first, then the scenario shard targets dispatch across SLURM jobs and run concurrently, each writing one shard Parquet — **pending a SLURM queue** (no scheduler in CI/sandbox; the documented manual/lab validation step, design Risks; the *behaviour* — preflight then one Parquet per shard — is exercised under the local controller in 4.3)
- [ ] 4.2 Confirm the cluster run's per-task `sample`/`fit`/`hc` results are byte-identical (read-back, sorted by task-identity key) to the local `large/` pipeline for the same scenario — **pending a SLURM queue** (the byte-identical comparison runs green off-cluster via `large/`'s `run-serial.R`; verified here with a local-controller stand-in: preflight → `tar_make()` → `single-core vs cluster estimates identical: TRUE`)
- [x] 4.3 Confirm off-cluster behaviour without a scheduler: `run.R` aborts/skips cleanly with a clear message (pointing at `large/`), and `tests/testthat/test-cluster-pipeline.R` exercises the preflight probe function directly (witness on success, actionable abort on a wrong R version) and statically asserts the shipped `_targets.R` graph carries no probe target (the pipeline stays clean)

## 5. Docs and reference

- [x] 5.1 Header comments in the `cluster/` files explaining the copy-edit-run flow, the one editable controller block, the connectivity probe, and the §4 ingredients (A shape / B backend / C scenario)
- [x] 5.2 Document (or link) the operational backend B prerequisites the labs pinned down — ManyLinux binary install path and login-node prerequisite checker — so worker nodes resolve dependencies without compiling
- [x] 5.3 Note the new `cluster/` template wherever `small/`/`large/` are referenced (e.g. the templates' README/index and any pkgdown article), and mark `cluster-pipeline` done in the `TARGETS-DESIGN.md` §12 DAG
- [x] 5.4 Write a "zero to a running cluster job" guide (a vignette/pkgdown article, or the `cluster/` README) that starts from the user's **non-R** site instructions and ends at a running job, in four steps: (1) a mapping table from the site's SLURM usage instructions (login node, `sbatch`, partition/queue, account/allocation, `module load`, scratch path, walltime, cores) to the `crew_controller_slurm()` arguments; (2) running the standalone connectivity+prerequisite preflight to confirm the mapping; (3) running a minimal (shrunken) scenario end-to-end through the `cluster/` template as the first job; (4) swapping in the user's own scenario (the inline block in `_targets.R`)

## 6. Validation

- [x] 6.1 Run `devtools::check()` (builds/passes without a scheduler, with `crew.cluster` only in `Suggests`) and `air format .` — 0 errors / 0 notes; the only WARNING is the environmental `qpdf` notice (missing system tool for PDF size checks), unrelated to this change
- [x] 6.2 Run `openspec validate cluster-pipeline --strict` and confirm it passes
