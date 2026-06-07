# Roadmap

A living view of where `ssdsims` is heading. The in-place, step-by-step
implementation plan and its **dependency DAG** live in
[`TARGETS-DESIGN.md`](./TARGETS-DESIGN.md) §12 (the completed steps and the
Mermaid graph that colours every node by status); detail for each change lives
in [`openspec/changes/`](./openspec/changes/) (active) and
[`openspec/changes/archive/`](./openspec/changes/archive/) (landed).

Section membership (`## Now` / `## Next` / `## Later` / `## Bluesky` / `## Done`)
encodes state — no `- [ ]` checkboxes. Each actionable line uses a priority
emoji from the legend below; OpenSpec-scoped items add a `[change-name]`
identifier between the emoji and the prose. Example:

- `- ‼️🛠️ [scenario-input-types] Accept the remaining scenario input types` (in `## Now`, implementation in progress)
- `- ✅ 2026-06-07 [cloud-upload] [🔗](openspec/changes/archive/2026-06-07-cloud-upload/) — Typed cloud-upload destinations on the runner` (in `## Done`)

## Priority legend

- ‼️ Critical — must-have for the reproducible-cluster-pipeline goal
- ❗️ Important — high value, scheduled
- 😀 Nice to have — quality-of-life, ergonomics, tidy-ups
- 🐢 Can wait — defer until the core is solid
- 📚 Documentation
- 💡 Exploratory — worth investigating
- ✅ Completed (appears only in `## Done`)

## In-flight legend

- ⏳ Design / investigation
- 🛠️ Implementation
- 🚀 Done, pending verification or archive

Append directly after the priority emoji (no space); drop on move to `## Done`.
Mutually exclusive. A `[change]` with artifacts under `openspec/changes/` but no
implementation yet (red/`proposed` in the §12 DAG) is queued in `## Next`; a
`[change]` whose every prerequisite has landed but which has no artifacts yet
(blue/`ready` in the DAG) is also queued, ready to propose.

## Decisions

<!-- Durable decisions that shape the roadmap. The ground-truth design rationale
     still lives in TARGETS-DESIGN.md; this records only what redirects the plan. -->

- **2026-06-07 — The `manifest` concept is parked, not removed.** The
  per-scenario manifest landed (`R/manifest.R`, #114) but has **no live
  consumer**: the shard runner deliberately does not depend on it, and its
  intended readers (`replay-helper`, `shard-completeness-assert`) are not yet
  built, so nothing in the shipped pipeline writes, records, or assembles a
  manifest today. Rather than ship four public functions with no caller, the
  writer/reader/recorder/assembler (`ssd_write_manifest()`,
  `ssd_read_manifest()`, `ssd_record_shard()`, `ssd_assemble_manifest()`) are
  **un-exported but kept** (now `@keywords internal`, dropped from `NAMESPACE`
  and the pkgdown reference; the code, tests, and `manifest` spec stay). The
  decision to re-export and wire them in is deferred to the
  `manifest-revival` task below, to be taken with the first real consumer.

## Now

<!-- What is actively being worked on this cycle. -->

- … _(nothing mid-implementation; the current cycle is spec-sync / archive / roadmap housekeeping)_

## Next

<!-- Queued up. Roughly in priority order. Doesn't have to be exhaustive. -->

- ❗️ [scenario-input-types] Extend `ssd_define_scenario()` to accept the remaining input types `ssd_run_scenario()` handles (`fitdists`, `tmbfit`, a generator function, a function-name string), materialised once in the constructor under the dqrng-only `ssd_data(..., .seed = NULL)` contract. Unblocked (deps `task-primer` / `local-dqrng-state` landed); gates `migrate-public-api`.
- ❗️ [migrate-public-api] Migrate `ssd_sim_data.data.frame` / `ssd_fit_dists_sims` / `ssd_hc_sims` to the new per-task contract, keeping the `_seed` wrappers as a one-release shim. Depends on `scenario-input-types` + `primer-primitives`; gates `cleanup-lecuyer`.
- ❗️ [replay-helper] `ssd_replay_task()` (§7) and `ssd_input_hash()` for the lightweight recipe — reproduce a failed branch locally with no `targets`. Ready to propose (prereqs `task-tables` + `manifest` landed).
- ❗️ [shard-failure-survival] §6.2 partial-failure survival: a bad task yields a shorter shard (not an abort), `summary` unions the survivors. Ready to propose (prereq `cluster-pipeline` landed).
- 😀 [nrow-max-setting] Add an explicit `nrow_max` draw-size setting (default `1000L`), decoupling the draw from the `nrow` axis to retire the §5 re-draw churn, and move the last carried columns (`n_max`, `ci`) off the task tables into the scenario slice. Proposed; breaking pre-release.

## Later

<!-- Less urgent but still on the list. -->

- ❗️ [shard-completeness-assert] §6.2 / §8.4 — a downstream `assert_<step>` target per shard comparing actual vs expected row counts, driving the fix-and-refresh loop. Blocked on `shard-failure-survival`.
- 😀 [mixed-code-lockin] §8.3 — document and runtime-test `tar_cue(depend = FALSE)` as a shard-pinning recipe plus the §8.4 forced-refresh loop. Ready (prereq `shard-atomic-rewrite` landed).
- 🐢 [cleanup-lecuyer] Remove the L'Ecuyer-CMRG helpers and `_seed` shims once the public step functions are off the L'Ecuyer path. Blocked on `migrate-public-api` + `mixed-code-lockin`.
- 😀 [error-call-origin] Audit user-facing functions so validation errors report the calling function as the origin, never an internal frame. Proposed; independent (off the DAG).
- 😀 [cleanup-as-ssd-data] Add a public `as_ssd_data()` coercing the already-named input forms into a validated `ssd_data()` collection. Proposed; independent (off the DAG).
- 😀 [tidyverse-rlang-alignment] Sweep the rest of `R/` to prefer rlang over base-R metaprogramming / `*apply()`, matching the migration `hive-partitioning` did for the factory. Independent (off the DAG).
- 😀 [canonical-call-sites] Sweep the remaining public constructors (`ssd_data()`, the `ssd_run_*` / `ssd_scenario_*` family) so arguments are passed in signature order, as the `ssd_define_scenario()` pass already did. Independent (off the DAG).
- 🐢 [manifest-revival] Revisit the parked `manifest` concept (see _Decisions_): wire it into a real consumer and re-export the writer/reader/recorder/assembler, or fold it down further. Take this up with the first of `replay-helper` / `shard-completeness-assert` that needs trusted per-shard provenance.

## Bluesky

<!-- Aspirational. Not a commitment. -->

- 💡 [dataset-provenance] Stop transporting generated datasets inline; store only the name + generator reference + `.seed` and regenerate from that provenance. The escape hatch `scenario-input-types` defers — not a near-term need while datasets are tiny. Off the DAG.

## Done

<!-- Trailing log of shipped work, in implementation order. The §12 DAG and its
     `### Archived` prose are the fuller record; `openspec/changes/archive/` is
     ground truth. -->

- ✅ 2026-06-02 [ssd-define-scenario] [🔗](openspec/changes/archive/2026-06-02-ssd-define-scenario/) — Public S3 scenario constructor (data-frame input only).
- ✅ 2026-06-02 [task-list-loop-baseline] [🔗](openspec/changes/archive/2026-06-02-task-list-loop-baseline/) — Three per-step task lists + a three-`pmap()`-loop runner baseline.
- ✅ 2026-06-02 [task-list-loop-baseline-fold] [🔗](openspec/changes/archive/2026-06-02-task-list-loop-baseline-fold/) — Fold the `data` step into `fit` (`head(sample, nrow)` inline); steps become `sample` / `fit` / `hc`.
- ✅ 2026-06-02 [dqrng-init] [🔗](openspec/changes/archive/2026-06-02-dqrng-init/) — Add `dqrng`; `dqRNGkind("pcg64")` + method (un)registration on load/unload.
- ✅ 2026-06-02 [dqrng-primer-arg] [🔗](openspec/changes/archive/2026-06-02-dqrng-primer-arg/) — Rename the `local_dqrng_state()` / `with_dqrng_state()` `state` argument to `primer`, fixing the GLOSSARY misnomer.
- ✅ 2026-06-02 [local-dqrng-state] [🔗](openspec/changes/archive/2026-06-02-local-dqrng-state/) — `local_dqrng_state(seed, primer)` scoped seed/stream wrapper with `withr`-style restore.
- ✅ 2026-06-04 [task-primer] [🔗](openspec/changes/archive/2026-06-04-task-primer/) — `task_primer(params)` 64-bit hash with NA-as-`INT_MIN` encoding.
- ✅ 2026-06-04 [primer-primitives] [🔗](openspec/changes/archive/2026-06-04-primer-primitives/) — Refactor the `_state` primitives so each task installs its primer exactly once, then runs state-less ops.
- ✅ 2026-06-04 [scenario-accessors] [🔗](openspec/changes/archive/2026-06-04-scenario-accessors/) — Materialise datasets / `min_pmix` on the scenario, reached by name via accessors (no registry).
- ✅ 2026-06-04 [partition-by] [🔗](openspec/changes/archive/2026-06-04-partition-by/) — Scenario `partition_by` knob choosing Hive path levels vs Parquet columns per step.
- ✅ 2026-06-04 [task-tables] [🔗](openspec/changes/archive/2026-06-04-task-tables/) — Per-step task tables with `(seed, primer)` plus the `*_shards` wrappers feeding `tar_map`'s `values` (static branching).
- ✅ 2026-06-04 [shard-runner-baseline] [🔗](openspec/changes/archive/2026-06-04-shard-runner-baseline/) — Single-core Hive-Parquet shard runner proving the write → glob-read → filter loop + m:n parent resolution in plain R.
- ✅ 2026-06-04 [hc-samples] [🔗](openspec/changes/archive/2026-06-04-hc-samples/) — Opt-in scalar `samples` knob retaining `ssd_hc()`'s per-row bootstrap draws (output retention only, RNG-neutral).
- ✅ 2026-06-05 [hive-partitioning] [🔗](openspec/changes/archive/2026-06-05-hive-partitioning/) — Wire the Hive write/read into the `targets` branches and pin the content-hash invalidation model (per-child Option-3 edges over the m:n fan-in).
- ✅ 2026-06-05 [step-scenario-slice] [🔗](openspec/changes/archive/2026-06-05-step-scenario-slice/) — Project each step's command onto its minimal scenario slice (and the `sample` slice per dataset), so a step-irrelevant edit leaves the other steps' shards cached.
- ✅ 2026-06-05 [path-axis-growth] [🔗](openspec/changes/archive/2026-06-05-path-axis-growth/) — Assert end-to-end that appending a dataset / growing `nsim` mints only new shards and caches the rest.
- ✅ 2026-06-05 [shard-atomic-rewrite] [🔗](openspec/changes/archive/2026-06-05-shard-atomic-rewrite/) — Inner-axis growth (a new `min_pmix`) atomically rewrites the affected shards byte-stably, leaving prior rows byte-identical.
- ✅ 2026-06-05 [scalar-ci-flag] [🔗](openspec/changes/archive/2026-06-05-scalar-ci-flag/) — Demote `ci` from a grid axis to a scalar flag and retire the §1.2 `ci = FALSE` collapse.
- ✅ 2026-06-07 [blob-storage-format] [🔗](openspec/changes/archive/2026-06-07-blob-storage-format/) — Benchmark the `fit`-object Parquet encoding; keep the interim ASCII-`VARCHAR` `encode_obj()` and tighten the `shard-runner` spec (no code change).
- ✅ 2026-06-07 [dists-simulation-setting] [🔗](openspec/changes/archive/2026-06-07-dists-simulation-setting/) — Reconcile `dists` as a fit-level simulation setting (not a cross-join axis) across spec, signature, and docs.
- ✅ 2026-06-07 [est-method-setting] [🔗](openspec/changes/archive/2026-06-07-est-method-setting/) — Reclassify `est_method` from an hc axis to an hc-level simulation setting (~3× hc cost reduction; CIs re-baseline).
- ✅ 2026-06-07 [cost-estimation] [🔗](openspec/changes/archive/2026-06-07-cost-estimation/) — `ssd_calibrate_cost()` / `ssd_estimate_cost()`: a per-task cost model that predicts a scenario's ballpark total and longest single task read-only.
- ✅ 2026-06-07 [manifest] [🔗](openspec/changes/archive/2026-06-07-manifest/) — Per-scenario JSON manifest writer/reader/recorder/assembler (#114). _Subsequently un-exported — see Decisions / `manifest-revival`._
- ✅ 2026-06-07 [task-rng-postcheck] [🔗](openspec/changes/task-rng-postcheck/) — Per-task RNG-backend postcondition: each `*_data_task_primer()` verifies dqrng still held the `user_unif_rand` slot for the whole body, aborting on a foreign-RNG hijack. _Treated as merged for this roadmap split; the change directory is not yet physically archived._
- ✅ 2026-06-07 [cluster-pipeline] [🔗](openspec/changes/archive/2026-06-07-cluster-pipeline/) — Editable SLURM `crew.cluster` targets template (`inst/targets-templates/cluster/`) with a standalone connectivity/worker preflight and a zero-to-running-job guide (#115). _Real-SLURM end-to-end run (tasks 4.1/4.2) remains a documented manual/lab step._
- ✅ 2026-06-07 [cloud-upload] [🔗](openspec/changes/archive/2026-06-07-cloud-upload/) — Typed, self-validating upload destinations on the runner (`ssd_upload_azure()` / `ssd_upload_dryrun()` + class-dispatched generics); BREAKING removal of `scenario$upload` (#114/#129).
- ✅ 2026-06-07 [dual-summary-outputs] [🔗](openspec/changes/archive/2026-06-07-dual-summary-outputs/) — Optional `path_with_samples` full summary retaining the `dists`/`samples` list-columns, emitted iff `scenario$hc$samples` is `TRUE` (#140).
