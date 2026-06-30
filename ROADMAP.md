# Roadmap

A living view of where `ssdsims` is heading. The principal design and its completed
step-by-step implementation plan live in [`TARGETS-DESIGN.md`](./TARGETS-DESIGN.md) §12; detail for each change lives in [`openspec/changes/`](./openspec/changes/) (active) and [`openspec/changes/archive/`](./openspec/changes/archive/) (landed).

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
implementation yet is queued in `## Next`; a
`[change]` whose every prerequisite has landed but which has no artifacts yet
is also queued, ready to propose.

## Now

<!-- What is actively being worked on this cycle. -->


## Next

<!-- Queued up. Roughly in priority order. Doesn't have to be exhaustive. -->

- ❗️ [replay-helper] `ssd_replay_task()` (§7) and `ssd_input_hash()` for the lightweight recipe — reproduce a failed branch locally with no `targets`. Ready to propose (prereqs `task-tables` + `manifest` landed).
- ❗️ [shard-failure-survival] §6.2 partial-failure survival: a bad task yields a shorter shard (not an abort), `summary` unions the survivors. Ready to propose (prereq `cluster-pipeline` landed).
- 📚 [cluster-controller] Run the controller on a long-running SLURM job.
- 😀 [azure-open-performance] Analyze performance of `ssd_open_uploaded()` for many files.

## Later

<!-- Less urgent but still on the list. -->

- ❗️ [shard-completeness-assert] §6.2 / §8.4 — a downstream `assert_<step>` target per shard comparing actual vs expected row counts, driving the fix-and-refresh loop. Blocked on `shard-failure-survival`.
- 😀 [mixed-code-lockin] §8.3 — document and runtime-test `tar_cue(depend = FALSE)` as a shard-pinning recipe plus the §8.4 forced-refresh loop. Ready (prereq `shard-atomic-rewrite` landed).
- 😀⏳ [error-call-origin] Audit user-facing functions so validation errors report the calling function as the origin, never an internal frame. Proposed; independent.
- 😀 [tidyverse-rlang-alignment] Sweep the rest of `R/` to prefer rlang over base-R metaprogramming / `*apply()`, matching the migration `hive-partitioning` did for the factory. Also establish `print()` methods for all objects. Independent.
- 😀 [canonical-call-sites] Sweep the remaining public constructors (`ssd_data()`, the `ssd_run_*` / `ssd_scenario_*` family) so arguments are passed in signature order, as the `ssd_define_scenario()` pass already did. Independent.
- 😀⏳ [cleanup-as-ssd-data] Add a public `as_ssd_data()` coercing the already-named input forms into a validated `ssd_data()` collection. Proposed.

## Bluesky

<!-- Aspirational. Not a commitment. -->

- 💡 [dataset-provenance] Stop transporting generated datasets inline; store only the name + generator reference + `.seed` and regenerate from that provenance. The escape hatch `scenario-input-types` defers — not a near-term need while datasets are tiny.
- 💡 [shard-granularity-benchmark] An empirical harness that sweeps *shard granularity* (how task rows bundle into shards) × problem size and measures **realized parallel wall time, parallel efficiency, and per-shard duration** — the coarse-vs-fine scheduling tradeoff and `crew`/worker dispatch overhead. Complements the existing per-task, single-core `cost-estimation` model (which predicts cost but not realized parallel runtime) and would inform a default shard-sizing heuristic; also distinct from `cost-analysis-targets`, which mines run times from a *single* existing run rather than sweeping granularity. Origin: the deferred PoC harness in PR #54 (`scripts/targets/experiments/`, closed superseded) — the *idea* is salvageable, the code is not (it predates the `ssd_scenario_*_shards()` sharding API and reached into internals); a revival would be rebuilt on the current API.
- 🐢 [manifest-revival] Revisit the parked `manifest` concept (see _Decisions_): wire it into a real consumer and re-export the writer/reader/recorder/assembler, or fold it down further. Take this up with the first of `replay-helper` / `shard-completeness-assert` that needs trusted per-shard provenance. **Hashing is revisited here too** — the cloud-upload sha256 (recording an upload hash / shipping the per-shard `meta.json` sidecar alongside the blob for transfer-corruption detection) was dropped with the manifest and comes back with it.

## Done

<!-- Trailing log of shipped work, in implementation order. The §12 DAG and its
     `### Archived` prose are the fuller record; `openspec/changes/archive/` is
     ground truth. -->

- ✅ 2026-06-30 [stingy-duckplyr-pipeline] [🔗](openspec/changes/archive/2026-06-30-stingy-duckplyr-pipeline/) — Adopt duckplyr `prudence = "stingy"` across the internal fan-in reads and the upload read-back generics so the union/filter/projection/write provably stay in DuckDB and any un-translatable verb fails loudly instead of silently materialising into R; compute cost-analysis timings in DuckDB via the `dd$` escape hatch, stop `ssd_write_parquet()` downgrading frames to `lavish`, and reshape touched call sites into native `|>` pipelines. BREAKING (opt-out provided): the read-back generics default to `prudence = "stingy"`.
- ✅ 2026-06-30 [cleanup-lecuyer] [🔗](openspec/changes/archive/2026-06-30-cleanup-lecuyer/) — Retire the legacy public API and remove the dead code behind it: drop the monolith runners (`ssd_run_scenario()` / `ssd_sim_data()` families, `ssd_fit_dists_sims()`, `ssd_hc_sims()`) and the L'Ecuyer-CMRG RNG (`lecuyer-cmrg-seed.R` + the `*_state`/`*_seed` shims), leaving one RNG path (dqrng) and one runner family (`ssd_run_scenario_baseline()` / `ssd_run_scenario_shards()`). **Folds in the former `migrate-public-api`** (migration is moot once the monolith is deleted, not ported). Proposed; independent — the replacement surface is already shipped. Gates `readme`.
- ✅ 2026-06-30 [readme] [🔗](openspec/changes/archive/2026-06-30-readme/) — **Blocked by cleanup-lecuyer**. Update README and integrate in the rest of the documentation. Create vignette("ssdsims") . Perform comprehensive review of all vignettes for content, output artifacts, and style. Use vignette(...) style links everywhere.
- ✅ 2026-06-19 [azure-summary] [🔗](openspec/changes/archive/2026-06-19-azure-summary/) — Conveniently access the summary Parquet files from Azure.
- ✅ 2026-06-19 [scenario-option-vocabulary] [🔗](openspec/changes/archive/2026-06-19-scenario-option-vocabulary/) — Retire the old umbrella term for the genus **scenario option** (a **scenario axis** fans out into tasks; a **scenario setting**, the renamed within-task species, applies within each task), with always-qualified discipline (bare "option"/"setting" never used as terms) and `crew` config settings becoming **crew option**. Full-repo rename including archives, `NEWS.md`, and file names (e.g. the `opts.rds` fixture); `GLOSSARY.md` gains the *study* ⊃ *design* ⊃ *scenario* ⊃ *task* hierarchy plus a DoE / Morris-White-Crowther (2019) mapping. BREAKING (cosmetic): the `ci = FALSE` rejection error text. Settles the vocabulary before `scenario-combine`'s collection layer accretes more on it.
- ✅ 2026-06-19 [hc-readout-aggregation] [🔗](openspec/changes/archive/2026-06-19-hc-readout-aggregation/) — Per-overlap hc readout aggregation: reconcile design members that differ in the non-axis hc settings (`union` `proportion`/`est_method`, `any` `ci`/`samples`) into shared hc shards via per-cell demand reduction and `ci`-routing, instead of aborting. No ssdtools refactor.
- ✅ 2026-06-14 [scenario-input-types] [🔗](openspec/changes/archive/2026-06-14-scenario-input-types/) — Accept the generator inputs `ssd_run_scenario()` handles (`fitdists`, `tmbfit`, a generator function, a function-name string) in the declarative path via a new `ssd_gen(..., .n, .seed)` that materialises each once to a reproducible `Conc` tibble (name as the dqrng stream, required `.seed`/`.n`); `ssd_data()` is renamed `ssd_scenario_data()` (escaping the `ssdtools::ssd_data(x)` clash) and `ssd_define_scenario()` accepts only that collection, so the constructor stays RNG-free. Deps `task-primer` / `local-dqrng-state` **and** `task-rng-postcheck` (`ssd_gen()` reuses its `dqrng_usable()` gate + `chk_dqrng_backend_intact()` witness — postcheck's helpers and the dqrng-as-`Suggests` move landed physically alongside this change); gates `migrate-public-api`. Name-only regeneration is the deferred `dataset-provenance`.
- ✅ 2026-06-14 [replace-default-true] [🔗](openspec/changes/archive/2026-06-14-replace-default-true/) — Set the default as `replace = TRUE`, silently discard infeasible tasks with `replace = FALSE` and too many rows. Interacts with `nrow-max-setting`.
- ✅ 2026-06-14 [nrow-max-setting] [🔗](openspec/changes/archive/2026-06-14-nrow-max-setting/) — Add an explicit `nrow_max` draw-size setting (default `1000L`), decoupling the draw from the `nrow` axis to retire the §5 re-draw churn, and move the last carried columns (`n_max`, `ci`) off the task tables into the scenario slice. Proposed; breaking pre-release.
- ✅ 2026-06-14 [distset-hc-axis] [🔗](openspec/changes/archive/2026-06-14-distset-hc-axis/) — Iwasaki.
- ✅ 2026-06-14 [scenario-combine] [🔗](openspec/changes/archive/2026-06-14-scenario-combine/) — **Blocked by distset-hc-axis**. Provide a convenient way to run multiple `ssd_scenario` objects as a single targets pipeline.
- ✅ 2026-06-14 [cost-analysis-targets] [🔗](openspec/changes/archive/2026-06-14-cost-analysis-targets/) — Scenario-level analysis implemented (timing instrumentation, the tar_meta resolver, `ssd_analyse_cost()`/`ssd_compare_cost()`/`ssd_calibrate_cost_from_run()`); the design-level rollup (group 9) stays **blocked by scenario-combine**. Improve the cost estimation by analyzing an existing targets run. Includes tools to query the targets store for run times of the various targets and mapping this back to the scenario slices. Side change, folded: Record start and end of computation time for each task, and the start time of the simulation run, in the Parquet. Also folded: a **design-level rollup** (the former `cost-analysis-design`) — `ssd_analyse_cost()`/`ssd_compare_cost()`/`ssd_calibrate_cost_from_run()` accept an `ssd_design()` and aggregate across its per-scenario result trees (gated on `scenario-combine`); a prototype of the aggregation seam is kept as proof of work under the change's `exploration/`. Supports a project deliverable.
- ✅ 2026-06-10 [duckplyr-config] [🔗](openspec/changes/archive/2026-06-14-duckplyr-config/) — Pipeline-scoped duckplyr/DuckDB configuration: a `local_duckplyr_config()` scope on the step runners, `ssd_summarise()`, and `ssd_run_scenario_shards()` pins a single thread and a 1GB default `memory_limit` (environment variables `SSDSIMS_DUCKDB_THREADS`/`SSDSIMS_DUCKDB_MEMORY_LIMIT`), relaxes `preserve_insertion_order` (scope-wide; per-write COPY options cannot carry it), and the full summary writes byte-budgeted row groups (`samples_row_group_bytes`). Folds in [duckplyr-message]: the scope silences duckplyr's fallback telemetry (`DUCKPLYR_FALLBACK_COLLECT=0`/`DUCKPLYR_FALLBACK_AUTOUPLOAD=0`) (#151).
- ✅ 2026-06-07 [blob-storage-format] [🔗](openspec/changes/archive/2026-06-07-blob-storage-format/) — Benchmark the `fit`-object Parquet encoding; keep the interim ASCII-`VARCHAR` `encode_obj()` and tighten the `shard-runner` spec (no code change).
- ✅ 2026-06-07 [dists-scenario-setting] [🔗](openspec/changes/archive/2026-06-07-dists-scenario-setting/) — Reconcile `dists` as a fit-level scenario setting (not a cross-join axis) across spec, signature, and docs.
- ✅ 2026-06-07 [est-method-setting] [🔗](openspec/changes/archive/2026-06-07-est-method-setting/) — Reclassify `est_method` from an hc axis to an hc-level scenario setting (~3× hc cost reduction; CIs re-baseline).
- ✅ 2026-06-07 [cost-estimation] [🔗](openspec/changes/archive/2026-06-07-cost-estimation/) — `ssd_calibrate_cost()` / `ssd_estimate_cost()`: a per-task cost model that predicts a scenario's ballpark total and longest single task read-only.
- ✅ 2026-06-07 [manifest] [🔗](openspec/changes/archive/2026-06-07-manifest/) — Per-scenario JSON manifest writer/reader/recorder/assembler (#114). _Subsequently un-exported — see Decisions / `manifest-revival`._
- ✅ 2026-06-07 [task-rng-postcheck] [🔗](openspec/changes/archive/2026-06-14-task-rng-postcheck/) — Per-task RNG-backend postcondition: each `*_data_task_primer()` verifies dqrng still held the `user_unif_rand` slot for the whole body, aborting on a foreign-RNG hijack. dqrng moved `Imports` → `Suggests` with a `dqrng_usable()` gate and a `chk_dqrng_backend_intact()` witness. _Helpers landed physically alongside `scenario-input-types` (which depends on them)._
- ✅ 2026-06-07 [cluster-pipeline] [🔗](openspec/changes/archive/2026-06-07-cluster-pipeline/) — Editable SLURM `crew.cluster` targets template (`inst/targets-templates/cluster/`) with a standalone connectivity/worker preflight and a zero-to-running-job guide (#115). _Real-SLURM end-to-end run (tasks 4.1/4.2) remains a documented manual/lab step._
- ✅ 2026-06-07 [cloud-upload] [🔗](openspec/changes/archive/2026-06-07-cloud-upload/) — Typed, self-validating upload destinations on the runner (`ssd_upload_azure()` / `ssd_upload_dryrun()` + class-dispatched generics); BREAKING removal of `scenario$upload` (#114/#129).
- ✅ 2026-06-07 [dual-summary-outputs] [🔗](openspec/changes/archive/2026-06-07-dual-summary-outputs/) — Optional `path_with_samples` full summary retaining the `dists`/`samples` list-columns, emitted iff `scenario$hc$samples` is `TRUE` (#140).
- ✅ 2026-06-05 [hive-partitioning] [🔗](openspec/changes/archive/2026-06-05-hive-partitioning/) — Wire the Hive write/read into the `targets` branches and pin the content-hash invalidation model (per-child Option-3 edges over the m:n fan-in).
- ✅ 2026-06-05 [step-scenario-slice] [🔗](openspec/changes/archive/2026-06-05-step-scenario-slice/) — Project each step's command onto its minimal scenario slice (and the `sample` slice per dataset), so a step-irrelevant edit leaves the other steps' shards cached.
- ✅ 2026-06-05 [path-axis-growth] [🔗](openspec/changes/archive/2026-06-05-path-axis-growth/) — Assert end-to-end that appending a dataset / growing `nsim` mints only new shards and caches the rest.
- ✅ 2026-06-05 [shard-atomic-rewrite] [🔗](openspec/changes/archive/2026-06-05-shard-atomic-rewrite/) — Inner-axis growth (a new `min_pmix`) atomically rewrites the affected shards byte-stably, leaving prior rows byte-identical.
- ✅ 2026-06-05 [scalar-ci-flag] [🔗](openspec/changes/archive/2026-06-05-scalar-ci-flag/) — Demote `ci` from a grid axis to a scalar flag and retire the §1.2 `ci = FALSE` collapse.
- ✅ 2026-06-04 [task-primer] [🔗](openspec/changes/archive/2026-06-04-task-primer/) — `task_primer(params)` 64-bit hash with NA-as-`INT_MIN` encoding.
- ✅ 2026-06-04 [primer-primitives] [🔗](openspec/changes/archive/2026-06-04-primer-primitives/) — Refactor the `_state` primitives so each task installs its primer exactly once, then runs state-less ops.
- ✅ 2026-06-04 [scenario-accessors] [🔗](openspec/changes/archive/2026-06-04-scenario-accessors/) — Materialise datasets / `min_pmix` on the scenario, reached by name via accessors (no registry).
- ✅ 2026-06-04 [partition-by] [🔗](openspec/changes/archive/2026-06-04-partition-by/) — Scenario `partition_by` argument choosing Hive path levels vs Parquet columns per step.
- ✅ 2026-06-04 [task-tables] [🔗](openspec/changes/archive/2026-06-04-task-tables/) — Per-step task tables with `(seed, primer)` plus the `*_shards` wrappers feeding `tar_map`'s `values` (static branching).
- ✅ 2026-06-04 [shard-runner-baseline] [🔗](openspec/changes/archive/2026-06-04-shard-runner-baseline/) — Single-core Hive-Parquet shard runner proving the write → glob-read → filter loop + m:n parent resolution in plain R.
- ✅ 2026-06-04 [hc-samples] [🔗](openspec/changes/archive/2026-06-04-hc-samples/) — Opt-in scalar `samples` scenario option retaining `ssd_hc()`'s per-row bootstrap draws (output retention only, RNG-neutral).
- ✅ 2026-06-02 [ssd-define-scenario] [🔗](openspec/changes/archive/2026-06-02-ssd-define-scenario/) — Public S3 scenario constructor (data-frame input only).
- ✅ 2026-06-02 [task-list-loop-baseline] [🔗](openspec/changes/archive/2026-06-02-task-list-loop-baseline/) — Three per-step task lists + a three-`pmap()`-loop runner baseline.
- ✅ 2026-06-02 [task-list-loop-baseline-fold] [🔗](openspec/changes/archive/2026-06-02-task-list-loop-baseline-fold/) — Fold the `data` step into `fit` (`head(sample, nrow)` inline); steps become `sample` / `fit` / `hc`.
- ✅ 2026-06-02 [dqrng-init] [🔗](openspec/changes/archive/2026-06-02-dqrng-init/) — Add `dqrng`; `dqRNGkind("pcg64")` + method (un)registration on load/unload.
- ✅ 2026-06-02 [dqrng-primer-arg] [🔗](openspec/changes/archive/2026-06-02-dqrng-primer-arg/) — Rename the `local_dqrng_state()` / `with_dqrng_state()` `state` argument to `primer`, fixing the GLOSSARY misnomer.
- ✅ 2026-06-02 [local-dqrng-state] [🔗](openspec/changes/archive/2026-06-02-local-dqrng-state/) — `local_dqrng_state(seed, primer)` scoped seed/stream wrapper with `withr`-style restore.

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
- **2026-06-07 — Hashing dropped from the cloud-upload path, deferred with the
  manifest.** With no verification consumer yet, `ssd_upload_shard()` no longer
  computes a cloud sha256, and `TARGETS-DESIGN.md` §6.1 / the `cloud-upload`
  spec record no upload hash — a shard is shipped and read back **in place**
  (`ssd_open_uploaded()` / `ssd_summarise_uploaded()`), the row data itself the
  round-trip check. Hash-based transfer-corruption detection returns with
  `manifest-revival`. The parked `manifest`'s own per-shard sha256 (in
  `R/manifest.R`, §7/§8.5) is untouched — it stays kept-but-internal.
