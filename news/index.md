# Changelog

## ssdsims 0.0.0.9015

### Chore

- Auto-update from GitHub Actions
  ([\#147](https://github.com/poissonconsulting/ssdsims/issues/147)).

- Fix dep.

### Documentation

#### scenario-input-types

- Redesign to
  [`ssd_scenario_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_data.md) +
  [`ssd_gen()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_gen.md)
  ([\#118](https://github.com/poissonconsulting/ssdsims/issues/118)).

- Archive landed changes and establish ROADMAP.md
  ([\#143](https://github.com/poissonconsulting/ssdsims/issues/143)).

### Refactoring

- Make factory side-effect-free by removing probe from init
  ([\#145](https://github.com/poissonconsulting/ssdsims/issues/145)).

## ssdsims 0.0.0.9014

### Bug fixes

- Key `completed_shards` by relative partition path on Windows
  ([\#130](https://github.com/poissonconsulting/ssdsims/issues/130)).

### Features

#### task-shards

- Add optional full summary retaining dists/samples
  ([\#140](https://github.com/poissonconsulting/ssdsims/issues/140)).

- Add per-scenario manifest writer, reader, and shard sha256 assembly
  ([\#114](https://github.com/poissonconsulting/ssdsims/issues/114)).

- Add scenario cost-estimation calibrate/estimate workflow
  ([\#131](https://github.com/poissonconsulting/ssdsims/issues/131)).

- Add per-scenario manifest writer, reader, and shard sha256 assembly
  ([\#114](https://github.com/poissonconsulting/ssdsims/issues/114)).

- Add cluster targets template for SLURM (cluster-pipeline)
  ([\#115](https://github.com/poissonconsulting/ssdsims/issues/115)).

### Chore

- Change code ownership for now.

- Bump codecov-action to v7 and sharpen PR-title convention
  ([\#126](https://github.com/poissonconsulting/ssdsims/issues/126)).

### Documentation

#### openspec

- Propose `nrow-max-setting` and reconcile `migrate-public-api` ci
  primer
  ([\#134](https://github.com/poissonconsulting/ssdsims/issues/134)).

- Consolidate per-step task, shard, and runner docs onto umbrella pages
  ([\#137](https://github.com/poissonconsulting/ssdsims/issues/137)).

#### openspec

- Archive cost-estimation and axis reclassifications
  ([\#139](https://github.com/poissonconsulting/ssdsims/issues/139)).

- Add/correct author ORCID(s) in DESCRIPTION
  ([\#136](https://github.com/poissonconsulting/ssdsims/issues/136)).

#### openspec

- Sync and archive `blob-storage-format` change
  ([\#135](https://github.com/poissonconsulting/ssdsims/issues/135)).

- Consolidate PR conventions to CLAUDE.md
  ([\#132](https://github.com/poissonconsulting/ssdsims/issues/132)).

- Sync manifest spec, archive, update §12 roadmap, and trim AGENTS.md
  ([\#125](https://github.com/poissonconsulting/ssdsims/issues/125)).

#### openspec

- Propose cloud-upload capability with typed upload destinations
  ([\#122](https://github.com/poissonconsulting/ssdsims/issues/122)).

- Document keep-going error mode and pre-flight checks
  ([\#123](https://github.com/poissonconsulting/ssdsims/issues/123)).

#### openspec

- Propose est-method-setting and cost-estimation changes
  ([\#121](https://github.com/poissonconsulting/ssdsims/issues/121)).

### Refactoring

- Reclassify `est_method` and `dists` as hc/fit simulation settings
  ([\#127](https://github.com/poissonconsulting/ssdsims/issues/127)).

### Testing

- Pin name-order sensitivity in task_primer()
  ([\#138](https://github.com/poissonconsulting/ssdsims/issues/138)).

- Handle withr seed leak in hc_sims bootstrap CI tests
  ([\#128](https://github.com/poissonconsulting/ssdsims/issues/128)).

#### task-shards

- Guard targets-object tests so checks pass without
  `targets`/`tarchetypes`
  ([\#124](https://github.com/poissonconsulting/ssdsims/issues/124)).

## ssdsims 0.0.0.9013

### Features

#### targets

- Per-step minimal scenario slice for shard invalidation
  ([\#102](https://github.com/poissonconsulting/ssdsims/issues/102)).

### Chore

#### openspec

- Sync and archive `scalar-ci-flag`; tidy roadmap
  ([\#112](https://github.com/poissonconsulting/ssdsims/issues/112)).

#### openspec

- Archive `path-axis-growth` and `shard-atomic-rewrite`
  ([\#111](https://github.com/poissonconsulting/ssdsims/issues/111)).

### Continuous integration

- Require R \>= 4.3 and fix Windows Quarto vignette builds
  ([\#100](https://github.com/poissonconsulting/ssdsims/issues/100)).

### Documentation

- Reclassify `dists` as fit-level simulation setting
  ([\#119](https://github.com/poissonconsulting/ssdsims/issues/119)).

#### roadmap

- Sync DAG status and surface ready-to-propose changes
  ([\#101](https://github.com/poissonconsulting/ssdsims/issues/101)).

- Demote ci to scalar flag, retire ci = FALSE collapse
  ([\#104](https://github.com/poissonconsulting/ssdsims/issues/104)).

#### openspec

- Sync and archive hive-partitioning change
  ([\#103](https://github.com/poissonconsulting/ssdsims/issues/103)).

#### openspec

- Sync and archive step-scenario-slice change
  ([\#106](https://github.com/poissonconsulting/ssdsims/issues/106)).

#### roadmap

- Sync DAG status and surface ready-to-propose changes
  ([\#101](https://github.com/poissonconsulting/ssdsims/issues/101)).

#### openspec

- Sync specs and archive five completed changes
  ([\#98](https://github.com/poissonconsulting/ssdsims/issues/98)).

#### openspec

- Propose migrate-public-api change
  ([\#96](https://github.com/poissonconsulting/ssdsims/issues/96)).

### Refactoring

- Canonicalise `seed` position in
  [`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md)
  call sites
  ([\#116](https://github.com/poissonconsulting/ssdsims/issues/116)).

- Make `ci` a scalar flag, not a grid axis
  ([\#109](https://github.com/poissonconsulting/ssdsims/issues/109)).

### Testing

#### shard-runner

- Pin blob-encoding contracts; keep the ASCII VARCHAR (benchmark-gated)
  ([\#113](https://github.com/poissonconsulting/ssdsims/issues/113)).

#### task-shards

- Pin inner-axis-growth atomic byte-stable rewrite
  (shard-atomic-rewrite)
  ([\#107](https://github.com/poissonconsulting/ssdsims/issues/107)).

- Add end-to-end path-axis growth tests
  ([\#108](https://github.com/poissonconsulting/ssdsims/issues/108)).

- Use minimal nboot in bootstrap-executing tests
  ([\#105](https://github.com/poissonconsulting/ssdsims/issues/105)).

### Uncategorized

- Merge pull request
  [\#97](https://github.com/poissonconsulting/ssdsims/issues/97) from
  poissonconsulting/add-codeowners.

  Add CODEOWNERS assigning [@joethorley](https://github.com/joethorley)

## ssdsims 0.0.0.9012

### Features

- Scenario accessors, configurable partition_by/bundle, and shard
  runners (single-core + targets)
  ([\#94](https://github.com/poissonconsulting/ssdsims/issues/94)).

- Per-task dqrng primer derivation and a reproducible baseline runner
  ([\#88](https://github.com/poissonconsulting/ssdsims/issues/88)).

### Chore

- Sync parallel-safe-seeding spec and archive task-primer +
  primer-primitives
  ([\#90](https://github.com/poissonconsulting/ssdsims/issues/90)).

### Documentation

#### openspec

- Propose `task-rng-postcheck` — per-task dqrng RNG-backend
  postcondition
  ([\#91](https://github.com/poissonconsulting/ssdsims/issues/91)).

- Propose shard-runner-baseline change
  ([\#95](https://github.com/poissonconsulting/ssdsims/issues/95)).

#### openspec

- Propose registry, manifest, and task-tables changes
  ([\#92](https://github.com/poissonconsulting/ssdsims/issues/92)).

- Targets-design + roadmap revisions, and explore-captured decisions for
  `scenario-input-types` / `partition-by`
  ([\#87](https://github.com/poissonconsulting/ssdsims/issues/87)).

### Roadmap

- Add a step that tests incremental rebuilds when a scenario grows
  ([\#93](https://github.com/poissonconsulting/ssdsims/issues/93)).

## ssdsims 0.0.0.9011

### Features

- Fold the `data` task step into `fit`; propose partitioning/RNG roadmap
  changes
  ([\#79](https://github.com/poissonconsulting/ssdsims/issues/79)).

- Task-table derivation, baseline runner, and grouped pkgdown reference
  ([\#82](https://github.com/poissonconsulting/ssdsims/issues/82)).

- Derive task tables and baseline loop runner from a scenario
  ([\#80](https://github.com/poissonconsulting/ssdsims/issues/80)).

- Add
  [`local_dqrng_state()`](https://poissonconsulting.github.io/ssdsims/reference/local_dqrng_state.md)
  and
  [`with_dqrng_state()`](https://poissonconsulting.github.io/ssdsims/reference/local_dqrng_state.md)
  helpers
  ([\#78](https://github.com/poissonconsulting/ssdsims/issues/78)).

- Reentrant
  [`local_dqrng_backend()`](https://poissonconsulting.github.io/ssdsims/reference/local_dqrng_backend.md) +
  propose `task-list-loop-baseline` and `local-dqrng-state`
  ([\#74](https://github.com/poissonconsulting/ssdsims/issues/74)).

- Add declarative
  [`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md)
  and `ssd_data()`
  ([\#71](https://github.com/poissonconsulting/ssdsims/issues/71)).

- Add scenario-scoped dqrng `pcg64` RNG backend (dqrng-init)
  ([\#72](https://github.com/poissonconsulting/ssdsims/issues/72)).

### Chore

- Rename `CLAUDE.md` to `AGENTS.md`
  ([\#86](https://github.com/poissonconsulting/ssdsims/issues/86)).

- Sync `task-lists` spec and archive `task-list-loop-baseline-fold`
  ([\#85](https://github.com/poissonconsulting/ssdsims/issues/85)).

- Cleanup.

- Sync and archive `local-dqrng-state` change
  ([\#81](https://github.com/poissonconsulting/ssdsims/issues/81)).

- Sync specs and archive `ssd-define-scenario` and `dqrng-init`
  ([\#77](https://github.com/poissonconsulting/ssdsims/issues/77)).

- Initialize OpenSpec with foundational proposals and developer guide
  ([\#53](https://github.com/poissonconsulting/ssdsims/issues/53)).

### Documentation

- Add scenario-definition example script and Quarto vignette
  ([\#83](https://github.com/poissonconsulting/ssdsims/issues/83)).

- Expand development workflow and coding guidelines
  ([\#76](https://github.com/poissonconsulting/ssdsims/issues/76)).

- Add OpenSpec onboarding and workflow skills
  ([\#75](https://github.com/poissonconsulting/ssdsims/issues/75)).

- Add design for targets-based pipeline
  ([\#67](https://github.com/poissonconsulting/ssdsims/issues/67)).

### Refactoring

- Rename `state` argument to `primer` in
  [`local_dqrng_state()`](https://poissonconsulting.github.io/ssdsims/reference/local_dqrng_state.md)/[`with_dqrng_state()`](https://poissonconsulting.github.io/ssdsims/reference/local_dqrng_state.md)
  ([\#84](https://github.com/poissonconsulting/ssdsims/issues/84)).

## ssdsims 0.0.0.9010

### Chore

- Use function instead of string in
  [`do.call()`](https://rdrr.io/r/base/do.call.html).

### Refactoring

- Extract `fit_dists_state()` and `hc_state()` from `*_seed()` helpers
  ([\#68](https://github.com/poissonconsulting/ssdsims/issues/68)).

## ssdsims 0.0.0.9009

### Bug fixes

- Fix
  [`ssd_run_scenario.tmbfit()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario.md)
  to pass fitted estimates to the rng call
  ([\#64](https://github.com/poissonconsulting/ssdsims/issues/64)).

### Features

- Distinguish between RNG state and seed, add glossary documentation
  ([\#61](https://github.com/poissonconsulting/ssdsims/issues/61)).

### Chore

- Add input validation to `*_lecuyer_cmrg_*` seed functions
  ([\#63](https://github.com/poissonconsulting/ssdsims/issues/63)).

- Forward
  [`with_lecuyer_cmrg_seed()`](https://poissonconsulting.github.io/ssdsims/reference/local_lecuyer_cmrg_seed.md)
  to
  [`local_lecuyer_cmrg_seed()`](https://poissonconsulting.github.io/ssdsims/reference/local_lecuyer_cmrg_seed.md).

### Testing

- Replace
  [`withr::with_seed()`](https://withr.r-lib.org/reference/with_seed.html)
  with
  [`with_lecuyer_cmrg_seed()`](https://poissonconsulting.github.io/ssdsims/reference/local_lecuyer_cmrg_seed.md)
  in tests
  ([\#60](https://github.com/poissonconsulting/ssdsims/issues/60)).

- Remove spurious `.Random.seed` warning.

- Use default pillar settings.

## ssdsims 0.0.0.9008

### Chore

- Replace `seq_up()` with
  [`rlang::seq2()`](https://rlang.r-lib.org/reference/seq2.html)
  ([\#55](https://github.com/poissonconsulting/ssdsims/issues/55)).

### Continuous integration

- Update ccache-action reference.

- Bump action version.

## ssdsims 0.0.0.9007

- Ci: Unify fledge.yaml across cynkratemplate and fledge
  ([\#86](https://github.com/poissonconsulting/ssdsims/issues/86)).

## ssdsims 0.0.0.9006

### Chore

- Add ccache to `.gitignore` and `.Rbuildignore`.

### Continuous integration

- Create snapshot update PR against correct branch.

- Add reference to `/apply-patch` workflow in commit message.

- Clarify rationale for not deploying on schedule.

## ssdsims 0.0.0.9005

### Chore

- Auto-update from GitHub Actions.

  Run:
  <https://github.com/poissonconsulting/ssdsims/actions/runs/25267043302>

- Auto-update from GitHub Actions.

  Run:
  <https://github.com/poissonconsulting/ssdsims/actions/runs/22932710449>

- Auto-update from GitHub Actions.

  Run:
  <https://github.com/poissonconsulting/ssdsims/actions/runs/22789311191>

- Auto-update from GitHub Actions.

  Run:
  <https://github.com/poissonconsulting/ssdsims/actions/runs/22685862780>

- Auto-update from GitHub Actions.

  Run:
  <https://github.com/poissonconsulting/ssdsims/actions/runs/19879530192>

- Auto-update from GitHub Actions.

  Run:
  <https://github.com/poissonconsulting/ssdsims/actions/runs/19808536137>

- Auto-update from GitHub Actions.

  Run:
  <https://github.com/poissonconsulting/ssdsims/actions/runs/19751744050>

- Build-ignore.

- Auto-update from GitHub Actions.

  Run:
  <https://github.com/poissonconsulting/ssdsims/actions/runs/19489314581>

- Auto-update from GitHub Actions.

  Run:
  <https://github.com/poissonconsulting/ssdsims/actions/runs/19481934419>

- Format with air
  ([\#44](https://github.com/poissonconsulting/ssdsims/issues/44)).

### Continuous integration

- Add fledge workflow, no-op until it is used in this repo.

- Tweak ccache action.

- Cosmetics.

- Bump action versions.

- Install clang-format-21.

- Harmonize.

- Fix running without readr.

- Adapt workflows
  ([\#47](https://github.com/poissonconsulting/ssdsims/issues/47)).

### Documentation

- Fix title for
  [`?ssd_hc_sims`](https://poissonconsulting.github.io/ssdsims/reference/ssd_hc_sims.md)
  ([\#43](https://github.com/poissonconsulting/ssdsims/issues/43)).

### Uncategorized

- Merge branch ‘main’ of github.com:poissonconsulting/ssdsims.

- Merge branch ‘main’ of github.com:poissonconsulting/ssdsims.

- Merge pull request
  [\#46](https://github.com/poissonconsulting/ssdsims/issues/46) from
  poissonconsulting/copilot/rename-ssd-simulate-data.

## ssdsims 0.0.0.9004

- Vectorize
  [`ssd_fit_dists_sims()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_fit_dists_sims.md).

## ssdsims 0.0.0.9003

- Vectorize `ssd_hc_sims(ci_method)`.
- Vectorize
  [`ssd_sim_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_sim_data.md)

## ssdsims 0.0.0.9002

- Same as previous version.

## ssdsims 0.0.0.9001

- Merge pull request
  [\#30](https://github.com/poissonconsulting/ssdsims/issues/30) from
  poissonconsulting/joethorley/issue21.

- Merge branch ‘main’ of github.com:poissonconsulting/ssdsims.

- Pass … through
  [`ssd_run_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario.md).

- Add .progress = FALSE argument.

- Add `ci = FALSE` argument.

- Merge pull request
  [\#22](https://github.com/poissonconsulting/ssdsims/issues/22) from
  poissonconsulting/fits.

- Merge pull request
  [\#20](https://github.com/poissonconsulting/ssdsims/issues/20) from
  poissonconsulting/fits.

- Add `start_stream = 1L` argument.

- Renamed skip to start_seeds.

- Rename `ssd_get_seeds()` to `ssd_get_streams_seeds()`.

- Merge branch ‘main’ into seeds.

- Added `ssd_get_seeds()`.

- `generate_data()` vectorized for nrow.

- Added `ssd_generate_data()`.

## ssdsims 0.0.0.9000

- Added a `NEWS.md` file to track changes to the package.
