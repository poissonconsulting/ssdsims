## Why

The non-axis values a step consumes live in two different homes today: `n_max`
(sample) and `ci` (hc) ride as **carried columns on the task row**, while
`proportion`/`samples` (hc) and `dists` (fit) are threaded from the **scenario
slice** (`scenario_step_slice()`). The split is acknowledged historical accident
(the archived `scalar-ci-flag` design: *"`proportion`/`samples` are
scenario-threaded for historical reasons; `ci` follows the closer `n_max`
analogue"*). A task row should mean exactly one thing — the task's **identity**.

Separately, the `sample` draw size is derived as `n_max = max(scenario$nrow)`.
Because that derived value sits on the `sample` task row, **widening `nrow`
re-draws**: a larger `max(nrow)` changes the `sample` shard's bytes and
invalidates the `fit` shards that read it (`TARGETS-DESIGN.md` §5;
`R/targets-runner.R` "Extending nrow does not produce a stale sample draw").
The `sample` and `fit` steps are extremely cheap relative to the hc bootstrap,
so there is no reason to couple the draw size to the `nrow` axis and pay that
churn.

## What Changes

- **New scenario setting `nrow_max`** (`chk_whole_number`, default a reasonably
  high value, recommended `1000L`) — the **fixed** shared-draw size, replacing
  the derived `n_max = max(scenario$nrow)`. The effective per-dataset draw is
  `min(nrow_max, nrow(data))` for `replace = FALSE` (a high default ⇒ the full
  permutation, the maximal no-churn draw) and `nrow_max` rows for
  `replace = TRUE`. Because the draw size is now **fixed**, adding `nrow` values
  (up to the effective draw size) never re-draws — the `sample` shard stays
  cached and only new `fit` shards (keyed by `nrow`) mint.
- **`nrow_max` is a simulation setting**, placed among the non-`ci`-gated
  simulation settings of `ssd_define_scenario()` — leading the block as the
  sample-level setting, before `dists` (fit) and `est_method`/`proportion` (hc)
  and ahead of `ci` (the draw happens regardless of `ci`; cf. the #139 ci-gating
  grouping). `nrow` is validated at construction against the effective draw size
  (`nrow <= nrow(data)` for `replace = FALSE`, `nrow <= nrow_max` for
  `replace = TRUE`).
- **Direction B — a task row carries only its identity.** The `sample` task
  table drops the `n_max` column and the `hc` task table drops the `ci` column;
  both values move into the scenario slice and are read there by the runners
  (the `ci = FALSE ⟹ bootstrap-only knobs NA` canonicalisation stays — it is
  keyed off the scalar `ci`, which now lives in the slice, not an emitted
  column). The `sample` runner computes its draw size from `nrow_max` + the
  dataset (both already in the slice). After this change every task row is
  *purely* `task_axes(step)` + `<step>_id` + parent FK (+ the per-row
  `seed`/`primer` the shard path already attaches).
- **BREAKING (pre-1.0)**: for a fixed `seed`, the realised `sample` draw changes
  (the draw is now `nrow_max`/full-permutation rows, not `max(nrow)`), so sampled
  data and everything downstream re-baseline. The `head(., nrow)` prefix property
  (`TARGETS-DESIGN.md` §5) is preserved. Snapshots re-record.

Not pursued: *Direction A* ("everything on the row" — make `proportion`/
`samples`/`dists` carried columns too) — rejected for per-row storage bloat and
the awkward constant list-columns `proportion`/`dists` would become.

## Capabilities

### Modified Capabilities
- `scenario-definition`: add the `nrow_max` simulation setting (validation,
  default, signature placement) and validate `nrow` against the effective draw
  size rather than only `nrow(data)`.
- `task-lists`: the `sample` task table no longer carries the `n_max` column
  (the draw size is computed by the runner from `nrow_max` + the dataset); the
  `hc` task table no longer carries the `ci` column; the baseline runner reads
  `nrow_max`/`ci` from the scenario.
- `parallel-safe-seeding`: the `sample`-step draw size is the effective
  `nrow_max` (per-dataset `min(nrow_max, nrow(data))` for `replace = FALSE`;
  `nrow_max` for `replace = TRUE`), not `max(nrow)`.
- `scenario-accessors`: the `sample` slice additionally carries `nrow_max`; the
  `hc` slice additionally carries `ci`.
- `task-shards`: the "widen `max(nrow)` re-draws the `sample` shard" rebuild
  contract is replaced — with a fixed `nrow_max`, extending `nrow` (within the
  draw size) leaves the `sample` shard cached and mints only new `fit` shards;
  the per-step slice enumeration gains `nrow_max` (sample) and `ci` (hc).

## Impact

- **Code — constructor/validation**: `R/scenario.R` (`nrow_max` arg + `chk`,
  default, signature group; `nrow`-vs-draw-size validation), `R/params.R`
  (roxygen `@param`).
- **Code — task tables/runner**: `R/task-lists.R` (`sample_task_grid()` drops
  `grid$n_max`; `hc_grid_tbl()` drops the emitted `ci` column; baseline runner
  reads `nrow_max`/`ci` from `scenario$…` and computes the draw size).
- **Code — slice/shard runners**: `R/targets-runner.R` (`scenario_step_slice()`
  carries `nrow_max`/`ci`; `ssd_run_sample_step()` computes the draw size from
  `nrow_max` + dataset instead of `t$n_max`; `ssd_run_hc_step()` reads `ci` from
  the slice instead of `t$ci`).
- **No primer/partition change**: neither `n_max` nor `ci` was ever in
  `task_axes()`, so this is pure storage/plumbing — `task_primer()`,
  `path_key()`, and `partition_by` are untouched.
- **Docs/roadmap**: `TARGETS-DESIGN.md` (§5 draw-size prose), the `ROADMAP.md`
  entry, `GLOSSARY.md` (rework the `n_max` carried-column
  entry; add `nrow_max`), regenerated `man/`, example scripts/templates that pass
  `nrow`, and the re-baselined snapshots.
- **Dependencies**: an **independent tidy-up** in the
  `scalar-ci-flag` / `dists-simulation-setting` / `est-method-setting`
  "axis/row → setting" family — no prerequisites, no dependants.
