## Context

`ssd_scenario_targets()` (in `R/targets-runner.R`) builds the static-branching pipeline by `tar_map()`ping one `format = "file"`, `error = "null"` target per `partition_by` path cell per step. Each step's command is constructed with `bquote()`, which injects the result directories as literals but leaves `scenario` as a **bare symbol**:

```r
bquote(ssd_run_sample_step(tasks, scenario, .(sample_dir)))
bquote({ sample_done; ssd_run_fit_step(tasks, scenario, .(sample_dir), .(fit_dir)) })
bquote({ fit_done;    ssd_run_hc_step(tasks, scenario, .(fit_dir), .(hc_dir)) })
```

`targets` tracks `scenario` as a global referenced by every one of these commands, so the **whole scenario object is a dependency of every shard target across all three steps**. Editing *any* scenario field therefore invalidates and rebuilds *all* shards — even a knob that feeds only one step. Concretely, `scenario$hc$samples` is consumed only by `ssd_run_hc_step()`, yet bumping it today rebuilds every `sample` and `fit` shard too. `TARGETS-DESIGN.md` §12 records this as a pre-existing caveat this change closes, and the function's own roxygen states the coupling as if it were necessary ("`scenario` is referenced as a global, so editing it invalidates the dependent shards").

The coupling is conservative, not necessary: each runner reads only a small, fixed slice of the scenario. Reading the three runners (`R/targets-runner.R`) and the accessors they call (`R/accessors.R`) pins exactly what each step consumes:

| Step | Scenario fields actually consumed | How |
| --- | --- | --- |
| `sample` | `data` (the materialised datasets), `partition_by$sample` | `scenario_dataset(scenario, t$dataset)`; `shard_path()` → `partition_by[["sample"]]` |
| `fit` | `fit$dists`, `min_pmix_fns`, `partition_by$sample` (parent read), `partition_by$fit` (own path) | `scenario$fit$dists`; `scenario_min_pmix()` via the primer; `read_parent_shards(..., "sample")`; `shard_path(..., "fit")` |
| `hc` | `hc$proportion`, `hc$samples`, `partition_by$fit` (parent read), `partition_by$hc` (own path) | `scenario$hc$proportion`, `scenario$hc$samples`; `read_parent_shards(..., "fit")`; `shard_path(..., "hc")` |

`seed` and `primer` are **not** part of any slice: they ride in each shard's `tasks` list-column (decorated at grouping time by `ssd_scenario_<step>_shards()`), so a `seed` change already moves the `tasks` value, not the slice. The class tag (`ssdsims_scenario`, asserted by each runner's `chk_s3_class`) must survive slicing so the runners' input contract is unchanged.

## Goals / Non-Goals

**Goals:**

- Project each step's `tar_map()` command onto the **minimal slice** of the scenario it consumes, so editing a step-irrelevant field leaves the *other* steps' shards cached.
- Keep the per-task results byte-identical to today (and to `ssd_run_scenario_baseline()`): the slice carries exactly the fields the runner reads.
- A deterministic, hashable slice so `targets`' dependency hash over the per-step global is stable across re-runs of the same scenario.

**Non-Goals:**

- Changing the runners' bodies or signatures — they still receive a scenario-shaped object (now the slice) and call the same accessors.
- The path-axis minimal-rebuild contract (appending a dataset / growing `nsim`) — that is `path-axis-growth`, the path-axis counterpart.
- Pinning the invalidation model (cache-by-existence vs. content-hash) — that is `hive-partitioning` (§8); this change finalises its assertion *against* whatever that decision lands (see below).

## Decisions

### Decision: a per-step slice helper, not three ad-hoc subsets

Add an internal `scenario_step_slice(scenario, step)` that returns the minimal `ssdsims_scenario`-classed sub-object the named step's runner consumes (the table above), keeping the class tag so each runner's `chk_s3_class()` and accessor calls (`scenario_dataset()`, `scenario_min_pmix()`) work unchanged on the slice. Centralising the projection in one helper keeps the "what does step *X* read" knowledge in one place next to `shard_path()`/`read_parent_shards()` (which already encode the per-step path-axis reads), rather than scattering three hand-cut `list()`s through the factory.

The slice for each step:

- `sample` → `data`, `partition_by[c("sample")]`.
- `fit` → `fit$dists` (and the fit fields the primer needs), `min_pmix_fns`, `partition_by[c("sample", "fit")]`.
- `hc` → `hc$proportion`, `hc$samples`, `partition_by[c("fit", "hc")]`.

Each slice retains only the named step's own path plus any parent paths it reads (so `read_parent_shards()`'s `scenario$partition_by[[parent]]` still resolves). The exact field set is pinned by what the runners read, validated by the byte-identity assertion that already exists for the pipeline (`task-shards`: "Targets results match the single-core baseline runner") — the slice must not drop a field a runner reads, or that assertion breaks.

### Decision: deterministic and hashable

The slice MUST be a pure function of `scenario` (no environment capture, no timestamps), so two sourcings of the same scenario produce byte-identical slices and therefore identical `targets` dependency hashes — re-sourcing `_targets.R` must not spuriously invalidate shards. The `min_pmix_fns` functions are already materialised on the scenario at construction and hash on *name*, not body (`scenario-accessors`), so carrying them in the `fit` slice does not couple the `fit` shards to a function-body edit. `data` tibbles are likewise materialised once at construction; they hash by value, which is the intended `sample`-only dependency (editing a dataset *should* rebuild `sample`).

### Decision: splice a per-step slice symbol into each command, not the bare `scenario`

Refactor `step_map()` so the step's slice is bound as a per-step global (e.g. `sample_slice`, `fit_slice`, `hc_slice`) and **spliced** into the command via `bquote()`'s `.()`, replacing the bare `scenario` symbol:

```r
sample_slice <- scenario_step_slice(scenario, "sample")
bquote(ssd_run_sample_step(tasks, .(sample_slice), .(sample_dir)))
```

`targets` then tracks each command's dependency as the *slice* it splices, not the whole scenario. Because `.()` inlines the slice value into the command expression at sourcing time (the same mechanism already used for the result-dir literals), the per-step slice — not `scenario` — is what enters the target's dependency hash, so a field outside a step's slice no longer reaches that step's shards. The barriers (`sample_done`/`fit_done`) and `tasks`/path-axis names are untouched. *Alternative considered:* keep `scenario` a global and add a `tar_cue()` filter — rejected; that fights `targets`' dependency model instead of giving it a correctly-scoped global, and would not survive a re-source cleanly.

### Decision: the assertion finalises against the `hive-partitioning` invalidation model

This change owns the *per-step input* dependency edge — which scenario fields reach which step's command. How a shard's *value* then propagates downstream (does a rebuilt-but-byte-identical shard re-run `summary`? does growing `n_max` cascade into `fit`?) is the cache-by-existence vs. content-hash fork that `hive-partitioning` pins (`TARGETS-DESIGN.md` §8, the deferred `data`-step-vs-fold decision). The new requirement's expected-cached set ("only `hc` and `summary` rebuild") is therefore stated **against the model `hive-partitioning` lands**, exactly as `path-axis-growth` defers its expected-cached set to the same decision. This is a hard dependency: the assertion cannot be finalised — and so this change cannot land — before `hive-partitioning` pins the model.

### Decision: an optional sibling accessor delta

`scenario_step_slice()` is a name-resolving projection in the same family as `scenario_dataset()`/`scenario_min_pmix()` (it isolates *part* of a scenario for a step's runner, deterministically and hashing-stably). A small `## ADDED Requirements` delta to `scenario-accessors` records the slice helper as a member of that capability, so the accessor family stays the single home for "reach a scenario value the runner needs." It is kept internal (no export), matching the bare-symbol-replacement role.

## Risks / Trade-offs

- **Dropping a field a runner reads** → the slice silently produces wrong/aborting shards. *Mitigation:* the slice field set is pinned by reading the runners; the existing byte-identity-vs-baseline assertion (`task-shards`) is the regression guard — if the slice omits a consumed field, baseline equality breaks.
- **Over-slicing (carrying a step-irrelevant field)** → re-introduces the very over-rebuild this change removes for that field. *Mitigation:* the new requirement's scenarios assert the *negative* (sample/fit stay cached when an hc-only / fit-only knob moves), which fails if a slice is too broad.
- **Colliding with other in-flight `task-shards` deltas** → other changes also modify `task-shards`. *Mitigation:* use `## ADDED Requirements` with a new requirement name rather than `## MODIFIED Requirements` on an existing one, so the deltas compose.
- **Slice hash instability across re-source** → would spuriously rebuild. *Mitigation:* the slice is a pure function of the scenario's already-materialised fields (functions hash by name, datasets by value), so re-sourcing the same scenario yields identical slices.

## Open Questions

- **Where the helper lives** — next to `shard_path()`/`read_parent_shards()` in `R/targets-runner.R` (the only caller) or in `R/accessors.R` (the accessor family). Leaning `R/targets-runner.R` since it is the factory's private helper; revisit if a second caller appears. Not load-bearing for the contract.
- **Whether `partition_by` parent entries can be narrowed further** — a step reads only the parent path it globs, so the slice could in principle carry just `partition_by[c(parent, step)]` (already the plan). No finer split is needed; the inner/path complement is per-step and already encoded by `scenario_partition_axes()`.
