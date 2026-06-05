## Context

`TARGETS-DESIGN.md` §8.1 states the cheap-extension contract: adding a value to an axis that is **in `partition_by`** for a step creates new partition cells and therefore new shards, while existing shards' Parquets stay byte-identical and are reused — "no bookkeeping needed beyond `targets`' usual branch-level dependency tracking." A dataset name and a `sim` value are path axes for all three steps (§8.1 table), so appending a dataset or growing `nsim` is a pure path-axis growth: it mints new `tar_map()` targets at the next sourcing and leaves the rest cached. §6 grounds *why* this falls out for free — static branching makes extension "literally more named targets," and the targets lab's `static-extend` axis bumped a variant count `4 → 6` and confirmed the four originals are skipped while only the new variants and the fan-in rebuild.

The package already ships everything this exercises: `ssd_scenario_targets()` (the factory that `tar_map()`s one `format = "file"`, `error = "null"` target per path cell per step, wires the `tar_combine()` barriers, and adds the `summary`), the per-shard step runners, and `ssd_summarize()`. What is missing is an **end-to-end assertion** that a path-axis growth on a *real* scenario rebuilds only the necessary shards. This change adds that assertion as a new `task-shards` requirement plus its test. It writes no code and changes no behaviour; it pins behaviour the design leans on but never proves in the package.

## Goals / Non-Goals

**Goals:**

- Pin §8.1 end-to-end: after a completed `tar_make()`, appending a dataset (or growing `nsim`) re-sources to a strictly larger shard target set, builds **only** the new path cells' shards across `sample`/`fit`/`hc`, leaves every pre-existing shard **skipped**, and re-runs `summary` over the enlarged set.
- Do it on the shipped `ssd_scenario → ssd_scenario_*_shards → tar_map → tar_make` path, asserting build/skip status from `targets`' own metadata (`tar_progress()` / `tar_meta()`), not from wall-clock or file mtimes.

**Non-Goals:**

- The **inner-axis** rewrite contract (adding a value to an axis *not* in `partition_by`, which atomically rewrites every shard) — that is `shard-atomic-rewrite` (§8.2).
- The **step-slice** minimal-rebuild contract (a step-irrelevant knob leaving other steps cached) — that is `step-scenario-slice`.
- Pinning shards against *code* changes (`tar_cue(depend = FALSE)`, §8.3) or forced re-run after a fix (`tar_invalidate()`, §8.4).
- Defining or changing the invalidation model itself — that is `hive-partitioning` (§8); this change consumes whatever model it pins.

## Decisions

### Decision: a new ADDED requirement on `task-shards`, not a modification of an existing one

The cheap-extension behaviour is a property of the same factory/runner stack `task-shards` already specifies, so it belongs in that capability. We add it as `## ADDED Requirements` with a **new** requirement rather than editing an existing one, because several `task-shards` changes are in flight (`shard-atomic-rewrite`, `step-scenario-slice`); a fresh requirement avoids a delta collision on a shared header. *Alternative considered:* fold a scenario into the existing "A static-branching targets pipeline runs a scenario" requirement — rejected, it would collide with sibling deltas and conflate the steady-state run with the incremental-growth contract.

### Decision: the expected-cached set is locked to the landed `hive-partitioning` + `step-scenario-slice` model

§8 had an open fork between **cache-by-file-existence** (`file existence ⇒ cache hit`, §8.1) and **content-hash** invalidation for `format = "file"` targets. `hive-partitioning` has now pinned it: **cache-by-existence over per-child upstream edges** (each child shard names only the parent shard targets its tasks read). The requirement is written to hold under either model — the contract is "new path cells build, pre-existing shards do not rebuild, `summary` re-runs" — and the test's precise `tar_progress()` skip/build set is locked to the pinned model.

A second decision turned out to matter just as much, and it is **why this change is downstream of `step-scenario-slice` and not merely a sibling of it.** The factory references each step's scenario *slice* (not the bare `scenario` global) as the command's global dependency. For the **dataset** flavour, `step-scenario-slice` builds the `sample` slice **per dataset**, so an appended dataset is a brand-new path cell whose own slice the existing `sample` shards never carried — their commands stay byte-identical and they skip. Without that per-dataset slice, appending a dataset would change the one `sample`-step global and rebuild *every* `sample` shard (byte-identically, so `fit`/`hc` still skip, but `sample` would not). For the **`nsim`** flavour there was never an issue: `nsim` appears in no step's slice, so growing it changes no command. With both upstream changes landed, the contract holds on the shipped **default** factory — no `cue` is needed — which is what the test drives.

A `tar_outdated()`-only dataset-growth check already ships with `step-scenario-slice`; this change's test is the **stronger, end-to-end** counterpart — it runs the real second `tar_make()` and reads `built`/`skipped` from `tar_progress()`, corroborates the cached shards' **byte-identical** Parquet, asserts the **`summary` row count grew**, and adds the **`nsim` flavour** that `step-scenario-slice` does not cover.

### Decision: assert build/skip from `targets` metadata, not timing

Build-vs-skip is read from `tar_progress()` (`built`/`skipped` per target) and corroborated with `tar_meta()` for the new shards, both keyed by the deterministic `tar_map()` target names (e.g. `fit_step_<dataset>_<sim>_<rescale>` from the step's path axes). This is the addressable-named-shard property §6 calls out and is robust where mtime/wall-clock heuristics are not. The two `tar_make()` runs share one throwaway `targets` store under `withr::local_tempdir()` so the second run sees the first run's cache.

### Decision: same `partition_by` ⇒ same layout root ⇒ caching is possible

A path-axis growth keeps `partition_by` fixed, so `scenario_results_dir(scenario)` is unchanged (the layout-keyed root, `task-shards`' "Same layout yields the same root"). Reusing that root is *precisely* what lets the original shards' `format = "file"` outputs satisfy the cache: a changed `partition_by` would mint a fresh root and force a full rebuild, which is **not** path-axis growth. The test holds `partition_by` constant and grows only the dataset/`sim` axis.

### Decision: cover both growth flavours — dataset and `nsim`

§8.1's table lists *both* "new dataset name" and "`nsim` grows" as path-axis growth for all three steps. The requirement carries a `#### Scenario:` for each, since they exercise distinct axes (a new `dataset=` cell vs. new `sim=` cells) of the same contract.

## Risks / Trade-offs

- **Finalisation coupling to `hive-partitioning`.** The exact skip-set is not locked until that model lands → mitigated by writing the requirement model-agnostically and gating only the test's precise enumeration on the dependency, called out in Impact and above.
- **Target-name brittleness.** Asserting on `tar_map()` names couples the test to the suffix scheme → mitigated by deriving expected names from the shard tables (`ssd_scenario_*_shards()`) rather than hard-coding strings, so a suffix change surfaces in one place.
- **`summary` byte-stability.** §8.2 notes `summary` re-runs only because the shard *set* it reads grew (more rows ⇒ new bytes); the test asserts `summary` is `built` (not skipped) and that its row count reflects the added shards, so a spuriously byte-stable summary would be caught.
- **Test runtime.** Two full `tar_make()` runs per growth flavour → mitigated by a deliberately tiny scenario (one extra dataset / a `+1` `nsim`) and a small distribution set.

## Open Questions

- **Exact skip-set enumeration.** ~~Pending `hive-partitioning`'s invalidation-model decision~~ — **resolved**: `hive-partitioning` (cache-by-existence over per-child edges) and `step-scenario-slice` (per-dataset `sample` slice) have landed, and the test's precise `tar_progress()` skip/build set is locked to that model on the default factory (no `cue`).
- **`nsim` growth representation.** Whether growing `nsim` adds *only* the new `sim=` path cells (existing `sim` shards untouched) under every shipped `partition_by`, or interacts with a `bundle` that groups sims — confirmed for the default per-`sim` path layout the test uses; a bundled-`sim` layout is out of scope here.
