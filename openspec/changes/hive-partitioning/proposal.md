## Why

The Hive-partitioned **storage** half of `TARGETS-DESIGN.md` §6 has already landed: the `task-shards` and `shard-runner` live specs ship `ssd_run_sample_step()`/`ssd_run_fit_step()`/`ssd_run_hc_step()` writing one `format = "file"` Parquet per `partition_by` path cell, `read_parent_shards` resolving the **m:n** child↔parent fan-in at read time (projecting each task's `<parent>_id` onto the parent's path axes), and a `ssd_scenario_targets()` factory whose results are byte-identical to the single-core baseline. That work is done and is **not** re-specified here.

What is *not* settled is the **invalidation model** (§8). The shipped factory wires the steps together with coarse `tarchetypes::tar_combine()` barriers (`sample_done` / `fit_done` / `hc_done`) and no `tar_cue`: each step's shard targets merely *order after* every upstream shard target. So `targets`' default content-hash propagation, riding on a step-wide barrier, means **rewriting one parent shard marks the whole downstream step out of date** — every child shard re-runs, not just the children that actually read the rewritten parent. The §8 cache-by-existence-vs-content-hash fork, the §8.3 code-pin (`tar_cue(depend = FALSE)`) semantics, and the deferred §8 `data`-step-vs-fold decision are all still open, and the downstream steps (`shard-atomic-rewrite`, `path-axis-growth`, `step-scenario-slice`) cannot finalise their expected-cached-set assertions until they are pinned.

This change scopes to the **caching/invalidation half only**: pin the shard invalidation model and replace the coarse barriers with per-child Option-3 upstream edges (§6), so a one-parent-shard rewrite invalidates exactly the set of child shards that read it.

## What Changes

- **Pin the shard invalidation model (§8).** Decide and implement the `tar_cue` / content-hash semantics for the shard `*_step` targets. The model is **content-hash via `format = "file"`** (the storage half already chose `format = "file"`): a shard is up to date iff its output Parquet exists and the bytes its body depends on are unchanged; a code/dependency pin is available via `tar_cue(depend = FALSE)` (§8.3) for the trusted-shard case, with the documented carve-outs (a missing output, a changed task-table grouping, or an errored target still rebuilds). This is now a stated requirement of `task-shards`, not an emergent default.
- **Per-child Option-3 upstream edges (§6).** Replace the step-wide `tar_combine()` barriers with per-child edges computed at sourcing time: each child shard target names the specific parent shard target(s) its tasks read (the set of parent path cells the child's `<parent>_id` values project onto), so rewriting one parent shard invalidates only the child shards that read it — not the whole downstream step. The portable partition path remains the read-time contract; the named edge is the additional static-branching dependency the cache propagates over.
- **Settle the deferred `data`-step-vs-fold decision (§8).** As part of pinning the model, record that under content-hash invalidation the fold (`head(sample, nrow)` inside `fit`, no materialised `data` shard) is retained, and state the consequence the model carries (growing `max(nrow)` rewrites the `sample` shards that feed the widened fit tasks; a stale short draw is not produced because the widened parent's bytes change and propagate over the per-child edge). No `data` step is reinstated.

## Capabilities

### New Capabilities
<!-- None: this change refines an existing capability's invalidation contract; no new capability is introduced. -->

### Modified Capabilities
- `task-shards`: adds the invalidation-model requirements (the content-hash/`tar_cue` semantics, the per-child Option-3 fan-in edges, and the settled fold decision). The storage/read requirements already in the live spec are unchanged.

## Impact

- **Code**: `R/targets-runner.R` — `ssd_scenario_targets()` drops the `sample_done`/`fit_done`/`hc_done` `tar_combine()` barriers and instead wires each child `*_step` target to its specific parent `*_step` target name(s), computed at sourcing time from the shard tables (a helper that maps a child shard's tasks to the parent path cells they read). The `tar_cue` policy is set on the shard targets. `summary` continues to read the result directories from disk (it does not depend on each shard value).
- **No storage change**: the Parquet layout, the per-shard runners, and `read_parent_shards`' m:n read are untouched — results stay byte-identical to the baseline.
- **Tests**: `tests/testthat/test-task-shards.R` (or a sibling) gains coverage that rewriting one parent shard re-runs only the child shards reading it and leaves the rest cached, and that the `tar_cue(depend = FALSE)` pin holds against a code edit (with the documented carve-outs).
- **Downstream dependency**: `shard-atomic-rewrite` (§8.2 inner-axis rewrite), `path-axis-growth` (§8.1 minimal-rebuild-on-growth), and `step-scenario-slice` finalise their expected-cached-set assertions **against the invalidation model this change pins**. They are downstream of `hive-partitioning`; their assertions are not authoritative until this change lands.
- **Docs**: roxygen on `ssd_scenario_targets()` describing the per-child edge wiring and the cue policy; `TARGETS-DESIGN.md` §8's deferred-decision note is settled by this change (the doc itself is edited by a separate pass, not here).
