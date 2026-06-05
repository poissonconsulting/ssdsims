## Context

`TARGETS-DESIGN.md` §8 frames extension as one question per growth type: *does the new scenario change the set of shards, or the contents of an existing shard?* §8.1 (path-axis growth) is the "new shards, old ones untouched" case; §8.2 (inner-axis growth) is the "same shards, rewritten contents" case. Adding a value to an axis that is **not** in a step's `partition_by` — e.g. a new `min_pmix` for the `fit` step — does not mint new shards; it adds one task to **every** affected shard, so each affected shard must be **atomically rewritten** with its larger task set.

The mechanism for this already exists and is **landed**: static branching over the grouped shard tables (`task-tables`) plus per-layout result roots and per-shard step runners (`task-shards`). When the inner axis grows, the shard table's `tasks` list-column changes, `targets` marks exactly those step branches stale, the per-shard runner re-runs all K tasks in the shard, and the `format = "file"` target overwrites the shard's single Parquet. There is no append path: one Parquet per shard, rewritten whole.

What does **not** exist is the explicit **contract** for this behaviour, nor a test pinning it. §8.2 makes three load-bearing claims that nothing currently asserts: (1) the rewrite is whole-file (no in-place append); (2) the rows present before the growth re-emit **byte-identical** to the prior Parquet, because per-task `(seed, primer)` is unchanged and Parquet writes are deterministic; (3) shards in steps the growth does not touch stay cached. §8.4 (forced re-run after a code fix) and §9 (reproducibility) both inherit that byte-stability, so a silent regression here would corrupt their guarantees too.

This change adds **no new code and no new API**. It contributes one `task-shards` requirement and one end-to-end regression test.

## Goals / Non-Goals

**Goals:**

- Pin the §8.2 inner-axis-growth contract as a `task-shards` requirement: adding an inner-axis value atomically rewrites the affected step's shards (whole-file overwrite, no append), the prior rows re-emit byte-identical, and untouched steps stay cached.
- An end-to-end regression test that grows a `min_pmix` value, re-runs `tar_make()`, and asserts: affected `fit` shards rebuilt and overwritten; prior rows byte-identical to the captured prior Parquet; the added task's row present; untouched `sample` shards cached.
- Call out the byte-stability dependency on deterministic Parquet writes (fixed column order under a pinned `duckplyr`/DuckDB).

**Non-Goals:**

- Implementing the rewrite mechanism (already landed via `task-tables`/`task-shards`).
- Path-axis growth — new shards, old ones untouched (§8.1, pinned by `path-axis-growth`).
- Pinning shards *despite a code change* (§8.3) or forced re-run after a code fix (§8.4) — the latter is pinned by `mixed-code-lockin`, which builds on this rewrite contract.
- Finalising the cache-invalidation *model* (cache-by-existence vs. content-hash, the §8 fork) — that is `hive-partitioning`; this change is downstream of it.

## Decisions

### Decision: pin a contract + test, add no code

The rewrite-on-inner-growth behaviour falls out of static branching plus per-layout roots (`R/task-shards.R` / `R/targets-runner.R`, landed). The gap is purely that §8.2's three claims are asserted nowhere. So this change is a spec requirement plus a regression test — no exports, no new code paths. *Alternative considered:* fold the assertion into the existing `task-tables` byte-identity test — rejected; inner-axis growth is a distinct extension behaviour (a *re-run*, not a first build) and deserves its own named requirement and regression so a regression is attributable.

### Decision: use `task-shards` `## ADDED Requirements`, not `MODIFIED`

The new behaviour is an additional guarantee on top of the existing `task-shards` requirements, not a change to any of them. Adding a fresh requirement (rather than editing an existing one) also avoids colliding with other in-flight `task-shards` changes that touch the same spec. So the delta is `## ADDED Requirements` with one new requirement, "Inner-axis growth atomically rewrites the affected shards byte-stably."

### Decision: `min_pmix` (a `fit` inner axis) is the canonical test vector

§8.2's table lists `min_pmix` as the first inner-axis-growth example ("rewrite all fit shards, was K tasks each → now K+1"). It is the cleanest vector: `min_pmix` is inner to `fit` under the default `partition_by`, so growing it rewrites the `fit` shards while leaving the `sample` shards (a different step, not touched) cached. The test grows the scenario's `fit` grid by one `min_pmix`, re-runs `tar_make()` into the **same per-layout root** (the layout is unchanged — `min_pmix` is not a `partition_by` axis, so `scenario_results_dir()` is identical), and asserts the rewrite/byte-identity/cache split.

### Decision: byte-identity is checked against the captured prior Parquet

The test captures each affected `fit` shard's Parquet bytes before the growth, then after the re-run reads the rewritten shard and asserts the prior tasks' rows are byte-identical to the captured bytes (joined on the task-identity key) and that the file differs only by the added `min_pmix` task row(s). This is the read-back-value identity the `task-shards` "Targets results match the single-core baseline runner" requirement already uses as its oracle, extended across a re-run. The whole-file-overwrite claim is checked by confirming the shard is a single Parquet that changed (not an appended-to file).

### Decision: byte-stability rests on deterministic Parquet writes — called out, not introduced

"Prior rows come out byte-identical" depends on the Parquet writer emitting a fixed/deterministic column order under a **pinned** `duckplyr`/DuckDB. This change **calls that dependency out** in the requirement and test rationale; it does **not** introduce the pin — that rides with the pipeline's existing write helpers (`task-tables`/`task-shards`). §8.2 makes the same point: byte-stable writes are what make `format = "file"` content-hashing meaningful for the `summary` no-rebuild corollary.

### Decision: the precise cached-vs-rebuilt set finalises against `hive-partitioning`

§8 forks on the invalidation model — cache-by-existence (this section's model) vs. `targets`' native content-hash default — and `hive-partitioning` is the change that **pins** that fork over the m:n child↔parent shard fan-in. The byte-stable-rewrite contract (prior rows identical, whole-file overwrite) holds under *either* model. But the exact set of shards asserted *cached* vs. *rebuilt* — in particular whether an unaffected downstream shard whose upstream bytes are unchanged stays cached — is a function of that model. This change therefore states the dependency explicitly and finalises the test's cached-set assertion against the model `hive-partitioning` pins, rather than guessing it now. The `sample`-shards-stay-cached assertion (a different step entirely, with no changed input) holds regardless and can be asserted unconditionally.

## Risks / Trade-offs

- **Test brittleness vs. the invalidation model** → the byte-identity and whole-file-overwrite assertions are model-independent and can land now; the precise cached-vs-rebuilt *set* is sequenced behind `hive-partitioning` (see the decision above), so the test does not over-commit to a model that has not yet been pinned.
- **Byte-identity flakiness if the Parquet writer is not deterministic** → mitigated by leaning on the pinned `duckplyr`/DuckDB and deterministic column order the pipeline write helpers already establish; the test would surface any drift in that pin as a failure, which is the intended early-warning behaviour.
- **Redoing already-completed work** → §8.2 accepts this trade-off (Q2 in the design dialogue): the whole shard is recomputed to gain simpler cache semantics. To avoid the rewrite cost a user moves the axis into `partition_by` (turning inner-axis growth into path-axis growth, §8.1). This change pins the contract; it does not change the trade-off.

## Open Questions

- **Exact cached-vs-rebuilt assertion granularity.** The `sample`-stays-cached half is unconditional; whether the test also asserts on `hc` shards (whose `fit` upstream bytes change) is finalised once `hive-partitioning` pins the model — under content-hash propagation a `fit` shard that gained a row changes bytes and cascades, so any dependent `hc`/`summary` rebuilds; the precise expected set follows that model.
