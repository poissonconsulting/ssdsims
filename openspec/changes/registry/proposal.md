## Why

The scenario references datasets and `min_pmix` entries **by name only** (`TARGETS-DESIGN.md` §1.1): names enter the per-task hash, function bodies and data-frame contents do not, so a recompile/JIT or a non-behaviour-changing source edit never invalidates a cached branch. For that contract to hold, *something* must resolve those names to concrete payloads once per project — each dataset name to a Parquet file, each `min_pmix` name to a pinned function value. No such resolution layer exists today: `resolve_min_pmix()` does an ad-hoc `ssdtools`/global-env lookup and aborts with "there is no `min_pmix` registry yet," and dataset tibbles only ever live inline on the scenario. `registry` is the **targets-only** step (§12) that closes this gap, and is the first step to introduce Parquet I/O to the package.

## What Changes

- Add `ssd_register_dataset(name, data)` — writes `results/datasets/<name>.parquet` and updates `results/datasets/_index.json` (`name -> { rows, conc_col, sha256, source }`). Verifies the SSD `Conc`-column invariant (§1.1) at registration; other columns pass through.
- Add `ssd_register_min_pmix(name, fn)` — pins a single-argument function under a name for per-project resolution.
- Add a single **registry resolver** that, given a scenario, resolves each *referenced* dataset name (by **persisting** the inline tibble the scenario already carries — not regenerating it) and each referenced `min_pmix` name (to a pinned function value), once per project. One step, not two: datasets and `min_pmix` are the same name-only indirection (§1.1), differing only in the resolved payload (a Parquet file vs. a pinned function value).
- Reject re-registration under an existing name unless the bytes are identical (resolves the §11.2 open question in favour of *refuse-unless-identical*); function-body / data edits therefore cannot silently collide.
- Replace `resolve_min_pmix()`'s ad-hoc lookup with resolution through the registry (falling back to the current `ssdtools`/global-env lookup only when no registry entry exists, so the baseline runner keeps working).
- Introduce Parquet interaction via `duckplyr` (the team's Parquet engine — DuckDB; see `AGENTS.md`), behind thin `ssd_read_parquet()` / `ssd_write_parquet()` internals, plus a shared file-sha256 helper.

## Capabilities

### New Capabilities
- `registry`: a name-keyed registry of the scenario's datasets and `min_pmix` functions — register, resolve, and persist each referenced name once per project (datasets to Parquet, `min_pmix` to a pinned value), with a `Conc` invariant, a content-hash index, and refuse-unless-identical re-registration.

### Modified Capabilities
<!-- None: `resolve_min_pmix()` is an internal helper, not a spec-level requirement; the baseline runner's observable behaviour is unchanged when no registry entry exists. -->

## Impact

- **New code**: `R/registry.R` (`ssd_register_dataset()`, `ssd_register_min_pmix()`, the resolver, the `_index.json` reader/writer, the `Conc` check); a shared `ssd_file_sha256()` internal; rewiring of `resolve_min_pmix()` in `R/task-lists.R` to consult the registry first. Tests in `tests/testthat/test-registry.R`.
- **APIs**: New exports `ssd_register_dataset()`, `ssd_register_min_pmix()`, and the resolver entry point. Roxygen/`man/` and a new `_pkgdown.yml` reference group.
- **Dependencies**: adds `duckplyr` (Parquet I/O — the team preference, see `AGENTS.md`) and `digest` (file sha256) to `Imports`.
- **On-disk layout**: introduces `results/datasets/<name>.parquet` and `results/datasets/_index.json` (§1.1).
- **Downstream**: unblocks `task-tables` (§12) — the per-shard step bodies read each dataset's persisted Parquet and resolve `min_pmix` by name through the registry. **Depends on** `scenario-input-types` (§12) only softly: that step materialises generator inputs inline at construction, so the dataset side of `registry` merely *persists* the tibble the scenario already holds; name-only regeneration is the deferred `dataset-provenance` step.
