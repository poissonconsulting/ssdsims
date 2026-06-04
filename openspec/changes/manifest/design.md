## Context

`TARGETS-DESIGN.md` §8.5 specifies a per-scenario manifest — a small JSON sidecar to the results directory — holding `seed`, `datasets`, `min_pmix`, `fit`, `hc`, `partition_by`, `completed_shards` (shard partition path → sha256, including the cloud copy's sha256 when uploaded), and toolchain versions (§8.5 names `r_version` / `dqrng_version` / `ssdtools_version` as examples). §9 leans on those versions for the bit-stability contract (`ssdtools`' RNG flow and `base::sample.int`'s sub-truncation are stable only within a pinned toolchain); this change records the **complete session info** rather than only the three named packages, so drift caused by any dependency is diagnosable. §7 leans on the per-shard sha256 so a lightweight replay can verify the local upstream against the cluster's actual bytes. Nothing in the package writes or reads such a manifest yet. This change adds it as a standalone capability, ahead of the `task-tables` pipeline that populates it.

## Goals / Non-Goals

**Goals:**

- A JSON writer/reader for the §8.5 declarative fields plus complete session info, round-tripping losslessly.
- A per-shard sha256 recording mechanism that is safe under parallel shard execution.
- Assembly of the per-shard records into the manifest's `completed_shards` map.

**Non-Goals:**

- Computing shard Parquets or the partition layout (that is `task-tables` / `hive-partitioning`); `manifest` only records what a shard write reports.
- Reading `completed_shards` to drive replay or completeness assertions (those are `replay-helper` / `shard-completeness-assert`); `manifest` only provides the data.
- The cloud upload itself (`cloud-upload`); `manifest` only carries the optional `cloud_sha256` field when a caller supplies it.

## Decisions

### Decision: JSON sidecar, not Parquet

§8.5 calls the manifest "a small JSON sidecar." JSON is human-readable, diffable, and portable, and the manifest is tiny (a few names, numeric knobs, version strings, and one entry per shard). The bulk results stay Parquet; only this metadata is JSON. `jsonlite::write_json(..., auto_unbox = TRUE, pretty = TRUE)` / `read_json(..., simplifyVector = FALSE)` give a stable round-trip. *Alternative considered:* a Parquet/`yaml` manifest — rejected; JSON matches the design and needs no extra heavy dependency beyond `jsonlite`.

### Decision: per-shard sidecars + assembly, not many writers mutating one JSON

§8.5 says "each step target writes each shard's sha256 **alongside the Parquet** on success." Shard targets run in parallel (§6), so having every shard append to one `manifest.json` is a write race. Decision: each shard writes its own small sidecar (e.g. `<shard-dir>/.sha256.json`) next to its Parquet — one writer per file, no contention — and a downstream **assembler** unions those sidecars into the manifest's `completed_shards` map. This is the natural reading of "alongside the Parquet" and mirrors the §6 fan-in that reads result directories rather than pulling shard values back. *Alternative considered:* a lock around a shared manifest — rejected as fragile on a cluster filesystem and unnecessary given the sidecar fan-in.

### Decision: `manifest` does not own Parquet paths; the recorder takes a caller-supplied shard directory

Path infrastructure already exists and is *not* duplicated here: `path_key()` (in `R/task-lists.R`, shipped) renders a row's Hive partition path from its axes, and the result-directory roots (`results/{sample,fit,hc}/…` and `results/datasets/…`) are introduced by `task-tables` and `registry`. `manifest` defines no paths of its own. `ssd_record_shard(dir, partition_key, sha256, ...)` is *handed* the shard's directory (the caller composes it from `path_key()` + the step's result root) and writes its sidecar there — so yes, the sha256 sidecar sits side-by-side with the shard's `part.parquet`, but the location is supplied, not computed by `manifest`.

Scope boundary (answering "does it belong in this change?"): `manifest` owns the manifest **document** — the sidecar record format, the reader, and the `completed_shards` assembler. The *invocation* — the "on success, write the sidecar" call inside each step target — lives in `task-tables`, where shard writes and their paths are defined (`task-tables` task 3.4 already calls the recorder). The recorder helper stays in `manifest` because it is the format owner and must agree with the reader/assembler; `task-tables` calls it. *Alternative considered:* move `ssd_record_shard()` wholesale into `task-tables` — rejected as the default because it would split the sidecar format from the reader/assembler that must round-trip with it; cheap to revisit if the boundary feels wrong.

### Decision: split the manifest into a stable head and an accreting tail

`ssd_write_manifest(scenario, dir)` writes the declarative head (scenario fields + session info) once at pipeline init — a pure function of the scenario and the toolchain. `completed_shards` is the accreting tail, assembled from the per-shard sidecars after a run (or incrementally). Keeping them separable means a re-run with the same scenario rewrites an identical head, and only the tail moves as shards complete. The reader returns both merged.

### Decision: capture complete session info, not just three version strings

Rather than pin only `r`/`dqrng`/`ssdtools` (the §9 examples), the manifest records **complete session info** captured when `ssd_write_manifest()` runs — R version, platform/OS, and every attached/loaded package version — via `sessioninfo::session_info()` (the poissonconsulting house tool), serialised to a structured `session_info` block (`platform` + a `name -> version` package map). The three bit-stability-critical versions (`r_version`, `dqrng_version`, `ssdtools_version`) are *also* surfaced as a flat convenience subset for quick reads and for the §9 reproducibility contract. *Why the full record?* Results can be moved by any dependency in the RNG path (e.g. an `ssdtools` transitive dep), not only the three named packages; recording the whole environment makes a re-run's drift diagnosable instead of guessable, at negligible cost (the manifest is tiny). *Alternative considered:* the three-string pin from §9 — rejected as too narrow; it cannot explain drift caused by an unnamed dependency. *Fallback:* `utils::sessionInfo()` if `sessioninfo` is unavailable. These are descriptive — the manifest records the toolchain a result set was produced under; it does not *enforce* a version on read (a mismatch is a signal for the replay/verify layer, not an abort here).

### Decision: sha256 over the shard Parquet bytes, via the shared helper

`ssd_record_shard()` computes the recorded sha256 with the same `ssd_file_sha256()` internal `registry` uses for its index, so dataset-index hashes and shard hashes are computed identically. The optional `cloud_sha256` is supplied by the caller (`cloud-upload`) and stored verbatim when present; `manifest` does not perform uploads.

## Risks / Trade-offs

- **Sidecar proliferation** (one `.sha256.json` per shard) → mitigated: they are tiny, live inside the shard's own partition directory, and are unioned by the assembler; they also double as the per-shard integrity record §7's replay reads.
- **Manifest/head drift if the scenario changes mid-run** → the head is a pure function of the scenario; the pipeline writes it at init from the same construction-time object that mints the shards (§6 static branching), so head and shard set cannot disagree within a run.
- **Shared `ssd_file_sha256()` / `digest` ownership with `registry`** → the two changes are parallel in the DAG; whichever lands first adds the helper and `digest` to a shared utils file and `DESCRIPTION`, the other reuses it. Neither orders the other.
- **`jsonlite` numeric/integer fidelity** → use `auto_unbox = TRUE` and read with `simplifyVector = FALSE`; cover integer `seed`/`nboot` and logical knobs in a round-trip test so coercions are caught.

## Open Questions

- **Manifest location.** `<results>/manifest.json` at the results root is the §8.5 default; whether a multi-scenario project namespaces manifests per scenario is deferred until more than one scenario shares a results tree (not a near-term need).
- **Incremental vs. post-run assembly.** Assembling `completed_shards` once after `tar_make()` returns is simplest and matches §6.2's "errors read after the run." An incremental assembler is a later optimisation if manifests are queried mid-run.
