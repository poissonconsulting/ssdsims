## Context

`TARGETS-DESIGN.md` §8.5 specifies a per-scenario manifest — a small JSON sidecar to the results directory — holding `seed`, `datasets`, `min_pmix`, `fit`, `hc`, `partition_by`, `completed_shards` (shard partition path → sha256, including the cloud copy's sha256 when uploaded), and `r_version` / `dqrng_version` / `ssdtools_version`. §9 leans on those version pins for the bit-stability contract (`ssdtools`' RNG flow and `base::sample.int`'s sub-truncation are stable only within a pinned toolchain). §7 leans on the per-shard sha256 so a lightweight replay can verify the local upstream against the cluster's actual bytes. Nothing in the package writes or reads such a manifest yet. This change adds it as a standalone capability, ahead of the `task-tables` pipeline that populates it.

## Goals / Non-Goals

**Goals:**

- A JSON writer/reader for the §8.5 declarative fields plus version pins, round-tripping losslessly.
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

### Decision: split the manifest into a stable head and an accreting tail

`ssd_write_manifest(scenario, dir)` writes the declarative head (scenario fields + version pins) once at pipeline init — a pure function of the scenario and the toolchain. `completed_shards` is the accreting tail, assembled from the per-shard sidecars after a run (or incrementally). Keeping them separable means a re-run with the same scenario rewrites an identical head, and only the tail moves as shards complete. The reader returns both merged.

### Decision: version pins captured at write time

`r_version <- as.character(getRversion())`, `dqrng_version <- as.character(utils::packageVersion("dqrng"))`, `ssdtools_version <- as.character(utils::packageVersion("ssdtools"))`, recorded when `ssd_write_manifest()` runs (§9). These are descriptive pins for the reproducibility contract — the manifest records the toolchain a result set was produced under; it does not *enforce* a version on read (a mismatch is a signal for the replay/verify layer, not an abort here).

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
