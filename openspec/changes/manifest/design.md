## Context

`TARGETS-DESIGN.md` §8.5 specifies a per-scenario manifest — a small JSON sidecar to the results directory — holding `seed`, `datasets`, `min_pmix`, `fit`, `hc`, `partition_by`, `completed_shards` (shard partition path → sha256), and toolchain versions (§8.5 names `r_version` / `dqrng_version` / `ssdtools_version` as examples). §9 leans on those versions for the bit-stability contract (`ssdtools`' RNG flow and `base::sample.int`'s sub-truncation are stable only within a pinned toolchain); this change records the **complete session info** rather than only the three named packages, so drift caused by any dependency is diagnosable. §7 leans on the per-shard sha256 so a lightweight replay can verify the local upstream against the cluster's actual bytes. Nothing in the package writes or reads such a manifest yet. This change adds it as a **standalone, downstream** capability: it depends only on the scenario (for the head) and on the shards' *existence* (to hash them), never the reverse — it does not gate the `task-tables` pipeline. See the dependency decision below.

## Goals / Non-Goals

**Goals:**

- A JSON writer/reader for the §8.5 declarative fields plus complete session info, round-tripping losslessly.
- A per-shard sha256 recording mechanism that is safe under parallel shard execution.
- Assembly of `completed_shards` from the shards on disk (hashing them directly, or using a per-shard sidecar when present).

**Non-Goals:**

- Computing shard Parquets or the partition layout (that is `task-tables` / `hive-partitioning`); `manifest` only describes shards that already exist.
- Reading `completed_shards` to drive replay or completeness assertions (those are `replay-helper` / `shard-completeness-assert`); `manifest` only provides the data.
- The cloud upload itself, and any cloud-specific verification (`cloud-upload`); `manifest` records a single trusted-as-produced sha256 per shard and nothing cloud-specific (see the decoupling decision below).

## Decisions

### Decision: dependency direction — `manifest` does not gate `task-tables`

The manifest is provenance/verification metadata, so it must not sit upstream of the pipeline that produces the data it describes. Its **head** (scenario fields + session info) depends only on the scenario; its **`completed_shards`** depends on the shards already existing (they must be hashed), i.e. on `task-tables`' *outputs*. And `task-tables` reads nothing from the manifest. So `manifest → task-tables` would be a dependency inversion; the real edges are `ssd-define-scenario → manifest`, `manifest → replay-helper`, and `manifest → shard-completeness-assert` (with `cloud-upload` recording the cloud sha256). TARGETS-DESIGN.md §12's DAG is updated to match.

**Latest point it is needed.** As a *build* dependency: just before the first consumer (`replay-helper` / `shard-completeness-assert`); it never gates `task-tables`, `hive-partitioning`, or `cluster-pipeline`. *Operationally* (the one that bites): before the first expensive cluster run whose results you intend to trust/reproduce/replay — exact session info and the trusted-as-produced sha cannot be reconstructed after such a run. For local dev and the toy pipeline, never. The change can therefore be implemented at any time; it is sequenced by its consumers, not by `task-tables`.

### Decision: JSON sidecar, not Parquet

§8.5 calls the manifest "a small JSON sidecar." JSON is human-readable, diffable, and portable, and the manifest is tiny (a few names, numeric knobs, version strings, and one entry per shard). The bulk results stay Parquet; only this metadata is JSON. `jsonlite::write_json(..., auto_unbox = TRUE, pretty = TRUE)` / `read_json(..., simplifyVector = FALSE)` give a stable round-trip. *Alternative considered:* a Parquet/`yaml` manifest — rejected; JSON matches the design and needs no extra heavy dependency beyond `jsonlite`.

### Decision: the assembler hashes shards on disk; per-shard sidecars are the at-write-time enhancement

The baseline `completed_shards` assembler walks the results tree and hashes each shard's `part.parquet` directly, so it needs **no** hook into the `task-tables` runner — the shards are the truth, and the manifest is built *from* them after a run. This is what keeps `manifest` off `task-tables`' critical path (see the dependency decision below).

§8.5's "each step target writes each shard's sha256 **alongside the Parquet** on success" is an *optimization* for the cluster/replay path: recording at write time avoids re-hashing large files and, crucially, captures the **trusted-as-produced** sha — which post-hoc hashing of possibly-touched files cannot. That recording is per-shard `meta.json` sidecars (one writer per file, no race), and the same assembler unions sidecars where present and falls back to hashing for shards without one. The sidecar-writing call is wired in by the consumers that need it (`replay-helper` / `cloud-upload`), not by the happy-path pipeline. *Alternative considered:* a lock around a shared `manifest.json` — rejected as fragile on a cluster filesystem and unnecessary given the per-file fan-in.

### Decision: `manifest` does not own Parquet paths; the recorder takes a caller-supplied shard directory

Path infrastructure already exists and is *not* duplicated here: `path_key()` (in `R/task-lists.R`, shipped) renders a row's Hive partition path from its axes, and the result-directory roots (`results/{sample,fit,hc}/…`) are introduced by `task-tables`. `manifest` defines no paths of its own. `ssd_record_shard(dir, partition_key, sha256)` is *handed* the shard's directory (the caller composes it from `path_key()` + the step's result root) and writes its `meta.json` sidecar there — so yes, the sidecar sits side-by-side with the shard's `part.parquet`, but the location is supplied, not computed by `manifest`.

Scope boundary (answering "does it belong in this change?"): `manifest` owns the manifest **document** — the sidecar record format, the reader, and the `completed_shards` assembler (including the post-hoc hashing path). The at-write-time *invocation* — the "on success, write the sidecar" call — lives with the consumers that need the trusted-as-produced sha (`replay-helper`, `cloud-upload`), not with the happy-path `task-tables` runner. The recorder helper stays in `manifest` because it is the format owner and must agree with the reader/assembler. *Alternative considered:* move `ssd_record_shard()` wholesale into a consumer step — rejected because it would split the sidecar format from the reader/assembler that must round-trip with it.

### Decision: split the manifest into a stable head and an accreting tail

`ssd_write_manifest(scenario, dir)` writes the declarative head (scenario fields + session info) once at pipeline init — a pure function of the scenario and the toolchain. `completed_shards` is the accreting tail, assembled from the shards on disk (hashing them, or reading per-shard sidecars where present) after a run. Keeping them separable means a re-run with the same scenario rewrites an identical head, and only the tail moves as shards complete. The reader returns both merged.

### Decision: capture complete session info, not just three version strings

Rather than pin only `r`/`dqrng`/`ssdtools` (the §9 examples), the manifest records **complete session info** captured when `ssd_write_manifest()` runs — R version, platform/OS, and every attached/loaded package version — via `sessioninfo::session_info()` (the poissonconsulting house tool), serialised to a structured `session_info` block (`platform` + a `name -> version` package map). The three bit-stability-critical versions (`r_version`, `dqrng_version`, `ssdtools_version`) are *also* surfaced as a flat convenience subset for quick reads and for the §9 reproducibility contract. *Why the full record?* Results can be moved by any dependency in the RNG path (e.g. an `ssdtools` transitive dep), not only the three named packages; recording the whole environment makes a re-run's drift diagnosable instead of guessable, at negligible cost (the manifest is tiny). *Alternative considered:* the three-string pin from §9 — rejected as too narrow; it cannot explain drift caused by an unnamed dependency. *Fallback:* `utils::sessionInfo()` if `sessioninfo` is unavailable. These are descriptive — the manifest records the toolchain a result set was produced under; it does not *enforce* a version on read (a mismatch is a signal for the replay/verify layer, not an abort here).

### Decision: sha256 over the shard Parquet bytes, via the shared helper

`ssd_record_shard()` and the assembler compute sha256 with a shared `ssd_file_sha256()` internal (introduced by this change). `manifest` records one trusted-as-produced sha256 per shard and performs no uploads.

### Decision: one trusted sha256 per shard — no `cloud_sha256` field; `manifest` and `cloud-upload` are decoupled

§8.5 originally listed "the cloud copy's sha256 if `upload` is set" alongside the local sha. We drop that second field. Under a faithful byte-for-byte upload the cloud copy *is* the same bytes, so its sha256 equals the recorded `sha256`; the two diverge only if the upload rewrites the bytes or if "cloud sha256" means the provider's *native* digest (an S3 ETag / GCS crc32c — a different algorithm, useful only for a download-free integrity check). Neither is `manifest`'s concern. `cloud-upload` instead **ships the `meta.json` sidecar alongside the shard** and verifies a round-trip by re-downloading and re-hashing against the recorded `sha256` — the single trusted-as-produced value travels with the data and is the source of truth.

Consequence: `manifest` carries **no cloud-specific surface** (head + per-shard `sha256` + assembler), and `cloud-upload` becomes a pure downstream *reader* — it reads `completed_shards` / the sidecars to know what to ship and verifies by re-hash, needing nothing added to the sidecar format. The dependency is therefore one-way and loose (`cloud-upload → manifest`), so the two changes can be proposed, reviewed, and merged independently. *Alternative considered:* keep `cloud_sha256` as an optional passthrough — rejected as speculative surface for a consumer that does not yet exist, and as a needless coupling. If a download-free provider-checksum check is ever wanted, it is a deliberate future extension to the sidecar format (owned by `manifest`), proposed then.

## Risks / Trade-offs

- **Sidecar proliferation** (one `meta.json` per shard) → mitigated: they are tiny, live inside the shard's own partition directory, and are unioned by the assembler; they also double as the per-shard integrity record §7's replay reads.
- **Manifest/head drift if the scenario changes mid-run** → the head is a pure function of the scenario; the pipeline writes it at init from the same construction-time object that mints the shards (§6 static branching), so head and shard set cannot disagree within a run.
- **`digest` / `ssd_file_sha256()` ownership** → this change introduces both (in a shared utils file); `scenario-accessors` no longer needs them since it dropped dataset persistence, so there is no cross-change ordering to coordinate.
- **`jsonlite` numeric/integer fidelity** → use `auto_unbox = TRUE` and read with `simplifyVector = FALSE`; cover integer `seed`/`nboot` and logical knobs in a round-trip test so coercions are caught.

## Open Questions

- **Manifest location.** `<results>/manifest.json` at the results root is the §8.5 default; whether a multi-scenario project namespaces manifests per scenario is deferred until more than one scenario shares a results tree (not a near-term need).
- **Incremental vs. post-run assembly.** Assembling `completed_shards` once after `tar_make()` returns is simplest and matches §6.2's "errors read after the run." An incremental assembler is a later optimisation if manifests are queried mid-run.
