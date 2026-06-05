## Context

The `fit` step is the only step whose per-task result is non-tabular: a `fitdists` object. `shard-runner` requires every shard's Parquet to carry "the per-task results as columns", so the fit object has to live in a column of the `fit` shard's Parquet. The interim path in `R/targets-runner.R` does this with an encode/decode seam:

```r
encode_obj <- function(x) rawToChar(serialize(x, connection = NULL, ascii = TRUE))
decode_obj <- function(s) unserialize(charToRaw(s))
```

`ssd_run_fit_step()` writes `tibble::tibble(fit_id = t$fit_id, fit_blob = encode_obj(fit))` into the shard; `ssd_run_hc_step()` reads the parent shard back, picks `fit_tbl$fit_blob[fit_tbl$fit_id == t$fit_id]`, and `decode_obj()`s it before estimating the hazard concentration. The seam exists precisely so the encoding can be swapped without touching the call sites.

Why ASCII at all? Two constraints forced it. (1) **duckplyr cannot store a raw or list column** — `compute_parquet()` over a duckdb tibble has no column type for an R `raw` vector or a list-of-raw, so the object cannot be carried as-is. (2) The result must be **byte-identical to the in-memory baseline** (`shard-runner`'s "free re-layout" contract): the value `ssd_run_hc_step()` decodes must be the same object the baseline would have fed to the hc primitive. An ASCII `serialize()` into a `VARCHAR` satisfies both: a string is a first-class duckplyr column type and `unserialize(serialize(x, ascii = TRUE))` is lossless. The cost is that `ascii = TRUE` is ~2× the size of the binary serialisation and noticeably CPU-heavier to produce and parse; for runs with many or large fits the blob layer dominates shard size and the fit/hc I/O time.

There is also the §6 summary read path. `ssd_summarize()` reads the **hc** layer (not the fit layer), and at the DuckDB level it `select(-any_of(c("dists", "samples")))` so the heavy list-style columns are projected out before anything is pulled into R. The fit blob is not in the summary's input today, but the design intent is that a heavy non-tabular column is always *projectable out* — a reader that does not need the object never pays to decode it. Whatever encoding we choose must preserve that property: the blob column must be skippable by column projection, not something that forces a decode on read.

## Goals / Non-Goals

**Goals:**

- Replace the interim ASCII-`VARCHAR` blob with a more efficient lossless encoding for the `fit` step's `fitdists` object in the shard Parquet.
- Preserve the encode/decode seam in `R/targets-runner.R` so call sites stay unchanged beyond the column type/name.
- Keep the byte-identity oracle (round-trip equals the baseline's object) and the projectable-blob property (summary read path can skip the column without decoding).

**Non-Goals:**

- Changing the Hive partition layout, the inner/path axis split, or the m:n parent-shard resolution (`shard-runner` / `task-shards`).
- Storing a blob for any step other than `fit` (sample draws and hc estimates are tabular).
- Replacing duckplyr as the Parquet engine, or changing the summary's column set.

## Decisions

### Decision: keep the encode/decode seam; only the encoding and column type change

`encode_obj()`/`decode_obj()` stay the single seam through which a non-tabular result enters and leaves the shard Parquet. `ssd_run_fit_step()` and `ssd_run_hc_step()` keep calling them; the only thing that moves is *how* they encode and which Parquet column type carries the result. This keeps the change small, keeps the engine swappable, and keeps the byte-identity contract local to one pair of functions that a round-trip test can pin. *Alternative considered:* inlining the encoding at the two call sites — rejected; it would scatter the format and break the swappability the seam was built for.

### Decision: evaluate the candidate encodings against the three constraints

The candidates and how each scores against the byte-identity oracle, the duckplyr/Parquet column-type constraint, and the projectable-blob property:

- **(a) binary `serialize(ascii = FALSE)` in a base64-text or `BLOB` column.** Lossless (`unserialize` round-trips a `fitdists` exactly), so it passes the byte-identity oracle. The binary form is ~half the ASCII size and cheaper to produce/parse, which is the whole point. The column-type constraint is the crux: duckdb has a native `BLOB` type, but duckplyr's R-side cannot *materialise* a raw column, so the safe form is **base64 text in a `VARCHAR`** — a string column duckplyr handles, carrying the compact binary payload (still smaller than ASCII serialisation despite the ~4/3 base64 expansion, and far cheaper to serialise). It is projectable out like any other string column. Needs a base64 codec (base R has none; a small dependency or a vendored helper). This is the leading candidate.
- **(b) Arrow-native nested representation.** Store the object's structure as a nested/struct Arrow column. In principle the most "native" and most projectable. But a `fitdists` object is an arbitrary R S3 structure (model fits, call objects, environments) with no faithful Arrow schema; reconstructing it byte-identically from a nested column is not generally possible, so it risks failing the byte-identity oracle, and it would couple the format to the object's internal layout. Rejected unless (a) proves infeasible.
- **(c) sidecar object store keyed by `fit_id`.** Write each fit to its own file (e.g. an `.rds`) under the shard, and carry only `fit_id` in the Parquet. Trivially lossless and the Parquet stays tabular with no blob column at all (so projection is moot). But it multiplies file count (one extra file per fit), splits a shard's result across files (complicating the "one Parquet per shard" model and the atomic-rewrite/replay story), and `ssd_run_hc_step()` would read N sidecars instead of one Parquet scan. Heavier operationally than the problem warrants; kept as a fallback only if inline storage is shown to be the bottleneck for very large fits.
- **(d) jsonlite object serialisation (`jsonlite::serializeJSON()` / `unserializeJSON()`) in a JSON `VARCHAR`** *(reviewer-preferred, #101)*. Like the interim path it produces **text**, so it drops straight into the existing `VARCHAR` `fit_blob` column — duckplyr is happy with a string column, the column is projectable out exactly as today, and no `raw`/`BLOB` column type and no base64 codec are needed. Unlike a bare `serialize()` dump, `serializeJSON()` records R type/attribute structure as structured JSON, which is more inspectable and portable. The two open points are the same the others face: (1) **byte-identity** — `unserializeJSON(serializeJSON(fit))` must equal the baseline's `fitdists`; jsonlite reconstructs most R structure faithfully, but environments/closures buried in a model fit are the risk to pin with the round-trip oracle (the same exposure as (b), though jsonlite preserves far more R structure than a generic Arrow schema); and (2) **size/CPU** — JSON text is unlikely to beat binary on bytes, so it competes with the interim ASCII form rather than undercutting it, winning on robustness/inspectability/zero-codec rather than raw compactness. Folded into the benchmark gate below alongside (a).

### Decision (or Open Question): chosen format

Two candidates lead, both lossless and both carried in the existing `VARCHAR` `fit_blob` column (so duckplyr stays happy and the column stays projectable out, touching only `encode_obj()`/`decode_obj()`): **(a) binary `serialize(ascii = FALSE)` as base64 text** — the smallest/cheapest option, but needing a base64 codec — and **(d) jsonlite `serializeJSON()`** — the reviewer-preferred option (#101): text like the interim path, so zero codec and no column-type change, structured/inspectable, but unlikely to beat binary on bytes. The open question is therefore not just (a)'s **size/CPU break-even and base64 dependency** but the **compactness-vs-robustness trade between (a) and (d)**: ASCII `serialize` is itself fairly compact, so before locking an encoding in, the implementation SHALL benchmark `encode_obj`/`decode_obj` size and time on a representative `fitdists` across the interim ASCII-`VARCHAR`, base64-binary (a), and jsonlite JSON-`VARCHAR` (d) — and SHALL confirm the byte-identity oracle holds for each (the `serializeJSON`/`unserializeJSON` round-trip on a `fitdists` with model-fit internals is the case to verify for (d)). The encoding is chosen on that basis: (a) where compactness dominates, (d) where the zero-codec / inspectable-text properties are worth the size, and if neither clearly beats the interim ASCII form on realistic fit sizes the encoding stays as-is and this change just records the decision and tightens the spec contract. This is recorded as a benchmark-gated decision rather than an unconditional swap.

### Decision: the spec states the contracts, not the encoding

The spec delta adds a requirement that the non-tabular result uses an *efficient lossless* encoding with two testable properties — lossless round-trip through the shard Parquet (byte-identity oracle) and a projectable-out blob column — without naming `serialize`/base64. That keeps the spec stable if the codec is revisited, while the byte-identity and projection scenarios pin the behaviour a test must hold.

## Risks / Trade-offs

- **base64 expansion eating the binary win** → mitigated by the benchmark gate above: the swap lands only if binary+base64 is actually smaller/faster than ASCII on realistic fits; otherwise the interim encoding stays.
- **byte-identity regression** → mitigated by a round-trip test that asserts `decode_obj(encode_obj(fit))` is identical to `fit` *and* that a fit written to a shard Parquet and read back through the hc path equals the baseline's object (the existing oracle).
- **a new dependency for base64** → small and optional; if undesirable, a vendored base64 helper (a few lines over `serialize`'s raw vector) avoids it. Decided with the benchmark.
- **projection property silently lost** → mitigated by a test that the summary-style projection (`select(-any_of(...))` at the DuckDB level) drops the blob column without decoding it.

## Open Questions

- **encoding choice: binary+base64 (a) vs jsonlite (d) vs interim ASCII.** Decided by the benchmark in the chosen-format decision — compactness/CPU for (a) against the zero-codec, inspectable, no-new-column-type properties of (d), with the byte-identity oracle required of whichever wins. For (a) this also subsumes whether to take a small base64 dependency or vendor a helper; (d) needs no codec but adds a `jsonlite` dependency.
- **break-even for the sidecar fallback.** At what fit size inline storage stops being acceptable and option (c) becomes worth its file-count/operational cost — deferred until a real run shows the inline blob is the bottleneck (not a near-term need; current fits are small).
