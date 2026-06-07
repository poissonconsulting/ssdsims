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

### Decision: keep the interim ASCII-`VARCHAR` encoding (benchmark-gated; the swap does not land)

Two candidates led into the benchmark, both lossless in principle and both carried in the existing `VARCHAR` `fit_blob` column (so duckplyr stays happy and the column stays projectable out, touching only `encode_obj()`/`decode_obj()`): **(a) binary `serialize(ascii = FALSE)` as base64 text** — expected to be the smallest/cheapest option, but needing a base64 codec — and **(d) jsonlite `serializeJSON()`** — the reviewer-preferred option (#101): text like the interim path, so zero codec and no column-type change, structured/inspectable, but unlikely to beat binary on bytes. The gate was: benchmark `encode_obj`/`decode_obj` size and time on a representative `fitdists`, confirm the byte-identity oracle for each, and swap **only if** a candidate clearly beats the interim ASCII form — otherwise keep the interim encoding and just record the decision and tighten the spec contract.

The benchmark (`benchmark-blob-encoding.R` in this change dir; a 4-distribution `fitdists` from `ssd_run_scenario_baseline()`) settled it — and **against** the proposal's premise that ASCII is ~2× the binary size:

| encoding | in-R string | on-disk Parquet | encode | decode | lossless |
| --- | ---: | ---: | ---: | ---: | :---: |
| interim ASCII `VARCHAR` | 783.5 KB | 250.5 KB | 25.0 ms | 34.3 ms | ✅ |
| (a) binary + base64 | 1259.9 KB | 372.2 KB | 15.8 ms | 11.2 ms | ✅ |
| (d) jsonlite JSON | 52.0 KB | 8.2 KB | 32.8 ms | 14.3 ms | ❌ |
| (+) gzip + base64 *(not a listed candidate)* | 194.7 KB | 196.0 KB | 22.3 ms | 9.9 ms | ✅ |

What the numbers show:

- **(a) base64-binary is *larger*, not smaller** — ~1.6× in-R (1259.9 vs 783.5 KB) and ~1.5× on disk (372.2 vs 250.5 KB) than the interim ASCII. Two reasons the premise failed: `serialize(ascii = FALSE)` on a `fitdists` is only marginally smaller than the ASCII form (the object is mostly already-compact doubles), and base64's 4/3 expansion plus the fact that **Parquet already compresses the ASCII `VARCHAR` column for free** wipes out any binary win. (a) *is* faster to encode/decode, but the size regression is the dominant concern — the whole motivation was that the blob layer dominates *shard size*. **(a) fails the size half of the gate → rejected.**
- **(d) jsonlite is not lossless** — smallest by far, but `unserializeJSON(serializeJSON(fit))` does not reconstruct the fit: the TMB objective-function closure carried in `model$fn` comes back as data, not a closure (`all.equal()` flags the `fn` component), so it **fails the byte-identity oracle**. It also returns a class-`json` vector that duckdb rejects until unclassed. This is exactly the environments/closures-in-a-model-fit risk flagged for (d). **Rejected.**
- **The interim ASCII `VARCHAR` already satisfies every constraint** — lossless (oracle ✅), a plain string column duckplyr stores and can project out, and **no new dependency**. Parquet's column compression means its real on-disk cost (250.5 KB) is far below its in-R string size (783.5 KB).

Per the gate, since neither listed candidate clearly beats the interim form (one is worse, one is lossy), **the encoding stays as `rawToChar(serialize(x, ascii = TRUE))` and this change records the decision and tightens the spec contract** (the spec now states the lossless / projectable-blob contracts the round-trip and projection tests pin, without naming the codec). `encode_obj()`/`decode_obj()`, the `fit_blob` write, and the `decode_obj()` read are unchanged; no `DESCRIPTION` `Imports` entry is added.

The one lossless option that *does* beat the interim form is **gzip-then-base64** (the `(+)` row: ~3× smaller in-R, ~22% smaller on disk, faster to decode). It is not one of the (a)–(d) candidates this change evaluated, needs the base64 dependency plus a compression step, and its on-disk advantage over the already-Parquet-compressed ASCII column is modest, so it is recorded as a follow-up open question rather than smuggled into this change.

### Decision: the spec states the contracts, not the encoding

The spec delta adds a requirement that the non-tabular result uses an *efficient lossless* encoding with two testable properties — lossless round-trip through the shard Parquet (byte-identity oracle) and a projectable-out blob column — without naming `serialize`/base64. That keeps the spec stable if the codec is revisited, while the byte-identity and projection scenarios pin the behaviour a test must hold.

## Risks / Trade-offs

- **base64 expansion eating the binary win** → this is exactly what the benchmark found: base64-binary came in ~1.5–1.6× *larger* than the interim ASCII, so the swap did not land and the interim encoding stays.
- **byte-identity regression** → mitigated by a round-trip test that asserts `decode_obj(encode_obj(fit))` is identical to `fit` *and* that a fit written to a shard Parquet and read back through the hc path equals the baseline's object (the existing oracle). This is also what disqualified jsonlite (d), which silently loses the `model$fn` closure.
- **projection property silently lost** → mitigated by a test that the summary-style projection (`select(-any_of(...))` at the DuckDB level) drops the blob column without decoding it.

## Open Questions

- **gzip-then-base64 as a future encoding.** The only evaluated lossless option that beats the interim ASCII form (the `(+)` benchmark row: ~3× smaller in-R, ~22% smaller on disk, faster to decode). Out of scope here because it is not one of the (a)–(d) candidates this change set out to evaluate and it would add the base64 dependency plus a compression step; revisit as its own change if a real run shows the (already Parquet-compressed) ASCII blob is the shard-size bottleneck.
- **break-even for the sidecar fallback.** At what fit size inline storage stops being acceptable and option (c) becomes worth its file-count/operational cost — deferred until a real run shows the inline blob is the bottleneck (not a near-term need; current fits are small).
