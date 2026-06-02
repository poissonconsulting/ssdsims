## Context

`TARGETS-DESIGN.md` §2 fixes per-task RNG as `dqrng::dqset.seed(seed = scenario$seed, stream = task_primer(task_params))`. `dqrng-init` landed the pcg64 backend; `local-dqrng-state` (in progress) lands the `(seed, state)` install/restore wrapper. The remaining primitive is the **primer derivation** itself: turning a task's parameters into the length-2 integer `state`. The mechanism is fully worked out and validated in `scripts/experiment-dqrng-hash.R` (sections 1–4): `rlang::hash()` gives a 32-hex-char xxhash128 digest; the first 16 hex chars (64 bits) are packed into two int32s for dqrng's `stream`; the `0x80000000` bit pattern maps to `NA_integer_` (dqrng reads it as INT_MIN). This change promotes that prototype to a tested package function.

## Goals / Non-Goals

**Goals:**

- Exported `task_primer(params)` returning a length-2 integer primer, deterministic in `params`, suitable for `dqset.seed(seed, stream = .)`.
- Internal `hex8_to_int32()` performing the signed-int32 conversion with the `0x80000000` → `NA_integer_` mapping (the §2 64-bit encoding).
- Document the canonical, name-keyed hash-input contract per RNG-consuming step (`sample`/`fit`/`hc`; `fit` truncates inline, RNG-free) and the `min_pmix`-by-name rule, so `task-tables` builds the right `params`.

**Non-Goals:**

- Building the `sample`/`fit`/`hc` task tables or deciding which columns exist (`task-list-loop-baseline` #80 + fold, `task-tables`).
- Calling `dqset.seed()` / installing the primer (`local-dqrng-state`) or wiring it into the per-task operations (`state-primitives`).
- Activating the dqrng backend (`dqrng-init`).

## Decisions

### Decision: lift the validated prototype verbatim, do not reinvent

`task_primer()` and `hex8_to_int32()` are copied from `scripts/experiment-dqrng-hash.R` (section 1), which is the validated reference (0 empirical collisions at 100 k tasks; reproducibility and dqrng-routing checked end-to-end). Keeping the package function byte-identical to the experiment preserves that validation and lets the script remain the living proof. *Alternative considered:* a different hash slice width (32 or 128 bits) — rejected; 32 bits collides too soon for large scenarios, and dqrng's `stream` is a 64-bit (length-2 int) quantity, so 64 bits is both the natural width and the validated one.

### Decision: `hex8_to_int32()` computes via double then wraps to signed int32

R's `strtoi(., 16L)` overflows on 8 hex digits, so the helper splits into two 16-bit halves, combines in double precision (`hi * 65536 + lo`), and maps `u >= 2^31` to `u - 2^32` (signed wrap), with `0x80000000` landing on `NA_integer_` because R encodes INT_MIN as `NA`. This is exactly the experiment's implementation. *Alternative considered:* `bitwShiftL`/`bitwOr` — rejected; those operate on R int32 and overflow on the high bit, the very value we must represent.

### Decision: `task_primer()` accepts a plain list or a task-table row, normalising the row to a canonical plain list

The `{sample,fit,hc}_tasks` tables are tibbles, and a task is one **row**, so passing the row directly is the natural call — and the §7 replay path reconstructs a task from its row. But a one-row tibble carries class / `row.names` / pillar attributes and wraps list-valued columns; none of that should enter (or destabilise) the hash. So `task_primer(params)` accepts **either** a plain named list **or** a single-row data frame, and when given a row it applies an internal `normalize_task_row()` — effectively the **inverse of `tibble::tibble_row()`** (answering the review question directly: yes, we need it): `as.list()` the row, drop all attributes, unwrap length-1 **list-style** columns to their element, but leave **df-style** (packed/nested data-frame) columns as data frames. The hashed value is that canonical plain list, so `task_primer(tasks[i, ])` and `task_primer(as.list(row))` agree byte-for-byte.

`task_primer()` normalises **structure, not meaning**. The *semantic* canonical form — `min_pmix` referenced by name, and `nrow` absent from the **`sample`** primer (see the next decision) — stays a caller/`task-tables` contract, because which columns exist in the row is a schema decision only the table construction can make; the function remains schema-agnostic so all four step tables share it. *Alternative considered:* require callers to pre-flatten rows themselves — rejected; the table runner and the §7 replay helper would each re-implement the same attribute-strip/unwrap, and any drift between them would silently move primers. Centralising the row→list inverse in `task_primer()` keeps one source of truth.

### Decision: where truncation lives (inline in `fit`) and what it means for primers

After `task-list-loop-baseline-fold`, the truncation lives **inline in the `fit` step**: the `sample` step draws `n_max = max(nrow)` rows once (`slice_sample`, RNG), and `fit` does a pure `head(sample, nrow)` — **RNG-free** — before fitting. Three consequences for the primer contract, all reflected in the spec:

1. **The truncation takes no primer.** `head()` consumes no randomness, so there is nothing to seed; there is no separate truncation task carrying `(seed, primer)`.
2. **`nrow` must be absent from the `sample` primer — load-bearing, not cosmetic.** The §5 sub-truncation guarantee (a size-`n` result is a byte-identical prefix of the size-`n_max` draw) holds only because every `nrow` shares *one* `sample` draw, keyed `(dataset, sim, replace)`. If `nrow` entered the sample primer, each size would seed a different stream and `head()` would no longer be a valid prefix — the property collapses. So excluding `nrow` from the sample primer is a correctness requirement.
3. **`nrow` IS in the `fit`/`hc` primers — and should be.** `nrow` is a `fit` axis (inherited by `hc`), so their primers vary with `nrow`. That is correct: a fit/hc on a 5-row truncation is a genuinely different computation from one on a 10-row truncation, and must get its own stream. The earlier reconciliation wrongly said "`nrow` never enters an RNG primer"; the precise statement is "`nrow` is excluded from the **sample** primer only".

**Replay implication (§7).** A failed `fit`/`hc` task reconstructs its data by reconstructing its parent **`sample`** draw — seed by the `sample` primer (looked up via the row's `sample_id` foreign key), draw `n_max`, then `head(nrow)` — exactly the truncation `fit` does inline. The replay-helper follows the `<parent>_id` chain to the `sample` row; `task-primer` only needs to guarantee that the `sample` primer is reproducible from `(dataset, sim, replace)`.

### Decision: validate input minimally

`params` must be a plain named list or a single-row data frame; we add a light `chk` guard to that effect (and, for a data-frame input, that it has exactly one row) so obvious misuse aborts with an actionable message in the user-facing frame. After normalisation we rely on `rlang::hash()` (any R object is hashable). *Alternative considered:* no validation — rejected; a bare scalar or a multi-row frame passed by mistake would hash to a valid-but-meaningless primer, so the guard earns its keep.

## Risks / Trade-offs

- **Hash stability across R / `rlang` versions** → if `rlang::hash()` changes its algorithm, primers (and thus RNG sequences) move. Mitigation: this is the same reproducibility envelope the whole design pins in the manifest (§9: pin R and package versions); the function is deterministic *within* a pinned environment, which is the contract. A test asserts stability against a recorded primer for a fixed `params` to catch an unexpected change.
- **Caller passes a function value, or includes `nrow` in the `sample` primer** → primer silently differs from intent, and an `nrow`-in-sample mistake would break the §5 sub-truncation property. Mitigation: documented contract + the spec scenarios; `task-tables` (downstream) builds `params` and is where the canonical form is assembled and tested at the table level (the `sample` primer is verified `nrow`-free; the `fit`/`hc` primers are verified to vary with `nrow`).
- **64-bit collision** → theoretical 50% at ~4.3 billion tasks, far beyond ssdsims' 10²–10⁴-task scenarios; validated at 100 k with 0 collisions. Accepted.
- **List-column vs df-column ambiguity in `normalize_task_row()`** → unwrapping must distinguish a list-style column (a length-1 list → unwrap to its element) from a df-style column (a one-row data frame → keep as a data frame). Mitigation: decide per column with `is.data.frame()` first (df-column, keep), then `is.list()` (list-column, unwrap `[[1]]`), else an atomic column (take the scalar) — and test a row carrying each kind.

## Migration Plan

Additive: a new file, one new export, one internal helper, and tests. No existing function changes, no data migration. Removing the file and its `NAMESPACE` entry fully reverts.

## Open Questions

- ~~Should `task_primer()` accept the eventual typed `task_params`/row object directly, or keep taking a plain list?~~ **Resolved** (review, krlmlr): accept **both** a plain list and a single-row data frame, normalising the row to a canonical plain list via `normalize_task_row()` (the inverse of `tibble::tibble_row()`). The semantic name-keyed form stays a `task-tables` contract.
- Does `normalize_task_row()` belong in this change or with `task-tables`? Kept here so the row→list inverse is defined alongside the function that depends on it; `task-tables` consumes it without re-implementing.
