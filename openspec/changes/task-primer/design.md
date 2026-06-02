## Context

`TARGETS-DESIGN.md` §2 fixes per-task RNG as `dqrng::dqset.seed(seed = scenario$seed, stream = task_primer(task_params))`. `dqrng-init` landed the pcg64 backend; `local-dqrng-state` (in progress) lands the `(seed, state)` install/restore wrapper. The remaining primitive is the **primer derivation** itself: turning a task's parameters into the length-2 integer `state`. The mechanism is fully worked out and validated in `scripts/experiment-dqrng-hash.R` (sections 1–4): `rlang::hash()` gives a 32-hex-char xxhash128 digest; the first 16 hex chars (64 bits) are packed into two int32s for dqrng's `stream`; the `0x80000000` bit pattern maps to `NA_integer_` (dqrng reads it as INT_MIN). This change promotes that prototype to a tested package function.

## Goals / Non-Goals

**Goals:**

- Exported `task_primer(params)` returning a length-2 integer primer, deterministic in `params`, suitable for `dqset.seed(seed, stream = .)`.
- Internal `hex8_to_int32()` performing the signed-int32 conversion with the `0x80000000` → `NA_integer_` mapping (the §2 64-bit encoding).
- Document the canonical, name-keyed hash-input contract per step and the `min_pmix`-by-name / `nrow`-excluded rules, so `task-tables` builds the right `params`.

**Non-Goals:**

- Building the `{data,fit,hc}_tasks` tables or deciding which columns exist (`task-list-loop-baseline`, `task-tables`).
- Calling `dqset.seed()` / installing the primer (`local-dqrng-state`) or wiring it into the per-task operations (`state-primitives`).
- Activating the dqrng backend (`dqrng-init`).

## Decisions

### Decision: lift the validated prototype verbatim, do not reinvent

`task_primer()` and `hex8_to_int32()` are copied from `scripts/experiment-dqrng-hash.R` (section 1), which is the validated reference (0 empirical collisions at 100 k tasks; reproducibility and dqrng-routing checked end-to-end). Keeping the package function byte-identical to the experiment preserves that validation and lets the script remain the living proof. *Alternative considered:* a different hash slice width (32 or 128 bits) — rejected; 32 bits collides too soon for large scenarios, and dqrng's `stream` is a 64-bit (length-2 int) quantity, so 64 bits is both the natural width and the validated one.

### Decision: `hex8_to_int32()` computes via double then wraps to signed int32

R's `strtoi(., 16L)` overflows on 8 hex digits, so the helper splits into two 16-bit halves, combines in double precision (`hi * 65536 + lo`), and maps `u >= 2^31` to `u - 2^32` (signed wrap), with `0x80000000` landing on `NA_integer_` because R encodes INT_MIN as `NA`. This is exactly the experiment's implementation. *Alternative considered:* `bitwShiftL`/`bitwOr` — rejected; those operate on R int32 and overflow on the high bit, the very value we must represent.

### Decision: `task_primer()` hashes its input faithfully; the name-keyed canonical form is a caller contract

`task_primer(params)` is a pure function of `params` — it hashes whatever it is given. The "`min_pmix` by name" and "`nrow` excluded" rules are therefore properties of **how callers construct `params`**, which this change *documents and tests* but does not enforce by inspecting `params` (the function is intentionally schema-agnostic so all three step tables can share it). The spec's `min_pmix`-by-name and `nrow`-excluded scenarios are exercised by passing name-keyed params (a name string in place of a function; no `nrow` key for data tasks) and asserting primer stability — demonstrating the contract the `task-tables` step must honour. *Alternative considered:* a typed `task_params` object that strips function values and `nrow`; rejected as premature — the canonicalisation belongs with the task-table construction that knows each step's schema, and a schema-agnostic `task_primer()` keeps this change small and the function reusable.

### Decision: validate input minimally

`params` must be hashable by `rlang::hash()` (any R object is). We add a light `chk` guard that `params` is a list (the canonical name-keyed form) to catch obvious misuse with an actionable message in the user-facing frame, but otherwise rely on `rlang::hash()`. *Alternative considered:* no validation — rejected; a bare scalar passed by mistake would hash to a valid-but-meaningless primer, so the list guard earns its keep.

## Risks / Trade-offs

- **Hash stability across R / `rlang` versions** → if `rlang::hash()` changes its algorithm, primers (and thus RNG sequences) move. Mitigation: this is the same reproducibility envelope the whole design pins in the manifest (§9: pin R and package versions); the function is deterministic *within* a pinned environment, which is the contract. A test asserts stability against a recorded primer for a fixed `params` to catch an unexpected change.
- **Caller passes a function value or includes `nrow`** → primer silently differs from intent. Mitigation: documented contract + the spec scenarios; `task-tables` (downstream) builds `params` and is where the canonical form is assembled and tested at the table level.
- **64-bit collision** → theoretical 50% at ~4.3 billion tasks, far beyond ssdsims' 10²–10⁴-task scenarios; validated at 100 k with 0 collisions. Accepted.

## Migration Plan

Additive: a new file, one new export, one internal helper, and tests. No existing function changes, no data migration. Removing the file and its `NAMESPACE` entry fully reverts.

## Open Questions

- Should `task_primer()` accept the eventual typed `task_params`/row object directly (once `task-tables` defines one), or keep taking a plain list? Leaning plain list now; revisit when `task-tables` lands so the table can pass a row's name-keyed slice without an adapter.
