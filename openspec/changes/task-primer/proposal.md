## Why

The targets redesign roots all per-task RNG in a single scenario `seed` plus a per-task **primer** passed to dqrng's `stream` argument: `dqrng::dqset.seed(seed, stream = task_primer(task_params))` (`TARGETS-DESIGN.md` §2). The primer is what makes each task's RNG fully specified by a small `(seed, state)` pair — no precomputed L'Ecuyer lattice, extension is implicit (new tasks hash to new primers), and a failed branch replays locally from its row. `dqrng-init` landed the backend and `local-dqrng-state` (in progress) lands the `(seed, state)` install wrapper; this step lands the missing piece — deriving the `state` (primer) from a task's parameters — so `state-primitives` can wire it into the per-task operations. The derivation is already validated in `scripts/experiment-dqrng-hash.R`.

## What Changes

- Add an exported `task_primer(params)` that derives a length-2 integer primer from `rlang::hash(params)` (xxhash128), packing **64 bits** as `c(hi32, lo32)` for dqrng's `stream` argument.
- Add the internal `hex8_to_int32()` helper that converts an 8-hex-char slice to a signed int32, mapping the reserved bit pattern `0x80000000` (INT_MIN) to `NA_integer_` — the §2 encoding dqrng accepts in `stream` and treats as INT_MIN, recovering the full 64 bits.
- Specify the canonical, name-keyed hash input per RNG-consuming step of the three-step model (`sample`: `(dataset, sim, replace)` — the draw, no `nrow`; `fit`: parent identity + `nrow` + fit-grid row; `hc`: parent identity + hc-grid row including `ci`), with function-valued parameters (`min_pmix`) referenced **by name** so a recompile/JIT does not move a task's primer. `fit` truncates its sample inline (`head(sample, nrow)`, RNG-free); the truncation takes no primer. Building the actual task tables is `task-list-loop-baseline` (#80 + fold) / `task-tables`; this change owns only the primer function and its input contract.
- Guarantee, by test, reproducibility (`task_primer(p)` is deterministic and stable for identical `p`), sensitivity (different `p` ⇒ different primer), and collision-resistance on the `scripts/experiment-dqrng-hash.R` examples.

## Capabilities

### New Capabilities
<!-- None: this extends the existing parallel-safe-seeding capability. -->

### Modified Capabilities
- `parallel-safe-seeding`: add the `task_primer()` derivation (hash → length-2 integer primer with the NA/INT_MIN 64-bit encoding) as the dqrng-path companion to the existing seed/state helpers.

## Impact

- **New code**: `R/task-primer.R` with exported `task_primer()` and internal `hex8_to_int32()`; `tests/testthat/test-task-primer.R`.
- **APIs**: New export `task_primer()`; `NAMESPACE`/`man/` entries. `hex8_to_int32()` stays internal.
- **Dependencies**: None added — uses existing `rlang` (`hash()`). Consumes (does not add) the `dqrng` backend from `dqrng-init`.
- **Downstream**: Unblocks `state-primitives`, which calls `local_dqrng_state(seed, primer)` once per task body using a primer from `task_primer()`; `task-tables` stores the resulting `(seed, primer)` on each task row.
