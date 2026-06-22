## Why

The pipeline's duckplyr frames are created at duckplyr's permissive defaults
(`as_duckdb_tibble()` → `lavish`, `read_parquet_duckdb()` → `thrifty`), so two
failure modes are only ever a convention away: an accidental `nrow()`/`$` can
slurp a shard union into a one-CPU/1-GB worker (the OOM the `duckplyr-config`
change exists to prevent), and an un-translatable verb can **silently** fall
back to dplyr — materialising into R and quietly leaving DuckDB. `prudence =
"stingy"` turns both into loud, catchable errors instead of silent hazards, and
makes "everything computes in DuckDB, collect only when R needs the rows" an
enforced contract rather than a hope. A spike (this change's
`exploration/`) confirmed the internal fan-ins already satisfy this — stingy
passes clean with no new failures — so adopting it is a free hardening that
also future-proofs every later edit.

## What Changes

- The internal, never-collected fan-in reads (`ssd_summarise()`,
  `ssd_summarise_member()`, `ssd_summarise_design()`) read their Parquet with
  `prudence = "stingy"`, so the union/filter/projection/write provably stay in
  DuckDB and any future fallback fails loudly instead of materialising into R.
- `ssd_write_parquet()` stops downgrading an already-`duckplyr` frame to
  `lavish` at the write seam (the spike showed the re-wrap masks stingy right
  before the write); it preserves an existing frame's prudence and wraps a
  plain R tibble as `stingy`.
- The genuine R boundaries keep their **explicit** `collect()` (decoding
  `fit` objects, the cost-analysis timing reads) but switch the read to
  `stingy` so nothing materialises implicitly along the way.
- The cost-analysis timing reads compute per-task durations **in DuckDB** via
  the `dd$` escape hatch (`dd$epoch(.end) - dd$epoch(.start)`) before the
  explicit `collect()`, rather than `difftime()` in R after it.
- **BREAKING (opt-out provided)**: the upload read-back generics
  (`ssd_open_uploaded()`, `ssd_summarise_uploaded()`) gain a `prudence`
  argument defaulting to `"stingy"`, so a returned table no longer
  auto-materialises (an accidental `nrow()` on a remote glob errors instead of
  downloading); `prudence = "lavish"` restores the old auto-materialising
  behaviour, and `collect()`/`compute_parquet()` work unchanged.
- Every touched call site is reshaped from deeply nested calls into native
  `|>` pipelines (matching the codebase's pipe convention).

## Capabilities

### New Capabilities
<!-- none -->

### Modified Capabilities
- `duckplyr-config`: add the requirement that pipeline duckplyr frames are
  **stingy** — no implicit materialisation, no silent dplyr fallback,
  collection is always explicit — and that the Parquet write seam preserves a
  frame's prudence rather than forcing `lavish`.
- `cloud-upload`: the read-back generics `ssd_open_uploaded()` and
  `ssd_summarise_uploaded()` gain a `prudence` argument defaulting to
  `"stingy"` (a returned table no longer auto-materialises) with a documented
  `"lavish"` opt-out.

## Impact

- **Code**: `R/targets-runner.R` (`ssd_summarise()`, `ssd_write_parquet()`,
  `ssd_read_parquet()`), `R/design-targets.R` (`ssd_summarise_member()`,
  `ssd_summarise_design()`), `R/cost-analysis.R` (`read_step_timings()`,
  `design_fast_hc()`), `R/upload.R` (the two read-back generics).
- **Behaviour**: pipeline results are byte-/value-identical (stingy changes
  *when/whether* materialisation happens, never the rows); the only
  user-visible change is the upload generics' new stingy default, covered by
  the `prudence = "lavish"` opt-out.
- **Dependencies**: none new — `prudence` and the `dd$` escape hatch are
  existing duckplyr features (`dd$` resolves symbolically; no `dd` package is
  required at runtime).
- **Tests**: the existing `NOT_CRAN` fan-in suites are the oracle; add
  targeted assertions for the stingy reads, the write-seam prudence
  preservation, the in-engine duration math, and the upload opt-out.
