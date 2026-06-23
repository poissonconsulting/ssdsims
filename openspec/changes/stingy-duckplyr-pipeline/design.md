## Context

The pipeline reads and writes Parquet through a thin duckplyr seam
(`ssd_read_parquet()`/`ssd_write_parquet()`) and fans shards in through three
never-collected DuckDB pipelines (`ssd_summarise()`,
`ssd_summarise_member()`, `ssd_summarise_design()`). Today those frames carry
duckplyr's default prudence — `lavish` for `as_duckdb_tibble()`, `thrifty`
for `read_parquet_duckdb()` — which permits two silent hazards:

1. **Accidental materialisation.** A stray `nrow()`/`$`/`[[` pulls a shard
   union into R. On the one-CPU/1-GB cluster worker that is the OOM the
   `duckplyr-config` change already fights with thread/memory caps.
2. **Silent dplyr fallback.** An un-translatable verb makes duckplyr
   materialise columns and run the op in R, leaving DuckDB without a sound —
   the opposite of "compute everything in DuckDB".

`prudence = "stingy"` forbids both: a stingy frame never auto-materialises and
cannot fall back (a fallback needs materialised columns first), so an
unsupported verb errors loudly. `collect()` still works on a stingy frame
(duckplyr promotes it to `lavish` for that one call), so "collect only when R
needs the rows" is exactly the stingy contract.

A spike (preserved in `exploration/`) applied stingy to all three fan-in reads
plus a prudence-preserving write seam, reinstalled, and ran the `NOT_CRAN`
suites: `test-design-targets` 25/0, `test-run-scenario` 15/0,
`test-cost-analysis` 13/0, with the same 6 pre-existing (unrelated) baseline
failures and **zero** new ones. The fan-in verbs (`select(any_of)`,
`filter(%in%)`, constant `mutate`, `union_all`, `compute_parquet`) all execute
in DuckDB — no `dd$` rescue is needed there.

## Goals / Non-Goals

**Goals:**

- Make stingy the prudence of every internal pipeline duckplyr frame, so
  no implicit materialisation and no silent fallback are possible.
- Keep R-boundary collection **explicit** (decode `fit` objects; the
  cost-analysis timing reads) — stingy read, intentional `collect()`.
- Push the cost-analysis per-task duration math into DuckDB with the `dd$`
  escape hatch instead of `difftime()` in R.
- Give the upload read-back generics a stingy default with a `lavish`
  opt-out, preserving their lazy/composable contract.
- Reshape every touched nested call into a native `|>` pipeline.

**Non-Goals:**

- Changing any result. Stingy alters *when/whether* materialisation happens,
  never the rows; the `duckplyr-config` value-identity guarantee is unchanged.
- Re-piping flat stepwise code that is not deeply nested (the fan-in bodies
  are already stepwise reassignments and stay as-is).
- Removing or weakening the `skip_on_cran()` gate on the slow targets
  pipeline tests — it is correct and unrelated to this change.

## Decisions

### D1. Stingy on the three never-collected fan-in reads
Thread `prudence = "stingy"` into the `read_parquet_duckdb()` call in
`ssd_summarise()`, `ssd_summarise_member()`, and `ssd_summarise_design()`.
These never `collect()`, so stingy is a pure contract upgrade — proven safe by
the spike. *Alternative considered:* leave them `thrifty` and add a test that
asserts no fallback. Rejected — `thrifty` still permits a future fallback to
materialise silently; stingy makes the engine itself the enforcer.

### D2. The write seam preserves prudence
`ssd_write_parquet()` currently does
`compute_parquet(as_duckdb_tibble(df), path)`. `as_duckdb_tibble()` defaults to
`lavish`, so it **downgrades a stingy fan-in frame to lavish right before the
write** — masking exactly the fallback stingy is meant to catch (the spike
surfaced this). New behaviour: if `df` is already a `duckplyr_df`, pass it to
`compute_parquet()` unchanged (keeping its prudence); otherwise wrap a plain R
tibble as `stingy`. *Alternative:* always force stingy. Rejected — re-wrapping
an existing frame is what loses its prudence; pass-through is both simpler and
correct.

### D3. Explicit collect at the R boundaries, stingy read
`ssd_read_parquet()` and the cost-analysis reads genuinely need rows in R
(decoding `fit` objects; computing/returning timings). Keep the explicit
`collect()`; switch the read to `stingy` so nothing materialises implicitly
between read and the deliberate collect. `collect()` on a stingy frame is the
sanctioned escape, so this composes cleanly.

### D4. In-engine duration math via `dd$epoch`
The cost-analysis reads compute `seconds = as.numeric(difftime(.end, .start))`
in R after `collect()`. Move it before the collect as
`mutate(seconds = dd$epoch(.end) - dd$epoch(.start))`. `dd$epoch()` on a
`TIMESTAMP` returns fractional seconds since the epoch, so the difference is
sub-second-exact (a standalone probe returned `1.25` for a 1.25 s gap, matching
`difftime(units = "secs")`). `dd$` resolves symbolically inside duckplyr's
translation layer — no `dd` package is needed at runtime; the symbol is never
evaluated as an R object, so `@autoglobal`/`globalVariables` handling for `dd`
must be confirmed under `R CMD check` (see Risks). *Alternative:*
`dd$date_diff('second', ...)` — rejected, it truncates to whole seconds.

### D5. Upload generics: stingy default + lavish opt-out
`ssd_open_uploaded()` and `ssd_summarise_uploaded()` gain
`prudence = "stingy"`, threaded into their `read_parquet_duckdb()` calls. The
returned table stays lazy and composable (`collect()`/`compute_parquet()`
unchanged), but an accidental `nrow()` on a remote glob now errors instead of
downloading. `prudence = "lavish"` restores the prior auto-materialising
behaviour for callers who want it. *Alternative:* keep upload at `thrifty`
(internal-only stingy). Rejected per the chosen scope — but the opt-out is the
concession that keeps the published contract usable.

Both generics place a `...` **before** the optional arguments
(`ssd_open_uploaded(upload, step, ..., prudence)`;
`ssd_summarise_uploaded(upload, step, ..., drop_samples, prudence)`), so
`drop_samples`/`prudence` are keyword-only, mirroring `ssd_upload_azure()`'s
`...`-before-`prefix` convention. The concrete (Azure) methods open with
`rlang::check_dots_empty()` so a mis-positioned argument aborts loudly rather
than being silently swallowed into the dots (a `lavish` passed positionally
must not vanish). *Alternative:* trailing `prudence` with no dots. Rejected —
it invites brittle positional calls and silent arg drift as the signature
grows.

### D6. Pipe style on touched sites only
Reshape the deeply nested touched calls (e.g.
`as_tibble(collect(distinct(select(rel, ...))))`) into native `|>` chains —
the codebase is native `|>` throughout (no magrittr). Limited to lines D1–D5
already modify, plus the optional `duckplyr_current_settings()` drive-by (same
file/concern, same `as_tibble(collect(read_sql_duckdb(...)))` nest).

## Risks / Trade-offs

- **`dd$epoch` and `R CMD check` globals** → `dd` is an undefined symbol to
  static analysis. Confirm the existing `@autoglobal`/`globalVariables`
  machinery covers it (or add `dd` to the globals) so check stays clean;
  the spike proved the runtime behaviour, not the check-time lint.
- **Upload stingy default is a behaviour change** → mitigated by the
  `prudence = "lavish"` opt-out and a documented note; `collect()`/`count()`
  round-trip checks still work because explicit collection is unaffected.
- **A future fan-in edit introduces an un-translatable verb** → that is the
  *intended* outcome: stingy turns it into a loud test failure instead of a
  silent OOM. The remedy is the `dd$` escape hatch (rewrite the verb
  in-engine), not reverting to lavish.
- **Byte-identity** → unchanged. Stingy does not touch `threads`,
  `memory_limit`, or `preserve_insertion_order`; the `duckplyr-config`
  value-/byte-identity guarantees stand.
