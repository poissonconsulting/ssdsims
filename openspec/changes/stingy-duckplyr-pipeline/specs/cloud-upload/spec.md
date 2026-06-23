## ADDED Requirements

### Requirement: Read-back generics default to stingy prudence with a lavish opt-out
The upload read-back generics `ssd_open_uploaded()` and `ssd_summarise_uploaded()` SHALL accept a `prudence` argument that controls the returned duckplyr frame's automatic materialisation, defaulting to `"stingy"`.
With the default, the returned table SHALL remain lazy and composable with
`dplyr` verbs and SHALL be materialisable with an explicit `dplyr::collect()`
or writable with `duckplyr::compute_parquet()`, but an implicit materialisation
(for example `nrow()` or `$` access) against the remote glob SHALL raise a
catchable error rather than triggering an unbounded download/scan. Passing
`prudence = "lavish"` SHALL restore automatic materialisation (the prior
behaviour) for callers who want the returned table to compute on first access.
The `prudence` value SHALL be threaded into the underlying
`read_parquet_duckdb()` read. The optional arguments SHALL be **keyword-only**:
each generic SHALL place a `...` before its optional arguments
(`prudence` for `ssd_open_uploaded()`; `drop_samples` and `prudence` for
`ssd_summarise_uploaded()`), and a positional or otherwise unexpected extra
argument SHALL be rejected with a catchable error rather than silently
absorbed.

#### Scenario: Stingy default avoids accidental remote materialisation
- **WHEN** `ssd_open_uploaded(upload, step)` is called with the default
  `prudence`
- **THEN** the returned table SHALL be stingy — composable with `dplyr` verbs
  and collectable explicitly, but an implicit `nrow()`/`$` access SHALL error
  rather than scanning/downloading the remote Parquet

#### Scenario: Lavish opt-out restores automatic materialisation
- **WHEN** `ssd_open_uploaded(upload, step, prudence = "lavish")` (or
  `ssd_summarise_uploaded(upload, ..., prudence = "lavish")`) is called
- **THEN** the returned table SHALL materialise automatically on first access,
  matching the behaviour before the stingy default

#### Scenario: Explicit collection works under either prudence
- **WHEN** the table returned by either generic is passed to
  `dplyr::collect()` or `duckplyr::compute_parquet()`
- **THEN** it SHALL materialise/serialise the rows regardless of the `prudence`
  setting

#### Scenario: Optional arguments are keyword-only
- **WHEN** `ssd_open_uploaded()` or `ssd_summarise_uploaded()` is called with a
  positional extra argument (one that would land in `...`)
- **THEN** the call SHALL raise a catchable error (the dots are checked empty),
  so `drop_samples`/`prudence` must be supplied by name
