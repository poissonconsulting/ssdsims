## ADDED Requirements

### Requirement: Pipeline duckplyr frames are stingy
The package SHALL create the internal pipeline's duckplyr frames with
`prudence = "stingy"`, so that within the pipeline no duckplyr frame
materialises implicitly and none falls back to dplyr silently. The
never-collected fan-in reads — `ssd_summarise()`, `ssd_summarise_member()`,
and `ssd_summarise_design()` — SHALL read their Parquet with
`prudence = "stingy"`, and the read/filter/projection/union SHALL execute in
DuckDB through to the write without collecting into R. The reads that genuinely
return rows to R (`ssd_read_parquet()` and the cost-analysis timing reads) SHALL
also read with `prudence = "stingy"` and SHALL materialise **only** via an
explicit `dplyr::collect()`. An operation in a stingy pipeline frame that
duckplyr cannot translate SHALL surface as a loud, catchable R error rather than
a silent dplyr fallback (which would materialise into R and risk an
out-of-memory kill on a constrained worker). The configuration SHALL NOT change
any result: for a fixed scenario the written Parquets SHALL remain
value-identical (and, under the single-threaded default, byte-identical) to a
run with permissive prudence.

#### Scenario: Fan-in unions never materialise into R
- **WHEN** `ssd_summarise()`, `ssd_summarise_member()`, or
  `ssd_summarise_design()` fans shard Parquets into a summary
- **THEN** the union, filter, projection, and write SHALL execute in DuckDB
  with the read frame at `prudence = "stingy"`, and the union SHALL NOT be
  collected into R

#### Scenario: An untranslatable verb fails loud, not silently in dplyr
- **WHEN** a pipeline stingy frame is asked to perform an operation duckplyr
  cannot translate to DuckDB
- **THEN** the operation SHALL raise a catchable R error rather than falling
  back to a dplyr implementation that materialises the data into R

#### Scenario: R-boundary reads collect explicitly
- **WHEN** `ssd_read_parquet()` or a cost-analysis timing read needs rows in R
- **THEN** the read SHALL be `prudence = "stingy"` and the rows SHALL reach R
  only through an explicit `dplyr::collect()`

#### Scenario: Stingy does not change results
- **WHEN** the same scenario is summarised under stingy pipeline frames and
  under permissive prudence
- **THEN** the written summaries SHALL contain the same multiset of rows
  (byte-identical under the single-threaded default)

### Requirement: The Parquet write seam preserves frame prudence
`ssd_write_parquet()` SHALL NOT downgrade an already-`duckplyr` frame's prudence
when writing. When given a frame that is already a `duckplyr_df`, it SHALL pass
that frame to the Parquet write unchanged (preserving its prudence — so a stingy
fan-in frame stays stingy through the write and a fallback cannot be masked by a
`lavish` re-wrap). When given a plain R data frame, it SHALL convert it with
`prudence = "stingy"` so the write itself cannot fall back to dplyr.

#### Scenario: A stingy fan-in frame stays stingy through the write
- **WHEN** a fan-in passes a stingy `duckplyr_df` to `ssd_write_parquet()`
- **THEN** the frame SHALL be written without being re-wrapped to `lavish`, so
  any untranslatable verb in the chain still errors at the write rather than
  materialising into R

#### Scenario: A plain tibble is wrapped stingy for the write
- **WHEN** a plain R data frame (e.g. a per-shard result table) is passed to
  `ssd_write_parquet()`
- **THEN** it SHALL be converted to a duckplyr frame with `prudence = "stingy"`
  before the Parquet write
