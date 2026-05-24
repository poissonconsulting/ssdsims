# ssdsims targets examples

Four end-to-end `targets` pipelines that execute an `ssdsims_scenario`
at progressively coarser granularities. Each writes one Parquet file
per job (per target branch).

| Granularity              | Branches               | Path                              |
| ------------------------ | ---------------------- | --------------------------------- |
| `per-task/`              | one per task row       | `results/task_<NNNN>.parquet`     |
| `per-sim/`               | one per sim id         | `results/sim_<NNNN>.parquet`      |
| `per-parameter-slice/`   | one per `nrow` slice   | `results/nrow_<N>.parquet`        |
| `whole-scenario/`        | one (the whole run)    | `results/whole.parquet`           |

## Knobs

Each `_targets.R` reads scenario size from environment variables:

| Variable                      | Default | Effect                                     |
| ----------------------------- | ------- | ------------------------------------------ |
| `SSDSIMS_EXAMPLE_NSIM`        | `4`     | Number of simulations.                     |
| `SSDSIMS_EXAMPLE_NROW`        | `5,10`  | Comma-separated `nrow` values.             |
| `SSDSIMS_EXAMPLE_NBOOT`       | `50`    | Bootstrap replicates passed to `ssd_hc()`. |

Enlarging `SSDSIMS_EXAMPLE_NSIM` is the resumability case the design is
optimised for — see the next section.

## Resumability under knob changes

`targets` caches per-branch results by content hash. Whether a knob
change re-runs previously-computed branches depends on which knob and
which granularity:

| Knob → Granularity          | per-task           | per-sim            | per-parameter-slice                 | whole-scenario  |
| --------------------------- | ------------------ | ------------------ | ----------------------------------- | --------------- |
| `NSIM` grows (4 → 100)      | new sims only      | new sims only      | all `nrow` slices grow → re-run all | full re-run     |
| `NROW` grows (add a value)  | new tasks only     | all sims widen → re-run all | new `nrow` slice only      | full re-run     |
| `NBOOT` changes             | every task         | every sim          | every slice                         | full re-run     |
| `seed` changes              | every task         | every sim          | every slice                         | full re-run     |

The two left columns are the resumability sweet spot: grow `NSIM` and
existing branches stay cached because the L'Ecuyer-CMRG substream
states for each (sim, stream) tuple are deterministic from the master
seed and independent of `nsim`.

## Persistence — Parquet vs targets' native store

These examples write one Parquet file per job (`format = "file"`,
managed via `ssd_write_job_parquet()`). Tradeoffs:

- **Pro:** portable, language-agnostic, columnar query without R.
- **Pro:** the fits S3 object (and any other R-only payload) is stored
  as a `*_qs2` raw blob column inside the same Parquet file — a single
  artifact per job, no sibling files.
- **Con:** more boilerplate than letting `targets` use its native
  `qs`-based object store (`format = "qs"` is the default and would
  remove the explicit write/read code).
- **Con:** the `qs2` blob columns are not directly queryable from
  Python/DuckDB/etc.; you still need `ssdsims::ssd_read_job_parquet()`
  to round-trip them.

If you don't need cross-language access, let `targets` handle storage:
return the result tibble directly from the target and skip
`ssd_write_job_parquet()`. The branching logic and resumability story
are unchanged.

## Running

From the package root, after `devtools::load_all()`:

```r
# Pick a granularity:
dir <- system.file("targets-examples", "per-sim", package = "ssdsims")
# In dev mode without an install, use the source path:
dir <- "inst/targets-examples/per-sim"

old <- setwd(dir)
on.exit(setwd(old))
targets::tar_make(callr_function = NULL)  # devtools-loaded pkg lives in this R session
```

See `scripts/example-targets.R` for a driver that runs all four granularities
twice (downsized then full) and compares each against a direct
`ssd_run_scenario2()` call.
