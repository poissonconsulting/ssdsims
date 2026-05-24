# Per-sim pipeline

One target branch per `sim` id → one Parquet file per sim. Coarser
than `per-task/` but the same resumability story for `nsim` growth.

Files written: `results/sim_<NNNN>.parquet`.

Use when the per-task overhead (one target per row) outweighs the
benefit of finer caching — e.g. when each sim contains many tasks
that are cheap individually but share fixed costs (data caching, file
open).
