# Per-parameter-slice pipeline

One target branch per `nrow` value → one Parquet file per slice.
Useful when each slice corresponds to a meaningful experiment (e.g.
"how does HC5 behave at n=5 vs n=10?") and slices are added one at a
time over the life of a project.

Files written: `results/nrow_<N>.parquet`.

Trade-off: growing `nsim` invalidates every slice (because each
slice contains all sims). If `nsim` growth is your primary
resumability concern, use `per-task/` or `per-sim/` instead.
