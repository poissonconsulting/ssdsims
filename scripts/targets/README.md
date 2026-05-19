# ssdsims targets workflow

A [`targets`](https://docs.ropensci.org/targets/) pipeline that runs the
example in `scripts/example.R` end-to-end and persists every HC result as
hive-partitioned Parquet, so the whole dataset can be queried lazily with
[`duckplyr`](https://duckplyr.tidyverse.org/) (and any other DuckDB or Arrow
reader). Parquet is written through duckplyr too
(`duckplyr::compute_parquet()`), and the dynamic branches run in parallel
through a [`crew`](https://wlandau.github.io/crew/) controller sized to
`parallel::detectCores()`.

## Layout

```
scripts/targets/
├── _targets.R     # pipeline definition
├── R/functions.R  # flatten_hc_sims, write_partition, scenario runners
├── run.R          # convenience entry point
├── collect.R      # duckplyr collection examples
└── .gitignore     # ignores _targets/ and data/
```

`data/` and `_targets/` are produced at runtime and gitignored.

## What it builds

* **`basic_hcs`** — the simple `sim → fit → hc` chain from lines 1–9 of
  `scripts/example.R` (default `nsim = 100`, `nrow = 6`).
* **Scenario 1** — `ci = TRUE`, `samples = TRUE`, `est_method = "multi"`,
  varying `proportion × ci_method × nboot`, as in the first
  `ssd_run_scenario()` call in `scripts/example.R`.
* **Scenario 2** — `ci = FALSE`, `samples = FALSE`,
  varying `proportion × est_method × nboot`, as in the second call.

The pipeline branches dynamically over `nrow_levels = c(5, 6, 10, 20, 50)`.
For each `nrow` it runs `ssd_sim_data()` → `ssd_fit_dists_sims()` **once**
and feeds the resulting `fitdists` into both scenarios (cheaper than calling
`ssd_run_scenario()` twice, since that would re-simulate and re-fit).

## Running

From this directory:

```r
targets::tar_make()
```

or from the repo root:

```r
targets::tar_make(
  script = "scripts/targets/_targets.R",
  store  = "scripts/targets/_targets"
)
```

`tar_make()` automatically uses the `crew` controller configured in
`_targets.R`, which spawns one local worker per detected core. Override
by editing the `crew::crew_controller_local(workers = …)` call.

Useful inspections:

```r
targets::tar_visnetwork()
targets::tar_read(basic_hcs)
targets::tar_read(scenario1_parquet)   # vector of 5 parquet paths
```

The two bootstrap scenarios are the expensive part — scenario 1 in
particular runs `nboot` up to 500 with `samples = TRUE` across 4 × 7 = 28
`(proportion, ci_method)` combinations per `nrow` branch.

## Output

Each `nrow` branch writes one Parquet file under a hive-partitioned tree:

```
data/
├── basic/nrow=6/data.parquet
├── scenario1/nrow=5/data.parquet
├── scenario1/nrow=6/data.parquet
├── scenario1/nrow=10/data.parquet
├── scenario1/nrow=20/data.parquet
├── scenario1/nrow=50/data.parquet
└── scenario2/nrow=…/data.parquet
```

Columns: every parameter that varied (`sim`, `nboot`, `est_method`,
`ci_method`, `parametric`, …) plus the unnested HC fields (`dist`,
`proportion`, `est`, `se`, `lcl`, `ucl`, `wt`, `level`, `boot_method`,
`pboot`, `dists`, and — when `samples = TRUE` — `samples`).

`samples` is stored as a `list<double>` Parquet column (one bootstrap
sample vector per row); `dists` is `list<string>`. Both round-trip
through arrow and DuckDB.

## Collecting with duckplyr

See `collect.R`. The short version:

```r
library(duckplyr)

scenario1 <- duckplyr::read_parquet_duckdb(
  "data/scenario1/**/*.parquet",
  options = list(hive_partitioning = TRUE)
)

scenario1 |>
  dplyr::group_by(nrow, proportion, ci_method) |>
  dplyr::summarise(mean_est = mean(est, na.rm = TRUE), .groups = "drop") |>
  dplyr::collect()
```

`nrow` is recovered from the partition directory name on read.
