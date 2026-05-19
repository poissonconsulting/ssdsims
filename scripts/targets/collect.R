## Collect the pipeline's Parquet output with duckplyr.
##
## After `targets::tar_make()` has run, the `data/` directory contains three
## hive-partitioned Parquet datasets (one per scenario), each partitioned by
## `nrow`. duckplyr reads them lazily via DuckDB; collect() materialises.
##
## Run from this directory (`scripts/targets/`) or adjust the paths.

library(duckplyr)
library(dplyr)

data_root <- "data"

read_scenario <- function(scenario) {
  duckplyr::read_parquet_duckdb(
    file.path(data_root, scenario, "**", "*.parquet"),
    options = list(hive_partitioning = TRUE)
  )
}

basic <- read_scenario("basic")
scenario1 <- read_scenario("scenario1")
scenario2 <- read_scenario("scenario2")

## ---- Inspect ---------------------------------------------------------------

basic
scenario1
scenario2

## ---- Example aggregations --------------------------------------------------

## Mean HC by nrow and proportion for scenario 1.
scenario1 |>
  group_by(nrow, proportion, ci_method) |>
  summarise(
    n = n(),
    mean_est = mean(est, na.rm = TRUE),
    mean_lcl = mean(lcl, na.rm = TRUE),
    mean_ucl = mean(ucl, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(nrow, proportion, ci_method) |>
  collect()

## Scenario 2: point estimates by est_method.
scenario2 |>
  group_by(nrow, proportion, est_method) |>
  summarise(
    n = n(),
    mean_est = mean(est, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(nrow, proportion, est_method) |>
  collect()

## The `samples` column on scenario1 is a LIST<DOUBLE> in Parquet (one row per
## bootstrap replicate sample vector). Pull a single row into R to inspect:
##
##   scenario1 |>
##     filter(nrow == 10, proportion == 0.05, ci_method == "weighted_samples") |>
##     head(1) |>
##     collect() |>
##     dplyr::pull(samples) |>
##     str()
