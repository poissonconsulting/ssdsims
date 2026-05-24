# Per-task pipeline

One target branch per task → one Parquet file per task. The most
resumable layout: growing `nsim` adds new branches and leaves
previously-computed branches untouched.

Files written: `results/task_<NNNN>.parquet`.

Reading back:
```r
files <- list.files("results", full.names = TRUE)
job <- dplyr::bind_rows(lapply(files, ssdsims::ssd_read_job_parquet))
```
