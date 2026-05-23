## Define the matrix of experiment configurations.
##
## Each atomic task is a single `fit_dists_seed()` call — one `(sim,
## stream, nrow)` tuple. Within that task we also run the HC sweep
## (the experiment's HC params) over the fit, so each task produces
## exactly one parquet file.
##
## "Splits" decide how atomic tasks get bundled into branches (= jobs).
## Coarser splits → fewer, fatter branches that amortise crew/worker
## dispatch overhead. Finer splits → more, smaller branches with
## better interruption / fault tolerance and finer scheduling.
##
## Baseline finishes in ~1 minute on 2 CPUs; bump the knobs below to
## grow the experiment.

## ───────────────────────── KNOBS ─────────────────────────
## Each line is independent — bump one and the others stay put.

GRID <- list(
  ## Fit-task axes (each combo is one fit_dists_seed call).
  nrow_levels = c(6L, 10L, 20L), # KNOB: more values → more fits; larger nrow → slower fit/bootstrap
  nsim = 2L, # KNOB: more sims per nrow → linearly more fit tasks
  stream = 1L, # usually constant; pin per pipeline

  ## HC sweep — applied inside every fit task.
  ci_method = c("multi_fixed", "weighted_samples"), # KNOB: more methods → bigger HC sweep
  nboot = c(1L, 5L), # KNOB: bigger → slower bootstrap per HC
  proportion = 0.05 # KNOB: more values → bigger HC sweep
)

## To run a longer experiment, replace GRID with one of these (or your own):
##
## MEDIUM_GRID <- list(
##   nrow_levels = c(5L, 6L, 10L, 20L, 50L),
##   nsim        = 20L,
##   stream      = 1L,
##   ci_method   = c("multi_fixed", "weighted_samples", "MACL", "GMACL"),
##   nboot       = c(1L, 10L, 100L),
##   proportion  = c(0.01, 0.05, 0.1)
## )

## ─────────────────────── SPLITS ───────────────────────
## A split is the set of fit-grid columns to group atomic tasks by.
## Branches iterate over groups (`iteration = "group"`), so each
## branch receives the rows belonging to one bucket.

SPLITS <- list(
  ## One branch per nrow. Each branch contains nsim fits.
  by_nrow = c("nrow"),

  ## One branch per sim across all nrows. Each branch contains
  ## #nrow_levels fits. Useful when the HC sweep dominates so you
  ## want to keep workers busy across sims.
  by_sim = c("sim"),

  ## One branch per (nrow, sim) — atomic at the fit layer.
  ## Maximum parallelism (one fit per worker), highest dispatch
  ## overhead.
  atomic = c("nrow", "sim")
)

configs <- lapply(names(SPLITS), function(sp_name) {
  list(
    id = sp_name,
    split = sp_name,
    split_axes = SPLITS[[sp_name]],
    grid = GRID
  )
})
names(configs) <- names(SPLITS)
