## Example scenario (new declarative API)
##
## A companion to scripts/example.R, rebuilt on the targets-bound API that
## landed with `ssd_define_scenario()` (TARGETS-DESIGN.md section 1). Where
## example.R drives the legacy `ssd_run_scenario()` straight to results, this
## script walks the three stages the new pipeline separates:
##
##   1. assemble + validate the data        -> ssd_data()
##   2. declare the scenario (no RNG, no IO) -> ssd_define_scenario()
##   3. expand into per-step task tables     -> ssd_scenario_tasks()
##   4. run the baseline in-process loop      -> ssd_run_scenario_baseline()
##
## Stages 1-3 are pure and side-effect-free: no random numbers are drawn and
## nothing is written. Only the baseline runner (stage 4) touches the RNG, and
## it draws from the ambient stream, so pin it with `withr::with_seed()` for a
## reproducible local run.

library(ssdsims)

# --- 1. Assemble and validate the datasets --------------------------
# `ssd_data()` is the single input entry point: it checks each frame for a
# numeric `Conc` column and names the collection (here from the argument
# names). A scenario can also take a bare data frame directly.
datasets <- ssd_data(
  boron = ssddata::ccme_boron,
  cadmium = ssddata::ccme_cadmium
)

# --- 2. Declare the scenario ----------------------------------------
# Purely declarative: stores the seed, the replicate count, the sample sizes,
# the dataset *names*, and the fit/hc argument grids. `min_pmix` is kept by
# name only. `ci = c(FALSE, TRUE)` is the deliberate way to ask for both the
# point estimate and bootstrap intervals -- a bare `ci = FALSE` forbids the
# bootstrap-only knobs (`nboot`, `ci_method`, `parametric`).
scenario <- ssd_define_scenario(
  datasets,
  nsim = 3L,
  nrow = c(5L, 10L, 20L),
  seed = 42L,
  dists = c("lnorm", "gamma"),
  proportion = c(0.05, 0.2),
  ci = c(FALSE, TRUE),
  nboot = c(10L, 100L),
  est_method = "multi",
  ci_method = "weighted_samples"
)
print(scenario)

# --- 3. Expand into the four per-step task tables -------------------
# Still RNG-free. Each table carries a path-style `<step>_id` primary key and
# its parent's id as a foreign key. The single expensive draw lives in the
# `sample` step (keyed by dataset/sim/replace); `nrow` is just a cross-join
# axis of the cheap `data` truncation, so one draw is shared across sizes.
tasks <- ssd_scenario_tasks(scenario)
print(tasks)

# The bootstrap-only knobs collapse to NA on the `ci = FALSE` rows and fan out
# only on the `ci = TRUE` rows -- inspect the hc table to see the asymmetry.
print(tasks$hc)

# Per-step derivations are also available individually:
ssd_scenario_sample_tasks(scenario)

# --- 4. Run the baseline in-process loop ----------------------------
# The no-frills runner: in-process, no `targets`, no shards, no Parquet. It is
# not reproducible on its own, so pin the ambient RNG for a deterministic run.
withr::with_seed(42L, {
  out <- ssd_run_scenario_baseline(scenario)
})

# Each element is the task table augmented with a per-task result list column.
out$hc

# Unnest the hazard-concentration estimates back onto their task identities.
# `proportion` is not a task axis -- it is passed whole to each hc call, so
# every hc result already carries its own `proportion` column.
hcs <- tidyr::unnest(
  out$hc[c("dataset", "sim", "nrow", "ci", "est_method", "hc")],
  hc,
  names_sep = "_"
)
print(
  hcs[c("dataset", "sim", "nrow", "ci", "hc_proportion", "hc_est")],
  n = Inf
)
