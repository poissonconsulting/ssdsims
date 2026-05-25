## scripts/example-expanded.R
##
## Loop-free expansion of the second example scenario from
## scripts/example.R, using `slice_sample_state()`, `fit_dists_seed()`
## and `hc_seed()` as building blocks. Each call site is written out
## explicitly so the per-step fan-out and the RNG state flow are
## visible without any control flow:
##
##   data fan-out:  2 sim * 5 nrow                                = 10
##   fit  fan-out:  10 (one per data row; fit-arg grid is 1)     = 10
##   hc   fan-out:  10 * 6 nboot * 3 est_method                  = 180
##
## seed = 42L is fixed, kind is pre-initialized to L'Ecuyer-CMRG, so
## the reference call -- `ssd_run_scenario(seed = 42L, ...)` -- and
## this expansion can be compared byte-for-byte on `data` and `hc`.
## `fits` are compared by `ssdtools::estimates()` because the TMB
## `model` slot of a `fitdists` holds a C++ external pointer that
## `identical()` cannot meaningfully compare across two calls.

library(ssdsims)

seed_val <- 42L
stream_val <- 1L

# --- Reference run (slow path, since `nrow` is a vector) ------------
RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
set.seed(seed_val)
reference <- ssd_run_scenario(
  ssddata::ccme_boron,
  nsim = 2L,
  nrow = c(5L, 6L, 10L, 20L, 50L),
  proportion = c(0.01, 0.05, 0.1, 0.2),
  est_method = c("arithmetic", "geometric", "multi"),
  ci = FALSE,
  parametric = TRUE,
  nboot = c(1, 5, 10, 50, 100, 500),
  samples = FALSE,
  delta = Inf,
  seed = seed_val,
  .progress = FALSE
)

# --- Expansion: pre-init RNG to match the reference -----------------
RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
set.seed(seed_val)

# Per-sim L'Ecuyer-CMRG states. The slow path in
# `ssd_sim_data.data.frame` does NOT propagate `seed` to its
# recursive `nsim = 1` calls, so the data states are derived with
# `seed = NULL` from the pre-set global state (kind + .Random.seed).
state_s1 <- ssdsims:::get_lecuyer_cmrg_stream_state(
  seed = NULL,
  stream = stream_val,
  start_sim = 1L
)
state_s2 <- ssdsims:::get_lecuyer_cmrg_stream_state(
  seed = NULL,
  stream = stream_val,
  start_sim = 2L
)

# --- Stage 1: data slices via slice_sample_state() ------------------
# 2 sim * 5 nrow = 10 calls. The state is per-sim (same across nrow
# because the slow path re-derives from `start_sim` alone).
data_s1_n5 <- ssdsims:::slice_sample_state(
  ssddata::ccme_boron,
  n = 5L,
  replace = FALSE,
  state = state_s1
)
data_s1_n6 <- ssdsims:::slice_sample_state(
  ssddata::ccme_boron,
  n = 6L,
  replace = FALSE,
  state = state_s1
)
data_s1_n10 <- ssdsims:::slice_sample_state(
  ssddata::ccme_boron,
  n = 10L,
  replace = FALSE,
  state = state_s1
)
data_s1_n20 <- ssdsims:::slice_sample_state(
  ssddata::ccme_boron,
  n = 20L,
  replace = FALSE,
  state = state_s1
)
data_s1_n50 <- ssdsims:::slice_sample_state(
  ssddata::ccme_boron,
  n = 50L,
  replace = FALSE,
  state = state_s1
)
data_s2_n5 <- ssdsims:::slice_sample_state(
  ssddata::ccme_boron,
  n = 5L,
  replace = FALSE,
  state = state_s2
)
data_s2_n6 <- ssdsims:::slice_sample_state(
  ssddata::ccme_boron,
  n = 6L,
  replace = FALSE,
  state = state_s2
)
data_s2_n10 <- ssdsims:::slice_sample_state(
  ssddata::ccme_boron,
  n = 10L,
  replace = FALSE,
  state = state_s2
)
data_s2_n20 <- ssdsims:::slice_sample_state(
  ssddata::ccme_boron,
  n = 20L,
  replace = FALSE,
  state = state_s2
)
data_s2_n50 <- ssdsims:::slice_sample_state(
  ssddata::ccme_boron,
  n = 50L,
  replace = FALSE,
  state = state_s2
)

# --- Stage 2: fits via fit_dists_seed() -----------------------------
# 10 calls (one per data row). `fit_dists_seed()` derives its own
# state internally from (seed = seed_val, stream, sim), independent
# of the global RNG.
fit_s1_n5 <- ssdsims:::fit_dists_seed(
  data_s1_n5,
  sim = 1L,
  stream = stream_val,
  seed = seed_val,
  dists = ssdtools::ssd_dists_bcanz(),
  rescale = FALSE,
  computable = FALSE,
  at_boundary_ok = TRUE,
  min_pmix = ssdtools::ssd_min_pmix,
  range_shape1 = c(0.05, 20),
  range_shape2 = c(0.05, 20),
  silent = TRUE
)
fit_s1_n6 <- ssdsims:::fit_dists_seed(
  data_s1_n6,
  sim = 1L,
  stream = stream_val,
  seed = seed_val,
  dists = ssdtools::ssd_dists_bcanz(),
  rescale = FALSE,
  computable = FALSE,
  at_boundary_ok = TRUE,
  min_pmix = ssdtools::ssd_min_pmix,
  range_shape1 = c(0.05, 20),
  range_shape2 = c(0.05, 20),
  silent = TRUE
)
fit_s1_n10 <- ssdsims:::fit_dists_seed(
  data_s1_n10,
  sim = 1L,
  stream = stream_val,
  seed = seed_val,
  dists = ssdtools::ssd_dists_bcanz(),
  rescale = FALSE,
  computable = FALSE,
  at_boundary_ok = TRUE,
  min_pmix = ssdtools::ssd_min_pmix,
  range_shape1 = c(0.05, 20),
  range_shape2 = c(0.05, 20),
  silent = TRUE
)
fit_s1_n20 <- ssdsims:::fit_dists_seed(
  data_s1_n20,
  sim = 1L,
  stream = stream_val,
  seed = seed_val,
  dists = ssdtools::ssd_dists_bcanz(),
  rescale = FALSE,
  computable = FALSE,
  at_boundary_ok = TRUE,
  min_pmix = ssdtools::ssd_min_pmix,
  range_shape1 = c(0.05, 20),
  range_shape2 = c(0.05, 20),
  silent = TRUE
)
fit_s1_n50 <- ssdsims:::fit_dists_seed(
  data_s1_n50,
  sim = 1L,
  stream = stream_val,
  seed = seed_val,
  dists = ssdtools::ssd_dists_bcanz(),
  rescale = FALSE,
  computable = FALSE,
  at_boundary_ok = TRUE,
  min_pmix = ssdtools::ssd_min_pmix,
  range_shape1 = c(0.05, 20),
  range_shape2 = c(0.05, 20),
  silent = TRUE
)
fit_s2_n5 <- ssdsims:::fit_dists_seed(
  data_s2_n5,
  sim = 2L,
  stream = stream_val,
  seed = seed_val,
  dists = ssdtools::ssd_dists_bcanz(),
  rescale = FALSE,
  computable = FALSE,
  at_boundary_ok = TRUE,
  min_pmix = ssdtools::ssd_min_pmix,
  range_shape1 = c(0.05, 20),
  range_shape2 = c(0.05, 20),
  silent = TRUE
)
fit_s2_n6 <- ssdsims:::fit_dists_seed(
  data_s2_n6,
  sim = 2L,
  stream = stream_val,
  seed = seed_val,
  dists = ssdtools::ssd_dists_bcanz(),
  rescale = FALSE,
  computable = FALSE,
  at_boundary_ok = TRUE,
  min_pmix = ssdtools::ssd_min_pmix,
  range_shape1 = c(0.05, 20),
  range_shape2 = c(0.05, 20),
  silent = TRUE
)
fit_s2_n10 <- ssdsims:::fit_dists_seed(
  data_s2_n10,
  sim = 2L,
  stream = stream_val,
  seed = seed_val,
  dists = ssdtools::ssd_dists_bcanz(),
  rescale = FALSE,
  computable = FALSE,
  at_boundary_ok = TRUE,
  min_pmix = ssdtools::ssd_min_pmix,
  range_shape1 = c(0.05, 20),
  range_shape2 = c(0.05, 20),
  silent = TRUE
)
fit_s2_n20 <- ssdsims:::fit_dists_seed(
  data_s2_n20,
  sim = 2L,
  stream = stream_val,
  seed = seed_val,
  dists = ssdtools::ssd_dists_bcanz(),
  rescale = FALSE,
  computable = FALSE,
  at_boundary_ok = TRUE,
  min_pmix = ssdtools::ssd_min_pmix,
  range_shape1 = c(0.05, 20),
  range_shape2 = c(0.05, 20),
  silent = TRUE
)
fit_s2_n50 <- ssdsims:::fit_dists_seed(
  data_s2_n50,
  sim = 2L,
  stream = stream_val,
  seed = seed_val,
  dists = ssdtools::ssd_dists_bcanz(),
  rescale = FALSE,
  computable = FALSE,
  at_boundary_ok = TRUE,
  min_pmix = ssdtools::ssd_min_pmix,
  range_shape1 = c(0.05, 20),
  range_shape2 = c(0.05, 20),
  silent = TRUE
)

# --- Stage 3: hc via hc_seed() --------------------------------------
# 2 sim * 5 nrow * 6 nboot * 3 est_method = 180 calls, in the
# canonical (sim, nrow, nboot, est_method) order produced by
# `tidyr::expand_grid()` inside `ssd_hc_sims()`.
hc_s1_n5_nb1_arithmetic <- ssdsims:::hc_seed(
  fit_s1_n5,
  sim = 1L,
  stream = stream_val,
  nboot = 1,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n5_nb1_geometric <- ssdsims:::hc_seed(
  fit_s1_n5,
  sim = 1L,
  stream = stream_val,
  nboot = 1,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n5_nb1_multi <- ssdsims:::hc_seed(
  fit_s1_n5,
  sim = 1L,
  stream = stream_val,
  nboot = 1,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n5_nb5_arithmetic <- ssdsims:::hc_seed(
  fit_s1_n5,
  sim = 1L,
  stream = stream_val,
  nboot = 5,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n5_nb5_geometric <- ssdsims:::hc_seed(
  fit_s1_n5,
  sim = 1L,
  stream = stream_val,
  nboot = 5,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n5_nb5_multi <- ssdsims:::hc_seed(
  fit_s1_n5,
  sim = 1L,
  stream = stream_val,
  nboot = 5,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n5_nb10_arithmetic <- ssdsims:::hc_seed(
  fit_s1_n5,
  sim = 1L,
  stream = stream_val,
  nboot = 10,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n5_nb10_geometric <- ssdsims:::hc_seed(
  fit_s1_n5,
  sim = 1L,
  stream = stream_val,
  nboot = 10,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n5_nb10_multi <- ssdsims:::hc_seed(
  fit_s1_n5,
  sim = 1L,
  stream = stream_val,
  nboot = 10,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n5_nb50_arithmetic <- ssdsims:::hc_seed(
  fit_s1_n5,
  sim = 1L,
  stream = stream_val,
  nboot = 50,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n5_nb50_geometric <- ssdsims:::hc_seed(
  fit_s1_n5,
  sim = 1L,
  stream = stream_val,
  nboot = 50,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n5_nb50_multi <- ssdsims:::hc_seed(
  fit_s1_n5,
  sim = 1L,
  stream = stream_val,
  nboot = 50,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n5_nb100_arithmetic <- ssdsims:::hc_seed(
  fit_s1_n5,
  sim = 1L,
  stream = stream_val,
  nboot = 100,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n5_nb100_geometric <- ssdsims:::hc_seed(
  fit_s1_n5,
  sim = 1L,
  stream = stream_val,
  nboot = 100,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n5_nb100_multi <- ssdsims:::hc_seed(
  fit_s1_n5,
  sim = 1L,
  stream = stream_val,
  nboot = 100,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n5_nb500_arithmetic <- ssdsims:::hc_seed(
  fit_s1_n5,
  sim = 1L,
  stream = stream_val,
  nboot = 500,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n5_nb500_geometric <- ssdsims:::hc_seed(
  fit_s1_n5,
  sim = 1L,
  stream = stream_val,
  nboot = 500,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n5_nb500_multi <- ssdsims:::hc_seed(
  fit_s1_n5,
  sim = 1L,
  stream = stream_val,
  nboot = 500,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n6_nb1_arithmetic <- ssdsims:::hc_seed(
  fit_s1_n6,
  sim = 1L,
  stream = stream_val,
  nboot = 1,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n6_nb1_geometric <- ssdsims:::hc_seed(
  fit_s1_n6,
  sim = 1L,
  stream = stream_val,
  nboot = 1,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n6_nb1_multi <- ssdsims:::hc_seed(
  fit_s1_n6,
  sim = 1L,
  stream = stream_val,
  nboot = 1,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n6_nb5_arithmetic <- ssdsims:::hc_seed(
  fit_s1_n6,
  sim = 1L,
  stream = stream_val,
  nboot = 5,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n6_nb5_geometric <- ssdsims:::hc_seed(
  fit_s1_n6,
  sim = 1L,
  stream = stream_val,
  nboot = 5,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n6_nb5_multi <- ssdsims:::hc_seed(
  fit_s1_n6,
  sim = 1L,
  stream = stream_val,
  nboot = 5,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n6_nb10_arithmetic <- ssdsims:::hc_seed(
  fit_s1_n6,
  sim = 1L,
  stream = stream_val,
  nboot = 10,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n6_nb10_geometric <- ssdsims:::hc_seed(
  fit_s1_n6,
  sim = 1L,
  stream = stream_val,
  nboot = 10,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n6_nb10_multi <- ssdsims:::hc_seed(
  fit_s1_n6,
  sim = 1L,
  stream = stream_val,
  nboot = 10,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n6_nb50_arithmetic <- ssdsims:::hc_seed(
  fit_s1_n6,
  sim = 1L,
  stream = stream_val,
  nboot = 50,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n6_nb50_geometric <- ssdsims:::hc_seed(
  fit_s1_n6,
  sim = 1L,
  stream = stream_val,
  nboot = 50,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n6_nb50_multi <- ssdsims:::hc_seed(
  fit_s1_n6,
  sim = 1L,
  stream = stream_val,
  nboot = 50,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n6_nb100_arithmetic <- ssdsims:::hc_seed(
  fit_s1_n6,
  sim = 1L,
  stream = stream_val,
  nboot = 100,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n6_nb100_geometric <- ssdsims:::hc_seed(
  fit_s1_n6,
  sim = 1L,
  stream = stream_val,
  nboot = 100,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n6_nb100_multi <- ssdsims:::hc_seed(
  fit_s1_n6,
  sim = 1L,
  stream = stream_val,
  nboot = 100,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n6_nb500_arithmetic <- ssdsims:::hc_seed(
  fit_s1_n6,
  sim = 1L,
  stream = stream_val,
  nboot = 500,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n6_nb500_geometric <- ssdsims:::hc_seed(
  fit_s1_n6,
  sim = 1L,
  stream = stream_val,
  nboot = 500,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n6_nb500_multi <- ssdsims:::hc_seed(
  fit_s1_n6,
  sim = 1L,
  stream = stream_val,
  nboot = 500,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n10_nb1_arithmetic <- ssdsims:::hc_seed(
  fit_s1_n10,
  sim = 1L,
  stream = stream_val,
  nboot = 1,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n10_nb1_geometric <- ssdsims:::hc_seed(
  fit_s1_n10,
  sim = 1L,
  stream = stream_val,
  nboot = 1,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n10_nb1_multi <- ssdsims:::hc_seed(
  fit_s1_n10,
  sim = 1L,
  stream = stream_val,
  nboot = 1,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n10_nb5_arithmetic <- ssdsims:::hc_seed(
  fit_s1_n10,
  sim = 1L,
  stream = stream_val,
  nboot = 5,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n10_nb5_geometric <- ssdsims:::hc_seed(
  fit_s1_n10,
  sim = 1L,
  stream = stream_val,
  nboot = 5,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n10_nb5_multi <- ssdsims:::hc_seed(
  fit_s1_n10,
  sim = 1L,
  stream = stream_val,
  nboot = 5,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n10_nb10_arithmetic <- ssdsims:::hc_seed(
  fit_s1_n10,
  sim = 1L,
  stream = stream_val,
  nboot = 10,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n10_nb10_geometric <- ssdsims:::hc_seed(
  fit_s1_n10,
  sim = 1L,
  stream = stream_val,
  nboot = 10,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n10_nb10_multi <- ssdsims:::hc_seed(
  fit_s1_n10,
  sim = 1L,
  stream = stream_val,
  nboot = 10,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n10_nb50_arithmetic <- ssdsims:::hc_seed(
  fit_s1_n10,
  sim = 1L,
  stream = stream_val,
  nboot = 50,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n10_nb50_geometric <- ssdsims:::hc_seed(
  fit_s1_n10,
  sim = 1L,
  stream = stream_val,
  nboot = 50,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n10_nb50_multi <- ssdsims:::hc_seed(
  fit_s1_n10,
  sim = 1L,
  stream = stream_val,
  nboot = 50,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n10_nb100_arithmetic <- ssdsims:::hc_seed(
  fit_s1_n10,
  sim = 1L,
  stream = stream_val,
  nboot = 100,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n10_nb100_geometric <- ssdsims:::hc_seed(
  fit_s1_n10,
  sim = 1L,
  stream = stream_val,
  nboot = 100,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n10_nb100_multi <- ssdsims:::hc_seed(
  fit_s1_n10,
  sim = 1L,
  stream = stream_val,
  nboot = 100,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n10_nb500_arithmetic <- ssdsims:::hc_seed(
  fit_s1_n10,
  sim = 1L,
  stream = stream_val,
  nboot = 500,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n10_nb500_geometric <- ssdsims:::hc_seed(
  fit_s1_n10,
  sim = 1L,
  stream = stream_val,
  nboot = 500,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n10_nb500_multi <- ssdsims:::hc_seed(
  fit_s1_n10,
  sim = 1L,
  stream = stream_val,
  nboot = 500,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n20_nb1_arithmetic <- ssdsims:::hc_seed(
  fit_s1_n20,
  sim = 1L,
  stream = stream_val,
  nboot = 1,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n20_nb1_geometric <- ssdsims:::hc_seed(
  fit_s1_n20,
  sim = 1L,
  stream = stream_val,
  nboot = 1,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n20_nb1_multi <- ssdsims:::hc_seed(
  fit_s1_n20,
  sim = 1L,
  stream = stream_val,
  nboot = 1,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n20_nb5_arithmetic <- ssdsims:::hc_seed(
  fit_s1_n20,
  sim = 1L,
  stream = stream_val,
  nboot = 5,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n20_nb5_geometric <- ssdsims:::hc_seed(
  fit_s1_n20,
  sim = 1L,
  stream = stream_val,
  nboot = 5,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n20_nb5_multi <- ssdsims:::hc_seed(
  fit_s1_n20,
  sim = 1L,
  stream = stream_val,
  nboot = 5,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n20_nb10_arithmetic <- ssdsims:::hc_seed(
  fit_s1_n20,
  sim = 1L,
  stream = stream_val,
  nboot = 10,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n20_nb10_geometric <- ssdsims:::hc_seed(
  fit_s1_n20,
  sim = 1L,
  stream = stream_val,
  nboot = 10,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n20_nb10_multi <- ssdsims:::hc_seed(
  fit_s1_n20,
  sim = 1L,
  stream = stream_val,
  nboot = 10,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n20_nb50_arithmetic <- ssdsims:::hc_seed(
  fit_s1_n20,
  sim = 1L,
  stream = stream_val,
  nboot = 50,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n20_nb50_geometric <- ssdsims:::hc_seed(
  fit_s1_n20,
  sim = 1L,
  stream = stream_val,
  nboot = 50,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n20_nb50_multi <- ssdsims:::hc_seed(
  fit_s1_n20,
  sim = 1L,
  stream = stream_val,
  nboot = 50,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n20_nb100_arithmetic <- ssdsims:::hc_seed(
  fit_s1_n20,
  sim = 1L,
  stream = stream_val,
  nboot = 100,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n20_nb100_geometric <- ssdsims:::hc_seed(
  fit_s1_n20,
  sim = 1L,
  stream = stream_val,
  nboot = 100,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n20_nb100_multi <- ssdsims:::hc_seed(
  fit_s1_n20,
  sim = 1L,
  stream = stream_val,
  nboot = 100,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n20_nb500_arithmetic <- ssdsims:::hc_seed(
  fit_s1_n20,
  sim = 1L,
  stream = stream_val,
  nboot = 500,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n20_nb500_geometric <- ssdsims:::hc_seed(
  fit_s1_n20,
  sim = 1L,
  stream = stream_val,
  nboot = 500,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n20_nb500_multi <- ssdsims:::hc_seed(
  fit_s1_n20,
  sim = 1L,
  stream = stream_val,
  nboot = 500,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n50_nb1_arithmetic <- ssdsims:::hc_seed(
  fit_s1_n50,
  sim = 1L,
  stream = stream_val,
  nboot = 1,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n50_nb1_geometric <- ssdsims:::hc_seed(
  fit_s1_n50,
  sim = 1L,
  stream = stream_val,
  nboot = 1,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n50_nb1_multi <- ssdsims:::hc_seed(
  fit_s1_n50,
  sim = 1L,
  stream = stream_val,
  nboot = 1,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n50_nb5_arithmetic <- ssdsims:::hc_seed(
  fit_s1_n50,
  sim = 1L,
  stream = stream_val,
  nboot = 5,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n50_nb5_geometric <- ssdsims:::hc_seed(
  fit_s1_n50,
  sim = 1L,
  stream = stream_val,
  nboot = 5,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n50_nb5_multi <- ssdsims:::hc_seed(
  fit_s1_n50,
  sim = 1L,
  stream = stream_val,
  nboot = 5,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n50_nb10_arithmetic <- ssdsims:::hc_seed(
  fit_s1_n50,
  sim = 1L,
  stream = stream_val,
  nboot = 10,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n50_nb10_geometric <- ssdsims:::hc_seed(
  fit_s1_n50,
  sim = 1L,
  stream = stream_val,
  nboot = 10,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n50_nb10_multi <- ssdsims:::hc_seed(
  fit_s1_n50,
  sim = 1L,
  stream = stream_val,
  nboot = 10,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n50_nb50_arithmetic <- ssdsims:::hc_seed(
  fit_s1_n50,
  sim = 1L,
  stream = stream_val,
  nboot = 50,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n50_nb50_geometric <- ssdsims:::hc_seed(
  fit_s1_n50,
  sim = 1L,
  stream = stream_val,
  nboot = 50,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n50_nb50_multi <- ssdsims:::hc_seed(
  fit_s1_n50,
  sim = 1L,
  stream = stream_val,
  nboot = 50,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n50_nb100_arithmetic <- ssdsims:::hc_seed(
  fit_s1_n50,
  sim = 1L,
  stream = stream_val,
  nboot = 100,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n50_nb100_geometric <- ssdsims:::hc_seed(
  fit_s1_n50,
  sim = 1L,
  stream = stream_val,
  nboot = 100,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n50_nb100_multi <- ssdsims:::hc_seed(
  fit_s1_n50,
  sim = 1L,
  stream = stream_val,
  nboot = 100,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n50_nb500_arithmetic <- ssdsims:::hc_seed(
  fit_s1_n50,
  sim = 1L,
  stream = stream_val,
  nboot = 500,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n50_nb500_geometric <- ssdsims:::hc_seed(
  fit_s1_n50,
  sim = 1L,
  stream = stream_val,
  nboot = 500,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s1_n50_nb500_multi <- ssdsims:::hc_seed(
  fit_s1_n50,
  sim = 1L,
  stream = stream_val,
  nboot = 500,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n5_nb1_arithmetic <- ssdsims:::hc_seed(
  fit_s2_n5,
  sim = 2L,
  stream = stream_val,
  nboot = 1,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n5_nb1_geometric <- ssdsims:::hc_seed(
  fit_s2_n5,
  sim = 2L,
  stream = stream_val,
  nboot = 1,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n5_nb1_multi <- ssdsims:::hc_seed(
  fit_s2_n5,
  sim = 2L,
  stream = stream_val,
  nboot = 1,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n5_nb5_arithmetic <- ssdsims:::hc_seed(
  fit_s2_n5,
  sim = 2L,
  stream = stream_val,
  nboot = 5,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n5_nb5_geometric <- ssdsims:::hc_seed(
  fit_s2_n5,
  sim = 2L,
  stream = stream_val,
  nboot = 5,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n5_nb5_multi <- ssdsims:::hc_seed(
  fit_s2_n5,
  sim = 2L,
  stream = stream_val,
  nboot = 5,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n5_nb10_arithmetic <- ssdsims:::hc_seed(
  fit_s2_n5,
  sim = 2L,
  stream = stream_val,
  nboot = 10,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n5_nb10_geometric <- ssdsims:::hc_seed(
  fit_s2_n5,
  sim = 2L,
  stream = stream_val,
  nboot = 10,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n5_nb10_multi <- ssdsims:::hc_seed(
  fit_s2_n5,
  sim = 2L,
  stream = stream_val,
  nboot = 10,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n5_nb50_arithmetic <- ssdsims:::hc_seed(
  fit_s2_n5,
  sim = 2L,
  stream = stream_val,
  nboot = 50,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n5_nb50_geometric <- ssdsims:::hc_seed(
  fit_s2_n5,
  sim = 2L,
  stream = stream_val,
  nboot = 50,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n5_nb50_multi <- ssdsims:::hc_seed(
  fit_s2_n5,
  sim = 2L,
  stream = stream_val,
  nboot = 50,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n5_nb100_arithmetic <- ssdsims:::hc_seed(
  fit_s2_n5,
  sim = 2L,
  stream = stream_val,
  nboot = 100,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n5_nb100_geometric <- ssdsims:::hc_seed(
  fit_s2_n5,
  sim = 2L,
  stream = stream_val,
  nboot = 100,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n5_nb100_multi <- ssdsims:::hc_seed(
  fit_s2_n5,
  sim = 2L,
  stream = stream_val,
  nboot = 100,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n5_nb500_arithmetic <- ssdsims:::hc_seed(
  fit_s2_n5,
  sim = 2L,
  stream = stream_val,
  nboot = 500,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n5_nb500_geometric <- ssdsims:::hc_seed(
  fit_s2_n5,
  sim = 2L,
  stream = stream_val,
  nboot = 500,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n5_nb500_multi <- ssdsims:::hc_seed(
  fit_s2_n5,
  sim = 2L,
  stream = stream_val,
  nboot = 500,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n6_nb1_arithmetic <- ssdsims:::hc_seed(
  fit_s2_n6,
  sim = 2L,
  stream = stream_val,
  nboot = 1,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n6_nb1_geometric <- ssdsims:::hc_seed(
  fit_s2_n6,
  sim = 2L,
  stream = stream_val,
  nboot = 1,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n6_nb1_multi <- ssdsims:::hc_seed(
  fit_s2_n6,
  sim = 2L,
  stream = stream_val,
  nboot = 1,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n6_nb5_arithmetic <- ssdsims:::hc_seed(
  fit_s2_n6,
  sim = 2L,
  stream = stream_val,
  nboot = 5,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n6_nb5_geometric <- ssdsims:::hc_seed(
  fit_s2_n6,
  sim = 2L,
  stream = stream_val,
  nboot = 5,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n6_nb5_multi <- ssdsims:::hc_seed(
  fit_s2_n6,
  sim = 2L,
  stream = stream_val,
  nboot = 5,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n6_nb10_arithmetic <- ssdsims:::hc_seed(
  fit_s2_n6,
  sim = 2L,
  stream = stream_val,
  nboot = 10,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n6_nb10_geometric <- ssdsims:::hc_seed(
  fit_s2_n6,
  sim = 2L,
  stream = stream_val,
  nboot = 10,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n6_nb10_multi <- ssdsims:::hc_seed(
  fit_s2_n6,
  sim = 2L,
  stream = stream_val,
  nboot = 10,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n6_nb50_arithmetic <- ssdsims:::hc_seed(
  fit_s2_n6,
  sim = 2L,
  stream = stream_val,
  nboot = 50,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n6_nb50_geometric <- ssdsims:::hc_seed(
  fit_s2_n6,
  sim = 2L,
  stream = stream_val,
  nboot = 50,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n6_nb50_multi <- ssdsims:::hc_seed(
  fit_s2_n6,
  sim = 2L,
  stream = stream_val,
  nboot = 50,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n6_nb100_arithmetic <- ssdsims:::hc_seed(
  fit_s2_n6,
  sim = 2L,
  stream = stream_val,
  nboot = 100,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n6_nb100_geometric <- ssdsims:::hc_seed(
  fit_s2_n6,
  sim = 2L,
  stream = stream_val,
  nboot = 100,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n6_nb100_multi <- ssdsims:::hc_seed(
  fit_s2_n6,
  sim = 2L,
  stream = stream_val,
  nboot = 100,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n6_nb500_arithmetic <- ssdsims:::hc_seed(
  fit_s2_n6,
  sim = 2L,
  stream = stream_val,
  nboot = 500,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n6_nb500_geometric <- ssdsims:::hc_seed(
  fit_s2_n6,
  sim = 2L,
  stream = stream_val,
  nboot = 500,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n6_nb500_multi <- ssdsims:::hc_seed(
  fit_s2_n6,
  sim = 2L,
  stream = stream_val,
  nboot = 500,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n10_nb1_arithmetic <- ssdsims:::hc_seed(
  fit_s2_n10,
  sim = 2L,
  stream = stream_val,
  nboot = 1,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n10_nb1_geometric <- ssdsims:::hc_seed(
  fit_s2_n10,
  sim = 2L,
  stream = stream_val,
  nboot = 1,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n10_nb1_multi <- ssdsims:::hc_seed(
  fit_s2_n10,
  sim = 2L,
  stream = stream_val,
  nboot = 1,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n10_nb5_arithmetic <- ssdsims:::hc_seed(
  fit_s2_n10,
  sim = 2L,
  stream = stream_val,
  nboot = 5,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n10_nb5_geometric <- ssdsims:::hc_seed(
  fit_s2_n10,
  sim = 2L,
  stream = stream_val,
  nboot = 5,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n10_nb5_multi <- ssdsims:::hc_seed(
  fit_s2_n10,
  sim = 2L,
  stream = stream_val,
  nboot = 5,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n10_nb10_arithmetic <- ssdsims:::hc_seed(
  fit_s2_n10,
  sim = 2L,
  stream = stream_val,
  nboot = 10,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n10_nb10_geometric <- ssdsims:::hc_seed(
  fit_s2_n10,
  sim = 2L,
  stream = stream_val,
  nboot = 10,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n10_nb10_multi <- ssdsims:::hc_seed(
  fit_s2_n10,
  sim = 2L,
  stream = stream_val,
  nboot = 10,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n10_nb50_arithmetic <- ssdsims:::hc_seed(
  fit_s2_n10,
  sim = 2L,
  stream = stream_val,
  nboot = 50,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n10_nb50_geometric <- ssdsims:::hc_seed(
  fit_s2_n10,
  sim = 2L,
  stream = stream_val,
  nboot = 50,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n10_nb50_multi <- ssdsims:::hc_seed(
  fit_s2_n10,
  sim = 2L,
  stream = stream_val,
  nboot = 50,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n10_nb100_arithmetic <- ssdsims:::hc_seed(
  fit_s2_n10,
  sim = 2L,
  stream = stream_val,
  nboot = 100,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n10_nb100_geometric <- ssdsims:::hc_seed(
  fit_s2_n10,
  sim = 2L,
  stream = stream_val,
  nboot = 100,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n10_nb100_multi <- ssdsims:::hc_seed(
  fit_s2_n10,
  sim = 2L,
  stream = stream_val,
  nboot = 100,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n10_nb500_arithmetic <- ssdsims:::hc_seed(
  fit_s2_n10,
  sim = 2L,
  stream = stream_val,
  nboot = 500,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n10_nb500_geometric <- ssdsims:::hc_seed(
  fit_s2_n10,
  sim = 2L,
  stream = stream_val,
  nboot = 500,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n10_nb500_multi <- ssdsims:::hc_seed(
  fit_s2_n10,
  sim = 2L,
  stream = stream_val,
  nboot = 500,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n20_nb1_arithmetic <- ssdsims:::hc_seed(
  fit_s2_n20,
  sim = 2L,
  stream = stream_val,
  nboot = 1,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n20_nb1_geometric <- ssdsims:::hc_seed(
  fit_s2_n20,
  sim = 2L,
  stream = stream_val,
  nboot = 1,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n20_nb1_multi <- ssdsims:::hc_seed(
  fit_s2_n20,
  sim = 2L,
  stream = stream_val,
  nboot = 1,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n20_nb5_arithmetic <- ssdsims:::hc_seed(
  fit_s2_n20,
  sim = 2L,
  stream = stream_val,
  nboot = 5,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n20_nb5_geometric <- ssdsims:::hc_seed(
  fit_s2_n20,
  sim = 2L,
  stream = stream_val,
  nboot = 5,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n20_nb5_multi <- ssdsims:::hc_seed(
  fit_s2_n20,
  sim = 2L,
  stream = stream_val,
  nboot = 5,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n20_nb10_arithmetic <- ssdsims:::hc_seed(
  fit_s2_n20,
  sim = 2L,
  stream = stream_val,
  nboot = 10,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n20_nb10_geometric <- ssdsims:::hc_seed(
  fit_s2_n20,
  sim = 2L,
  stream = stream_val,
  nboot = 10,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n20_nb10_multi <- ssdsims:::hc_seed(
  fit_s2_n20,
  sim = 2L,
  stream = stream_val,
  nboot = 10,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n20_nb50_arithmetic <- ssdsims:::hc_seed(
  fit_s2_n20,
  sim = 2L,
  stream = stream_val,
  nboot = 50,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n20_nb50_geometric <- ssdsims:::hc_seed(
  fit_s2_n20,
  sim = 2L,
  stream = stream_val,
  nboot = 50,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n20_nb50_multi <- ssdsims:::hc_seed(
  fit_s2_n20,
  sim = 2L,
  stream = stream_val,
  nboot = 50,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n20_nb100_arithmetic <- ssdsims:::hc_seed(
  fit_s2_n20,
  sim = 2L,
  stream = stream_val,
  nboot = 100,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n20_nb100_geometric <- ssdsims:::hc_seed(
  fit_s2_n20,
  sim = 2L,
  stream = stream_val,
  nboot = 100,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n20_nb100_multi <- ssdsims:::hc_seed(
  fit_s2_n20,
  sim = 2L,
  stream = stream_val,
  nboot = 100,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n20_nb500_arithmetic <- ssdsims:::hc_seed(
  fit_s2_n20,
  sim = 2L,
  stream = stream_val,
  nboot = 500,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n20_nb500_geometric <- ssdsims:::hc_seed(
  fit_s2_n20,
  sim = 2L,
  stream = stream_val,
  nboot = 500,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n20_nb500_multi <- ssdsims:::hc_seed(
  fit_s2_n20,
  sim = 2L,
  stream = stream_val,
  nboot = 500,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n50_nb1_arithmetic <- ssdsims:::hc_seed(
  fit_s2_n50,
  sim = 2L,
  stream = stream_val,
  nboot = 1,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n50_nb1_geometric <- ssdsims:::hc_seed(
  fit_s2_n50,
  sim = 2L,
  stream = stream_val,
  nboot = 1,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n50_nb1_multi <- ssdsims:::hc_seed(
  fit_s2_n50,
  sim = 2L,
  stream = stream_val,
  nboot = 1,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n50_nb5_arithmetic <- ssdsims:::hc_seed(
  fit_s2_n50,
  sim = 2L,
  stream = stream_val,
  nboot = 5,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n50_nb5_geometric <- ssdsims:::hc_seed(
  fit_s2_n50,
  sim = 2L,
  stream = stream_val,
  nboot = 5,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n50_nb5_multi <- ssdsims:::hc_seed(
  fit_s2_n50,
  sim = 2L,
  stream = stream_val,
  nboot = 5,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n50_nb10_arithmetic <- ssdsims:::hc_seed(
  fit_s2_n50,
  sim = 2L,
  stream = stream_val,
  nboot = 10,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n50_nb10_geometric <- ssdsims:::hc_seed(
  fit_s2_n50,
  sim = 2L,
  stream = stream_val,
  nboot = 10,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n50_nb10_multi <- ssdsims:::hc_seed(
  fit_s2_n50,
  sim = 2L,
  stream = stream_val,
  nboot = 10,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n50_nb50_arithmetic <- ssdsims:::hc_seed(
  fit_s2_n50,
  sim = 2L,
  stream = stream_val,
  nboot = 50,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n50_nb50_geometric <- ssdsims:::hc_seed(
  fit_s2_n50,
  sim = 2L,
  stream = stream_val,
  nboot = 50,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n50_nb50_multi <- ssdsims:::hc_seed(
  fit_s2_n50,
  sim = 2L,
  stream = stream_val,
  nboot = 50,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n50_nb100_arithmetic <- ssdsims:::hc_seed(
  fit_s2_n50,
  sim = 2L,
  stream = stream_val,
  nboot = 100,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n50_nb100_geometric <- ssdsims:::hc_seed(
  fit_s2_n50,
  sim = 2L,
  stream = stream_val,
  nboot = 100,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n50_nb100_multi <- ssdsims:::hc_seed(
  fit_s2_n50,
  sim = 2L,
  stream = stream_val,
  nboot = 100,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n50_nb500_arithmetic <- ssdsims:::hc_seed(
  fit_s2_n50,
  sim = 2L,
  stream = stream_val,
  nboot = 500,
  est_method = "arithmetic",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n50_nb500_geometric <- ssdsims:::hc_seed(
  fit_s2_n50,
  sim = 2L,
  stream = stream_val,
  nboot = 500,
  est_method = "geometric",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc_s2_n50_nb500_multi <- ssdsims:::hc_seed(
  fit_s2_n50,
  sim = 2L,
  stream = stream_val,
  nboot = 500,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = c(0.01, 0.05, 0.1, 0.2),
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)


# --- Verification: byte-by-byte equivalence vs ssd_run_scenario() ---

expanded_data <- list(
  data_s1_n5,
  data_s1_n6,
  data_s1_n10,
  data_s1_n20,
  data_s1_n50,
  data_s2_n5,
  data_s2_n6,
  data_s2_n10,
  data_s2_n20,
  data_s2_n50
)
ref_unique_data <- reference[!duplicated(reference[c("sim", "nrow")]), ]$data
stopifnot(identical(expanded_data, ref_unique_data))

expanded_fits <- list(
  fit_s1_n5,
  fit_s1_n6,
  fit_s1_n10,
  fit_s1_n20,
  fit_s1_n50,
  fit_s2_n5,
  fit_s2_n6,
  fit_s2_n10,
  fit_s2_n20,
  fit_s2_n50
)
ref_unique_fits <- reference[!duplicated(reference[c("sim", "nrow")]), ]$fits
fit_estimates_equal <- function(a, b) {
  identical(lapply(a, ssdtools::estimates), lapply(b, ssdtools::estimates))
}
stopifnot(all(mapply(fit_estimates_equal, expanded_fits, ref_unique_fits)))

# Build the 180-entry hc list in canonical order, then compare.
expanded_hc <- list(
  hc_s1_n5_nb1_arithmetic,
  hc_s1_n5_nb1_geometric,
  hc_s1_n5_nb1_multi,
  hc_s1_n5_nb5_arithmetic,
  hc_s1_n5_nb5_geometric,
  hc_s1_n5_nb5_multi,
  hc_s1_n5_nb10_arithmetic,
  hc_s1_n5_nb10_geometric,
  hc_s1_n5_nb10_multi,
  hc_s1_n5_nb50_arithmetic,
  hc_s1_n5_nb50_geometric,
  hc_s1_n5_nb50_multi,
  hc_s1_n5_nb100_arithmetic,
  hc_s1_n5_nb100_geometric,
  hc_s1_n5_nb100_multi,
  hc_s1_n5_nb500_arithmetic,
  hc_s1_n5_nb500_geometric,
  hc_s1_n5_nb500_multi,
  hc_s1_n6_nb1_arithmetic,
  hc_s1_n6_nb1_geometric,
  hc_s1_n6_nb1_multi,
  hc_s1_n6_nb5_arithmetic,
  hc_s1_n6_nb5_geometric,
  hc_s1_n6_nb5_multi,
  hc_s1_n6_nb10_arithmetic,
  hc_s1_n6_nb10_geometric,
  hc_s1_n6_nb10_multi,
  hc_s1_n6_nb50_arithmetic,
  hc_s1_n6_nb50_geometric,
  hc_s1_n6_nb50_multi,
  hc_s1_n6_nb100_arithmetic,
  hc_s1_n6_nb100_geometric,
  hc_s1_n6_nb100_multi,
  hc_s1_n6_nb500_arithmetic,
  hc_s1_n6_nb500_geometric,
  hc_s1_n6_nb500_multi,
  hc_s1_n10_nb1_arithmetic,
  hc_s1_n10_nb1_geometric,
  hc_s1_n10_nb1_multi,
  hc_s1_n10_nb5_arithmetic,
  hc_s1_n10_nb5_geometric,
  hc_s1_n10_nb5_multi,
  hc_s1_n10_nb10_arithmetic,
  hc_s1_n10_nb10_geometric,
  hc_s1_n10_nb10_multi,
  hc_s1_n10_nb50_arithmetic,
  hc_s1_n10_nb50_geometric,
  hc_s1_n10_nb50_multi,
  hc_s1_n10_nb100_arithmetic,
  hc_s1_n10_nb100_geometric,
  hc_s1_n10_nb100_multi,
  hc_s1_n10_nb500_arithmetic,
  hc_s1_n10_nb500_geometric,
  hc_s1_n10_nb500_multi,
  hc_s1_n20_nb1_arithmetic,
  hc_s1_n20_nb1_geometric,
  hc_s1_n20_nb1_multi,
  hc_s1_n20_nb5_arithmetic,
  hc_s1_n20_nb5_geometric,
  hc_s1_n20_nb5_multi,
  hc_s1_n20_nb10_arithmetic,
  hc_s1_n20_nb10_geometric,
  hc_s1_n20_nb10_multi,
  hc_s1_n20_nb50_arithmetic,
  hc_s1_n20_nb50_geometric,
  hc_s1_n20_nb50_multi,
  hc_s1_n20_nb100_arithmetic,
  hc_s1_n20_nb100_geometric,
  hc_s1_n20_nb100_multi,
  hc_s1_n20_nb500_arithmetic,
  hc_s1_n20_nb500_geometric,
  hc_s1_n20_nb500_multi,
  hc_s1_n50_nb1_arithmetic,
  hc_s1_n50_nb1_geometric,
  hc_s1_n50_nb1_multi,
  hc_s1_n50_nb5_arithmetic,
  hc_s1_n50_nb5_geometric,
  hc_s1_n50_nb5_multi,
  hc_s1_n50_nb10_arithmetic,
  hc_s1_n50_nb10_geometric,
  hc_s1_n50_nb10_multi,
  hc_s1_n50_nb50_arithmetic,
  hc_s1_n50_nb50_geometric,
  hc_s1_n50_nb50_multi,
  hc_s1_n50_nb100_arithmetic,
  hc_s1_n50_nb100_geometric,
  hc_s1_n50_nb100_multi,
  hc_s1_n50_nb500_arithmetic,
  hc_s1_n50_nb500_geometric,
  hc_s1_n50_nb500_multi,
  hc_s2_n5_nb1_arithmetic,
  hc_s2_n5_nb1_geometric,
  hc_s2_n5_nb1_multi,
  hc_s2_n5_nb5_arithmetic,
  hc_s2_n5_nb5_geometric,
  hc_s2_n5_nb5_multi,
  hc_s2_n5_nb10_arithmetic,
  hc_s2_n5_nb10_geometric,
  hc_s2_n5_nb10_multi,
  hc_s2_n5_nb50_arithmetic,
  hc_s2_n5_nb50_geometric,
  hc_s2_n5_nb50_multi,
  hc_s2_n5_nb100_arithmetic,
  hc_s2_n5_nb100_geometric,
  hc_s2_n5_nb100_multi,
  hc_s2_n5_nb500_arithmetic,
  hc_s2_n5_nb500_geometric,
  hc_s2_n5_nb500_multi,
  hc_s2_n6_nb1_arithmetic,
  hc_s2_n6_nb1_geometric,
  hc_s2_n6_nb1_multi,
  hc_s2_n6_nb5_arithmetic,
  hc_s2_n6_nb5_geometric,
  hc_s2_n6_nb5_multi,
  hc_s2_n6_nb10_arithmetic,
  hc_s2_n6_nb10_geometric,
  hc_s2_n6_nb10_multi,
  hc_s2_n6_nb50_arithmetic,
  hc_s2_n6_nb50_geometric,
  hc_s2_n6_nb50_multi,
  hc_s2_n6_nb100_arithmetic,
  hc_s2_n6_nb100_geometric,
  hc_s2_n6_nb100_multi,
  hc_s2_n6_nb500_arithmetic,
  hc_s2_n6_nb500_geometric,
  hc_s2_n6_nb500_multi,
  hc_s2_n10_nb1_arithmetic,
  hc_s2_n10_nb1_geometric,
  hc_s2_n10_nb1_multi,
  hc_s2_n10_nb5_arithmetic,
  hc_s2_n10_nb5_geometric,
  hc_s2_n10_nb5_multi,
  hc_s2_n10_nb10_arithmetic,
  hc_s2_n10_nb10_geometric,
  hc_s2_n10_nb10_multi,
  hc_s2_n10_nb50_arithmetic,
  hc_s2_n10_nb50_geometric,
  hc_s2_n10_nb50_multi,
  hc_s2_n10_nb100_arithmetic,
  hc_s2_n10_nb100_geometric,
  hc_s2_n10_nb100_multi,
  hc_s2_n10_nb500_arithmetic,
  hc_s2_n10_nb500_geometric,
  hc_s2_n10_nb500_multi,
  hc_s2_n20_nb1_arithmetic,
  hc_s2_n20_nb1_geometric,
  hc_s2_n20_nb1_multi,
  hc_s2_n20_nb5_arithmetic,
  hc_s2_n20_nb5_geometric,
  hc_s2_n20_nb5_multi,
  hc_s2_n20_nb10_arithmetic,
  hc_s2_n20_nb10_geometric,
  hc_s2_n20_nb10_multi,
  hc_s2_n20_nb50_arithmetic,
  hc_s2_n20_nb50_geometric,
  hc_s2_n20_nb50_multi,
  hc_s2_n20_nb100_arithmetic,
  hc_s2_n20_nb100_geometric,
  hc_s2_n20_nb100_multi,
  hc_s2_n20_nb500_arithmetic,
  hc_s2_n20_nb500_geometric,
  hc_s2_n20_nb500_multi,
  hc_s2_n50_nb1_arithmetic,
  hc_s2_n50_nb1_geometric,
  hc_s2_n50_nb1_multi,
  hc_s2_n50_nb5_arithmetic,
  hc_s2_n50_nb5_geometric,
  hc_s2_n50_nb5_multi,
  hc_s2_n50_nb10_arithmetic,
  hc_s2_n50_nb10_geometric,
  hc_s2_n50_nb10_multi,
  hc_s2_n50_nb50_arithmetic,
  hc_s2_n50_nb50_geometric,
  hc_s2_n50_nb50_multi,
  hc_s2_n50_nb100_arithmetic,
  hc_s2_n50_nb100_geometric,
  hc_s2_n50_nb100_multi,
  hc_s2_n50_nb500_arithmetic,
  hc_s2_n50_nb500_geometric,
  hc_s2_n50_nb500_multi
)
stopifnot(identical(expanded_hc, reference$hc))

message(
  "OK example-2-expanded: 10 data + 10 fits + 180 hc match ssd_run_scenario(seed=42L,...) byte-for-byte (fits compared via estimates())."
)


# --- Example 3 (simplified) -----------------------------------------
#
# Same building blocks, downsized so the formatted file fits comfortably
# under 500 lines on its own. Fan-out:
#
#   data: 2 sim * 2 nrow                    = 4
#   fit:  4                                 = 4
#   hc:   2 sim * 2 nrow * 2 nboot * 1 em   = 8

RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
set.seed(seed_val)
reference3 <- ssd_run_scenario(
  ssddata::ccme_boron,
  nsim = 2L,
  nrow = c(5L, 10L),
  proportion = 0.05,
  est_method = "multi",
  ci_method = "weighted_samples",
  ci = FALSE,
  parametric = TRUE,
  nboot = c(10, 50),
  samples = FALSE,
  delta = Inf,
  seed = seed_val,
  .progress = FALSE
)

RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
set.seed(seed_val)
state3_s1 <- ssdsims:::get_lecuyer_cmrg_stream_state(
  seed = NULL,
  stream = stream_val,
  start_sim = 1L
)
state3_s2 <- ssdsims:::get_lecuyer_cmrg_stream_state(
  seed = NULL,
  stream = stream_val,
  start_sim = 2L
)

data3_s1_n5 <- ssdsims:::slice_sample_state(
  ssddata::ccme_boron,
  n = 5L,
  replace = FALSE,
  state = state3_s1
)
data3_s1_n10 <- ssdsims:::slice_sample_state(
  ssddata::ccme_boron,
  n = 10L,
  replace = FALSE,
  state = state3_s1
)
data3_s2_n5 <- ssdsims:::slice_sample_state(
  ssddata::ccme_boron,
  n = 5L,
  replace = FALSE,
  state = state3_s2
)
data3_s2_n10 <- ssdsims:::slice_sample_state(
  ssddata::ccme_boron,
  n = 10L,
  replace = FALSE,
  state = state3_s2
)

fit3_s1_n5 <- ssdsims:::fit_dists_seed(
  data3_s1_n5,
  sim = 1L,
  stream = stream_val,
  seed = seed_val,
  dists = ssdtools::ssd_dists_bcanz(),
  rescale = FALSE,
  computable = FALSE,
  at_boundary_ok = TRUE,
  min_pmix = ssdtools::ssd_min_pmix,
  range_shape1 = c(0.05, 20),
  range_shape2 = c(0.05, 20),
  silent = TRUE
)
fit3_s1_n10 <- ssdsims:::fit_dists_seed(
  data3_s1_n10,
  sim = 1L,
  stream = stream_val,
  seed = seed_val,
  dists = ssdtools::ssd_dists_bcanz(),
  rescale = FALSE,
  computable = FALSE,
  at_boundary_ok = TRUE,
  min_pmix = ssdtools::ssd_min_pmix,
  range_shape1 = c(0.05, 20),
  range_shape2 = c(0.05, 20),
  silent = TRUE
)
fit3_s2_n5 <- ssdsims:::fit_dists_seed(
  data3_s2_n5,
  sim = 2L,
  stream = stream_val,
  seed = seed_val,
  dists = ssdtools::ssd_dists_bcanz(),
  rescale = FALSE,
  computable = FALSE,
  at_boundary_ok = TRUE,
  min_pmix = ssdtools::ssd_min_pmix,
  range_shape1 = c(0.05, 20),
  range_shape2 = c(0.05, 20),
  silent = TRUE
)
fit3_s2_n10 <- ssdsims:::fit_dists_seed(
  data3_s2_n10,
  sim = 2L,
  stream = stream_val,
  seed = seed_val,
  dists = ssdtools::ssd_dists_bcanz(),
  rescale = FALSE,
  computable = FALSE,
  at_boundary_ok = TRUE,
  min_pmix = ssdtools::ssd_min_pmix,
  range_shape1 = c(0.05, 20),
  range_shape2 = c(0.05, 20),
  silent = TRUE
)

hc3_s1_n5_nb10_multi <- ssdsims:::hc_seed(
  fit3_s1_n5,
  sim = 1L,
  stream = stream_val,
  nboot = 10,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = 0.05,
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc3_s1_n5_nb50_multi <- ssdsims:::hc_seed(
  fit3_s1_n5,
  sim = 1L,
  stream = stream_val,
  nboot = 50,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = 0.05,
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc3_s1_n10_nb10_multi <- ssdsims:::hc_seed(
  fit3_s1_n10,
  sim = 1L,
  stream = stream_val,
  nboot = 10,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = 0.05,
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc3_s1_n10_nb50_multi <- ssdsims:::hc_seed(
  fit3_s1_n10,
  sim = 1L,
  stream = stream_val,
  nboot = 50,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = 0.05,
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc3_s2_n5_nb10_multi <- ssdsims:::hc_seed(
  fit3_s2_n5,
  sim = 2L,
  stream = stream_val,
  nboot = 10,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = 0.05,
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc3_s2_n5_nb50_multi <- ssdsims:::hc_seed(
  fit3_s2_n5,
  sim = 2L,
  stream = stream_val,
  nboot = 50,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = 0.05,
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc3_s2_n10_nb10_multi <- ssdsims:::hc_seed(
  fit3_s2_n10,
  sim = 2L,
  stream = stream_val,
  nboot = 10,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = 0.05,
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)
hc3_s2_n10_nb50_multi <- ssdsims:::hc_seed(
  fit3_s2_n10,
  sim = 2L,
  stream = stream_val,
  nboot = 50,
  est_method = "multi",
  ci_method = "weighted_samples",
  seed = seed_val,
  proportion = 0.05,
  ci = FALSE,
  parametric = TRUE,
  save_to = NULL,
  samples = FALSE,
  delta = Inf
)

expanded3_data <- list(data3_s1_n5, data3_s1_n10, data3_s2_n5, data3_s2_n10)
ref3_unique_data <- reference3[!duplicated(reference3[c("sim", "nrow")]), ]$data
stopifnot(identical(expanded3_data, ref3_unique_data))

expanded3_fits <- list(fit3_s1_n5, fit3_s1_n10, fit3_s2_n5, fit3_s2_n10)
ref3_unique_fits <- reference3[!duplicated(reference3[c("sim", "nrow")]), ]$fits
stopifnot(all(mapply(fit_estimates_equal, expanded3_fits, ref3_unique_fits)))

expanded3_hc <- list(
  hc3_s1_n5_nb10_multi,
  hc3_s1_n5_nb50_multi,
  hc3_s1_n10_nb10_multi,
  hc3_s1_n10_nb50_multi,
  hc3_s2_n5_nb10_multi,
  hc3_s2_n5_nb50_multi,
  hc3_s2_n10_nb10_multi,
  hc3_s2_n10_nb50_multi
)
stopifnot(identical(expanded3_hc, reference3$hc))

message(
  "OK example-3-simplified: 4 data + 4 fits + 8 hc match ssd_run_scenario(seed=42L,...) byte-for-byte."
)
