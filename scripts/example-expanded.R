## scripts/example-expanded.R
##
## Loop-free expansion of the second example scenario from
## scripts/example.R, using `slice_sample_state()`, `fit_dists_state()`
## and `hc_state()` as building blocks (the state-only primitives).
## Each call is a named entry of a single per-step list (`data_list`,
## `fit_list`, `hc_list`) so the per-step fan-out and the RNG state
## flow are visible without any control flow:
##
##   data: 2 sim * 5 nrow                                   = 10
##   fit:  10 (one per data entry; fit-arg grid = 1)        = 10
##   hc:   10 * 6 nboot * 3 est_method                      = 180
##
## seed = 42L is fixed, kind is pre-initialized to L'Ecuyer-CMRG, so
## the reference call -- `ssd_run_scenario(seed = 42L, ...)` -- and
## this expansion can be compared byte-for-byte on `data` and `hc`.
## `fits` are compared by `ssdtools::estimates()` because the TMB
## `model` slot of a `fitdists` holds a C++ external pointer that
## `identical()` cannot meaningfully compare across two calls.
##
## Two per-sim state lists:
##  * state_data_list = derived with seed = NULL (matches the slow
##    path in ssd_sim_data.data.frame: outer seed is NOT propagated
##    to the recursive nsim = 1 calls).
##  * state_fith_list = derived with seed = seed_val (matches the
##    fit_dists_seed / hc_seed call sites in ssd_fit_dists_sims /
##    ssd_hc_sims, which DO propagate the outer seed).

library(ssdsims)

seed_val <- 42L
stream_val <- 1L

# --- Reference run --------------------------------------------------
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

state_data_list <- list(
  s1 = ssdsims:::get_lecuyer_cmrg_stream_state(
    seed = NULL,
    stream = stream_val,
    start_sim = 1L
  ),
  s2 = ssdsims:::get_lecuyer_cmrg_stream_state(
    seed = NULL,
    stream = stream_val,
    start_sim = 2L
  )
)

state_fith_list <- list(
  s1 = ssdsims:::get_lecuyer_cmrg_stream_state(
    seed = seed_val,
    stream = stream_val,
    start_sim = 1L
  ),
  s2 = ssdsims:::get_lecuyer_cmrg_stream_state(
    seed = seed_val,
    stream = stream_val,
    start_sim = 2L
  )
)

# --- Stage 1: data slices via slice_sample_state() ------------------
data_list <- list(
  s1_n5 = ssdsims:::slice_sample_state(
    ssddata::ccme_boron,
    n = 5L,
    replace = FALSE,
    state = state_data_list$s1
  ),
  s1_n6 = ssdsims:::slice_sample_state(
    ssddata::ccme_boron,
    n = 6L,
    replace = FALSE,
    state = state_data_list$s1
  ),
  s1_n10 = ssdsims:::slice_sample_state(
    ssddata::ccme_boron,
    n = 10L,
    replace = FALSE,
    state = state_data_list$s1
  ),
  s1_n20 = ssdsims:::slice_sample_state(
    ssddata::ccme_boron,
    n = 20L,
    replace = FALSE,
    state = state_data_list$s1
  ),
  s1_n50 = ssdsims:::slice_sample_state(
    ssddata::ccme_boron,
    n = 50L,
    replace = FALSE,
    state = state_data_list$s1
  ),
  s2_n5 = ssdsims:::slice_sample_state(
    ssddata::ccme_boron,
    n = 5L,
    replace = FALSE,
    state = state_data_list$s2
  ),
  s2_n6 = ssdsims:::slice_sample_state(
    ssddata::ccme_boron,
    n = 6L,
    replace = FALSE,
    state = state_data_list$s2
  ),
  s2_n10 = ssdsims:::slice_sample_state(
    ssddata::ccme_boron,
    n = 10L,
    replace = FALSE,
    state = state_data_list$s2
  ),
  s2_n20 = ssdsims:::slice_sample_state(
    ssddata::ccme_boron,
    n = 20L,
    replace = FALSE,
    state = state_data_list$s2
  ),
  s2_n50 = ssdsims:::slice_sample_state(
    ssddata::ccme_boron,
    n = 50L,
    replace = FALSE,
    state = state_data_list$s2
  )
)

# --- Stage 2: fits via fit_dists_state() ----------------------------
fit_list <- list(
  s1_n5 = ssdsims:::fit_dists_state(
    data_list$s1_n5,
    state = state_fith_list$s1,
    dists = ssdtools::ssd_dists_bcanz(),
    rescale = FALSE,
    computable = FALSE,
    at_boundary_ok = TRUE,
    min_pmix = ssdtools::ssd_min_pmix,
    range_shape1 = c(0.05, 20),
    range_shape2 = c(0.05, 20),
    silent = TRUE
  ),
  s1_n6 = ssdsims:::fit_dists_state(
    data_list$s1_n6,
    state = state_fith_list$s1,
    dists = ssdtools::ssd_dists_bcanz(),
    rescale = FALSE,
    computable = FALSE,
    at_boundary_ok = TRUE,
    min_pmix = ssdtools::ssd_min_pmix,
    range_shape1 = c(0.05, 20),
    range_shape2 = c(0.05, 20),
    silent = TRUE
  ),
  s1_n10 = ssdsims:::fit_dists_state(
    data_list$s1_n10,
    state = state_fith_list$s1,
    dists = ssdtools::ssd_dists_bcanz(),
    rescale = FALSE,
    computable = FALSE,
    at_boundary_ok = TRUE,
    min_pmix = ssdtools::ssd_min_pmix,
    range_shape1 = c(0.05, 20),
    range_shape2 = c(0.05, 20),
    silent = TRUE
  ),
  s1_n20 = ssdsims:::fit_dists_state(
    data_list$s1_n20,
    state = state_fith_list$s1,
    dists = ssdtools::ssd_dists_bcanz(),
    rescale = FALSE,
    computable = FALSE,
    at_boundary_ok = TRUE,
    min_pmix = ssdtools::ssd_min_pmix,
    range_shape1 = c(0.05, 20),
    range_shape2 = c(0.05, 20),
    silent = TRUE
  ),
  s1_n50 = ssdsims:::fit_dists_state(
    data_list$s1_n50,
    state = state_fith_list$s1,
    dists = ssdtools::ssd_dists_bcanz(),
    rescale = FALSE,
    computable = FALSE,
    at_boundary_ok = TRUE,
    min_pmix = ssdtools::ssd_min_pmix,
    range_shape1 = c(0.05, 20),
    range_shape2 = c(0.05, 20),
    silent = TRUE
  ),
  s2_n5 = ssdsims:::fit_dists_state(
    data_list$s2_n5,
    state = state_fith_list$s2,
    dists = ssdtools::ssd_dists_bcanz(),
    rescale = FALSE,
    computable = FALSE,
    at_boundary_ok = TRUE,
    min_pmix = ssdtools::ssd_min_pmix,
    range_shape1 = c(0.05, 20),
    range_shape2 = c(0.05, 20),
    silent = TRUE
  ),
  s2_n6 = ssdsims:::fit_dists_state(
    data_list$s2_n6,
    state = state_fith_list$s2,
    dists = ssdtools::ssd_dists_bcanz(),
    rescale = FALSE,
    computable = FALSE,
    at_boundary_ok = TRUE,
    min_pmix = ssdtools::ssd_min_pmix,
    range_shape1 = c(0.05, 20),
    range_shape2 = c(0.05, 20),
    silent = TRUE
  ),
  s2_n10 = ssdsims:::fit_dists_state(
    data_list$s2_n10,
    state = state_fith_list$s2,
    dists = ssdtools::ssd_dists_bcanz(),
    rescale = FALSE,
    computable = FALSE,
    at_boundary_ok = TRUE,
    min_pmix = ssdtools::ssd_min_pmix,
    range_shape1 = c(0.05, 20),
    range_shape2 = c(0.05, 20),
    silent = TRUE
  ),
  s2_n20 = ssdsims:::fit_dists_state(
    data_list$s2_n20,
    state = state_fith_list$s2,
    dists = ssdtools::ssd_dists_bcanz(),
    rescale = FALSE,
    computable = FALSE,
    at_boundary_ok = TRUE,
    min_pmix = ssdtools::ssd_min_pmix,
    range_shape1 = c(0.05, 20),
    range_shape2 = c(0.05, 20),
    silent = TRUE
  ),
  s2_n50 = ssdsims:::fit_dists_state(
    data_list$s2_n50,
    state = state_fith_list$s2,
    dists = ssdtools::ssd_dists_bcanz(),
    rescale = FALSE,
    computable = FALSE,
    at_boundary_ok = TRUE,
    min_pmix = ssdtools::ssd_min_pmix,
    range_shape1 = c(0.05, 20),
    range_shape2 = c(0.05, 20),
    silent = TRUE
  )
)

# --- Stage 3: hc via hc_state() -------------------------------------
# 180 entries in the canonical (sim, nrow, nboot, est_method) order
# produced by tidyr::expand_grid() inside ssd_hc_sims().
hc_list <- list(
  s1_n5_nb1_arithmetic = ssdsims:::hc_state(
    fit_list$s1_n5,
    state = state_fith_list$s1,
    nboot = 1,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n5_nb1_geometric = ssdsims:::hc_state(
    fit_list$s1_n5,
    state = state_fith_list$s1,
    nboot = 1,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n5_nb1_multi = ssdsims:::hc_state(
    fit_list$s1_n5,
    state = state_fith_list$s1,
    nboot = 1,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n5_nb5_arithmetic = ssdsims:::hc_state(
    fit_list$s1_n5,
    state = state_fith_list$s1,
    nboot = 5,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n5_nb5_geometric = ssdsims:::hc_state(
    fit_list$s1_n5,
    state = state_fith_list$s1,
    nboot = 5,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n5_nb5_multi = ssdsims:::hc_state(
    fit_list$s1_n5,
    state = state_fith_list$s1,
    nboot = 5,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n5_nb10_arithmetic = ssdsims:::hc_state(
    fit_list$s1_n5,
    state = state_fith_list$s1,
    nboot = 10,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n5_nb10_geometric = ssdsims:::hc_state(
    fit_list$s1_n5,
    state = state_fith_list$s1,
    nboot = 10,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n5_nb10_multi = ssdsims:::hc_state(
    fit_list$s1_n5,
    state = state_fith_list$s1,
    nboot = 10,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n5_nb50_arithmetic = ssdsims:::hc_state(
    fit_list$s1_n5,
    state = state_fith_list$s1,
    nboot = 50,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n5_nb50_geometric = ssdsims:::hc_state(
    fit_list$s1_n5,
    state = state_fith_list$s1,
    nboot = 50,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n5_nb50_multi = ssdsims:::hc_state(
    fit_list$s1_n5,
    state = state_fith_list$s1,
    nboot = 50,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n5_nb100_arithmetic = ssdsims:::hc_state(
    fit_list$s1_n5,
    state = state_fith_list$s1,
    nboot = 100,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n5_nb100_geometric = ssdsims:::hc_state(
    fit_list$s1_n5,
    state = state_fith_list$s1,
    nboot = 100,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n5_nb100_multi = ssdsims:::hc_state(
    fit_list$s1_n5,
    state = state_fith_list$s1,
    nboot = 100,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n5_nb500_arithmetic = ssdsims:::hc_state(
    fit_list$s1_n5,
    state = state_fith_list$s1,
    nboot = 500,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n5_nb500_geometric = ssdsims:::hc_state(
    fit_list$s1_n5,
    state = state_fith_list$s1,
    nboot = 500,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n5_nb500_multi = ssdsims:::hc_state(
    fit_list$s1_n5,
    state = state_fith_list$s1,
    nboot = 500,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n6_nb1_arithmetic = ssdsims:::hc_state(
    fit_list$s1_n6,
    state = state_fith_list$s1,
    nboot = 1,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n6_nb1_geometric = ssdsims:::hc_state(
    fit_list$s1_n6,
    state = state_fith_list$s1,
    nboot = 1,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n6_nb1_multi = ssdsims:::hc_state(
    fit_list$s1_n6,
    state = state_fith_list$s1,
    nboot = 1,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n6_nb5_arithmetic = ssdsims:::hc_state(
    fit_list$s1_n6,
    state = state_fith_list$s1,
    nboot = 5,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n6_nb5_geometric = ssdsims:::hc_state(
    fit_list$s1_n6,
    state = state_fith_list$s1,
    nboot = 5,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n6_nb5_multi = ssdsims:::hc_state(
    fit_list$s1_n6,
    state = state_fith_list$s1,
    nboot = 5,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n6_nb10_arithmetic = ssdsims:::hc_state(
    fit_list$s1_n6,
    state = state_fith_list$s1,
    nboot = 10,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n6_nb10_geometric = ssdsims:::hc_state(
    fit_list$s1_n6,
    state = state_fith_list$s1,
    nboot = 10,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n6_nb10_multi = ssdsims:::hc_state(
    fit_list$s1_n6,
    state = state_fith_list$s1,
    nboot = 10,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n6_nb50_arithmetic = ssdsims:::hc_state(
    fit_list$s1_n6,
    state = state_fith_list$s1,
    nboot = 50,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n6_nb50_geometric = ssdsims:::hc_state(
    fit_list$s1_n6,
    state = state_fith_list$s1,
    nboot = 50,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n6_nb50_multi = ssdsims:::hc_state(
    fit_list$s1_n6,
    state = state_fith_list$s1,
    nboot = 50,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n6_nb100_arithmetic = ssdsims:::hc_state(
    fit_list$s1_n6,
    state = state_fith_list$s1,
    nboot = 100,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n6_nb100_geometric = ssdsims:::hc_state(
    fit_list$s1_n6,
    state = state_fith_list$s1,
    nboot = 100,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n6_nb100_multi = ssdsims:::hc_state(
    fit_list$s1_n6,
    state = state_fith_list$s1,
    nboot = 100,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n6_nb500_arithmetic = ssdsims:::hc_state(
    fit_list$s1_n6,
    state = state_fith_list$s1,
    nboot = 500,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n6_nb500_geometric = ssdsims:::hc_state(
    fit_list$s1_n6,
    state = state_fith_list$s1,
    nboot = 500,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n6_nb500_multi = ssdsims:::hc_state(
    fit_list$s1_n6,
    state = state_fith_list$s1,
    nboot = 500,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n10_nb1_arithmetic = ssdsims:::hc_state(
    fit_list$s1_n10,
    state = state_fith_list$s1,
    nboot = 1,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n10_nb1_geometric = ssdsims:::hc_state(
    fit_list$s1_n10,
    state = state_fith_list$s1,
    nboot = 1,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n10_nb1_multi = ssdsims:::hc_state(
    fit_list$s1_n10,
    state = state_fith_list$s1,
    nboot = 1,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n10_nb5_arithmetic = ssdsims:::hc_state(
    fit_list$s1_n10,
    state = state_fith_list$s1,
    nboot = 5,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n10_nb5_geometric = ssdsims:::hc_state(
    fit_list$s1_n10,
    state = state_fith_list$s1,
    nboot = 5,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n10_nb5_multi = ssdsims:::hc_state(
    fit_list$s1_n10,
    state = state_fith_list$s1,
    nboot = 5,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n10_nb10_arithmetic = ssdsims:::hc_state(
    fit_list$s1_n10,
    state = state_fith_list$s1,
    nboot = 10,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n10_nb10_geometric = ssdsims:::hc_state(
    fit_list$s1_n10,
    state = state_fith_list$s1,
    nboot = 10,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n10_nb10_multi = ssdsims:::hc_state(
    fit_list$s1_n10,
    state = state_fith_list$s1,
    nboot = 10,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n10_nb50_arithmetic = ssdsims:::hc_state(
    fit_list$s1_n10,
    state = state_fith_list$s1,
    nboot = 50,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n10_nb50_geometric = ssdsims:::hc_state(
    fit_list$s1_n10,
    state = state_fith_list$s1,
    nboot = 50,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n10_nb50_multi = ssdsims:::hc_state(
    fit_list$s1_n10,
    state = state_fith_list$s1,
    nboot = 50,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n10_nb100_arithmetic = ssdsims:::hc_state(
    fit_list$s1_n10,
    state = state_fith_list$s1,
    nboot = 100,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n10_nb100_geometric = ssdsims:::hc_state(
    fit_list$s1_n10,
    state = state_fith_list$s1,
    nboot = 100,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n10_nb100_multi = ssdsims:::hc_state(
    fit_list$s1_n10,
    state = state_fith_list$s1,
    nboot = 100,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n10_nb500_arithmetic = ssdsims:::hc_state(
    fit_list$s1_n10,
    state = state_fith_list$s1,
    nboot = 500,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n10_nb500_geometric = ssdsims:::hc_state(
    fit_list$s1_n10,
    state = state_fith_list$s1,
    nboot = 500,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n10_nb500_multi = ssdsims:::hc_state(
    fit_list$s1_n10,
    state = state_fith_list$s1,
    nboot = 500,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n20_nb1_arithmetic = ssdsims:::hc_state(
    fit_list$s1_n20,
    state = state_fith_list$s1,
    nboot = 1,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n20_nb1_geometric = ssdsims:::hc_state(
    fit_list$s1_n20,
    state = state_fith_list$s1,
    nboot = 1,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n20_nb1_multi = ssdsims:::hc_state(
    fit_list$s1_n20,
    state = state_fith_list$s1,
    nboot = 1,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n20_nb5_arithmetic = ssdsims:::hc_state(
    fit_list$s1_n20,
    state = state_fith_list$s1,
    nboot = 5,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n20_nb5_geometric = ssdsims:::hc_state(
    fit_list$s1_n20,
    state = state_fith_list$s1,
    nboot = 5,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n20_nb5_multi = ssdsims:::hc_state(
    fit_list$s1_n20,
    state = state_fith_list$s1,
    nboot = 5,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n20_nb10_arithmetic = ssdsims:::hc_state(
    fit_list$s1_n20,
    state = state_fith_list$s1,
    nboot = 10,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n20_nb10_geometric = ssdsims:::hc_state(
    fit_list$s1_n20,
    state = state_fith_list$s1,
    nboot = 10,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n20_nb10_multi = ssdsims:::hc_state(
    fit_list$s1_n20,
    state = state_fith_list$s1,
    nboot = 10,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n20_nb50_arithmetic = ssdsims:::hc_state(
    fit_list$s1_n20,
    state = state_fith_list$s1,
    nboot = 50,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n20_nb50_geometric = ssdsims:::hc_state(
    fit_list$s1_n20,
    state = state_fith_list$s1,
    nboot = 50,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n20_nb50_multi = ssdsims:::hc_state(
    fit_list$s1_n20,
    state = state_fith_list$s1,
    nboot = 50,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n20_nb100_arithmetic = ssdsims:::hc_state(
    fit_list$s1_n20,
    state = state_fith_list$s1,
    nboot = 100,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n20_nb100_geometric = ssdsims:::hc_state(
    fit_list$s1_n20,
    state = state_fith_list$s1,
    nboot = 100,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n20_nb100_multi = ssdsims:::hc_state(
    fit_list$s1_n20,
    state = state_fith_list$s1,
    nboot = 100,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n20_nb500_arithmetic = ssdsims:::hc_state(
    fit_list$s1_n20,
    state = state_fith_list$s1,
    nboot = 500,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n20_nb500_geometric = ssdsims:::hc_state(
    fit_list$s1_n20,
    state = state_fith_list$s1,
    nboot = 500,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n20_nb500_multi = ssdsims:::hc_state(
    fit_list$s1_n20,
    state = state_fith_list$s1,
    nboot = 500,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n50_nb1_arithmetic = ssdsims:::hc_state(
    fit_list$s1_n50,
    state = state_fith_list$s1,
    nboot = 1,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n50_nb1_geometric = ssdsims:::hc_state(
    fit_list$s1_n50,
    state = state_fith_list$s1,
    nboot = 1,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n50_nb1_multi = ssdsims:::hc_state(
    fit_list$s1_n50,
    state = state_fith_list$s1,
    nboot = 1,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n50_nb5_arithmetic = ssdsims:::hc_state(
    fit_list$s1_n50,
    state = state_fith_list$s1,
    nboot = 5,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n50_nb5_geometric = ssdsims:::hc_state(
    fit_list$s1_n50,
    state = state_fith_list$s1,
    nboot = 5,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n50_nb5_multi = ssdsims:::hc_state(
    fit_list$s1_n50,
    state = state_fith_list$s1,
    nboot = 5,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n50_nb10_arithmetic = ssdsims:::hc_state(
    fit_list$s1_n50,
    state = state_fith_list$s1,
    nboot = 10,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n50_nb10_geometric = ssdsims:::hc_state(
    fit_list$s1_n50,
    state = state_fith_list$s1,
    nboot = 10,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n50_nb10_multi = ssdsims:::hc_state(
    fit_list$s1_n50,
    state = state_fith_list$s1,
    nboot = 10,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n50_nb50_arithmetic = ssdsims:::hc_state(
    fit_list$s1_n50,
    state = state_fith_list$s1,
    nboot = 50,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n50_nb50_geometric = ssdsims:::hc_state(
    fit_list$s1_n50,
    state = state_fith_list$s1,
    nboot = 50,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n50_nb50_multi = ssdsims:::hc_state(
    fit_list$s1_n50,
    state = state_fith_list$s1,
    nboot = 50,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n50_nb100_arithmetic = ssdsims:::hc_state(
    fit_list$s1_n50,
    state = state_fith_list$s1,
    nboot = 100,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n50_nb100_geometric = ssdsims:::hc_state(
    fit_list$s1_n50,
    state = state_fith_list$s1,
    nboot = 100,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n50_nb100_multi = ssdsims:::hc_state(
    fit_list$s1_n50,
    state = state_fith_list$s1,
    nboot = 100,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n50_nb500_arithmetic = ssdsims:::hc_state(
    fit_list$s1_n50,
    state = state_fith_list$s1,
    nboot = 500,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n50_nb500_geometric = ssdsims:::hc_state(
    fit_list$s1_n50,
    state = state_fith_list$s1,
    nboot = 500,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n50_nb500_multi = ssdsims:::hc_state(
    fit_list$s1_n50,
    state = state_fith_list$s1,
    nboot = 500,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n5_nb1_arithmetic = ssdsims:::hc_state(
    fit_list$s2_n5,
    state = state_fith_list$s2,
    nboot = 1,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n5_nb1_geometric = ssdsims:::hc_state(
    fit_list$s2_n5,
    state = state_fith_list$s2,
    nboot = 1,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n5_nb1_multi = ssdsims:::hc_state(
    fit_list$s2_n5,
    state = state_fith_list$s2,
    nboot = 1,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n5_nb5_arithmetic = ssdsims:::hc_state(
    fit_list$s2_n5,
    state = state_fith_list$s2,
    nboot = 5,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n5_nb5_geometric = ssdsims:::hc_state(
    fit_list$s2_n5,
    state = state_fith_list$s2,
    nboot = 5,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n5_nb5_multi = ssdsims:::hc_state(
    fit_list$s2_n5,
    state = state_fith_list$s2,
    nboot = 5,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n5_nb10_arithmetic = ssdsims:::hc_state(
    fit_list$s2_n5,
    state = state_fith_list$s2,
    nboot = 10,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n5_nb10_geometric = ssdsims:::hc_state(
    fit_list$s2_n5,
    state = state_fith_list$s2,
    nboot = 10,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n5_nb10_multi = ssdsims:::hc_state(
    fit_list$s2_n5,
    state = state_fith_list$s2,
    nboot = 10,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n5_nb50_arithmetic = ssdsims:::hc_state(
    fit_list$s2_n5,
    state = state_fith_list$s2,
    nboot = 50,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n5_nb50_geometric = ssdsims:::hc_state(
    fit_list$s2_n5,
    state = state_fith_list$s2,
    nboot = 50,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n5_nb50_multi = ssdsims:::hc_state(
    fit_list$s2_n5,
    state = state_fith_list$s2,
    nboot = 50,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n5_nb100_arithmetic = ssdsims:::hc_state(
    fit_list$s2_n5,
    state = state_fith_list$s2,
    nboot = 100,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n5_nb100_geometric = ssdsims:::hc_state(
    fit_list$s2_n5,
    state = state_fith_list$s2,
    nboot = 100,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n5_nb100_multi = ssdsims:::hc_state(
    fit_list$s2_n5,
    state = state_fith_list$s2,
    nboot = 100,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n5_nb500_arithmetic = ssdsims:::hc_state(
    fit_list$s2_n5,
    state = state_fith_list$s2,
    nboot = 500,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n5_nb500_geometric = ssdsims:::hc_state(
    fit_list$s2_n5,
    state = state_fith_list$s2,
    nboot = 500,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n5_nb500_multi = ssdsims:::hc_state(
    fit_list$s2_n5,
    state = state_fith_list$s2,
    nboot = 500,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n6_nb1_arithmetic = ssdsims:::hc_state(
    fit_list$s2_n6,
    state = state_fith_list$s2,
    nboot = 1,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n6_nb1_geometric = ssdsims:::hc_state(
    fit_list$s2_n6,
    state = state_fith_list$s2,
    nboot = 1,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n6_nb1_multi = ssdsims:::hc_state(
    fit_list$s2_n6,
    state = state_fith_list$s2,
    nboot = 1,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n6_nb5_arithmetic = ssdsims:::hc_state(
    fit_list$s2_n6,
    state = state_fith_list$s2,
    nboot = 5,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n6_nb5_geometric = ssdsims:::hc_state(
    fit_list$s2_n6,
    state = state_fith_list$s2,
    nboot = 5,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n6_nb5_multi = ssdsims:::hc_state(
    fit_list$s2_n6,
    state = state_fith_list$s2,
    nboot = 5,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n6_nb10_arithmetic = ssdsims:::hc_state(
    fit_list$s2_n6,
    state = state_fith_list$s2,
    nboot = 10,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n6_nb10_geometric = ssdsims:::hc_state(
    fit_list$s2_n6,
    state = state_fith_list$s2,
    nboot = 10,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n6_nb10_multi = ssdsims:::hc_state(
    fit_list$s2_n6,
    state = state_fith_list$s2,
    nboot = 10,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n6_nb50_arithmetic = ssdsims:::hc_state(
    fit_list$s2_n6,
    state = state_fith_list$s2,
    nboot = 50,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n6_nb50_geometric = ssdsims:::hc_state(
    fit_list$s2_n6,
    state = state_fith_list$s2,
    nboot = 50,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n6_nb50_multi = ssdsims:::hc_state(
    fit_list$s2_n6,
    state = state_fith_list$s2,
    nboot = 50,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n6_nb100_arithmetic = ssdsims:::hc_state(
    fit_list$s2_n6,
    state = state_fith_list$s2,
    nboot = 100,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n6_nb100_geometric = ssdsims:::hc_state(
    fit_list$s2_n6,
    state = state_fith_list$s2,
    nboot = 100,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n6_nb100_multi = ssdsims:::hc_state(
    fit_list$s2_n6,
    state = state_fith_list$s2,
    nboot = 100,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n6_nb500_arithmetic = ssdsims:::hc_state(
    fit_list$s2_n6,
    state = state_fith_list$s2,
    nboot = 500,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n6_nb500_geometric = ssdsims:::hc_state(
    fit_list$s2_n6,
    state = state_fith_list$s2,
    nboot = 500,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n6_nb500_multi = ssdsims:::hc_state(
    fit_list$s2_n6,
    state = state_fith_list$s2,
    nboot = 500,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n10_nb1_arithmetic = ssdsims:::hc_state(
    fit_list$s2_n10,
    state = state_fith_list$s2,
    nboot = 1,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n10_nb1_geometric = ssdsims:::hc_state(
    fit_list$s2_n10,
    state = state_fith_list$s2,
    nboot = 1,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n10_nb1_multi = ssdsims:::hc_state(
    fit_list$s2_n10,
    state = state_fith_list$s2,
    nboot = 1,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n10_nb5_arithmetic = ssdsims:::hc_state(
    fit_list$s2_n10,
    state = state_fith_list$s2,
    nboot = 5,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n10_nb5_geometric = ssdsims:::hc_state(
    fit_list$s2_n10,
    state = state_fith_list$s2,
    nboot = 5,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n10_nb5_multi = ssdsims:::hc_state(
    fit_list$s2_n10,
    state = state_fith_list$s2,
    nboot = 5,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n10_nb10_arithmetic = ssdsims:::hc_state(
    fit_list$s2_n10,
    state = state_fith_list$s2,
    nboot = 10,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n10_nb10_geometric = ssdsims:::hc_state(
    fit_list$s2_n10,
    state = state_fith_list$s2,
    nboot = 10,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n10_nb10_multi = ssdsims:::hc_state(
    fit_list$s2_n10,
    state = state_fith_list$s2,
    nboot = 10,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n10_nb50_arithmetic = ssdsims:::hc_state(
    fit_list$s2_n10,
    state = state_fith_list$s2,
    nboot = 50,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n10_nb50_geometric = ssdsims:::hc_state(
    fit_list$s2_n10,
    state = state_fith_list$s2,
    nboot = 50,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n10_nb50_multi = ssdsims:::hc_state(
    fit_list$s2_n10,
    state = state_fith_list$s2,
    nboot = 50,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n10_nb100_arithmetic = ssdsims:::hc_state(
    fit_list$s2_n10,
    state = state_fith_list$s2,
    nboot = 100,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n10_nb100_geometric = ssdsims:::hc_state(
    fit_list$s2_n10,
    state = state_fith_list$s2,
    nboot = 100,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n10_nb100_multi = ssdsims:::hc_state(
    fit_list$s2_n10,
    state = state_fith_list$s2,
    nboot = 100,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n10_nb500_arithmetic = ssdsims:::hc_state(
    fit_list$s2_n10,
    state = state_fith_list$s2,
    nboot = 500,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n10_nb500_geometric = ssdsims:::hc_state(
    fit_list$s2_n10,
    state = state_fith_list$s2,
    nboot = 500,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n10_nb500_multi = ssdsims:::hc_state(
    fit_list$s2_n10,
    state = state_fith_list$s2,
    nboot = 500,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n20_nb1_arithmetic = ssdsims:::hc_state(
    fit_list$s2_n20,
    state = state_fith_list$s2,
    nboot = 1,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n20_nb1_geometric = ssdsims:::hc_state(
    fit_list$s2_n20,
    state = state_fith_list$s2,
    nboot = 1,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n20_nb1_multi = ssdsims:::hc_state(
    fit_list$s2_n20,
    state = state_fith_list$s2,
    nboot = 1,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n20_nb5_arithmetic = ssdsims:::hc_state(
    fit_list$s2_n20,
    state = state_fith_list$s2,
    nboot = 5,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n20_nb5_geometric = ssdsims:::hc_state(
    fit_list$s2_n20,
    state = state_fith_list$s2,
    nboot = 5,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n20_nb5_multi = ssdsims:::hc_state(
    fit_list$s2_n20,
    state = state_fith_list$s2,
    nboot = 5,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n20_nb10_arithmetic = ssdsims:::hc_state(
    fit_list$s2_n20,
    state = state_fith_list$s2,
    nboot = 10,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n20_nb10_geometric = ssdsims:::hc_state(
    fit_list$s2_n20,
    state = state_fith_list$s2,
    nboot = 10,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n20_nb10_multi = ssdsims:::hc_state(
    fit_list$s2_n20,
    state = state_fith_list$s2,
    nboot = 10,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n20_nb50_arithmetic = ssdsims:::hc_state(
    fit_list$s2_n20,
    state = state_fith_list$s2,
    nboot = 50,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n20_nb50_geometric = ssdsims:::hc_state(
    fit_list$s2_n20,
    state = state_fith_list$s2,
    nboot = 50,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n20_nb50_multi = ssdsims:::hc_state(
    fit_list$s2_n20,
    state = state_fith_list$s2,
    nboot = 50,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n20_nb100_arithmetic = ssdsims:::hc_state(
    fit_list$s2_n20,
    state = state_fith_list$s2,
    nboot = 100,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n20_nb100_geometric = ssdsims:::hc_state(
    fit_list$s2_n20,
    state = state_fith_list$s2,
    nboot = 100,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n20_nb100_multi = ssdsims:::hc_state(
    fit_list$s2_n20,
    state = state_fith_list$s2,
    nboot = 100,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n20_nb500_arithmetic = ssdsims:::hc_state(
    fit_list$s2_n20,
    state = state_fith_list$s2,
    nboot = 500,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n20_nb500_geometric = ssdsims:::hc_state(
    fit_list$s2_n20,
    state = state_fith_list$s2,
    nboot = 500,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n20_nb500_multi = ssdsims:::hc_state(
    fit_list$s2_n20,
    state = state_fith_list$s2,
    nboot = 500,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n50_nb1_arithmetic = ssdsims:::hc_state(
    fit_list$s2_n50,
    state = state_fith_list$s2,
    nboot = 1,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n50_nb1_geometric = ssdsims:::hc_state(
    fit_list$s2_n50,
    state = state_fith_list$s2,
    nboot = 1,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n50_nb1_multi = ssdsims:::hc_state(
    fit_list$s2_n50,
    state = state_fith_list$s2,
    nboot = 1,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n50_nb5_arithmetic = ssdsims:::hc_state(
    fit_list$s2_n50,
    state = state_fith_list$s2,
    nboot = 5,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n50_nb5_geometric = ssdsims:::hc_state(
    fit_list$s2_n50,
    state = state_fith_list$s2,
    nboot = 5,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n50_nb5_multi = ssdsims:::hc_state(
    fit_list$s2_n50,
    state = state_fith_list$s2,
    nboot = 5,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n50_nb10_arithmetic = ssdsims:::hc_state(
    fit_list$s2_n50,
    state = state_fith_list$s2,
    nboot = 10,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n50_nb10_geometric = ssdsims:::hc_state(
    fit_list$s2_n50,
    state = state_fith_list$s2,
    nboot = 10,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n50_nb10_multi = ssdsims:::hc_state(
    fit_list$s2_n50,
    state = state_fith_list$s2,
    nboot = 10,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n50_nb50_arithmetic = ssdsims:::hc_state(
    fit_list$s2_n50,
    state = state_fith_list$s2,
    nboot = 50,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n50_nb50_geometric = ssdsims:::hc_state(
    fit_list$s2_n50,
    state = state_fith_list$s2,
    nboot = 50,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n50_nb50_multi = ssdsims:::hc_state(
    fit_list$s2_n50,
    state = state_fith_list$s2,
    nboot = 50,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n50_nb100_arithmetic = ssdsims:::hc_state(
    fit_list$s2_n50,
    state = state_fith_list$s2,
    nboot = 100,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n50_nb100_geometric = ssdsims:::hc_state(
    fit_list$s2_n50,
    state = state_fith_list$s2,
    nboot = 100,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n50_nb100_multi = ssdsims:::hc_state(
    fit_list$s2_n50,
    state = state_fith_list$s2,
    nboot = 100,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n50_nb500_arithmetic = ssdsims:::hc_state(
    fit_list$s2_n50,
    state = state_fith_list$s2,
    nboot = 500,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n50_nb500_geometric = ssdsims:::hc_state(
    fit_list$s2_n50,
    state = state_fith_list$s2,
    nboot = 500,
    est_method = "geometric",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n50_nb500_multi = ssdsims:::hc_state(
    fit_list$s2_n50,
    state = state_fith_list$s2,
    nboot = 500,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = c(0.01, 0.05, 0.1, 0.2),
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  )
)

# --- Verification: byte-by-byte equivalence vs ssd_run_scenario() ---
ref_unique <- reference[!duplicated(reference[c("sim", "nrow")]), ]
stopifnot(identical(unname(data_list), ref_unique$data))

fit_estimates_equal <- function(a, b) {
  identical(lapply(a, ssdtools::estimates), lapply(b, ssdtools::estimates))
}
stopifnot(all(mapply(fit_estimates_equal, unname(fit_list), ref_unique$fits)))

stopifnot(identical(unname(hc_list), reference$hc))

message(
  "OK example-2-expanded: 10 data + 10 fits + 180 hc match ssd_run_scenario(seed=42L,...) byte-for-byte (fits compared via estimates())."
)


# --- Example 3 (simplified) -----------------------------------------
#
# Each step's grid at a different size, demonstrating the step-wise
# fan-out using `slice_sample_state()`, `fit_dists_state()` and
# `hc_state()`:
#
#   data: 2 sim * 1 nrow                                  = 2
#   fit:  data * 2 rescale                                = 4
#   hc:   fit  * 2 nboot * 1 est_method                   = 8
#
# nrow is a scalar so ssd_sim_data.data.frame takes the FAST path;
# the data states are then derived with seed = seed_val (NOT NULL).
# fit and hc states are the same as data states because all three
# stages derive from the same (seed=seed_val, sim) tuple.

RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
set.seed(seed_val)
reference3 <- ssd_run_scenario(
  ssddata::ccme_boron,
  nsim = 2L,
  nrow = 5L,
  rescale = c(FALSE, TRUE),
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

state_list3 <- list(
  s1 = ssdsims:::get_lecuyer_cmrg_stream_state(
    seed = seed_val,
    stream = stream_val,
    start_sim = 1L
  ),
  s2 = ssdsims:::get_lecuyer_cmrg_stream_state(
    seed = seed_val,
    stream = stream_val,
    start_sim = 2L
  )
)

data_list3 <- list(
  s1 = ssdsims:::slice_sample_state(
    ssddata::ccme_boron,
    n = 5L,
    replace = FALSE,
    state = state_list3$s1
  ),
  s2 = ssdsims:::slice_sample_state(
    ssddata::ccme_boron,
    n = 5L,
    replace = FALSE,
    state = state_list3$s2
  )
)

fit_list3 <- list(
  s1_rF = ssdsims:::fit_dists_state(
    data_list3$s1,
    state = state_list3$s1,
    dists = ssdtools::ssd_dists_bcanz(),
    rescale = FALSE,
    computable = FALSE,
    at_boundary_ok = TRUE,
    min_pmix = ssdtools::ssd_min_pmix,
    range_shape1 = c(0.05, 20),
    range_shape2 = c(0.05, 20),
    silent = TRUE
  ),
  s1_rT = ssdsims:::fit_dists_state(
    data_list3$s1,
    state = state_list3$s1,
    dists = ssdtools::ssd_dists_bcanz(),
    rescale = TRUE,
    computable = FALSE,
    at_boundary_ok = TRUE,
    min_pmix = ssdtools::ssd_min_pmix,
    range_shape1 = c(0.05, 20),
    range_shape2 = c(0.05, 20),
    silent = TRUE
  ),
  s2_rF = ssdsims:::fit_dists_state(
    data_list3$s2,
    state = state_list3$s2,
    dists = ssdtools::ssd_dists_bcanz(),
    rescale = FALSE,
    computable = FALSE,
    at_boundary_ok = TRUE,
    min_pmix = ssdtools::ssd_min_pmix,
    range_shape1 = c(0.05, 20),
    range_shape2 = c(0.05, 20),
    silent = TRUE
  ),
  s2_rT = ssdsims:::fit_dists_state(
    data_list3$s2,
    state = state_list3$s2,
    dists = ssdtools::ssd_dists_bcanz(),
    rescale = TRUE,
    computable = FALSE,
    at_boundary_ok = TRUE,
    min_pmix = ssdtools::ssd_min_pmix,
    range_shape1 = c(0.05, 20),
    range_shape2 = c(0.05, 20),
    silent = TRUE
  )
)

hc_list3 <- list(
  s1_rF_nb10 = ssdsims:::hc_state(
    fit_list3$s1_rF,
    state = state_list3$s1,
    nboot = 10,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = 0.05,
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_rF_nb50 = ssdsims:::hc_state(
    fit_list3$s1_rF,
    state = state_list3$s1,
    nboot = 50,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = 0.05,
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_rT_nb10 = ssdsims:::hc_state(
    fit_list3$s1_rT,
    state = state_list3$s1,
    nboot = 10,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = 0.05,
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_rT_nb50 = ssdsims:::hc_state(
    fit_list3$s1_rT,
    state = state_list3$s1,
    nboot = 50,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = 0.05,
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_rF_nb10 = ssdsims:::hc_state(
    fit_list3$s2_rF,
    state = state_list3$s2,
    nboot = 10,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = 0.05,
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_rF_nb50 = ssdsims:::hc_state(
    fit_list3$s2_rF,
    state = state_list3$s2,
    nboot = 50,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = 0.05,
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_rT_nb10 = ssdsims:::hc_state(
    fit_list3$s2_rT,
    state = state_list3$s2,
    nboot = 10,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = 0.05,
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_rT_nb50 = ssdsims:::hc_state(
    fit_list3$s2_rT,
    state = state_list3$s2,
    nboot = 50,
    est_method = "multi",
    ci_method = "weighted_samples",
    proportion = 0.05,
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  )
)

ref3_unique_data <- reference3[!duplicated(reference3[c("sim", "nrow")]), ]$data
stopifnot(identical(unname(data_list3), ref3_unique_data))

ref3_unique_fits <- reference3[
  !duplicated(reference3[c("sim", "nrow", "rescale")]),
]$fits
stopifnot(all(mapply(fit_estimates_equal, unname(fit_list3), ref3_unique_fits)))

stopifnot(identical(unname(hc_list3), reference3$hc))

message(
  "OK example-3-simplified: 2 data + 4 fits + 8 hc match ssd_run_scenario(seed=42L,...) byte-for-byte."
)
