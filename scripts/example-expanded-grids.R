## scripts/example-expanded-grids.R
##
## Companion to scripts/example-expanded.R, downsized to make each of
## the three steps fan out to a different size -- a four-eight-sixteen
## cascade keyed by sim, nrow, rescale, est_method. Built only from the
## state-only primitives `slice_sample_state()`, `fit_dists_state()`
## and `hc_state()`:
##
##   data: 2 sim * 2 nrow                                    = 4
##   fit:  data * 2 rescale                                  = 8
##   hc:   fit  * 1 nboot * 2 est_method * 1 ci_method * 1 p = 16
##
## Each call is a named entry of one of three lists. Byte-by-byte
## equivalent to `ssd_run_scenario(seed = 42L, ...)`:
##   - `data` and `hc` compared with `identical()`.
##   - `fits` compared via `ssdtools::estimates()` of each `tmbfit`
##     (the TMB `model` slot carries a C++ external pointer that
##     `identical()` cannot meaningfully compare).
##
## `state_list` is the unnamed length-2 list returned by
## `get_lecuyer_cmrg_stream_states()`, accessed by integer index:
## `state_list[[s]]` for sim `s`. Sub-streams are shared across data,
## fit and hc -- the current package behaviour. See
## scripts/example-expanded-grids-independent.R for the alternative
## design where each call gets its own sub-stream.

library(ssdsims)

seed_val <- 42L
stream_val <- 1L

# --- Reference run --------------------------------------------------
RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
set.seed(seed_val)
reference <- ssd_run_scenario(
  ssddata::ccme_boron,
  nsim = 2L,
  nrow = c(5L, 10L),
  rescale = c(FALSE, TRUE),
  proportion = 0.05,
  est_method = c("arithmetic", "multi"),
  ci_method = "weighted_samples",
  ci = FALSE,
  parametric = TRUE,
  nboot = 10,
  samples = FALSE,
  delta = Inf,
  seed = seed_val,
  .progress = FALSE
)

# --- Expansion: 2 substreams populated in one call ------------------
RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
set.seed(seed_val)
state_list <- ssdsims:::get_lecuyer_cmrg_stream_states(
  seed = seed_val,
  nsim = 2L,
  stream = stream_val,
  start_sim = 1L
)

# --- Stage 1: data slices (2 sim * 2 nrow = 4) ----------------------
data_list <- list(
  s1_n5 = ssdsims:::slice_sample_state(
    ssddata::ccme_boron,
    n = 5L,
    replace = FALSE,
    state = state_list[[1L]]
  ),
  s1_n10 = ssdsims:::slice_sample_state(
    ssddata::ccme_boron,
    n = 10L,
    replace = FALSE,
    state = state_list[[1L]]
  ),
  s2_n5 = ssdsims:::slice_sample_state(
    ssddata::ccme_boron,
    n = 5L,
    replace = FALSE,
    state = state_list[[2L]]
  ),
  s2_n10 = ssdsims:::slice_sample_state(
    ssddata::ccme_boron,
    n = 10L,
    replace = FALSE,
    state = state_list[[2L]]
  )
)

# --- Stage 2: fits (data * rescale = 4 * 2 = 8) ---------------------
fit_list <- list(
  s1_n5_rF = ssdsims:::fit_dists_state(
    data_list$s1_n5,
    state = state_list[[1L]],
    dists = ssdtools::ssd_dists_bcanz(),
    rescale = FALSE,
    computable = FALSE,
    at_boundary_ok = TRUE,
    min_pmix = ssd_pmix(ssd_min_pmix = ssdtools::ssd_min_pmix),
    range_shape1 = c(0.05, 20),
    range_shape2 = c(0.05, 20),
    silent = TRUE
  ),
  s1_n5_rT = ssdsims:::fit_dists_state(
    data_list$s1_n5,
    state = state_list[[1L]],
    dists = ssdtools::ssd_dists_bcanz(),
    rescale = TRUE,
    computable = FALSE,
    at_boundary_ok = TRUE,
    min_pmix = ssd_pmix(ssd_min_pmix = ssdtools::ssd_min_pmix),
    range_shape1 = c(0.05, 20),
    range_shape2 = c(0.05, 20),
    silent = TRUE
  ),
  s1_n10_rF = ssdsims:::fit_dists_state(
    data_list$s1_n10,
    state = state_list[[1L]],
    dists = ssdtools::ssd_dists_bcanz(),
    rescale = FALSE,
    computable = FALSE,
    at_boundary_ok = TRUE,
    min_pmix = ssd_pmix(ssd_min_pmix = ssdtools::ssd_min_pmix),
    range_shape1 = c(0.05, 20),
    range_shape2 = c(0.05, 20),
    silent = TRUE
  ),
  s1_n10_rT = ssdsims:::fit_dists_state(
    data_list$s1_n10,
    state = state_list[[1L]],
    dists = ssdtools::ssd_dists_bcanz(),
    rescale = TRUE,
    computable = FALSE,
    at_boundary_ok = TRUE,
    min_pmix = ssd_pmix(ssd_min_pmix = ssdtools::ssd_min_pmix),
    range_shape1 = c(0.05, 20),
    range_shape2 = c(0.05, 20),
    silent = TRUE
  ),
  s2_n5_rF = ssdsims:::fit_dists_state(
    data_list$s2_n5,
    state = state_list[[2L]],
    dists = ssdtools::ssd_dists_bcanz(),
    rescale = FALSE,
    computable = FALSE,
    at_boundary_ok = TRUE,
    min_pmix = ssd_pmix(ssd_min_pmix = ssdtools::ssd_min_pmix),
    range_shape1 = c(0.05, 20),
    range_shape2 = c(0.05, 20),
    silent = TRUE
  ),
  s2_n5_rT = ssdsims:::fit_dists_state(
    data_list$s2_n5,
    state = state_list[[2L]],
    dists = ssdtools::ssd_dists_bcanz(),
    rescale = TRUE,
    computable = FALSE,
    at_boundary_ok = TRUE,
    min_pmix = ssd_pmix(ssd_min_pmix = ssdtools::ssd_min_pmix),
    range_shape1 = c(0.05, 20),
    range_shape2 = c(0.05, 20),
    silent = TRUE
  ),
  s2_n10_rF = ssdsims:::fit_dists_state(
    data_list$s2_n10,
    state = state_list[[2L]],
    dists = ssdtools::ssd_dists_bcanz(),
    rescale = FALSE,
    computable = FALSE,
    at_boundary_ok = TRUE,
    min_pmix = ssd_pmix(ssd_min_pmix = ssdtools::ssd_min_pmix),
    range_shape1 = c(0.05, 20),
    range_shape2 = c(0.05, 20),
    silent = TRUE
  ),
  s2_n10_rT = ssdsims:::fit_dists_state(
    data_list$s2_n10,
    state = state_list[[2L]],
    dists = ssdtools::ssd_dists_bcanz(),
    rescale = TRUE,
    computable = FALSE,
    at_boundary_ok = TRUE,
    min_pmix = ssd_pmix(ssd_min_pmix = ssdtools::ssd_min_pmix),
    range_shape1 = c(0.05, 20),
    range_shape2 = c(0.05, 20),
    silent = TRUE
  )
)

# --- Stage 3: hc (fit * est_method = 8 * 2 = 16) --------------------
hc_list <- list(
  s1_n5_rF_arith = ssdsims:::hc_state(
    fit_list$s1_n5_rF,
    state = state_list[[1L]],
    nboot = 10,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = 0.05,
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n5_rF_multi = ssdsims:::hc_state(
    fit_list$s1_n5_rF,
    state = state_list[[1L]],
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
  s1_n5_rT_arith = ssdsims:::hc_state(
    fit_list$s1_n5_rT,
    state = state_list[[1L]],
    nboot = 10,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = 0.05,
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n5_rT_multi = ssdsims:::hc_state(
    fit_list$s1_n5_rT,
    state = state_list[[1L]],
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
  s1_n10_rF_arith = ssdsims:::hc_state(
    fit_list$s1_n10_rF,
    state = state_list[[1L]],
    nboot = 10,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = 0.05,
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n10_rF_multi = ssdsims:::hc_state(
    fit_list$s1_n10_rF,
    state = state_list[[1L]],
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
  s1_n10_rT_arith = ssdsims:::hc_state(
    fit_list$s1_n10_rT,
    state = state_list[[1L]],
    nboot = 10,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = 0.05,
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s1_n10_rT_multi = ssdsims:::hc_state(
    fit_list$s1_n10_rT,
    state = state_list[[1L]],
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
  s2_n5_rF_arith = ssdsims:::hc_state(
    fit_list$s2_n5_rF,
    state = state_list[[2L]],
    nboot = 10,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = 0.05,
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n5_rF_multi = ssdsims:::hc_state(
    fit_list$s2_n5_rF,
    state = state_list[[2L]],
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
  s2_n5_rT_arith = ssdsims:::hc_state(
    fit_list$s2_n5_rT,
    state = state_list[[2L]],
    nboot = 10,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = 0.05,
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n5_rT_multi = ssdsims:::hc_state(
    fit_list$s2_n5_rT,
    state = state_list[[2L]],
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
  s2_n10_rF_arith = ssdsims:::hc_state(
    fit_list$s2_n10_rF,
    state = state_list[[2L]],
    nboot = 10,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = 0.05,
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n10_rF_multi = ssdsims:::hc_state(
    fit_list$s2_n10_rF,
    state = state_list[[2L]],
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
  s2_n10_rT_arith = ssdsims:::hc_state(
    fit_list$s2_n10_rT,
    state = state_list[[2L]],
    nboot = 10,
    est_method = "arithmetic",
    ci_method = "weighted_samples",
    proportion = 0.05,
    ci = FALSE,
    parametric = TRUE,
    save_to = NULL,
    samples = FALSE,
    delta = Inf
  ),
  s2_n10_rT_multi = ssdsims:::hc_state(
    fit_list$s2_n10_rT,
    state = state_list[[2L]],
    nboot = 10,
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

# --- Verification ---------------------------------------------------
fit_estimates_equal <- function(a, b) {
  identical(lapply(a, ssdtools::estimates), lapply(b, ssdtools::estimates))
}

ref_unique_data <- reference[!duplicated(reference[c("sim", "nrow")]), ]$data
stopifnot(identical(unname(data_list), ref_unique_data))

ref_unique_fits <- reference[
  !duplicated(reference[c("sim", "nrow", "rescale")]),
]$fits
stopifnot(all(mapply(fit_estimates_equal, unname(fit_list), ref_unique_fits)))

stopifnot(identical(unname(hc_list), reference$hc))

message(
  "OK example-expanded-grids: 4 data + 8 fits + 16 hc match ssd_run_scenario(seed=42L,...) byte-for-byte."
)
