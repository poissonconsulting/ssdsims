## scripts/example-expanded-grids-independent.R
##
## Alternative to scripts/example-expanded-grids.R: each stochastic
## call gets its **own** L'Ecuyer-CMRG sub-stream rather than sharing
## one per sim. This is the design proposed in TARGETS-DESIGN.md §2,
## where the data, fit and hc steps consume disjoint sub-stream
## blocks sized to their respective grids.
##
## Fan-out (unchanged from the sibling file):
##   data: 2 sim * 2 nrow                                    = 4
##   fit:  data * 2 rescale                                  = 8
##   hc:   fit  * 1 nboot * 2 est_method * 1 ci_method * 1 p = 16
##                                                       sum = 28
##
## All 28 sub-streams come from a single
## `get_lecuyer_cmrg_stream_states(seed, nsim = 28, ...)` call. They
## are allocated by canonical-order index:
##   indices  1.. 4 -> data_states[1..4]
##   indices  5..12 -> fit_states[1..8]
##   indices 13..28 -> hc_states[1..16]
##
## Comparison vs `ssd_run_scenario()` is **NOT** byte-for-byte: the
## reference shares one state across stages and across grid axes, so
## independent sub-streams produce a different deterministic output.
## The check below compares **statistical properties** (means and
## standard deviations of `data$Conc`, `lnorm$meanlog` from `fits`,
## and the hc estimate) per matched cell.

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

# --- Sub-stream lattice: 28 independent states ----------------------
RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
set.seed(seed_val)
all_states <- ssdsims:::get_lecuyer_cmrg_stream_states(
  seed = seed_val,
  nsim = 28L,
  stream = stream_val,
  start_sim = 1L
)

# Allocate by canonical order. Names mirror the result-list keys.
data_state_keys <- c("s1_n5", "s1_n10", "s2_n5", "s2_n10")
fit_state_keys <- c(
  "s1_n5_rF",
  "s1_n5_rT",
  "s1_n10_rF",
  "s1_n10_rT",
  "s2_n5_rF",
  "s2_n5_rT",
  "s2_n10_rF",
  "s2_n10_rT"
)
hc_state_keys <- c(
  "s1_n5_rF_arith",
  "s1_n5_rF_multi",
  "s1_n5_rT_arith",
  "s1_n5_rT_multi",
  "s1_n10_rF_arith",
  "s1_n10_rF_multi",
  "s1_n10_rT_arith",
  "s1_n10_rT_multi",
  "s2_n5_rF_arith",
  "s2_n5_rF_multi",
  "s2_n5_rT_arith",
  "s2_n5_rT_multi",
  "s2_n10_rF_arith",
  "s2_n10_rF_multi",
  "s2_n10_rT_arith",
  "s2_n10_rT_multi"
)

data_states <- setNames(all_states[1L:4L], data_state_keys)
fit_states <- setNames(all_states[5L:12L], fit_state_keys)
hc_states <- setNames(all_states[13L:28L], hc_state_keys)

# --- Stage 1: data slices (each its own sub-stream) -----------------
data_list <- list(
  s1_n5 = ssdsims:::slice_sample_state(
    ssddata::ccme_boron,
    n = 5L,
    replace = FALSE,
    state = data_states$s1_n5
  ),
  s1_n10 = ssdsims:::slice_sample_state(
    ssddata::ccme_boron,
    n = 10L,
    replace = FALSE,
    state = data_states$s1_n10
  ),
  s2_n5 = ssdsims:::slice_sample_state(
    ssddata::ccme_boron,
    n = 5L,
    replace = FALSE,
    state = data_states$s2_n5
  ),
  s2_n10 = ssdsims:::slice_sample_state(
    ssddata::ccme_boron,
    n = 10L,
    replace = FALSE,
    state = data_states$s2_n10
  )
)

# --- Stage 2: fits (each its own sub-stream) ------------------------
fit_list <- list(
  s1_n5_rF = ssdsims:::fit_dists_state(
    data_list$s1_n5,
    state = fit_states$s1_n5_rF,
    dists = ssdtools::ssd_dists_bcanz(),
    rescale = FALSE,
    computable = FALSE,
    at_boundary_ok = TRUE,
    min_pmix = ssdtools::ssd_min_pmix,
    range_shape1 = c(0.05, 20),
    range_shape2 = c(0.05, 20),
    silent = TRUE
  ),
  s1_n5_rT = ssdsims:::fit_dists_state(
    data_list$s1_n5,
    state = fit_states$s1_n5_rT,
    dists = ssdtools::ssd_dists_bcanz(),
    rescale = TRUE,
    computable = FALSE,
    at_boundary_ok = TRUE,
    min_pmix = ssdtools::ssd_min_pmix,
    range_shape1 = c(0.05, 20),
    range_shape2 = c(0.05, 20),
    silent = TRUE
  ),
  s1_n10_rF = ssdsims:::fit_dists_state(
    data_list$s1_n10,
    state = fit_states$s1_n10_rF,
    dists = ssdtools::ssd_dists_bcanz(),
    rescale = FALSE,
    computable = FALSE,
    at_boundary_ok = TRUE,
    min_pmix = ssdtools::ssd_min_pmix,
    range_shape1 = c(0.05, 20),
    range_shape2 = c(0.05, 20),
    silent = TRUE
  ),
  s1_n10_rT = ssdsims:::fit_dists_state(
    data_list$s1_n10,
    state = fit_states$s1_n10_rT,
    dists = ssdtools::ssd_dists_bcanz(),
    rescale = TRUE,
    computable = FALSE,
    at_boundary_ok = TRUE,
    min_pmix = ssdtools::ssd_min_pmix,
    range_shape1 = c(0.05, 20),
    range_shape2 = c(0.05, 20),
    silent = TRUE
  ),
  s2_n5_rF = ssdsims:::fit_dists_state(
    data_list$s2_n5,
    state = fit_states$s2_n5_rF,
    dists = ssdtools::ssd_dists_bcanz(),
    rescale = FALSE,
    computable = FALSE,
    at_boundary_ok = TRUE,
    min_pmix = ssdtools::ssd_min_pmix,
    range_shape1 = c(0.05, 20),
    range_shape2 = c(0.05, 20),
    silent = TRUE
  ),
  s2_n5_rT = ssdsims:::fit_dists_state(
    data_list$s2_n5,
    state = fit_states$s2_n5_rT,
    dists = ssdtools::ssd_dists_bcanz(),
    rescale = TRUE,
    computable = FALSE,
    at_boundary_ok = TRUE,
    min_pmix = ssdtools::ssd_min_pmix,
    range_shape1 = c(0.05, 20),
    range_shape2 = c(0.05, 20),
    silent = TRUE
  ),
  s2_n10_rF = ssdsims:::fit_dists_state(
    data_list$s2_n10,
    state = fit_states$s2_n10_rF,
    dists = ssdtools::ssd_dists_bcanz(),
    rescale = FALSE,
    computable = FALSE,
    at_boundary_ok = TRUE,
    min_pmix = ssdtools::ssd_min_pmix,
    range_shape1 = c(0.05, 20),
    range_shape2 = c(0.05, 20),
    silent = TRUE
  ),
  s2_n10_rT = ssdsims:::fit_dists_state(
    data_list$s2_n10,
    state = fit_states$s2_n10_rT,
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

# --- Stage 3: hc (each its own sub-stream) --------------------------
hc_list <- list(
  s1_n5_rF_arith = ssdsims:::hc_state(
    fit_list$s1_n5_rF,
    state = hc_states$s1_n5_rF_arith,
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
    state = hc_states$s1_n5_rF_multi,
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
    state = hc_states$s1_n5_rT_arith,
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
    state = hc_states$s1_n5_rT_multi,
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
    state = hc_states$s1_n10_rF_arith,
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
    state = hc_states$s1_n10_rF_multi,
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
    state = hc_states$s1_n10_rT_arith,
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
    state = hc_states$s1_n10_rT_multi,
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
    state = hc_states$s2_n5_rF_arith,
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
    state = hc_states$s2_n5_rF_multi,
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
    state = hc_states$s2_n5_rT_arith,
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
    state = hc_states$s2_n5_rT_multi,
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
    state = hc_states$s2_n10_rF_arith,
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
    state = hc_states$s2_n10_rF_multi,
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
    state = hc_states$s2_n10_rT_arith,
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
    state = hc_states$s2_n10_rT_multi,
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

# --- Statistical comparison vs ssd_run_scenario() -------------------
# Two scenarios share the same data source, fit model and hc method;
# only the per-call RNG sub-stream differs. We expect data slice
# means/SDs to differ only by sampling noise, fit estimates to agree
# closely (the optimizer converges to the same MLE from independent
# random starts when the problem is well-conditioned), and hc point
# estimates to differ only via the bootstrap-noise component.

ref_data <- reference[!duplicated(reference[c("sim", "nrow")]), ]$data
ref_fits <- reference[
  !duplicated(reference[c("sim", "nrow", "rescale")]),
]$fits
ref_hc <- reference$hc

# Mean / SD of Conc per data slice.
data_cmp <- data.frame(
  key = data_state_keys,
  ref_mean = vapply(ref_data, \(d) mean(d$Conc), numeric(1)),
  new_mean = vapply(unname(data_list), \(d) mean(d$Conc), numeric(1)),
  ref_sd = vapply(ref_data, \(d) sd(d$Conc), numeric(1)),
  new_sd = vapply(unname(data_list), \(d) sd(d$Conc), numeric(1))
)
data_cmp$mean_diff <- abs(data_cmp$ref_mean - data_cmp$new_mean) /
  data_cmp$ref_mean
data_cmp$sd_diff <- abs(data_cmp$ref_sd - data_cmp$new_sd) / data_cmp$ref_sd

cat("Stage 1 (data): mean(Conc) and sd(Conc) per slice\n")
print(data_cmp, row.names = FALSE, digits = 3)

# lnorm meanlog estimate per fit. `ssdtools::estimates()` returns a flat
# list with `dist.parameter` names, so the key is `lnorm.meanlog`.
lnorm_meanlog <- function(fit) {
  est <- ssdtools::estimates(fit)
  if (!"lnorm.meanlog" %in% names(est)) {
    NA_real_
  } else {
    est[["lnorm.meanlog"]]
  }
}

fit_cmp <- data.frame(
  key = fit_state_keys,
  ref = vapply(ref_fits, lnorm_meanlog, numeric(1)),
  new = vapply(unname(fit_list), lnorm_meanlog, numeric(1))
)
fit_cmp$rel_diff <- abs(fit_cmp$ref - fit_cmp$new) / abs(fit_cmp$ref)

cat("\nStage 2 (fit): lnorm meanlog estimate per fit\n")
print(fit_cmp, row.names = FALSE, digits = 3)

# hc point estimate (5% hazard concentration) per hc row.
hc_cmp <- data.frame(
  key = hc_state_keys,
  ref = vapply(ref_hc, \(h) h$est[1L], numeric(1)),
  new = vapply(unname(hc_list), \(h) h$est[1L], numeric(1))
)
hc_cmp$rel_diff <- abs(hc_cmp$ref - hc_cmp$new) / abs(hc_cmp$ref)

cat("\nStage 3 (hc): est(0.05) per hc cell\n")
print(hc_cmp, row.names = FALSE, digits = 3)

cat(sprintf(
  "\nSummary of relative differences:\n  data mean: max %.2f%%, median %.2f%%\n  fit meanlog: max %.2f%%, median %.2f%%\n  hc est:      max %.2f%%, median %.2f%%\n",
  100 * max(data_cmp$mean_diff),
  100 * median(data_cmp$mean_diff),
  100 * max(fit_cmp$rel_diff, na.rm = TRUE),
  100 * median(fit_cmp$rel_diff, na.rm = TRUE),
  100 * max(hc_cmp$rel_diff, na.rm = TRUE),
  100 * median(hc_cmp$rel_diff, na.rm = TRUE)
))

message(
  "OK example-expanded-grids-independent: 28 independent sub-streams allocated; statistical comparison printed."
)
