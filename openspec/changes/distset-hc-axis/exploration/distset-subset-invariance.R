# Exploration: the subset-reuse invariance premise for the `distset` hc axis.
#
# The whole change rests on a single claim: re-averaging a *subset* of one union
# fit is byte-identical, at a fixed seed, to fitting that subset's distributions
# alone and running `ssd_hc()` the same way. Per-distribution fits are
# independent within `ssd_fit_dists()`, so the subset shares the union's
# per-distribution fits exactly; `subset.fitdists(strict = FALSE)` then drops to
# the pool's members, and `ssd_hc()` re-averages over just those.
#
# This script captures that premise before any code change. It asserts:
#   * `ssd_hc(subset(union_fit, set, strict = FALSE))` byte-equals
#     `ssd_hc(ssd_fit_dists(data, dists = set))`,
#   * for an averaged pool (BCANZ), a smaller pool (the Iwasaki-style set), and a
#     single-distribution set,
#   * with `ci = FALSE` (purely analytical) and `ci = TRUE` (bootstrap re-seeded
#     identically by the primer),
#   * with the subset performed *inside* the seeded region (the order the shared
#     hc primer uses), proving `subset.fitdists()` consumes no RNG.
#
# Run interactively: `source("exploration/distset-subset-invariance.R")`.

suppressMessages({
  library(ssdsims)
  library(ssdtools)
  library(ssddata)
})

data <- ssddata::ccme_boron

# The union every pool is a subset of (a superset of all the sets below).
union <- sort(unique(c(
  ssdtools::ssd_dists_bcanz(),
  c("burrIII3", "gamma", "llogis", "lnorm", "weibull"),
  "lnorm"
)))

sets <- list(
  BCANZ = ssdtools::ssd_dists_bcanz(),
  Iwasaki = c("burrIII3", "gamma", "llogis", "lnorm", "weibull"),
  lnorm = "lnorm"
)

# Fixed cell seed/primer pair (the `(seed, primer)` the hc task would install).
fit_seed <- 42L
fit_primer <- c(3L, 4L)
hc_seed <- 99L
hc_primer <- c(7L, 8L)

ssdsims:::local_dqrng_backend()

# One union fit, fit once at the fit cell's seed.
ssdsims:::local_dqrng_state(fit_seed, primer = fit_primer)
union_fit <- ssdtools::ssd_fit_dists(data, dists = union, silent = TRUE)

invariant_holds <- function(set, ci) {
  # Direct fit of just the set's members, at the *same* fit seed (independent
  # per-distribution fits => identical members to the union's).
  ssdsims:::local_dqrng_state(fit_seed, primer = fit_primer)
  direct_fit <- ssdtools::ssd_fit_dists(data, dists = set, silent = TRUE)

  # Subset path: seed, THEN subset, THEN hc -- the order the shared primer uses.
  ssdsims:::local_dqrng_state(hc_seed, primer = hc_primer)
  sub <- subset(union_fit, select = set, strict = FALSE)
  hc_sub <- ssdtools::ssd_hc(
    sub,
    proportion = 0.05,
    ci = ci,
    nboot = if (ci) 100L else 1000L,
    min_pboot = 0
  )

  # Direct path: same seed, hc on the directly-fit set.
  ssdsims:::local_dqrng_state(hc_seed, primer = hc_primer)
  hc_direct <- ssdtools::ssd_hc(
    direct_fit,
    proportion = 0.05,
    ci = ci,
    nboot = if (ci) 100L else 1000L,
    min_pboot = 0
  )

  identical(hc_sub, hc_direct)
}

results <- expand.grid(
  set = names(sets),
  ci = c(FALSE, TRUE),
  stringsAsFactors = FALSE
)
results$identical <- mapply(
  function(nm, ci) invariant_holds(sets[[nm]], ci),
  results$set,
  results$ci
)
print(results)

stopifnot(all(results$identical))
message("OK: subset-reuse is byte-identical to a direct fit at a fixed seed.")
