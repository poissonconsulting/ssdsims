## exploration/est-method-invariance.R
##
## Justification of work for the `est-method-setting` change: `est_method` is an
## hc **simulation setting**, not a bootstrap cross-join axis.
##
## Claim (verified below, at a fixed seed, for every `ci_method`):
##
##   ssd_hc(fit, ci = TRUE, est_method = m, <fixed seed>)
##
## returns, across m in ssd_est_methods():
##   * `samples` (the bootstrap draws)        -- BYTE-IDENTICAL
##   * `se` / `lcl` / `ucl` (the CI)          -- est_method-INVARIANT
##   * every other column except `est`        -- est_method-INVARIANT
##   * `est` (the point estimate)             -- DIFFERS (analytical, RNG-free)
##
## and, crucially:
##
##   ssd_hc(fit, ci = TRUE,  est_method = m, <seed S>)$est
##     == ssd_hc(fit, ci = FALSE, est_method = m, <seed S>)$est   (byte-identical)
##
## Implication for the design:
##   The bootstrap CI is a function of (nboot, ci_method, parametric) ALONE; the
##   only est_method-varying output is the analytical point `est`, which a
##   bootstrap-free `ci = FALSE` call reproduces exactly. So a vector
##   `est_method` need NOT re-run the bootstrap per method: run it ONCE, then
##   attach each method's analytical `est` to the shared CI. This is what
##   `hc_collapse_est_methods()` does (R/task-lists.R), serving the dqrng path
##   (`hc_data_task()`) and the legacy path (`hc_state()`) alike.
##
## This is a same-seed invariant. It does NOT claim equality with the pre-change
## pipeline: the dqrng hc primer hashes the hc-grid row, which used to include
## `est_method`, so dropping the axis re-seeds each hc task. The point `est` is
## unchanged (analytical); the CIs change numerically but stay statistically
## valid and become consistent across est_methods within a task. The same-seed
## invariant is also pinned as a regression test (tests/testthat/test-task-lists.R,
## "hazard-concentrations: collapsed est_methods match per-method ssd_hc ...") so
## a future ssdtools that broke the invariance would fail loudly.

devtools::load_all()

fit <- ssdtools::ssd_fit_dists(
  ssddata::ccme_boron,
  dists = c("lnorm", "gamma"),
  silent = TRUE
)
methods <- ssdtools::ssd_est_methods()
proportion <- c(0.05, 0.1)

# One bootstrap per est_method, each seeded IDENTICALLY.
boot <- function(m, ci_method) {
  local_dqrng_backend()
  local_dqrng_state(42L, primer = c(7L, 9L))
  ssdtools::ssd_hc(
    fit,
    proportion = proportion,
    ci = TRUE,
    nboot = 100L,
    est_method = m,
    ci_method = ci_method,
    parametric = TRUE,
    samples = TRUE,
    min_pboot = 0
  )
}

analytical <- function(m) {
  local_dqrng_backend()
  local_dqrng_state(42L, primer = c(7L, 9L))
  ssdtools::ssd_hc(
    fit,
    proportion = proportion,
    ci = FALSE,
    est_method = m,
    samples = FALSE,
    min_pboot = 0
  )
}

for (ci_method in ssdtools::ssd_ci_methods()) {
  res <- lapply(methods, boot, ci_method = ci_method)
  names(res) <- methods
  ref <- res[[1L]]
  inv_cols <- setdiff(names(ref), c("est", "est_method"))

  samples_identical <- all(vapply(
    res,
    \(r) identical(r$samples, ref$samples),
    logical(1)
  ))
  invariant <- all(vapply(
    res,
    \(r) all(vapply(inv_cols, \(cc) identical(r[[cc]], ref[[cc]]), logical(1))),
    logical(1)
  ))
  est_differs <- length(unique(lapply(res, \(r) r$est))) > 1L
  boot_eq_analytical <- all(vapply(
    methods,
    \(m) identical(boot(m, ci_method)$est, analytical(m)$est),
    logical(1)
  ))

  cat(sprintf(
    "ci_method = %-16s samples-identical=%s CI-invariant=%s est-differs=%s boot==analytical=%s\n",
    ci_method,
    samples_identical,
    invariant,
    est_differs,
    boot_eq_analytical
  ))
}
