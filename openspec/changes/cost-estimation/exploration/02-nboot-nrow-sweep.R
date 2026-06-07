# Exploration 2/2: calibrate the per-task cost model and extrapolate a scenario
#
#   Rscript --no-environ openspec/changes/cost-estimation/exploration/02-nboot-nrow-sweep.R
#
# This is the prototype of `ssd_calibrate_cost()` + `ssd_estimate_cost()`. It
# times ssd_hc() over small nboot x all ci_methods x a few nrow, fits the
# per-ci_method model  time ~ base + slope * max(nboot, n0)  (n0 by grid
# search), inspects the (weak, non-monotonic) nrow effect, and extrapolates the
# motivating scenario. The fitted coefficients are what the change ships as the
# default calibration.

suppressMessages(library(ssdtools))
suppressMessages(library(ssddata))

set.seed(42)
conc <- ssddata::ccme_boron$Conc
ci_methods <- ssd_ci_methods()
nrows <- c(5L, 10L, 20L, 50L)
nboots <- c(20L, 50L, 100L, 200L) # tiny: estimate slope + intercept cheaply
props <- c(0.01, 0.05, 0.1, 0.2) # vectorised into one call (free)

# Pre-fit one bcanz model per nrow (isolate hc cost from fit cost). nrow = 5L
# relaxes ssd_fit_dists()'s 6-row floor, matching ssdsims' internal fit call.
fits <- lapply(nrows, function(n) {
  d <- data.frame(Conc = sample(conc, n, replace = TRUE))
  list(n = n, fit = ssd_fit_dists(d, dists = ssd_dists_bcanz(), nrow = 5L))
})

tt <- function(expr) as.numeric(system.time(expr)["elapsed"])

rows <- list()
k <- 0
for (f in fits) {
  for (cim in ci_methods) {
    for (nb in nboots) {
      el <- tt(suppressWarnings(ssd_hc(
        f$fit,
        proportion = props, ci = TRUE, nboot = nb,
        est_method = "multi", ci_method = cim, parametric = TRUE,
        min_pboot = 0
      )))
      k <- k + 1
      rows[[k]] <- data.frame(nrow = f$n, ci_method = cim, nboot = nb, time = el)
    }
  }
}
df <- do.call(rbind, rows)

# --- per-ci_method fit: time ~ base + slope * max(nboot, n0) -----------------
coef_tbl <- do.call(rbind, lapply(split(df, df$ci_method), function(d) {
  n0s <- seq(0, 300, by = 5)
  best <- which.min(vapply(n0s, function(n0) summary(lm(time ~ pmax(nboot, n0), d))$sigma, numeric(1)))
  n0 <- n0s[best]
  m <- lm(time ~ pmax(nboot, n0), d)
  data.frame(
    ci_method = d$ci_method[1], n0 = n0,
    base_s = round(unname(predict(m, data.frame(nboot = 0))), 3),
    ms_per_boot = round(unname(coef(m)[2]) * 1000, 3)
  )
}))
rownames(coef_tbl) <- NULL
cat("=== calibration coefficients (the shipped default) ===\n")
print(coef_tbl[order(coef_tbl$ms_per_boot), ])

cat("\n=== nrow effect at nboot=200 (weak, non-monotonic; bounded factor) ===\n")
d2 <- df[df$nboot == 200, ]
print(round(tapply(d2$time, list(d2$ci_method, d2$nrow), mean), 1))

# --- extrapolate the motivating scenario (the estimator's job) ---------------
slope <- setNames(coef_tbl$ms_per_boot / 1000, coef_tbl$ci_method)
base <- setNames(coef_tbl$base_s, coef_tbl$ci_method)
nsim <- 10
nrow_n <- 4
nboot_vec <- c(1000, 5000, 10000, 50000)
per_task <- function(cim, nb) base[[cim]] + slope[[cim]] * nb
# After est-method-setting: one bootstrap per (nboot, ci_method) cell; est_method free.
cells <- expand.grid(cim = names(slope), nb = nboot_vec, stringsAsFactors = FALSE)
cells$t <- mapply(per_task, cells$cim, cells$nb)
total_h <- sum(cells$t) * nsim * nrow_n / 3600
longest_min <- max(cells$t) / 60
cat(sprintf(
  "\nmotivating scenario (10 sims x 4 nrow x 7 ci_method x 4 nboot, ci=TRUE):\n  total ~%.0f single-core hours | longest single task ~%.0f min (%s @ nboot=50000)\n",
  total_h, longest_min, cells$cim[which.max(cells$t)]
))

# In-session default (Intel Xeon @ 2.10 GHz, R 4.5.3, ssdtools 2.6.0.9002):
#   weighted_samples 5.68 | GMACL 18.90 | MACL 19.64 | arithmetic_samples 21.24
#   | geometric_samples 21.60 | multi_fixed 52.63 | multi_free 55.00 (ms/boot),
#   n0 ~ 25-30. Extrapolated total ~430 h (est_method collapsed), longest
#   multi_free @ nboot=50000 ~44 min.
