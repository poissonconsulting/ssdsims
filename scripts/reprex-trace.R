options(ssdsims.trace = TRUE)
library(ssdsims)

# Second example scenario from scripts/example.R (faster path: ci = FALSE).
res <- ssd_run_scenario(
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
  .progress = FALSE
)

dim(res)
head(res, 3)
