## Define the matrix of experiment configurations.
##
## Three problem sizes × four split granularities = 12 runs.
## Scenario 1 only (the bootstrap one; scenario 2 is too cheap to be
## interesting for batch-size studies).

nrow_levels_default <- c(5L, 6L, 10L, 20L, 50L)

sizes <- list(
  small = list(
    proportion = 0.05,
    ci_method = c("weighted_samples", "multi_fixed"),
    nboot = c(1L, 10L)
  ),
  medium = list(
    proportion = c(0.05, 0.1),
    ci_method = c("weighted_samples", "multi_fixed", "GMACL", "MACL"),
    nboot = c(1L, 10L, 100L)
  ),
  large = list(
    proportion = c(0.01, 0.05, 0.1),
    ci_method = c(
      "arithmetic_samples", "geometric_samples", "GMACL", "MACL",
      "multi_fixed", "multi_free", "weighted_samples"
    ),
    nboot = c(1L, 5L, 10L, 50L, 100L)
  )
)

splits <- list(
  split0 = c("nrow"),
  split1 = c("nrow", "ci_method"),
  split2 = c("nrow", "ci_method", "nboot"),
  split3 = c("nrow", "ci_method", "nboot", "proportion")
)

configs <- list()
for (sz_name in names(sizes)) {
  sz <- sizes[[sz_name]]
  for (sp_name in names(splits)) {
    cfg <- list(
      id = paste(sz_name, sp_name, sep = "_"),
      size = sz_name,
      split = sp_name,
      nsim = 2L,
      nrow_levels = nrow_levels_default,
      proportion = sz$proportion,
      ci_method = sz$ci_method,
      nboot = sz$nboot,
      split_axes = splits[[sp_name]]
    )
    configs[[cfg$id]] <- cfg
  }
}
