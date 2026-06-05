rlang::local_options(
  pillar.bold = FALSE,
  pillar.max_footer_lines = 7,
  pillar.min_title_chars = 20,
  .frame = testthat::teardown_env()
)

RNGkind("L'Ecuyer-CMRG")

# dqrng moved from Imports to Suggests (it is never loaded implicitly by
# ssdsims). The backend helpers require it to be already loaded, so attach it
# once for the whole suite when it is installed. Tests that exercise the backend
# also guard with `skip_if_not_installed("dqrng")` so they skip (rather than
# error) when dqrng is unavailable, e.g. under the suggests-removed check matrix.
if (requireNamespace("dqrng", quietly = TRUE)) {
  library(dqrng)
}
