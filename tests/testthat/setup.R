rlang::local_options(
  pillar.bold = FALSE,
  pillar.max_footer_lines = 7,
  pillar.min_title_chars = 20,
  .frame = testthat::teardown_env()
)

RNGkind("L'Ecuyer-CMRG")

# dqrng is a Suggested, conditionally-used dependency: ssdsims never loads it,
# so the suite (which exercises the dqrng backend throughout) opts in here.
# Loading the namespace is enough for `dqrng_usable()`.
requireNamespace("dqrng", quietly = TRUE)
