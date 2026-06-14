rlang::local_options(
  pillar.bold = FALSE,
  pillar.max_footer_lines = 7,
  pillar.min_title_chars = 20,
  .frame = testthat::teardown_env()
)

RNGkind("L'Ecuyer-CMRG")
