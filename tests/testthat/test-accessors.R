# ---- scenario_dataset() ----------------------------------------------------

test_that("scenario-accessors: scenario_dataset returns the materialised tibble", {
  s <- ssd_define_scenario(
    ssd_data(boron = ssddata::ccme_boron, cadmium = ssddata::ccme_cadmium),
    nsim = 1L,
    seed = 1L
  )
  out <- scenario_dataset(s, "boron")
  expect_s3_class(out, "tbl_df")
  expect_identical(out, s$data[["boron"]])
  expect_true("Conc" %in% names(out))
})

test_that("scenario-accessors: scenario_dataset errors on an unknown name", {
  s <- ssd_define_scenario(ssddata::ccme_boron, nsim = 1L, seed = 1L)
  expect_snapshot(error = TRUE, {
    scenario_dataset(s, "nope")
  })
})

# ---- scenario_min_pmix() ---------------------------------------------------

test_that("scenario-accessors: scenario_min_pmix returns the materialised function", {
  s <- ssd_define_scenario(ssddata::ccme_boron, nsim = 1L, seed = 1L)
  fn <- scenario_min_pmix(s, "ssd_min_pmix")
  expect_true(is.function(fn))
  expect_identical(fn, ssdtools::ssd_min_pmix)
  expect_length(formals(fn), 1L)
})

test_that("scenario-accessors: scenario_min_pmix errors on an unknown name", {
  s <- ssd_define_scenario(ssddata::ccme_boron, nsim = 1L, seed = 1L)
  expect_snapshot(error = TRUE, {
    scenario_min_pmix(s, "nope")
  })
})

# ---- resolve_min_pmix() rewired to the accessor ----------------------------

test_that("scenario-accessors: resolve_min_pmix resolves via the accessor", {
  s <- ssd_define_scenario(ssddata::ccme_boron, nsim = 1L, seed = 1L)
  # The runtime resolver now reads off the scenario, returning the same
  # function the accessor (and the old ssdtools lookup) yields.
  expect_identical(
    resolve_min_pmix(s, "ssd_min_pmix"),
    scenario_min_pmix(s, "ssd_min_pmix")
  )
  expect_identical(resolve_min_pmix(s, "ssd_min_pmix"), ssdtools::ssd_min_pmix)
})

test_that("scenario-accessors: baseline runner's default min_pmix is unchanged", {
  scenario <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 1L,
    nrow = 6L,
    seed = 42L,
    dists = "lnorm"
  )
  out1 <- ssd_run_scenario_baseline(scenario)
  out2 <- ssd_run_scenario_baseline(scenario)
  # Reproducible, and the default min_pmix path fits successfully off the
  # scenario's materialised function.
  expect_s3_class(out1$fit$fits[[1L]], "fitdists")
  expect_identical(out1$hc$hc, out2$hc$hc)
})
