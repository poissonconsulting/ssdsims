# ---- scenario_dataset() ----------------------------------------------------

test_that("scenario-accessors: scenario_dataset returns the materialised tibble", {
  s <- ssd_define_scenario(
    ssd_scenario_data(
      boron = ssddata::ccme_boron,
      cadmium = ssddata::ccme_cadmium
    ),
    nsim = 1L,
    seed = 1L
  )
  out <- scenario_dataset(s, "boron")
  expect_s3_class(out, "tbl_df")
  expect_identical(out, s$data[["boron"]])
  expect_true("Conc" %in% names(out))
})

test_that("scenario-accessors: scenario_dataset errors on an unknown name", {
  s <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 1L,
    seed = 1L
  )
  expect_snapshot(error = TRUE, {
    scenario_dataset(s, "nope")
  })
})

# ---- scenario_min_pmix() ---------------------------------------------------

test_that("scenario-accessors: scenario_min_pmix returns the materialised function", {
  s <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 1L,
    seed = 1L
  )
  fn <- scenario_min_pmix(s, "ssd_min_pmix")
  expect_true(is.function(fn))
  expect_identical(fn, ssdtools::ssd_min_pmix)
  expect_length(formals(fn), 1L)
})

test_that("scenario-accessors: scenario_min_pmix errors on an unknown name", {
  s <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 1L,
    seed = 1L
  )
  expect_snapshot(error = TRUE, {
    scenario_min_pmix(s, "nope")
  })
})

# ---- resolve_min_pmix() rewired to the accessor ----------------------------

test_that("scenario-accessors: resolve_min_pmix resolves via the accessor", {
  s <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 1L,
    seed = 1L
  )
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
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 1L,
    seed = 42L,
    nrow = 6L,
    dists = ssd_distset(lnorm = "lnorm")
  )
  out1 <- ssd_run_scenario_baseline(scenario)
  out2 <- ssd_run_scenario_baseline(scenario)
  # Reproducible, and the default min_pmix path fits successfully off the
  # scenario's materialised function.
  expect_s3_class(out1$fit$fits[[1L]], "fitdists")
  expect_identical(out1$hc$hc, out2$hc$hc)
})

# ---- scenario_step_slice() -------------------------------------------------

test_that("scenario-accessors: each step's slice carries its runner's inputs", {
  scenario <- ssd_define_scenario(
    ssd_scenario_data(
      boron = ssddata::ccme_boron,
      cadmium = ssddata::ccme_cadmium
    ),
    nsim = 1L,
    seed = 1L,
    dists = ssd_distset(set = c("lnorm", "gamma"))
  )

  sample_slice <- scenario_step_slice(scenario, "sample")
  fit_slice <- scenario_step_slice(scenario, "fit")
  hc_slice <- scenario_step_slice(scenario, "hc")

  # The class tag survives, so the runners' chk_s3_class() and the accessors work.
  expect_s3_class(sample_slice, "ssdsims_scenario")
  expect_s3_class(fit_slice, "ssdsims_scenario")
  expect_s3_class(hc_slice, "ssdsims_scenario")

  # sample: the datasets + the nrow_max draw-size setting + partition_by$sample,
  # and nothing else.
  expect_identical(scenario_dataset(sample_slice, "boron"), scenario$data$boron)
  expect_identical(sample_slice$nrow_max, scenario$nrow_max)
  expect_identical(sample_slice$partition_by, scenario$partition_by["sample"])
  expect_setequal(names(sample_slice), c("data", "nrow_max", "partition_by"))

  # fit: fit$dists + the min_pmix functions + partition_by for sample and fit.
  expect_identical(fit_slice$fit$dists, scenario$fit$dists)
  expect_identical(
    scenario_min_pmix(fit_slice, "ssd_min_pmix"),
    scenario$min_pmix_fns[["ssd_min_pmix"]]
  )
  expect_identical(
    fit_slice$partition_by,
    scenario$partition_by[c("sample", "fit")]
  )
  expect_setequal(names(fit_slice), c("fit", "min_pmix_fns", "partition_by"))
  expect_null(fit_slice$data)

  # hc: the hc settings the runner reads (incl. the scalar ci) + partition_by
  # for fit and hc.
  expect_identical(hc_slice$hc$proportion, scenario$hc$proportion)
  expect_identical(hc_slice$hc$ci, scenario$hc$ci)
  expect_identical(hc_slice$hc$samples, scenario$hc$samples)
  expect_identical(hc_slice$partition_by, scenario$partition_by[c("fit", "hc")])
  expect_setequal(names(hc_slice), c("hc", "partition_by"))
  expect_null(hc_slice$data)
})

test_that("scenario-accessors: the sample slice carries only the named dataset(s)", {
  df <- data.frame(Conc = exp(seq(-1, 2, length.out = 20)))
  two <- ssd_define_scenario(
    ssd_scenario_data(a = df, b = df),
    nsim = 1L,
    seed = 1L
  )
  three <- ssd_define_scenario(
    ssd_scenario_data(a = df, b = df, c = df),
    nsim = 1L,
    seed = 1L
  )
  # A per-shard sample slice carries only the dataset(s) that shard reads, so it
  # is independent of the other datasets: appending a dataset leaves the slice
  # for an existing dataset byte-identical (the path-axis-growth payoff).
  one <- scenario_step_slice(two, "sample", "a")
  expect_named(one$data, "a")
  expect_identical(one$data$a, two$data$a)
  expect_identical(one, scenario_step_slice(three, "sample", "a"))
})

test_that("scenario-accessors: scenario_step_slice is deterministic and hashable", {
  scenario <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 2L,
    seed = 42L,
    dists = ssd_distset(lnorm = "lnorm")
  )
  for (step in c("sample", "fit", "hc")) {
    a <- scenario_step_slice(scenario, step)
    b <- scenario_step_slice(scenario, step)
    expect_identical(a, b)
    expect_identical(rlang::hash(a), rlang::hash(b))
  }
})
