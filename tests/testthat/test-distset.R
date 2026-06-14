# ---- ssd_distset() constructor (task 2.1) ----------------------------------

test_that("scenario-definition: ssd_distset assembles a named, validated collection", {
  ds <- ssd_distset(
    BCANZ = ssdtools::ssd_dists_bcanz(),
    Iwasaki = c("burrIII3", "gamma", "llogis", "lnorm", "weibull"),
    lnorm = "lnorm"
  )
  expect_s3_class(ds, "ssdsims_distset")
  expect_named(ds, c("BCANZ", "Iwasaki", "lnorm"))
  expect_identical(ds$lnorm, "lnorm")
  expect_identical(
    ds$Iwasaki,
    c("burrIII3", "gamma", "llogis", "lnorm", "weibull")
  )
})

test_that("scenario-definition: ssd_distset prints stably", {
  skip_on_cran()
  expect_snapshot(
    ssd_distset(BCANZ = ssdtools::ssd_dists_bcanz(), lnorm = "lnorm")
  )
})

test_that("scenario-definition: ssd_distset rejects invalid input naming the offending set", {
  skip_on_cran()
  expect_snapshot(error = TRUE, ssd_distset())
  expect_snapshot(error = TRUE, ssd_distset(ssdtools::ssd_dists_bcanz()))
  expect_snapshot(error = TRUE, ssd_distset(a = "lnorm", a = "gamma"))
  expect_snapshot(error = TRUE, ssd_distset(bad = "not_a_distribution"))
  expect_snapshot(error = TRUE, ssd_distset(empty = character(0)))
  expect_snapshot(error = TRUE, ssd_distset(dup = c("lnorm", "lnorm")))
  expect_snapshot(error = TRUE, ssd_distset(`bad/name` = "lnorm"))
})

# ---- ssd_define_scenario(dists = ) input contract (task 2.2, 6.4) ----------

test_that("scenario-definition: a bare-vector / plain-list dists aborts naming ssd_distset()", {
  skip_on_cran()
  expect_snapshot(
    error = TRUE,
    ssd_define_scenario(
      ssd_scenario_data(ssddata::ccme_boron),
      nsim = 2L,
      seed = 1L,
      dists = ssdtools::ssd_dists_bcanz()
    )
  )
  expect_snapshot(
    error = TRUE,
    ssd_define_scenario(
      ssd_scenario_data(ssddata::ccme_boron),
      nsim = 2L,
      seed = 1L,
      dists = list(BCANZ = ssdtools::ssd_dists_bcanz())
    )
  )
})

test_that("scenario-definition: fit stores the union; hc stores the named sets", {
  s <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 1L,
    seed = 42L,
    dists = ssd_distset(
      BCANZ = ssdtools::ssd_dists_bcanz(),
      Iwasaki = c("burrIII3", "gamma", "llogis", "lnorm", "weibull")
    )
  )
  expect_identical(
    s$fit$dists,
    sort(unique(c(
      ssdtools::ssd_dists_bcanz(),
      c("burrIII3", "gamma", "llogis", "lnorm", "weibull")
    )))
  )
  expect_named(s$hc$distsets, c("BCANZ", "Iwasaki"))
})

# ---- task table: D sets multiply hc rows by D, not by est_method (6.4) ------

test_that("task-lists: D distribution sets multiply hc rows by D, not by est_method", {
  scenario <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 2L,
    seed = 42L,
    est_method = c("arithmetic", "multi"),
    dists = ssd_distset(
      BCANZ = ssdtools::ssd_dists_bcanz(),
      Iwasaki = c("burrIII3", "gamma", "llogis", "lnorm", "weibull"),
      lnorm = "lnorm"
    )
  )
  fit_tasks <- ssd_scenario_fit_tasks(scenario)
  hc_tasks <- ssd_scenario_hc_tasks(scenario)
  # ci = FALSE: D = 3 distsets per fit task, NOT multiplied by est_method.
  expect_identical(nrow(hc_tasks), nrow(fit_tasks) * 3L)
  expect_setequal(unique(hc_tasks$distset), c("BCANZ", "Iwasaki", "lnorm"))
  expect_false("est_method" %in% names(hc_tasks))
  expect_true("distset" %in% names(hc_tasks))
})

test_that("task-lists: a single-set collection yields one hc row per fit task (ci = FALSE)", {
  scenario <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 2L,
    seed = 42L,
    dists = ssd_distset(BCANZ = ssdtools::ssd_dists_bcanz())
  )
  fit_tasks <- ssd_scenario_fit_tasks(scenario)
  hc_tasks <- ssd_scenario_hc_tasks(scenario)
  expect_identical(nrow(hc_tasks), nrow(fit_tasks))
  expect_identical(unique(hc_tasks$distset), "BCANZ")
})

test_that("task-lists: ci = TRUE fans out over distset x nboot x ci_method x parametric", {
  scenario <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 1L,
    seed = 42L,
    ci = TRUE,
    nboot = c(10L, 100L),
    dists = ssd_distset(
      BCANZ = ssdtools::ssd_dists_bcanz(),
      lnorm = "lnorm"
    )
  )
  fit_tasks <- ssd_scenario_fit_tasks(scenario)
  hc_tasks <- ssd_scenario_hc_tasks(scenario)
  # distset (2) x nboot (2) x ci_method (1) x parametric (1) = 4 per fit task
  expect_identical(nrow(hc_tasks), nrow(fit_tasks) * 4L)
})

# ---- same-seed subset-reuse invariant (task 6.1, the correctness oracle) ----
#
# Per `exploration/distset-subset-invariance.R`: at a fixed cell seed, the hc of
# a pool computed by subsetting one union fit equals the hc of fitting that pool
# directly. Asserted through the shared `hc_data_task_primer()` chokepoint (the
# `dists =` subset argument) against a direct-fit baseline, NOT an old-vs-new
# pipeline equality.

test_that("hazard-concentrations: a pool re-averages the union fit identically to a direct fit, at a fixed seed", {
  data <- ssddata::ccme_boron
  union <- sort(unique(c(
    ssdtools::ssd_dists_bcanz(),
    c("burrIII3", "gamma", "llogis", "lnorm", "weibull")
  )))
  sets <- list(
    BCANZ = ssdtools::ssd_dists_bcanz(),
    Iwasaki = c("burrIII3", "gamma", "llogis", "lnorm", "weibull"),
    lnorm = "lnorm"
  )
  fit_primer <- c(3L, 4L)
  hc_primer <- c(7L, 8L)

  local_dqrng_backend()
  local_dqrng_state(42L, primer = fit_primer)
  union_fit <- ssdtools::ssd_fit_dists(data, dists = union, silent = TRUE)

  for (nm in names(sets)) {
    set <- sets[[nm]]
    for (ci in c(FALSE, TRUE)) {
      # subset path: the primer subsets the union fit to the set's members
      via_subset <- hc_data_task_primer(
        fits = union_fit,
        proportion = 0.05,
        ci = ci,
        nboot = if (ci) 50L else NA_integer_,
        est_method = "multi",
        ci_method = if (ci) "weighted_samples" else NA_character_,
        parametric = if (ci) TRUE else NA,
        seed = 99L,
        primer = hc_primer,
        dists = set,
        samples = ci
      )
      # direct path: fit the set alone at the same fit seed, hc with no subset
      local_dqrng_state(42L, primer = fit_primer)
      direct_fit <- ssdtools::ssd_fit_dists(data, dists = set, silent = TRUE)
      via_direct <- hc_data_task_primer(
        fits = direct_fit,
        proportion = 0.05,
        ci = ci,
        nboot = if (ci) 50L else NA_integer_,
        est_method = "multi",
        ci_method = if (ci) "weighted_samples" else NA_character_,
        parametric = if (ci) TRUE else NA,
        seed = 99L,
        primer = hc_primer,
        dists = NULL,
        samples = ci
      )
      expect_equal(
        via_subset$est,
        via_direct$est,
        info = paste0("set=", nm, " ci=", ci)
      )
      if (ci) {
        expect_equal(
          via_subset[c("se", "lcl", "ucl")],
          via_direct[c("se", "lcl", "ucl")],
          info = paste0("set=", nm, " ci=", ci, " (CI)")
        )
      }
    }
  }
})

# ---- empty subset yields zero rows (task 6.5, primer level) -----------------

test_that("hazard-concentrations: an all-dropped set yields zero hc rows (no abort)", {
  data <- ssddata::ccme_boron
  local_dqrng_backend()
  local_dqrng_state(42L, primer = c(1L, 2L))
  fit <- ssdtools::ssd_fit_dists(
    data,
    dists = c("lnorm", "gamma"),
    silent = TRUE
  )
  # A set whose members are not in the (sub)fit -> zero-length subset -> 0 rows.
  out <- hc_data_task_primer(
    fits = fit,
    proportion = 0.05,
    ci = FALSE,
    nboot = NA_integer_,
    est_method = "multi",
    ci_method = NA_character_,
    parametric = NA,
    seed = 99L,
    primer = c(3L, 4L),
    dists = "weibull"
  )
  expect_identical(nrow(out), 0L)
})
