# ---- combine_cost_breakdowns -----------------------------------------------

test_that("cost-analysis-design: breakdowns combine with a leading scenario column", {
  bd_a <- tibble::tibble(
    ci_method = c("weighted_samples", "weighted_arithmetic_samples"),
    nboot = c(100L, 200L),
    tasks = c(1L, 2L),
    seconds = c(1, 2)
  )
  bd_b <- tibble::tibble(
    ci_method = "weighted_samples",
    nboot = 100L,
    tasks = 3L,
    seconds = 5
  )
  combined <- combine_cost_breakdowns(list(s1 = bd_a, s2 = bd_b))
  expect_identical(names(combined)[[1]], "scenario")
  expect_identical(combined$scenario, c("s1", "s1", "s2"))
  expect_identical(combined$seconds, c(1, 2, 5))
})

test_that("cost-analysis-design: empty and NULL members drop out of the combine", {
  bd <- tibble::tibble(ci_method = "a", nboot = 1L, tasks = 1L, seconds = 1)
  empty <- bd[0, ]
  combined <- combine_cost_breakdowns(list(s1 = bd, s2 = empty, s3 = NULL))
  expect_identical(unique(combined$scenario), "s1")
  expect_identical(base::nrow(combine_cost_breakdowns(list())), 0L)
})

test_that("cost-analysis-design: combine rejects unnamed members", {
  bd <- tibble::tibble(ci_method = "a", nboot = 1L, tasks = 1L, seconds = 1)
  expect_error(combine_cost_breakdowns(list(bd, bd)), "fully named")
})

# ---- design_cost_totals ----------------------------------------------------

test_that("cost-analysis-design: totals sum, longest is the max, missing members skipped", {
  res <- design_cost_totals(
    member_totals = c(a = 10, b = 20, c = NA),
    member_longests = c(a = 5, b = 8, c = NA)
  )
  expect_s3_class(res$total, "difftime")
  expect_equal(as.numeric(res$total, units = "secs"), 30)
  expect_equal(as.numeric(res$longest, units = "secs"), 8)
  expect_identical(res$n_contributing, 2L)
})

test_that("cost-analysis-design: no contributing members gives NA longest and zero count", {
  res <- design_cost_totals(c(a = NA_real_), c(a = NA_real_))
  expect_true(is.na(as.numeric(res$longest, units = "secs")))
  expect_identical(res$n_contributing, 0L)
})

# ---- pool_calibration_from_frames ------------------------------------------

make_sweep_frame <- function(host, scale = 1) {
  grid <- expand.grid(
    nrow = c(5L, 10L, 20L),
    ci_method = ssdtools::ssd_ci_methods(),
    nboot = c(20L, 50L, 100L, 200L),
    stringsAsFactors = FALSE
  )
  grid$time <- scale * (0.01 + 1e-4 * grid$nboot) * (grid$nrow / 10)
  grid$.host <- host
  tibble::as_tibble(grid)
}

test_that("cost-analysis-design: single-host frames pool into a usable calibration", {
  frames <- list(
    a = make_sweep_frame("cpuX", scale = 1),
    b = make_sweep_frame("cpuX", scale = 1.2)
  )
  cal <- pool_calibration_from_frames(frames, fixed_addend = 0.05)
  expect_s3_class(cal, "ssdsims_cost_calibration")
  expect_identical(cal$provenance$source, "design-run")
  expect_identical(cal$provenance$cpu, "cpuX")
  expect_setequal(cal$provenance$members, c("a", "b"))

  scenario <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 1L,
    seed = 42L,
    ci = TRUE,
    nboot = c(100L, 200L),
    nrow = c(5L, 10L),
    ci_method = ssdtools::ssd_ci_methods()
  )
  est <- ssd_estimate_cost(scenario, cal)
  expect_s3_class(est, "ssdsims_cost_estimate")
  expect_gt(as.numeric(est$total, units = "secs"), 0)
})

test_that("cost-analysis-design: mixed hosts abort, host selection disambiguates", {
  frames <- list(
    a = make_sweep_frame("cpuX"),
    b = make_sweep_frame("cpuY")
  )
  expect_error(
    pool_calibration_from_frames(frames, fixed_addend = 0.05),
    "more than one"
  )
  cal <- pool_calibration_from_frames(
    frames,
    fixed_addend = 0.05,
    host = "cpuY"
  )
  expect_identical(cal$provenance$cpu, "cpuY")
  expect_error(
    pool_calibration_from_frames(frames, fixed_addend = 0.05, host = "cpuZ"),
    "not among"
  )
})

test_that("cost-analysis-design: pooling draws no random numbers", {
  frames <- list(a = make_sweep_frame("cpuX"))
  withr::local_seed(1)
  before <- .Random.seed
  pool_calibration_from_frames(frames, fixed_addend = 0.05)
  expect_identical(.Random.seed, before)
})

# ---- design_member_addressing ----------------------------------------------

test_that("cost-analysis-design: member addressing derives scenario= roots and prefixes", {
  scenario <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 1L,
    seed = 42L
  )
  addr <- design_member_addressing(
    list(base = scenario, wide = scenario),
    root = "results"
  )
  expect_identical(addr$scenario, c("base", "wide"))
  expect_identical(addr$prefix, c("base_", "wide_"))
  expect_identical(
    addr$root,
    file.path("results", c("scenario=base", "scenario=wide"))
  )
  expect_match(addr$results_dir[[1]], "scenario=base/layout=")
})

test_that("cost-analysis-design: member addressing rejects non-scenario elements", {
  expect_error(
    design_member_addressing(list(a = 1L)),
    class = "chk_error"
  )
})

# ---- format_design_breakdown -----------------------------------------------

test_that("cost-analysis-design: design breakdown renders per-scenario and totals", {
  bd <- tibble::tibble(
    scenario = c("a", "b"),
    ci_method = c("weighted_samples", "weighted_arithmetic_samples"),
    nboot = c(100L, 200L),
    tasks = c(1L, 2L),
    seconds = c(60, 3600)
  )
  expect_snapshot(cat(
    format_design_breakdown(
      bd,
      total = as.difftime(3660, units = "secs"),
      longest = as.difftime(3600, units = "secs")
    ),
    sep = "\n"
  ))
})
