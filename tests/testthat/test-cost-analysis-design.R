# Design-level cost analysis (ssdsims_design): the rollup seam helpers (promoted
# from the change's exploration/), and the design methods of ssd_analyse_cost(),
# ssd_compare_cost(), and ssd_calibrate_cost_from_run(). The integration tests
# materialise a design's SHARED seed=/layout= tree with the same step runners the
# pipeline uses (no live cluster), so the design methods read real timings.

# ---- promoted seam helpers -------------------------------------------------

test_that("design-cost: breakdowns combine with a leading scenario column", {
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
  expect_identical(names(combined)[[1L]], "scenario")
  expect_identical(combined$scenario, c("s1", "s1", "s2"))
  expect_identical(combined$seconds, c(1, 2, 5))
})

test_that("design-cost: empty and NULL members drop out of the combine", {
  bd <- tibble::tibble(ci_method = "a", nboot = 1L, tasks = 1L, seconds = 1)
  combined <- combine_cost_breakdowns(list(s1 = bd, s2 = bd[0, ], s3 = NULL))
  expect_identical(unique(combined$scenario), "s1")
  expect_identical(nrow(combine_cost_breakdowns(list())), 0L)
  expect_error(combine_cost_breakdowns(list(bd, bd)), "fully named")
})

test_that("design-cost: totals sum, longest is the max, missing members skipped", {
  res <- design_cost_totals(
    member_totals = c(a = 10, b = 20, c = NA),
    member_longests = c(a = 5, b = 8, c = NA)
  )
  expect_s3_class(res$total, "difftime")
  expect_equal(as.numeric(res$total, units = "secs"), 30)
  expect_equal(as.numeric(res$longest, units = "secs"), 8)
  expect_identical(res$n_contributing, 2L)

  none <- design_cost_totals(c(a = NA_real_), c(a = NA_real_))
  expect_true(is.na(as.numeric(none$longest, units = "secs")))
  expect_identical(none$n_contributing, 0L)
})

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

test_that("design-cost: single-host frames pool; mixed hosts abort unless selected", {
  one_host <- list(
    a = make_sweep_frame("cpuX", 1),
    b = make_sweep_frame("cpuX", 1.2)
  )
  cal <- pool_calibration_from_frames(one_host, fixed_addend = 0.05)
  expect_s3_class(cal, "ssdsims_cost_calibration")
  expect_identical(cal$provenance$source, "design-run")
  expect_identical(cal$provenance$cpu, "cpuX")
  expect_setequal(cal$provenance$members, c("a", "b"))

  mixed <- list(a = make_sweep_frame("cpuX"), b = make_sweep_frame("cpuY"))
  expect_error(
    pool_calibration_from_frames(mixed, fixed_addend = 0.05),
    "more than one"
  )
  expect_identical(
    pool_calibration_from_frames(
      mixed,
      fixed_addend = 0.05,
      host = "cpuY"
    )$provenance$cpu,
    "cpuY"
  )
  expect_error(
    pool_calibration_from_frames(mixed, fixed_addend = 0.05, host = "cpuZ"),
    "not among"
  )
})

test_that("design-cost: pooling draws no random numbers", {
  withr::local_seed(1)
  before <- .Random.seed
  pool_calibration_from_frames(
    list(a = make_sweep_frame("cpuX")),
    fixed_addend = 0.05
  )
  expect_identical(.Random.seed, before)
})

# ---- integration: a materialised two-member design -------------------------

# Materialise a design's SHARED seed=/layout= tree with the same step runners the
# pipeline drives (per seed group, over the union shard tables), plus the combined
# summary. Mirrors `design_group_targets()` without `targets`.
materialise_design <- function(design, root) {
  seeds <- vapply(design, function(s) s$seed, integer(1L))
  member_summaries <- character()
  for (sd in unique(seeds)) {
    members <- design[seeds == sd]
    ref <- design_reference_scenario(members)
    sroot <- scenario_results_dir(ref, root)
    sample_shards <- union_shards(members, "sample")
    for (i in seq_len(nrow(sample_shards))) {
      tasks <- sample_shards$tasks[[i]]
      ssd_run_sample_step(
        tasks,
        scenario_step_slice(ref, "sample", unique(tasks$dataset)),
        file.path(sroot, "sample")
      )
    }
    fit_shards <- union_shards(members, "fit")
    for (i in seq_len(nrow(fit_shards))) {
      ssd_run_fit_step(
        fit_shards$tasks[[i]],
        scenario_step_slice(ref, "fit"),
        file.path(sroot, "sample"),
        file.path(sroot, "fit")
      )
    }
    hc_shards <- union_shards(members, "hc")
    for (i in seq_len(nrow(hc_shards))) {
      tasks <- hc_shards$tasks[[i]]
      ssd_run_hc_step(
        tasks,
        scenario_step_slice(ref, "hc", distsets = unique(tasks$distset)),
        file.path(sroot, "fit"),
        file.path(sroot, "hc")
      )
    }
    for (nm in names(members)) {
      path <- file.path(root, paste0("summary-", nm, ".parquet"))
      ssd_summarise_member(
        file.path(sroot, "hc"),
        ssd_scenario_hc_tasks(members[[nm]])$hc_id,
        path
      )
      member_summaries[[nm]] <- path
    }
  }
  ssd_summarise_design(member_summaries, file.path(root, "summary.parquet"))
  root
}

design_fixture <- function() {
  data <- ssd_scenario_data(
    d = data.frame(Conc = exp(seq(-1, 2, length.out = 20)))
  )
  ssd_design(
    coarse = ssd_define_scenario(
      data,
      nsim = 1L,
      seed = 42L,
      nrow = c(5L, 6L),
      ci = TRUE,
      nboot = c(10L, 50L),
      dists = ssd_distset(lnorm = "lnorm")
    ),
    dense = ssd_define_scenario(
      data,
      nsim = 1L,
      seed = 42L,
      nrow = c(6L, 7L),
      ci = TRUE,
      nboot = c(10L, 50L),
      dists = ssd_distset(lnorm = "lnorm")
    )
  )
}

test_that("design-cost: analysis aggregates members with a per-scenario breakdown", {
  design <- design_fixture()
  root <- withr::local_tempdir()
  materialise_design(design, root)

  a <- ssd_analyse_cost(design, root = root)
  expect_s3_class(a, "ssdsims_cost_analysis")
  expect_true("scenario" %in% names(a$breakdown))
  expect_setequal(unique(a$breakdown$scenario), c("coarse", "dense"))
  expect_identical(a$provenance$n_contributing, 2L)
  expect_gt(as.numeric(a$total, units = "secs"), 0)

  # The design total equals the sum of the per-member observed totals, computed
  # independently here by filtering the shared tree to each member's ids.
  member_total <- function(member) {
    sroot <- scenario_results_dir(member, root)
    hc <- read_step_timings(
      sroot,
      "hc",
      "hc_id",
      keep_ids = ssd_scenario_hc_tasks(member)$hc_id
    )
    fit <- read_step_timings(
      sroot,
      "fit",
      "fit_id",
      keep_ids = ssd_scenario_fit_tasks(member)$fit_id
    )
    sum(hc$seconds) + sum(fit$seconds)
  }
  expect_equal(
    as.numeric(a$total, units = "secs"),
    member_total(design$coarse) + member_total(design$dense)
  )
})

test_that("design-cost: combined-summary fast path and shard fallback agree", {
  design <- design_fixture()
  root <- withr::local_tempdir()
  materialise_design(design, root)

  fast <- ssd_analyse_cost(design, root = root) # summary.parquet present
  expect_true(file.exists(file.path(root, "summary.parquet")))
  unlink(file.path(root, "summary.parquet"))
  fallback <- ssd_analyse_cost(design, root = root) # forced to shard reads

  expect_equal(
    as.numeric(fast$total, units = "secs"),
    as.numeric(fallback$total, units = "secs")
  )
  expect_equal(fast$breakdown$seconds, fallback$breakdown$seconds)
})

test_that("design-cost: a member with no readable run is excluded and counted", {
  data <- ssd_scenario_data(
    d = data.frame(Conc = exp(seq(-1, 2, length.out = 20)))
  )
  design <- ssd_design(
    ran = ssd_define_scenario(
      data,
      nsim = 1L,
      seed = 42L,
      nrow = 6L,
      ci = TRUE,
      nboot = 10L,
      dists = ssd_distset(lnorm = "lnorm")
    ),
    # A different seed -> its own seed group / tree, which we never materialise.
    unran = ssd_define_scenario(
      data,
      nsim = 1L,
      seed = 99L,
      nrow = 6L,
      ci = TRUE,
      nboot = 10L,
      dists = ssd_distset(lnorm = "lnorm")
    )
  )
  root <- withr::local_tempdir()
  # Materialise only the seed=42 member.
  materialise_design(ssd_design(ran = design$ran), root)

  a <- ssd_analyse_cost(design, root = root)
  expect_identical(a$provenance$n_contributing, 1L)
  expect_setequal(unique(a$breakdown$scenario), "ran")
})

test_that("design-cost: the seed-woven resolver matches the design factory's names", {
  skip_if_not_installed("targets")
  skip_if_not_installed("tarchetypes")
  design <- design_fixture()
  resolved <- cost_design_shard_target_names(design)
  expect_setequal(names(resolved), c("step", "cell", "name"))

  tg <- ssd_design_targets(design)
  collect_target_names <- function(x) {
    if (inherits(x, "tar_target")) {
      return(x$settings$name)
    }
    if (is.list(x)) {
      return(unlist(lapply(x, collect_target_names), use.names = FALSE))
    }
    character()
  }
  factory_names <- collect_target_names(tg)
  factory_shards <- factory_names[grepl("_step_", factory_names)]
  expect_true(all(resolved$name %in% factory_shards))
  # Every seed-woven shard name carries its seed component.
  expect_true(all(grepl("_step_42(_|$)", resolved$name)))
})

test_that("design-cost: the design shard envelope is target seconds minus task durations", {
  skip_if_not_installed("targets")
  skip_if_not_installed("tarchetypes")
  design <- design_fixture()
  root <- withr::local_tempdir()
  materialise_design(design, root)
  names_tbl <- cost_design_shard_target_names(design)
  meta <- tibble::tibble(
    name = c(
      names_tbl$name,
      "summary",
      "summary_coarse",
      "upload_fit_42_d",
      "stray"
    ),
    seconds = c(rep(1000, nrow(names_tbl)), 1, 1, 1, 7)
  )
  env <- design_shard_envelope(design, meta, root)
  expect_equal(
    env$envelope$overhead,
    env$envelope$seconds - env$envelope$task_seconds
  )
  expect_setequal(unique(env$envelope$step), c("fit", "hc"))
  # summary / summary_<name> / upload_<step> excluded; the stray target reported.
  expect_identical(env$unmatched, "stray")
})

test_that("design-cost: compare and recalibrate roll up across members", {
  design <- design_fixture()
  root <- withr::local_tempdir()
  materialise_design(design, root)

  cmp <- ssd_compare_cost(
    design,
    root = root,
    calibration = test_cost_calibration()
  )
  expect_s3_class(cmp, "ssdsims_cost_comparison")
  expect_true(cmp$total_ratio > 0)

  cal <- ssd_calibrate_cost_from_run(design, root = root)
  expect_s3_class(cal, "ssdsims_cost_calibration")
  expect_identical(cal$provenance$source, "design-run")
  expect_gt(
    as.numeric(ssd_estimate_cost(design$coarse, cal)$total, units = "secs"),
    0
  )
})

test_that("design-cost: analysis is read-only (no RNG, no writes)", {
  design <- design_fixture()
  root <- withr::local_tempdir()
  materialise_design(design, root)
  before_files <- list.files(root, recursive = TRUE)
  set.seed(1)
  before_seed <- .Random.seed
  ssd_analyse_cost(design, root = root)
  expect_identical(.Random.seed, before_seed)
  expect_setequal(list.files(root, recursive = TRUE), before_files)
})

# ---- deterministic design print snapshots ----------------------------------

test_that("design-cost: design analysis and comparison print is informative", {
  analysis <- new_ssdsims_cost_analysis(
    total = as.difftime(7261, units = "secs"),
    longest = as.difftime(120, units = "secs"),
    breakdown = tibble::tibble(
      scenario = c("coarse", "coarse", "dense"),
      ci_method = c("weighted_samples", "weighted_samples", "weighted_samples"),
      nboot = c(50L, 10L, 50L),
      tasks = c(2L, 2L, 2L),
      seconds = c(2400, 1261, 3600)
    ),
    fit_seconds = as.difftime(10, units = "secs"),
    hosts = "Test CPU @ 1.0GHz",
    measured = TRUE,
    envelope = NULL,
    provenance = list(source = "design-run", n_contributing = 2L)
  )
  expect_snapshot(print(analysis))

  comparison <- new_ssdsims_cost_comparison(
    predicted = list(
      total = as.difftime(7000, units = "secs"),
      longest = as.difftime(130, units = "secs")
    ),
    observed = analysis,
    total_ratio = 1.04,
    longest_ratio = 0.92
  )
  expect_snapshot(print(comparison))
})
